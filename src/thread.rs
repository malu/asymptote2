use std::{
    cmp::Reverse,
    sync::atomic::{AtomicBool, Ordering},
};

use crate::{
    eval::{eg, Eval, Score, PIECE_VALUES},
    hash::{Hash, Hashes},
    history::History,
    movegen::{
        generate_bishop_moves, generate_king_moves, generate_knight_moves, generate_pawn_moves,
        generate_queen_moves, generate_rook_moves, Move, MoveVec,
    },
    movepick::{MovePicker, MoveType},
    position::Position,
    stop_condition::{StopCondition, TimeManager},
    tt::{self, Entry, TranspositionTable, EXACT_BOUND, LOWER_BOUND, UPPER_BOUND},
    types::{Bitboard, Piece, Side, Square},
};

type Depth = i8;
const MATE_SCORE: Score = 30000;

const MAX_PLY: i16 = 127;

#[derive(Clone)]
pub struct Thread<'a> {
    eval: Eval,
    stack: Vec<Frame>,
    pub nodes: u64,
    tb_hits: u64,
    max_ply: usize,
    time_manager: TimeManager,
    pv: Vec<MoveVec>,
    hashes: Hashes,
    tt: &'a TranspositionTable,
    history: History,
    lmr: [[i8; 64]; 64],
    repetitions: Repetitions,
    start: std::time::Instant,

    prober: Option<fathom_syzygy::Prober<'a>>,

    pub id: u16,
    pub threads_total: u16,
    abort: &'a AtomicBool,
}

impl<'a> Thread<'a> {
    pub fn from_position_and_moves(
        position: &Position,
        moves: &[Move],
        tt: &'a TranspositionTable,
        abort: &'a AtomicBool,
        prober: Option<fathom_syzygy::Prober<'a>>,
    ) -> Self {
        let eval = Eval::from(&Position::default());
        let stack = vec![Frame::default(); MAX_PLY as usize + 1];

        let mut lmr = [[0; 64]; 64];
        for d in 2..64 {
            for m in 1..64 {
                lmr[d][m] = ((d as f32).ln() * (m as f32).ln() / 3.) as i8;
            }
        }

        let mut thread = Self {
            eval,
            stack,
            nodes: 0,
            tb_hits: 0,
            max_ply: 0,
            time_manager: TimeManager::new(position.side_to_move(), StopCondition::Infinite),
            pv: vec![MoveVec::new(); 128],
            hashes: Hashes::new(),
            tt,
            history: History::default(),
            lmr,
            repetitions: Repetitions::new(),
            start: std::time::Instant::now(),

            prober,

            id: 0,
            threads_total: 1,
            abort,
        };
        thread.update_with_position_and_moves(position, moves);
        thread
    }

    fn update_with_position_and_moves(&mut self, position: &Position, moves: &[Move]) {
        let mut position = position.clone();
        let mut hash = self.hashes.compute_hash_for_position(&position);
        for mov in moves {
            position = position.make_move(*mov);
            hash = self.hashes.compute_hash_for_position(&position);
            if mov.piece == Piece::Pawn || mov.capture.is_some() {
                self.repetitions.push_irreversible(hash.all);
            } else {
                self.repetitions.push_reversible(hash.all);
            }
        }

        self.eval = Eval::from(&position);
        self.time_manager = TimeManager::new(position.side_to_move(), StopCondition::Infinite);
        self.stack[0] = Frame {
            position,
            hash: hash.all,
            pk_hash: hash.pawn_king,
            current_move: None,
            killer_moves: [None, None],
        };
    }

    pub fn get_move(
        &mut self,
        stop_condition: StopCondition,
        root_prober: Option<fathom_syzygy::RootProber<'_>>,
    ) -> Option<Move> {
        self.time_manager.start(stop_condition);
        self.start = std::time::Instant::now();

        self.iterative_deepening(root_prober);

        self.pv[0].get(0).copied()
    }

    fn iterative_deepening(&mut self, mut root_prober: Option<fathom_syzygy::RootProber<'_>>) {
        let ply = 0;
        if self.should_end_now() {
            return;
        }

        if self.repetitions.has_occured_n_times(self.hash(ply), 3) {
            return;
        }

        if self.position(ply).half_move_clock() == 100 {
            if is_checkmate(self.position(ply)) {
                return;
            } else {
                return;
            }
        }

        let mut moves = {
            let mut moves = Vec::new();
            let entry = self
                .tt
                .get(self.hash(ply), self.position(ply))
                .filter(|entry| self.position(ply).is_move_pseudo_legal(entry.best_move));

            // Generate all legal moves
            let mut movepicker =
                MovePicker::new(entry.map(|entry| entry.best_move), Default::default());
            while let Some((_, mov)) = movepicker.next(&self.position(ply), &self.history) {
                if !self.position(ply).is_move_legal(mov) {
                    continue;
                }

                moves.push(mov.into());
            }
            moves
        };

        if let Some(result) = root_prober
            .as_mut()
            .and_then(|prober| prober.probe(&self.position(ply).into()))
        {
            if let Some(mov) = Move::from_syzygy_move(result.best_move, self.position(ply)) {
                self.pv[ply as usize].clear();
                self.pv[ply as usize].push(mov);
                return;
            }
        }

        for depth in 1..=127 {
            if !self.should_start_another_iteration(depth) {
                break;
            }

            if depth > 4 && moves.len() == 1 {
                break;
            }

            match self.search_root(Window::full(), depth, &mut moves) {
                SearchResult::Finished((score, mov)) | SearchResult::Partial((score, mov))
                    if self.id == 0 =>
                {
                    let elapsed = self.start.elapsed().as_micros();
                    let score = if score >= mate_in(MAX_PLY) {
                        format!("mate {}", (MATE_SCORE - score + 1) / 2)
                    } else if score <= mated_in(MAX_PLY) {
                        format!("mate -{}", (MATE_SCORE + score) / 2)
                    } else {
                        format!("cp {}", score)
                    };
                    let est_nodes = u128::from(self.nodes) * u128::from(self.threads_total);
                    print!(
                        "info depth {depth} seldepth {seldepth} score {score} time {elapsed} nodes {nodes} nps {nps} tbhits {tbhits} hashfull {hashfull} pv ",
                        seldepth = self.max_ply,
                        nodes = est_nodes,
                        tbhits = self.tb_hits,
                        elapsed = elapsed / 1000,
                        nps = 1000000 * est_nodes / std::cmp::max(1, elapsed),
                        hashfull = self.tt.usage_permille(),
                    );
                    for &mov in self.pv[0].iter() {
                        if let Some(promotion) = mov.promotion {
                            print!("{}{}{} ", mov.from, mov.to, promotion)
                        } else {
                            print!("{}{} ", mov.from, mov.to);
                        }
                    }
                    println!();

                    if self.pv[0].get(0).is_none() {
                        self.pv[0].push(mov);
                    }
                }
                _ => {}
            }
        }
    }

    fn search_root(
        &mut self,
        mut window: Window,
        depth: Depth,
        moves: &mut [RootMove],
    ) -> SearchResult<(Score, Move)> {
        let depth = std::cmp::max(1, depth);
        let ply = 0;

        let mut searched_moves = 0;
        let mut best_move_index = None;
        let mut best_score = mated_in(ply);
        let mut result = SearchResult::Aborted;

        for (i, root_move) in moves.iter_mut().enumerate() {
            let mov = root_move.mov;
            // Move legallity was already checked
            let mut extension = 0;

            self.make_move(ply, mov);
            if self.position(ply + 1).in_check() && depth == 1 {
                extension = 1;
            }
            let mut score;

            let nodes_before = self.nodes;
            if searched_moves == 0 {
                score = self
                    .search(ply + 1, -window, depth + extension - 1)
                    .map(|v| -v)
            } else {
                score = self
                    .search(ply + 1, -(window.zero()), depth + extension - 1)
                    .map(|v| -v);

                if score
                    .finished()
                    .filter(|score| score > &window.alpha())
                    .is_some()
                    && !window.is_zero()
                {
                    score = self
                        .search(ply + 1, -window, depth + extension - 1)
                        .map(|v| -v);
                }
            }
            root_move.subtree_size = self.nodes - nodes_before;

            self.unmake_move(ply);
            searched_moves += 1;

            match score {
                SearchResult::Aborted | SearchResult::Partial(_) => {
                    if let Some(i) = best_move_index {
                        moves[0..i + 1].rotate_right(1);
                        moves[1..].sort_by_key(|root_move| Reverse(root_move.subtree_size));
                    }

                    return result;
                }
                SearchResult::Finished(score) => {
                    if score > best_score {
                        best_move_index = Some(i);
                        best_score = score;
                        result = SearchResult::Partial((score, mov));

                        if !window.is_zero() && score < window.beta() {
                            self.copy_pv(ply, mov);
                        }
                    }

                    if score > window.alpha() {
                        window.increase_alpha(score);
                    }

                    if score >= window.beta() {
                        self.pv[0].clear();
                        self.pv[0].push(mov);
                        break;
                    }
                }
            }
        }

        let result = result.finish();

        if let Some(i) = best_move_index {
            moves[0..i + 1].rotate_right(1);
            moves[1..].sort_by_key(|root_move| Reverse(root_move.subtree_size));
        }

        result
    }

    fn search(&mut self, ply: i16, mut window: Window, depth: Depth) -> SearchResult<Score> {
        if self.should_end_now() {
            return SearchResult::Aborted;
        }

        if self.repetitions.has_occured_n_times(self.hash(ply), 3) {
            return SearchResult::Finished(0);
        }

        if let Some(prober) = self.prober {
            let position = self.position(ply);
            if position.half_move_clock() == 0
                && position.en_passant_sq().is_none()
                && position.castling().is_none()
                && (window.is_zero() || depth <= 0)
            {
                let piece_count = position.all_pieces().popcount();
                let max_pieces = prober
                    .max_pieces()
                    .try_into()
                    .expect("max syzygy pieces fits into usize");
                if piece_count <= max_pieces {
                    let pos = position.into();
                    let tb_score = prober.probe(&pos).map(|wdl| match wdl {
                        fathom_syzygy::Wdl::Win => tb_win_in(ply),
                        fathom_syzygy::Wdl::Loss => tb_loss_in(ply),
                        _ => 0,
                    });

                    if let Some(tb_score) = tb_score {
                        self.tb_hits += 1;
                        return SearchResult::Finished(tb_score);
                    }
                }
            }
        }

        if self.position(ply).half_move_clock() == 100 {
            if is_checkmate(self.position(ply)) {
                return SearchResult::Finished(mated_in(ply));
            } else {
                return SearchResult::Finished(0);
            }
        }

        if depth <= 0 {
            return self.qsearch(ply, window);
        }

        if ply == MAX_PLY {
            return SearchResult::Finished(self.evaluate_current_position(ply));
        }

        {
            // Mate distance pruning
            // If our know best result (alpha) is better than any mate we could achieve, just
            // return alpha.
            let alpha = std::cmp::max(window.alpha(), mated_in(ply));
            let beta = std::cmp::min(window.beta(), mate_in(ply));
            if alpha >= beta {
                return SearchResult::Finished(alpha);
            }
        }

        let entry = self
            .tt
            .get(self.hash(ply), self.position(ply))
            .filter(|entry| self.position(ply).is_move_pseudo_legal(entry.best_move))
            .filter(|entry| self.position(ply).is_move_legal(entry.best_move));
        if let Some(entry) = entry {
            let score = entry.score.to_score(ply);
            if entry.depth >= depth && window.is_zero() {
                if entry.bound & LOWER_BOUND > 0 && score >= window.beta() {
                    return SearchResult::Finished(score);
                } else if entry.bound & UPPER_BOUND > 0 && score <= window.alpha() {
                    return SearchResult::Finished(score);
                }
            }

            if depth == 1 && entry.bound & EXACT_BOUND == EXACT_BOUND {
                self.copy_pv(ply, entry.best_move);
                return SearchResult::Finished(score);
            }
        }

        let eval = self.evaluate_current_position(ply);

        // Null move pruning
        if depth >= 6
            && !self.position(ply).in_check()
            && window.is_zero()
            && eval >= window.beta()
            && self
                .eval
                .non_pawn_material(self.position(ply).side_to_move())
                > 0
        {
            let r = 2;
            self.make_nullmove(ply);
            let result = self.search(ply + 1, -window, depth - 1 - r).map(|v| -v);
            self.unmake_nullmove();
            if let SearchResult::Finished(score) = result {
                if score >= window.beta() {
                    return result;
                }
            }
        }

        // ProbCut
        if depth >= 6 && !self.position(ply).in_check() && window.is_zero() {
            let mut moves = MoveVec::new();
            let position = self.position(ply);
            let side = position.side_to_move();
            generate_pawn_moves(
                position,
                position.pieces(!side)
                    | side.promotion_rank().as_bb()
                    | position
                        .en_passant_sq()
                        .map_or(Bitboard::empty(), Square::as_bb),
                &mut moves,
            );
            generate_knight_moves(position, position.pieces(!side), &mut moves);
            generate_bishop_moves(position, position.pieces(!side), &mut moves);
            generate_rook_moves(position, position.pieces(!side), &mut moves);
            generate_queen_moves(position, position.pieces(!side), &mut moves);
            generate_king_moves(position, position.pieces(!side), &mut moves);

            let probcut_window = Window::Zero {
                alpha: window.alpha() + 200,
            };

            for mov in moves {
                if !self.position(ply).is_move_legal(mov) {
                    continue;
                }

                if !self.position(ply).see_ge(mov, 0) {
                    continue;
                }

                self.make_move(ply, mov);
                let score = self.search(ply + 1, -probcut_window, depth - 3).map(|v| -v);
                self.unmake_move(ply);
                let SearchResult::Finished(score) = score else {
                    return SearchResult::Aborted;
                };

                if score >= probcut_window.beta() {
                    return SearchResult::Finished(window.beta());
                }
            }
        }

        // Static beta pruning
        if depth == 1
            && window.is_zero()
            && !self.position(ply).in_check()
            && eval >= window.beta() + 100
        {
            return SearchResult::Finished(window.beta());
        }

        let killer_moves = self.frame(ply).killer_moves;
        let mut movepicker = MovePicker::new(entry.map(|entry| entry.best_move), killer_moves);

        let mut failed_quiet_moves = MoveVec::new();

        let mut searched_moves = 0;
        let mut bound = UPPER_BOUND;
        let mut best_move = None;
        let mut best_score = mated_in(ply);
        let mut result = SearchResult::Finished(best_score);
        while let Some((mtype, mov)) = movepicker.next(&self.position(ply), &self.history) {
            if !self.position(ply).is_move_legal(mov) {
                continue;
            }

            self.make_move(ply, mov);

            let mut extension = 0;
            if self.position(ply + 1).in_check() {
                extension = 1;
            }

            let mut reduction = 0;
            if depth > 2 && mtype == MoveType::Quiet && best_score > mated_in(MAX_PLY) {
                reduction +=
                    self.lmr[std::cmp::min(searched_moves, 63)][std::cmp::min(depth, 63) as usize];

                reduction = reduction.clamp(0, depth - 1);
            }

            let mut score;
            if searched_moves == 0 {
                score = self
                    .search(ply + 1, -window, depth + extension - 1)
                    .map(|v| -v)
            } else {
                score = self
                    .search(ply + 1, -(window.zero()), depth + extension - reduction - 1)
                    .map(|v| -v);

                if score
                    .finished()
                    .filter(|score| score > &window.alpha())
                    .is_some()
                {
                    score = self
                        .search(ply + 1, -(window.zero()), depth + extension - 1)
                        .map(|v| -v);
                }

                if score
                    .finished()
                    .filter(|score| score > &window.alpha())
                    .is_some()
                    && !window.is_zero()
                {
                    score = self
                        .search(ply + 1, -window, depth + extension - 1)
                        .map(|v| -v);
                }
            }

            self.unmake_move(ply);
            searched_moves += 1;

            match score {
                SearchResult::Aborted => {
                    return SearchResult::Aborted;
                }
                SearchResult::Partial(_score) => {
                    return SearchResult::Aborted;
                }
                SearchResult::Finished(score) => {
                    if score > best_score {
                        best_move = Some(mov);
                        best_score = score;
                        result = SearchResult::Finished(best_score);

                        if !window.is_zero() && score < window.beta() {
                            self.copy_pv(ply, mov);
                        }
                    }

                    if score > window.alpha() {
                        window.increase_alpha(score);

                        if !window.is_zero() {
                            bound = EXACT_BOUND;
                        }
                    }

                    if score >= window.beta() {
                        if mtype == MoveType::Killer || mtype == MoveType::Quiet {
                            self.store_killer_move(ply, mov);
                            self.update_history(
                                self.position(ply).side_to_move(),
                                depth,
                                mov,
                                &failed_quiet_moves,
                            );
                        }

                        bound = LOWER_BOUND;
                        break;
                    } else {
                        if mtype == MoveType::Quiet {
                            failed_quiet_moves.push(mov);
                        }
                    }
                }
            }
        }

        if searched_moves == 0 {
            if self.position(ply).in_check() {
                result = SearchResult::Finished(mated_in(ply));
            } else {
                result = SearchResult::Finished(0);
            }
        }

        if let Some(best_move) = best_move {
            let hash = self.hash(ply);
            self.tt.insert(Entry {
                best_move,
                hash,
                score: tt::Score::from_score(best_score, ply),
                bound,
                depth,
            });
        }

        result
    }

    fn qsearch(&mut self, ply: i16, mut window: Window) -> SearchResult<Score> {
        if ply == MAX_PLY {
            return SearchResult::Finished(self.evaluate_current_position(ply));
        }

        if self.should_end_now() {
            return SearchResult::Aborted;
        }

        if self.repetitions.has_occured_n_times(self.hash(ply), 3) {
            return SearchResult::Finished(0);
        }

        if self.position(ply).half_move_clock() == 100 {
            if is_checkmate(self.position(ply)) {
                return SearchResult::Finished(mated_in(ply));
            } else {
                return SearchResult::Finished(0);
            }
        }

        let mut best_move = None;
        let mut best_score = mated_in(ply);
        let mut moves = MoveVec::new();
        let mut result;

        if self.position(ply).in_check() {
            let position = self.position(ply);
            let side = position.side_to_move();

            generate_pawn_moves(position, !position.pieces(side), &mut moves);
            generate_knight_moves(position, !position.pieces(side), &mut moves);
            generate_bishop_moves(position, !position.pieces(side), &mut moves);
            generate_rook_moves(position, !position.pieces(side), &mut moves);
            generate_queen_moves(position, !position.pieces(side), &mut moves);
            generate_king_moves(position, !position.pieces(side), &mut moves);

            result = SearchResult::Finished(best_score);
        } else {
            let eval = self.evaluate_current_position(ply);

            let position = self.position(ply);
            let side = position.side_to_move();

            best_score = eval;
            if eval > window.alpha() {
                window.increase_alpha(eval);
            }
            result = SearchResult::Finished(best_score);

            if eval >= window.beta() {
                return result;
            }

            generate_pawn_moves(
                position,
                position.pieces(!side)
                    | side.promotion_rank().as_bb()
                    | position
                        .en_passant_sq()
                        .map_or(Bitboard::empty(), Square::as_bb),
                &mut moves,
            );
            generate_knight_moves(position, position.pieces(!side), &mut moves);
            generate_bishop_moves(position, position.pieces(!side), &mut moves);
            generate_rook_moves(position, position.pieces(!side), &mut moves);
            generate_queen_moves(position, position.pieces(!side), &mut moves);
            generate_king_moves(position, position.pieces(!side), &mut moves);

            let mut best_noisy_value = 0;
            moves.retain(|mov| {
                let good = position.see_ge(*mov, 0);
                let victim = mov.capture.map_or(0, |p| eg(PIECE_VALUES[p]));
                let promotion = mov.promotion.map_or(0, |p| eg(PIECE_VALUES[p]));
                if good {
                    best_noisy_value = std::cmp::max(best_noisy_value, victim + promotion);
                }

                good
            });

            if eval + 200 + best_noisy_value < window.alpha() {
                return SearchResult::Finished(window.alpha());
            }
        }

        for &mov in moves.iter() {
            if !self.position(ply).is_move_legal(mov) {
                continue;
            }

            self.make_move(ply, mov);
            let score = self.qsearch(ply + 1, -window).map(|v| -v);
            self.unmake_move(ply);

            match score {
                SearchResult::Aborted => {
                    return SearchResult::Aborted;
                }
                SearchResult::Partial(_score) => {
                    return SearchResult::Aborted;
                }
                SearchResult::Finished(score) => {
                    if score > best_score {
                        best_move = Some(mov);
                        best_score = score;
                        result = SearchResult::Finished(best_score);
                    }

                    if score > window.alpha() {
                        window.increase_alpha(score)
                    }

                    if score >= window.beta() {
                        break;
                    }
                }
            }
        }
        let _ = best_move;

        result
    }

    fn should_start_another_iteration(&self, depth: Depth) -> bool {
        let abort = self.abort.load(Ordering::Relaxed);
        if abort {
            return false;
        }

        let start_another = self.time_manager.should_start_another_iteration(depth);
        if !start_another {
            self.abort.store(true, Ordering::Relaxed);
        }

        start_another
    }

    fn should_end_now(&self) -> bool {
        if self.nodes % 1024 != 0 {
            return false;
        }

        let should_end = self.abort.load(Ordering::Relaxed) || self.time_manager.should_end_now();
        if should_end {
            self.abort.store(true, Ordering::Relaxed);
        }

        should_end
    }

    fn frame(&self, ply: i16) -> &Frame {
        &self.stack[ply as usize]
    }

    fn position(&self, ply: i16) -> &Position {
        &self.frame(ply).position
    }

    fn hash(&self, ply: i16) -> Hash {
        self.frame(ply).hash
    }

    fn evaluate_current_position(&mut self, ply: i16) -> Score {
        let frame = &self.stack[ply as usize];
        self.eval.evaluate(&frame.position, frame.pk_hash)
    }

    fn make_move(&mut self, ply: i16, mov: Move) {
        self.nodes += 1;

        let frame = &mut self.stack[ply as usize];
        frame.current_move = Some(mov);

        let hash_diff = self.hashes.make_move(&frame.position, mov);
        let hash = frame.hash ^ hash_diff.all;
        let pk_hash = frame.pk_hash ^ hash_diff.pawn_king;
        self.eval.make_move(&frame.position, mov);
        let position = frame.position.make_move(mov);

        if let Some(next_frame) = self.stack.get_mut(1 + ply as usize) {
            next_frame.position = position;
            next_frame.hash = hash;
            next_frame.pk_hash = pk_hash;
            next_frame.current_move = None;
        }

        if let Some(next_frame) = self.stack.get_mut(2 + ply as usize) {
            next_frame.killer_moves = [None, None];
        }

        if mov.piece == Piece::Pawn || mov.capture.is_some() {
            self.repetitions.push_irreversible(hash);
        } else {
            self.repetitions.push_reversible(hash);
        }

        self.max_ply = std::cmp::max(self.max_ply, 1 + ply as usize);
        self.pv[1 + ply as usize].clear();
    }

    fn unmake_move(&mut self, ply: i16) {
        let frame = &mut self.stack[ply as usize];
        let mov = frame.current_move.take().unwrap();
        self.eval.unmake_move(&frame.position, mov);
        self.repetitions.pop();
    }

    fn make_nullmove(&mut self, ply: i16) {
        self.nodes += 1;

        let frame = &mut self.stack[ply as usize];
        frame.current_move = None;

        let hash_diff = self.hashes.make_nullmove(&frame.position);
        let hash = frame.hash ^ hash_diff.all;
        let pk_hash = frame.pk_hash ^ hash_diff.pawn_king;
        self.eval.make_nullmove();
        let position = frame.position.make_nullmove();

        if let Some(next_frame) = self.stack.get_mut(1 + ply as usize) {
            next_frame.position = position;
            next_frame.hash = hash;
            next_frame.pk_hash = pk_hash;
            next_frame.current_move = None;
        }

        if let Some(next_frame) = self.stack.get_mut(2 + ply as usize) {
            next_frame.killer_moves = [None, None];
        }

        self.repetitions.push_reversible(hash);

        self.max_ply = std::cmp::max(self.max_ply, 1 + ply as usize);
        self.pv[1 + ply as usize].clear();
    }

    fn unmake_nullmove(&mut self) {
        self.eval.unmake_nullmove();
        self.repetitions.pop();
    }

    fn copy_pv(&mut self, ply: i16, mov: Move) {
        let ply = ply as usize;
        if let [to, from] = &mut self.pv[ply..=ply + 1] {
            to.clear();
            to.push(mov);
            to.extend(from.iter().copied());
        }
    }

    fn store_killer_move(&mut self, ply: i16, mov: Move) {
        let frame = &mut self.stack[ply as usize];
        let killers = &mut frame.killer_moves;
        if killers[0] != Some(mov) {
            killers[1] = killers[0];
            killers[0] = Some(mov);
        }
    }

    fn update_history(
        &mut self,
        side: Side,
        depth: i8,
        best_move: Move,
        failed_quiet_moves: &[Move],
    ) {
        self.history.decrease_score(side, failed_quiet_moves, depth);
        self.history.increase_score(side, best_move, depth);
    }
}

#[derive(Clone, Debug, Default)]
struct Frame {
    position: Position,
    hash: Hash,
    pk_hash: Hash,
    current_move: Option<Move>,
    killer_moves: [Option<Move>; 2],
}

#[derive(Copy, Clone, Debug)]
enum Window {
    Pv { alpha: Score, beta: Score },
    Zero { alpha: Score },
}

impl Window {
    fn full() -> Self {
        Window::Pv {
            alpha: -MATE_SCORE,
            beta: MATE_SCORE,
        }
    }

    fn alpha(self) -> Score {
        match self {
            Window::Pv { alpha, .. } => alpha,
            Window::Zero { alpha } => alpha,
        }
    }

    fn beta(self) -> Score {
        match self {
            Window::Pv { beta, .. } => beta,
            Window::Zero { alpha } => alpha + 1,
        }
    }

    fn zero(self) -> Window {
        match self {
            Window::Pv { alpha, .. } => Window::Zero { alpha },
            Window::Zero { .. } => self,
        }
    }

    fn is_zero(self) -> bool {
        matches!(self, Window::Zero { .. })
    }

    fn increase_alpha(&mut self, new_alpha: Score) {
        match self {
            Window::Pv { ref mut alpha, .. } if new_alpha > *alpha => *alpha = new_alpha,
            _ => {}
        }
    }
}

impl std::ops::Neg for Window {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Window::Pv { alpha, beta } => Window::Pv {
                alpha: -beta,
                beta: -alpha,
            },
            Window::Zero { alpha } => Window::Zero { alpha: -alpha - 1 },
        }
    }
}

pub fn mated_in(ply: i16) -> Score {
    -MATE_SCORE + ply
}

pub fn mate_in(ply: i16) -> Score {
    MATE_SCORE - ply
}

pub fn tb_loss_in(ply: i16) -> Score {
    -MATE_SCORE + MAX_PLY + ply
}

pub fn tb_win_in(ply: i16) -> Score {
    MATE_SCORE - MAX_PLY - ply
}

#[derive(Copy, Clone)]
enum SearchResult<T> {
    Finished(T),
    Partial(T),
    Aborted,
}

impl<T> SearchResult<T> {
    fn finished(self) -> Option<T> {
        match self {
            SearchResult::Finished(t) => Some(t),
            _ => None,
        }
    }

    fn map<U, F>(self, f: F) -> SearchResult<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            SearchResult::Finished(v) => SearchResult::Finished(f(v)),
            SearchResult::Partial(v) => SearchResult::Partial(f(v)),
            SearchResult::Aborted => SearchResult::Aborted,
        }
    }

    fn finish(self) -> SearchResult<T> {
        match self {
            SearchResult::Finished(_) => self,
            SearchResult::Partial(v) => SearchResult::Finished(v),
            SearchResult::Aborted => self,
        }
    }
}

#[derive(Clone)]
struct Repetitions {
    hashes: Vec<Hash>,
    segments: Vec<usize>,
}

impl Repetitions {
    fn new() -> Self {
        Self {
            hashes: vec![],
            segments: vec![0],
        }
    }

    fn pop(&mut self) {
        self.hashes.pop();
        if self.segments.last().copied() == Some(1) {
            self.segments.pop();
        } else if let Some(segment) = self.segments.last_mut() {
            *segment -= 1;
        }
    }

    fn push_reversible(&mut self, hash: Hash) {
        self.hashes.push(hash);
        if let Some(segment) = self.segments.last_mut() {
            *segment += 1;
        }
    }

    fn push_irreversible(&mut self, hash: Hash) {
        self.hashes.push(hash);
        self.segments.push(1);
    }

    fn has_occured_n_times(&self, hash: Hash, count: usize) -> bool {
        if count == 0 {
            return true;
        }

        let segment = self.segments.last().copied().unwrap_or(0);

        self.hashes
            .iter()
            .rev()
            .take(segment)
            .step_by(2)
            .filter(|h| **h == hash)
            .nth(count - 1)
            .is_some()
    }
}

fn is_checkmate(position: &Position) -> bool {
    let mut moves = MoveVec::new();
    let side = position.side_to_move();

    generate_pawn_moves(position, !position.pieces(side), &mut moves);
    generate_knight_moves(position, !position.pieces(side), &mut moves);
    generate_bishop_moves(position, !position.pieces(side), &mut moves);
    generate_rook_moves(position, !position.pieces(side), &mut moves);
    generate_queen_moves(position, !position.pieces(side), &mut moves);
    generate_king_moves(position, !position.pieces(side), &mut moves);

    !moves.iter().any(|mov| position.is_move_legal(*mov))
}

struct RootMove {
    mov: Move,
    subtree_size: u64,
}

impl From<Move> for RootMove {
    fn from(mov: Move) -> Self {
        Self {
            mov,
            subtree_size: 0,
        }
    }
}
