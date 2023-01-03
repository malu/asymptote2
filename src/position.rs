use fathom_syzygy as fathom;
use std::{cmp::Ordering, fmt::Write};

use crate::{
    eval::Score,
    movegen::{
        bishop_from, generate_bishop_moves, generate_king_moves, generate_knight_moves,
        generate_pawn_moves, generate_queen_moves, generate_rook_moves, rook_from, Move, MoveVec,
        KING_ATTACKS, KNIGHT_ATTACKS,
    },
    types::{Bitboard, File, Piece, PieceMap, Rank, Side, Square, SquareMap},
};

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct Castling(u8);

impl Castling {
    pub fn is_none(self) -> bool {
        self.0 == 0
    }

    pub fn set_white_kingside(&mut self, set: bool) {
        if set {
            self.0 |= 0b0001;
        } else {
            self.0 &= !0b0001;
        }
    }

    pub fn set_white_queenside(&mut self, set: bool) {
        if set {
            self.0 |= 0b0010;
        } else {
            self.0 &= !0b0010;
        }
    }

    pub fn set_black_kingside(&mut self, set: bool) {
        if set {
            self.0 |= 0b0100;
        } else {
            self.0 &= !0b0100;
        }
    }

    pub fn set_black_queenside(&mut self, set: bool) {
        if set {
            self.0 |= 0b1000;
        } else {
            self.0 &= !0b1000;
        }
    }

    pub fn white_kingside(&self) -> bool {
        self.0 & 0b0001 > 0
    }

    pub fn white_queenside(&self) -> bool {
        self.0 & 0b0010 > 0
    }

    pub fn black_kingside(&self) -> bool {
        self.0 & 0b0100 > 0
    }

    pub fn black_queenside(&self) -> bool {
        self.0 & 0b1000 > 0
    }

    pub fn mask(&mut self, mask: u8) {
        self.0 &= mask;
    }
}

#[rustfmt::skip]
pub static CASTLING_MASKS: SquareMap<u8> = SquareMap::new([
    // Rank 1
    0xFD, 0xFF, 0xFF, 0xFF, 0xFC, 0xFF, 0xFF, 0xFE,
    // Rank 2
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    // Rank 3
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    // Rank 4
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    // Rank 5
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    // Rank 6
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    // Rank 7
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    // Rank 8
    0xF7, 0xFF, 0xFF, 0xFF, 0xF3, 0xFF, 0xFF, 0xFB,
]);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Position {
    pieces: PieceMap<Bitboard>,
    king_sq: [Square; 2],
    white_pieces: Bitboard,
    black_pieces: Bitboard,
    all_pieces: Bitboard,
    side_to_move: Side,
    castling: Castling,
    en_passant_file: Option<File>,

    half_move_clock: u8,
    full_move_clock: usize,

    // TODO is this useful?
    attacks: [Attacks; 2],
}

impl Position {
    pub fn from_fen(fen: &str) -> Option<Position> {
        let mut pawns = Bitboard::default();
        let mut knights = Bitboard::default();
        let mut bishops = Bitboard::default();
        let mut rooks = Bitboard::default();
        let mut queens = Bitboard::default();
        let mut kings = Bitboard::default();
        let mut white_pieces = Bitboard::default();
        let mut black_pieces = Bitboard::default();

        let mut parts = fen.split(' ');

        let board = parts.next()?;
        let mut file: u8 = 0;
        let mut rank: u8 = 7;
        for c in board.chars() {
            let piece = match c {
                'p' | 'P' => Piece::Pawn,
                'n' | 'N' => Piece::Knight,
                'b' | 'B' => Piece::Bishop,
                'r' | 'R' => Piece::Rook,
                'q' | 'Q' => Piece::Queen,
                'k' | 'K' => Piece::King,
                '1'..='8' => {
                    file += c.to_digit(10)? as u8;
                    continue;
                }
                '/' => {
                    file = 0;
                    rank -= 1;
                    continue;
                }
                _ => continue,
            };

            let white = c.is_uppercase();
            let square = Square::from_file_rank(File::new(file)?, Rank::new(rank)?);
            file += 1;

            match piece {
                Piece::Pawn => pawns |= square,
                Piece::Knight => knights |= square,
                Piece::Bishop => bishops |= square,
                Piece::Rook => rooks |= square,
                Piece::Queen => queens |= square,
                Piece::King => kings |= square,
            }

            if white {
                white_pieces |= square;
            } else {
                black_pieces |= square;
            }
        }

        let side_to_move = match parts.next().unwrap_or("w") {
            "w" => Side::White,
            "b" => Side::Black,
            _ => return None,
        };

        let castling = {
            let castling = parts.next().unwrap_or_default();
            let mut result = Castling::default();
            if castling.contains('K') {
                result.set_white_kingside(true);
            }

            if castling.contains('Q') {
                result.set_white_queenside(true);
            }

            if castling.contains('k') {
                result.set_black_kingside(true);
            }

            if castling.contains('q') {
                result.set_black_queenside(true);
            }

            result
        };

        let en_passant_file = match parts.next().and_then(|ep| ep.chars().next()).unwrap_or('-') {
            'a' => Some(File::A),
            'b' => Some(File::B),
            'c' => Some(File::C),
            'd' => Some(File::D),
            'e' => Some(File::E),
            'f' => Some(File::F),
            'g' => Some(File::G),
            'h' => Some(File::H),
            _ => None,
        };

        let half_move_clock = parts
            .next()
            .and_then(|half| half.parse::<u8>().ok())
            .unwrap_or(0);
        let full_move_clock = parts
            .next()
            .and_then(|full| full.parse::<usize>().ok())
            .unwrap_or(1);

        let king_sq = [
            (kings & white_pieces).into_iter().next()?,
            (kings & black_pieces).into_iter().next()?,
        ];

        let attacks = Default::default();

        let mut pos = Position {
            pieces: PieceMap::new([pawns, knights, bishops, rooks, queens, kings]),
            king_sq,
            white_pieces,
            black_pieces,
            all_pieces: white_pieces | black_pieces,
            side_to_move,
            en_passant_file,
            castling,
            half_move_clock,
            full_move_clock,
            attacks,
        };

        pos.update_all_attacks();

        Some(pos)
    }

    pub fn fen(&self) -> String {
        let mut result = String::new();
        for rank in (0..8).rev() {
            let rank = Rank::new(rank).unwrap();
            let mut consecutive_empty = 0;
            for file in 0..8 {
                let file = File::new(file).unwrap();
                let sq = Square::from_file_rank(file, rank);
                match self.find_piece(sq) {
                    None => consecutive_empty += 1,
                    Some(piece) => {
                        if consecutive_empty > 0 {
                            write!(result, "{}", consecutive_empty).unwrap();
                            consecutive_empty = 0;
                        }

                        let mut c = piece.as_char();
                        if self.white_pieces.contains(sq) {
                            c = c.to_ascii_uppercase();
                        }
                        write!(result, "{}", c).unwrap();
                    }
                }
            }

            if consecutive_empty > 0 {
                write!(result, "{}", consecutive_empty).unwrap();
            }

            if rank != Rank::One {
                result.push('/');
            }
        }

        match self.side_to_move {
            Side::White => result.push_str(" w"),
            Side::Black => result.push_str(" b"),
        }

        if self.castling.0 > 0 {
            let mut castling = String::new();
            if self.castling.white_kingside() {
                castling.push('K');
            }

            if self.castling.white_queenside() {
                castling.push('Q');
            }

            if self.castling.black_kingside() {
                castling.push('k');
            }

            if self.castling.black_queenside() {
                castling.push('q');
            }

            if !castling.is_empty() {
                write!(result, " {castling}").unwrap();
            }
        }

        match self.en_passant_file {
            Some(file) => {
                write!(
                    result,
                    " {file}{rank}",
                    rank = (!self.side_to_move).skip_rank()
                )
                .unwrap();
            }
            None => {
                result.push_str(" -");
            }
        };

        write!(result, " {}", self.half_move_clock).unwrap();
        write!(result, " {}", self.full_move_clock).unwrap();

        result
    }

    pub const fn pawns(&self) -> Bitboard {
        *self.pieces.index_const(Piece::Pawn)
    }

    pub const fn knights(&self) -> Bitboard {
        *self.pieces.index_const(Piece::Knight)
    }

    pub const fn bishops(&self) -> Bitboard {
        *self.pieces.index_const(Piece::Bishop)
    }

    pub const fn rooks(&self) -> Bitboard {
        *self.pieces.index_const(Piece::Rook)
    }

    pub const fn queens(&self) -> Bitboard {
        *self.pieces.index_const(Piece::Queen)
    }

    pub const fn kings(&self) -> Bitboard {
        *self.pieces.index_const(Piece::King)
    }

    pub const fn king_sq(&self, side: Side) -> Square {
        self.king_sq[side as usize]
    }

    pub const fn pieces(&self, side: Side) -> Bitboard {
        match side {
            Side::White => self.white_pieces,
            Side::Black => self.black_pieces,
        }
    }

    pub const fn all_pieces(&self) -> Bitboard {
        self.all_pieces
    }

    pub const fn attacks(&self, side: Side, piece: Piece) -> Bitboard {
        let attacks = &self.attacks[side as usize];
        match piece {
            Piece::Pawn => attacks.pawns,
            Piece::Knight => attacks.knights,
            Piece::Bishop => attacks.bishops,
            Piece::Rook => attacks.rooks,
            Piece::Queen => attacks.queens,
            Piece::King => attacks.kings,
        }
    }

    pub const fn en_passant_file(&self) -> Option<File> {
        self.en_passant_file
    }

    pub const fn en_passant_sq(&self) -> Option<Square> {
        match self.en_passant_file() {
            Some(file) => {
                let rank = self.side_to_move.en_passant_rank();
                Some(Square::from_file_rank(file, rank))
            }
            None => None,
        }
    }

    pub const fn castling(&self) -> Castling {
        self.castling
    }

    pub const fn side_to_move(&self) -> Side {
        self.side_to_move
    }

    pub const fn half_move_clock(&self) -> u8 {
        self.half_move_clock
    }

    pub fn in_check(&self) -> bool {
        self.attacks[!self.side_to_move as usize]
            .any
            .contains(self.king_sq(self.side_to_move))
    }

    pub fn find_piece(&self, sq: Square) -> Option<Piece> {
        if !(self.all_pieces & sq) {
            return None;
        }

        if self.pawns() & sq {
            return Some(Piece::Pawn);
        }

        if self.knights() & sq {
            return Some(Piece::Knight);
        }

        if self.bishops() & sq {
            return Some(Piece::Bishop);
        }

        if self.rooks() & sq {
            return Some(Piece::Rook);
        }

        if self.queens() & sq {
            return Some(Piece::Queen);
        }

        if self.kings() & sq {
            return Some(Piece::King);
        }

        None
    }

    pub fn see_ge(&self, mov: Move, threshold: Score) -> bool {
        const SEE_VALUE: PieceMap<Score> = PieceMap::new([100, 300, 300, 500, 900, 10000]);
        let see_value = |piece: Piece| SEE_VALUE[piece];

        let initial_gain = mov.capture.map(see_value).unwrap_or_default()
            + mov
                .promotion
                .map(|p| see_value(p) - see_value(Piece::Pawn))
                .unwrap_or_default();
        let mut next_victim_value = see_value(mov.promotion.unwrap_or(mov.piece));

        let mut score = initial_gain - threshold;
        if score < 0 {
            return false;
        }

        if score - next_victim_value >= 0 {
            return true;
        }

        let mut side = !self.side_to_move();
        let mut occupancy = self.all_pieces() & !(mov.from.as_bb() | mov.to.as_bb());
        if let Some(ep) = mov
            .to
            .backward(self.side_to_move())
            .filter(|_| mov.en_passant)
        {
            occupancy ^= ep;
        }

        let promotion = mov.to.rank() == Rank::One || mov.to.rank() == Rank::Eight;

        let to_bb = mov.to.as_bb();
        let bq = self.bishops() | self.queens();
        let rq = self.rooks() | self.queens();

        let mut attackers = Bitboard::empty();
        attackers |= (to_bb.west(1) | to_bb.east(1)).backward(1, Side::White)
            & self.pawns()
            & self.pieces(Side::White);
        attackers |= (to_bb.west(1) | to_bb.east(1)).backward(1, Side::Black)
            & self.pawns()
            & self.pieces(Side::Black);
        attackers |= KNIGHT_ATTACKS[mov.to] & self.knights();
        attackers |= bishop_from(mov.to, occupancy) & bq;
        attackers |= rook_from(mov.to, occupancy) & rq;
        attackers |= KING_ATTACKS[mov.to] & self.kings();
        attackers &= occupancy;

        if mov.piece == Piece::King {
            // SEE test is successful if king cannot be recaptured because currently score >= 0
            return (self.pieces(!self.side_to_move()) & attackers).is_empty();
        }

        while (attackers & self.pieces(side)).at_least_one() {
            let us = self.pieces(side) & attackers;
            if !promotion && (us & self.pawns()).at_least_one() {
                score = next_victim_value - score - 1;
                if score < 0 {
                    break;
                }

                next_victim_value = see_value(Piece::Pawn);

                let lsb_bb = (us & self.pawns()).lsb_bb();
                occupancy ^= lsb_bb;
                attackers |= bishop_from(mov.to, occupancy) & bq;
            } else if (us & self.knights()).at_least_one() {
                score = next_victim_value - score - 1;
                if score < 0 {
                    break;
                }

                next_victim_value = see_value(Piece::Knight);

                let lsb_bb = (us & self.knights()).lsb_bb();
                occupancy ^= lsb_bb;
            } else if (us & self.bishops()).at_least_one() {
                score = next_victim_value - score - 1;
                if score < 0 {
                    break;
                }

                next_victim_value = see_value(Piece::Bishop);

                let lsb_bb = (us & self.bishops()).lsb_bb();
                occupancy ^= lsb_bb;
                attackers |= bishop_from(mov.to, occupancy) & bq;
            } else if (us & self.rooks()).at_least_one() {
                score = next_victim_value - score - 1;
                if score < 0 {
                    break;
                }

                next_victim_value = see_value(Piece::Rook);

                let lsb_bb = (us & self.rooks()).lsb_bb();
                occupancy ^= lsb_bb;
                attackers |= rook_from(mov.to, occupancy) & rq;
            } else if promotion && (us & self.pawns()).at_least_one() {
                score = next_victim_value - score - 1 + see_value(Piece::Queen)
                    - see_value(Piece::Pawn);
                if score < 0 {
                    break;
                }

                next_victim_value = see_value(Piece::Queen);

                let lsb_bb = (us & self.pawns()).lsb_bb();
                occupancy ^= lsb_bb;
                attackers |= bishop_from(mov.to, occupancy) & bq;
            } else if (us & self.queens()).at_least_one() {
                score = next_victim_value - score - 1;
                if score < 0 {
                    break;
                }

                next_victim_value = see_value(Piece::Queen);

                let lsb_bb = (us & self.queens()).lsb_bb();
                occupancy ^= lsb_bb;
                attackers |=
                    bishop_from(mov.to, occupancy) & bq | rook_from(mov.to, occupancy) & rq;
            } else if (us & self.kings()).at_least_one() {
                score = next_victim_value - score - 1;

                // Do not need to update attackers because we will stop now,
                // either because there are no more captures or the king would
                // be recaptured.

                // Capture wasn't enough or king gets recaptured -> side-to-move lost exchange
                if score < 0 || (self.pieces(!side) & attackers).at_least_one() {
                    return self.side_to_move() != side;
                }

                // side-to-move won exchange
                return self.side_to_move() == side;
            } else {
                // no more captures
                unreachable!();
            }

            side = !side;
            attackers &= occupancy;
        }

        // The side which made the last move (which also pushed the score to at
        // least 0) is successful, since the other side cannot recapture.
        self.side_to_move != side
    }

    // TODO this function can probably be optimized
    pub fn is_move_pseudo_legal(&self, mov: Move) -> bool {
        let us = self.pieces(self.side_to_move());
        let mut moves = MoveVec::new();
        match mov.piece {
            Piece::Pawn => {
                generate_pawn_moves(self, !us, &mut moves);
                moves.contains(&mov)
            }
            Piece::Knight => {
                generate_knight_moves(self, !us, &mut moves);
                moves.contains(&mov)
            }
            Piece::Bishop => {
                generate_bishop_moves(self, !us, &mut moves);
                moves.contains(&mov)
            }
            Piece::Rook => {
                generate_rook_moves(self, !us, &mut moves);
                moves.contains(&mov)
            }
            Piece::Queen => {
                generate_queen_moves(self, !us, &mut moves);
                moves.contains(&mov)
            }
            Piece::King => {
                generate_king_moves(self, !us, &mut moves);
                moves.contains(&mov)
            }
        }
    }

    pub fn is_move_legal(&self, mov: Move) -> bool {
        let mut us = self.pieces(self.side_to_move);
        let mut them = self.pieces(!self.side_to_move);
        let mut pawns = self.pawns();
        let mut knights = self.knights();
        let mut bishops = self.bishops();
        let mut rooks = self.rooks();
        let mut queens = self.queens();
        let mut kings = self.kings();

        // Castling moves are encoded as the king capturing our rook
        if mov.piece == Piece::King && (us & self.rooks()) & mov.to {
            // blockers are checked during move generation

            let (traversed, king, rook) = match mov.to.file().cmp(&mov.from.file()) {
                Ordering::Less => {
                    // queenside
                    // TODO FRC
                    let traversed = (File::C.as_bb() | File::D.as_bb() | File::E.as_bb())
                        & mov.from.rank().as_bb();
                    (
                        traversed,
                        Square::from_file_rank(File::C, mov.from.rank()),
                        Square::from_file_rank(File::D, mov.from.rank()),
                    )
                }
                Ordering::Greater => {
                    // kingside
                    let traversed = (File::E.as_bb() | File::F.as_bb() | File::G.as_bb())
                        & mov.from.rank().as_bb();
                    (
                        traversed,
                        Square::from_file_rank(File::G, mov.from.rank()),
                        Square::from_file_rank(File::F, mov.from.rank()),
                    )
                }
                _ => {
                    // this shouldn't happen
                    return false;
                }
            };

            if (self.attacks[(!self.side_to_move) as usize].any & traversed).at_least_one() {
                return false;
            }

            // Remove rook
            rooks ^= mov.to;
            us ^= mov.to;

            // Remove king
            kings ^= mov.from;
            us ^= mov.from;

            // Add rook
            rooks ^= rook;
            us ^= rook;

            // Add king
            kings ^= king;
            us ^= king;
        } else {
            // Non-castling moves
            if let Some(capture) = mov.capture {
                if mov.en_passant {
                    let captured_sq = mov.to.as_bb().backward(1, self.side_to_move);

                    // Remove their pawn
                    pawns ^= captured_sq;
                    them ^= captured_sq;
                } else {
                    let captured_piece_bb = match capture {
                        Piece::Pawn => &mut pawns,
                        Piece::Knight => &mut knights,
                        Piece::Bishop => &mut bishops,
                        Piece::Rook => &mut rooks,
                        Piece::Queen => &mut queens,
                        Piece::King => &mut kings,
                    };

                    // Remove their piece
                    *captured_piece_bb ^= mov.to;
                    them ^= mov.to;
                }
            }

            let original_piece_bb = match mov.piece {
                Piece::Pawn => &mut pawns,
                Piece::Knight => &mut knights,
                Piece::Bishop => &mut bishops,
                Piece::Rook => &mut rooks,
                Piece::Queen => &mut queens,
                Piece::King => &mut kings,
            };

            // Remove our piece
            *original_piece_bb ^= mov.from;
            us ^= mov.from;

            let new_piece_bb = match mov.promotion.unwrap_or(mov.piece) {
                Piece::Pawn => &mut pawns,
                Piece::Knight => &mut knights,
                Piece::Bishop => &mut bishops,
                Piece::Rook => &mut rooks,
                Piece::Queen => &mut queens,
                Piece::King => &mut kings,
            };

            // Add our piece
            *new_piece_bb ^= mov.to;
            us ^= mov.to;
        }

        let all_pieces = us | them;

        if mov.piece == Piece::King || mov.en_passant || self.in_check() {
            let attacked = attacked_squares(
                !self.side_to_move,
                them & pawns,
                them & knights,
                them & (bishops | queens),
                them & (rooks | queens),
                self.king_sq(!self.side_to_move),
                all_pieces,
            );

            (attacked & kings & us).is_empty()
        } else {
            let diagonally_attacked = self.attacks[!self.side_to_move as usize].bishops
                | self.attacks[!self.side_to_move as usize].queens;
            if diagonally_attacked.contains(mov.from)
                && (bishop_from(self.king_sq(self.side_to_move), all_pieces)
                    & (bishops | queens)
                    & them)
                    .at_least_one()
            {
                return false;
            }

            let orthogonally_attacked = self.attacks[!self.side_to_move as usize].rooks
                | self.attacks[!self.side_to_move as usize].queens;
            if orthogonally_attacked.contains(mov.from)
                && (rook_from(self.king_sq(self.side_to_move), all_pieces)
                    & (rooks | queens)
                    & them)
                    .at_least_one()
            {
                return false;
            }

            true
        }
    }

    pub fn make_move(&self, mov: Move) -> Self {
        let mut pos = self.clone();
        let mut us = pos.pieces(pos.side_to_move);
        let mut them = pos.pieces(!pos.side_to_move);

        pos.half_move_clock += 1;

        // Castling moves are encoded as the king capturing our rook
        if mov.piece == Piece::King && (us & pos.rooks()) & mov.to {
            let (king, rook) = match mov.to.file().cmp(&mov.from.file()) {
                Ordering::Less => {
                    // queenside
                    // TODO FRC
                    (
                        Square::from_file_rank(File::C, mov.from.rank()),
                        Square::from_file_rank(File::D, mov.from.rank()),
                    )
                }
                Ordering::Greater => {
                    // kingside
                    (
                        Square::from_file_rank(File::G, mov.from.rank()),
                        Square::from_file_rank(File::F, mov.from.rank()),
                    )
                }
                _ => unreachable!(),
            };

            // Remove rook
            pos.pieces[Piece::Rook] ^= mov.to;
            us ^= mov.to;

            // Remove king
            pos.pieces[Piece::King] ^= mov.from;
            us ^= mov.from;

            // Add rook
            pos.pieces[Piece::Rook] ^= rook;
            us ^= rook;

            // Add king
            pos.pieces[Piece::King] ^= king;
            pos.king_sq[pos.side_to_move as usize] = king;
            us ^= king;
        } else {
            // Non-castling moves
            if let Some(capture) = mov.capture {
                if mov.en_passant {
                    let captured_sq = mov.to.as_bb().backward(1, pos.side_to_move);

                    // Remove their pawn
                    pos.pieces[Piece::Pawn] ^= captured_sq;
                    them ^= captured_sq;
                } else {
                    // Remove their piece
                    pos.pieces[capture] ^= mov.to;
                    them ^= mov.to;
                }
            }

            // Remove our piece
            pos.pieces[mov.piece] ^= mov.from;
            us ^= mov.from;

            // Add our piece
            pos.pieces[mov.promotion.unwrap_or(mov.piece)] ^= mov.to;
            us ^= mov.to;

            if mov.piece == Piece::King {
                pos.king_sq[pos.side_to_move as usize] = mov.to;
            }
        }

        pos.all_pieces = us | them;
        if pos.side_to_move == Side::White {
            pos.white_pieces = us;
            pos.black_pieces = them;
        } else {
            pos.white_pieces = them;
            pos.black_pieces = us;
        }

        if mov.piece == Piece::Pawn || mov.capture.is_some() {
            pos.half_move_clock = 0;
        }

        pos.castling.mask(CASTLING_MASKS[mov.from]);
        pos.castling.mask(CASTLING_MASKS[mov.to]);

        if mov.piece == Piece::Pawn
            && mov.from.rank() == pos.side_to_move.initial_pawn_rank()
            && mov.to.rank() == pos.side_to_move.double_step_pawn_rank()
        {
            pos.en_passant_file = Some(mov.from.file());
        } else {
            pos.en_passant_file = None;
        }

        if pos.side_to_move == Side::Black {
            pos.full_move_clock += 1;
        }

        pos.side_to_move = !pos.side_to_move;

        pos.update_attacks_after_move(mov);

        pos
    }

    #[inline]
    fn update_all_attacks(&mut self) {
        let side = self.side_to_move;
        let us = self.pieces(side);
        let them = self.pieces(!side);

        let our_pawns = self.pawns() & us;
        let their_pawns = self.pawns() & them;
        self.attacks[side as usize].pawns =
            (our_pawns.west(1) | our_pawns.east(1)).forward(1, side);
        self.attacks[!side as usize].pawns =
            (their_pawns.west(1) | their_pawns.east(1)).forward(1, !side);

        self.attacks[side as usize].knights = Bitboard::empty();
        for knight in self.knights() & us {
            self.attacks[side as usize].knights |= KNIGHT_ATTACKS[knight];
        }

        self.attacks[!side as usize].knights = Bitboard::empty();
        for knight in self.knights() & them {
            self.attacks[!side as usize].knights |= KNIGHT_ATTACKS[knight];
        }

        self.attacks[side as usize].bishops = Bitboard::empty();
        for bishop in self.bishops() & us {
            self.attacks[side as usize].bishops |= bishop_from(bishop, self.all_pieces());
        }

        self.attacks[!side as usize].bishops = Bitboard::empty();
        for bishop in self.bishops() & them {
            self.attacks[!side as usize].bishops |= bishop_from(bishop, self.all_pieces());
        }

        self.attacks[side as usize].rooks = Bitboard::empty();
        for rook in self.rooks() & us {
            self.attacks[side as usize].rooks |= rook_from(rook, self.all_pieces());
        }

        self.attacks[!side as usize].rooks = Bitboard::empty();
        for rook in self.rooks() & them {
            self.attacks[!side as usize].rooks |= rook_from(rook, self.all_pieces());
        }

        self.attacks[side as usize].queens = Bitboard::empty();
        for queen in self.queens() & us {
            self.attacks[side as usize].queens |=
                bishop_from(queen, self.all_pieces()) | rook_from(queen, self.all_pieces());
        }

        self.attacks[!side as usize].queens = Bitboard::empty();
        for queen in self.queens() & them {
            self.attacks[!side as usize].queens |=
                bishop_from(queen, self.all_pieces()) | rook_from(queen, self.all_pieces());
        }

        self.attacks[side as usize].kings = KING_ATTACKS[self.king_sq(side)];
        self.attacks[!side as usize].kings = KING_ATTACKS[self.king_sq(!side)];

        self.attacks[0].update_any();
        self.attacks[1].update_any();
    }

    #[inline]
    fn update_attacks_after_move(&mut self, mov: Move) {
        // Move has already been made. `!self.side_to_move` is the side which made the move `mov`.
        let side = !self.side_to_move;
        let us = self.pieces(side);
        let them = self.pieces(!side);

        // Always reevaluate if king moved (strictly only required if it was a castling move) or if
        // it was an en passant capture.
        if mov.piece == Piece::King || mov.en_passant {
            self.update_all_attacks();
        }

        let our_pawns = self.pawns() & us;
        let their_pawns = self.pawns() & them;
        self.attacks[side as usize].pawns =
            (our_pawns.west(1) | our_pawns.east(1)).forward(1, side);
        self.attacks[!side as usize].pawns =
            (their_pawns.west(1) | their_pawns.east(1)).forward(1, !side);

        let reeval_our_knights = mov.piece == Piece::Knight || mov.promotion == Some(Piece::Knight);
        if reeval_our_knights {
            self.attacks[side as usize].knights = Bitboard::empty();
            for knight in self.knights() & us {
                self.attacks[side as usize].knights |= KNIGHT_ATTACKS[knight];
            }
        }

        let reeval_their_knights = mov.capture == Some(Piece::Knight);
        if reeval_their_knights {
            self.attacks[!side as usize].knights = Bitboard::empty();
            for knight in self.knights() & them {
                self.attacks[!side as usize].knights |= KNIGHT_ATTACKS[knight];
            }
        }

        let reeval_our_bishops = mov.piece == Piece::Bishop
            || mov.promotion == Some(Piece::Bishop)
            || self.attacks[side as usize].bishops.contains(mov.from)
            || self.attacks[side as usize].bishops.contains(mov.to);
        if reeval_our_bishops {
            self.attacks[side as usize].bishops = Bitboard::empty();
            for bishop in self.bishops() & us {
                self.attacks[side as usize].bishops |= bishop_from(bishop, self.all_pieces());
            }
        }

        let reeval_their_bishops = mov.capture == Some(Piece::Bishop)
            || self.attacks[!side as usize].bishops.contains(mov.from)
            || self.attacks[!side as usize].bishops.contains(mov.to);
        if reeval_their_bishops {
            self.attacks[!side as usize].bishops = Bitboard::empty();
            for bishop in self.bishops() & them {
                self.attacks[!side as usize].bishops |= bishop_from(bishop, self.all_pieces());
            }
        }

        let reeval_our_rooks = mov.piece == Piece::Rook
            || mov.promotion == Some(Piece::Rook)
            || self.attacks[side as usize].rooks.contains(mov.from)
            || self.attacks[side as usize].rooks.contains(mov.to);
        if reeval_our_rooks {
            self.attacks[side as usize].rooks = Bitboard::empty();
            for rook in self.rooks() & us {
                self.attacks[side as usize].rooks |= rook_from(rook, self.all_pieces());
            }
        }

        let reeval_their_rooks = mov.capture == Some(Piece::Rook)
            || self.attacks[!side as usize].rooks.contains(mov.from)
            || self.attacks[!side as usize].rooks.contains(mov.to);
        if reeval_their_rooks {
            self.attacks[!side as usize].rooks = Bitboard::empty();
            for rook in self.rooks() & them {
                self.attacks[!side as usize].rooks |= rook_from(rook, self.all_pieces());
            }
        }

        let reeval_our_queens = mov.piece == Piece::Queen
            || mov.promotion == Some(Piece::Queen)
            || self.attacks[side as usize].queens.contains(mov.from)
            || self.attacks[side as usize].queens.contains(mov.to);
        if reeval_our_queens {
            self.attacks[side as usize].queens = Bitboard::empty();
            for queen in self.queens() & us {
                self.attacks[side as usize].queens |=
                    bishop_from(queen, self.all_pieces()) | rook_from(queen, self.all_pieces());
            }
        }

        let reeval_their_queens = mov.capture == Some(Piece::Queen)
            || self.attacks[!side as usize].queens.contains(mov.from)
            || self.attacks[!side as usize].queens.contains(mov.to);
        if reeval_their_queens {
            self.attacks[!side as usize].queens = Bitboard::empty();
            for queen in self.queens() & them {
                self.attacks[!side as usize].queens |=
                    bishop_from(queen, self.all_pieces()) | rook_from(queen, self.all_pieces());
            }
        }

        self.attacks[side as usize].kings = KING_ATTACKS[self.king_sq(side)];
        self.attacks[!side as usize].kings = KING_ATTACKS[self.king_sq(!side)];

        self.attacks[0].update_any();
        self.attacks[1].update_any();
    }
}

fn attacked_squares(
    side: Side,
    pawns: Bitboard,
    knights: Bitboard,
    bishops: Bitboard,
    rooks: Bitboard,
    king_sq: Square,
    all_pieces: Bitboard,
) -> Bitboard {
    let mut attacking = Bitboard::default();

    attacking |= (pawns.west(1) | pawns.east(1)).forward(1, side);

    // TODO is bitshifting faster here?
    for knight in knights {
        attacking |= KNIGHT_ATTACKS[knight];
    }

    for bishop in bishops {
        attacking |= bishop_from(bishop, all_pieces);
    }

    for rook in rooks {
        attacking |= rook_from(rook, all_pieces);
    }

    attacking |= KING_ATTACKS[king_sq];

    attacking
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Attacks {
    pawns: Bitboard,
    knights: Bitboard,
    bishops: Bitboard,
    rooks: Bitboard,
    queens: Bitboard,
    kings: Bitboard,
    any: Bitboard,
}

impl Attacks {
    fn update_any(&mut self) {
        self.any = self.pawns | self.knights | self.bishops | self.rooks | self.queens | self.kings;
    }
}

pub const STARTING_POSITION_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

impl Default for Position {
    fn default() -> Self {
        Position::from_fen(STARTING_POSITION_FEN).unwrap()
    }
}

pub fn perft(pos: &Position, depth: usize) -> u64 {
    let start = std::time::Instant::now();
    if depth == 0 {
        return 1;
    }

    let us = pos.pieces(pos.side_to_move);
    let mut moves = MoveVec::new();

    generate_pawn_moves(pos, !us, &mut moves);
    generate_knight_moves(pos, !us, &mut moves);
    generate_bishop_moves(pos, !us, &mut moves);
    generate_rook_moves(pos, !us, &mut moves);
    generate_queen_moves(pos, !us, &mut moves);
    generate_king_moves(pos, !us, &mut moves);

    let mut total = 0;

    // `for mov in moves` generates _a lot_ of memcpy's which `for &mov inm moves.iter()` does not.
    for &mov in moves.iter() {
        if !pos.is_move_legal(mov) {
            continue;
        }

        let pos = pos.make_move(mov);
        let n = internal_perft(&pos, depth - 1);
        println!("{mov}: {n}");
        total += n;
    }

    let duration = std::time::Instant::now() - start;

    println!("Nodes searched: {total}");
    println!("Took {} ms", duration.as_millis());
    if duration.as_millis() > 0 {
        println!("NPS {}", 1000 * total as u128 / duration.as_millis());
    }

    total
}

pub fn internal_perft(pos: &Position, depth: usize) -> u64 {
    if depth == 0 {
        return 1;
    }

    let us = pos.pieces(pos.side_to_move);
    let mut moves = MoveVec::new();

    generate_pawn_moves(pos, !us, &mut moves);
    generate_knight_moves(pos, !us, &mut moves);
    generate_bishop_moves(pos, !us, &mut moves);
    generate_rook_moves(pos, !us, &mut moves);
    generate_queen_moves(pos, !us, &mut moves);
    generate_king_moves(pos, !us, &mut moves);

    let mut total = 0;

    // `for mov in moves` generates _a lot_ of memcpy's which `for &mov inm moves.iter()` does not.
    for &mov in moves.iter() {
        if !pos.is_move_legal(mov) {
            continue;
        }

        // Some might consider this cheating. But who cares about perft performance?
        if depth == 1 {
            total += 1;
            continue;
        }

        let pos = pos.make_move(mov);
        total += internal_perft(&pos, depth - 1);
    }

    total
}

impl From<&Position> for fathom::Position {
    fn from(pos: &Position) -> Self {
        let castling = {
            let mut c = 0;
            if pos.castling.white_kingside() {
                c |= fathom::CASTLE_WHITE_KINGSIDE;
            };
            if pos.castling.white_queenside() {
                c |= fathom::CASTLE_WHITE_QUEENSIDE;
            };
            if pos.castling.black_kingside() {
                c |= fathom::CASTLE_BLACK_KINGSIDE;
            };
            if pos.castling.black_queenside() {
                c |= fathom::CASTLE_BLACK_QUEENSIDE;
            };

            c
        };

        fathom::Position {
            white: pos.white_pieces.as_u64(),
            black: pos.black_pieces.as_u64(),
            kings: pos.kings().as_u64(),
            queens: pos.queens().as_u64(),
            rooks: pos.rooks().as_u64(),
            bishops: pos.bishops().as_u64(),
            knights: pos.knights().as_u64(),
            pawns: pos.pawns().as_u64(),
            rule50: pos.half_move_clock.into(),
            castling,
            ep: pos
                .en_passant_sq()
                .map(Square::as_u8)
                .unwrap_or_default()
                .into(),
            turn: u8::from(pos.side_to_move == Side::White),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const KIWIPETE_FEN: &str =
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";

    #[test]
    fn parse_starting_position_fen() {
        let position = Position::from_fen(STARTING_POSITION_FEN).unwrap();

        assert_eq!(position.side_to_move, Side::White);
        assert_eq!(position.en_passant_file, None);
        assert_eq!(position.half_move_clock, 0);
        assert!(position.pawns() & Square::from_file_rank(File::A, Rank::Two));
        assert!(position.pawns() & Square::from_file_rank(File::H, Rank::Seven));
        assert!(position.knights() & Square::from_file_rank(File::B, Rank::One));
        assert!(position.knights() & Square::from_file_rank(File::G, Rank::Eight));
        assert!(position.kings() & Square::from_file_rank(File::E, Rank::One));
        assert!(position.kings() & Square::from_file_rank(File::E, Rank::Eight));
        assert!(position.white_pieces & Square::from_file_rank(File::A, Rank::One));
        assert!(position.black_pieces & Square::from_file_rank(File::A, Rank::Eight));
        assert!(position.castling.white_kingside());
        assert!(position.castling.white_queenside());
        assert!(position.castling.black_kingside());
        assert!(position.castling.black_queenside());
    }

    #[test]
    fn parse_other_fen() {
        let position =
            Position::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b kq e3 0 1")
                .unwrap();

        assert_eq!(position.side_to_move, Side::Black);
        assert_eq!(position.en_passant_file, Some(File::E));
        assert!(position.pawns() & Square::from_file_rank(File::E, Rank::Four));
        assert!(position.white_pieces & Square::from_file_rank(File::E, Rank::Four));
        assert!(!position.castling.white_kingside());
        assert!(!position.castling.white_queenside());
        assert!(position.castling.black_kingside());
        assert!(position.castling.black_queenside());
    }

    #[ignore]
    #[test]
    fn test_perft() {
        const PERFT_POSITIONS: &[(&str, &str, &[(usize, u64)])] = &[
            (
                "Starting position",
                STARTING_POSITION_FEN,
                &[
                    (1, 20),
                    (2, 400),
                    (3, 8902),
                    (4, 197281),
                    (5, 4865609),
                    (6, 119060324),
                ],
            ),
            (
                "Kiwipete position",
                KIWIPETE_FEN,
                &[
                    (1, 48),
                    (2, 2039),
                    (3, 97862),
                    (4, 4085603),
                    (5, 193690690),
                    //(6, 8031647685),
                ],
            ),
            (
                "Position 5 - https://www.chessprogramming.org/Perft_Results",
                "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8",
                &[(1, 44), (2, 1486), (3, 62379), (4, 2103487), (5, 89941194)],
            ),
        ];
        for (name, fen, results) in PERFT_POSITIONS {
            let position = Position::from_fen(fen).unwrap();
            for (depth, expected) in results.iter() {
                assert_eq!(
                    perft(&position, *depth),
                    *expected,
                    "Perft for '{name}' failed at depth {depth}"
                );
            }
        }
    }

    #[test]
    fn test_starting_position_fen() {
        let position = Position::from_fen(STARTING_POSITION_FEN).unwrap();
        assert_eq!(position.fen(), STARTING_POSITION_FEN);
    }

    #[test]
    fn test_kiwipete_fen() {
        let position = Position::from_fen(KIWIPETE_FEN).unwrap();
        assert_eq!(position.fen(), KIWIPETE_FEN);
    }
}
