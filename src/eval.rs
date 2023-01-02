use crate::{
    movegen::Move,
    position::Position,
    tune::Trace,
    types::{Bitboard, File, Piece, PieceMap, Rank, Side, Square, SquareMap},
};

pub type Score = i16;
pub(crate) type EScore = i32;

#[rustfmt::skip]
pub const PIECE_VALUES: PieceMap<EScore> = PieceMap::new([
    S( 100,  169),
    S( 397,  306),
    S( 434,  339),
    S( 528,  623),
    S(1077, 1159),
    S(   0,    0),
]);

#[rustfmt::skip]
pub static PAWN_PST: SquareMap<EScore> = SquareMap::visual([
    S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0),
    S(  35,  176), S(  63,  162), S(  51,  132), S(  59,  120), S(  59,  120), S(  51,  132), S(  63,  162), S(  35,  176),
    S( -45,   65), S(  -8,   65), S(  29,   29), S(  38,   20), S(  38,   20), S(  29,   29), S(  -8,   65), S( -45,   65),
    S( -42,  -18), S(   5,  -29), S(   0,  -43), S(  19,  -57), S(  19,  -57), S(   0,  -43), S(   5,  -29), S( -42,  -18),
    S( -50,  -42), S(  -8,  -47), S( -12,  -62), S(  11,  -67), S(  11,  -67), S( -12,  -62), S(  -8,  -47), S( -50,  -42),
    S( -41,  -53), S(   1,  -51), S(  -8,  -63), S(  -9,  -54), S(  -9,  -54), S(  -8,  -63), S(   1,  -51), S( -41,  -53),
    S( -52,  -46), S(   5,  -47), S(  -9,  -46), S( -30,  -41), S( -30,  -41), S(  -9,  -46), S(   5,  -47), S( -52,  -46),
    S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0),
]);

#[rustfmt::skip]
pub static KNIGHT_PST: SquareMap<EScore> = SquareMap::visual([
    S( -89,  -72), S( -34,  -39), S( -33,  -13), S(  -4,  -13), S(  -4,  -13), S( -33,  -13), S( -34,  -39), S( -89,  -72),
    S( -43,  -25), S( -19,    0), S(  61,   -7), S(  16,   16), S(  16,   16), S(  61,   -7), S( -19,    0), S( -43,  -25),
    S(  -9,  -18), S(  71,   -4), S(  56,   28), S(  80,   22), S(  80,   22), S(  56,   28), S(  71,   -4), S(  -9,  -18),
    S(  11,   -2), S(  18,   25), S(  43,   39), S(  45,   46), S(  45,   46), S(  43,   39), S(  18,   25), S(  11,   -2),
    S( -14,   -2), S(  12,   17), S(  18,   39), S(  18,   46), S(  18,   46), S(  18,   39), S(  12,   17), S( -14,   -2),
    S( -21,  -10), S(   6,    5), S(  16,   14), S(  13,   33), S(  13,   33), S(  16,   14), S(   6,    5), S( -21,  -10),
    S( -30,  -30), S( -35,   -9), S(  -4,    1), S(  -2,   12), S(  -2,   12), S(  -4,    1), S( -35,   -9), S( -30,  -30),
    S( -50,  -40), S( -20,  -51), S( -45,   -7), S( -32,   -4), S( -32,   -4), S( -45,   -7), S( -20,  -51), S( -50,  -40),
]);

#[rustfmt::skip]
pub static BISHOP_PST: SquareMap<EScore> = SquareMap::visual([
    S( -29,  -17), S( -18,  -12), S( -34,  -12), S( -17,   -7), S( -17,   -7), S( -34,  -12), S( -18,  -12), S( -29,  -17),
    S( -42,   -5), S(  11,    0), S(   5,    4), S(   3,   -2), S(   3,   -2), S(   5,    4), S(  11,    0), S( -42,   -5),
    S( -15,    8), S(  30,    0), S(  36,   10), S(  32,    3), S(  32,    3), S(  36,   10), S(  30,    0), S( -15,    8),
    S(  -6,    4), S(  -2,   15), S(  24,   17), S(  45,   17), S(  45,   17), S(  24,   17), S(  -2,   15), S(  -6,    4),
    S( -10,   -2), S(   5,    5), S(   6,   18), S(  25,   21), S(  25,   21), S(   6,   18), S(   5,    5), S( -10,   -2),
    S(  -1,  -10), S(  11,    0), S(  17,   10), S(   8,   19), S(   8,   19), S(  17,   10), S(  11,    0), S(  -1,  -10),
    S(  -2,  -20), S(  23,  -19), S(  11,   -5), S(  -3,    8), S(  -3,    8), S(  11,   -5), S(  23,  -19), S(  -2,  -20),
    S( -42,  -19), S( -19,   -5), S( -20,  -24), S( -32,    0), S( -32,    0), S( -20,  -24), S( -19,   -5), S( -42,  -19),
]);

#[rustfmt::skip]
pub static ROOK_PST: SquareMap<EScore> = SquareMap::visual([
    S(   5,   16), S(  11,   13), S(   5,   19), S(  37,   16), S(  37,   16), S(   5,   19), S(  11,   13), S(   5,   16),
    S(  29,    6), S(  32,    7), S(  67,    1), S(  64,    2), S(  64,    2), S(  67,    1), S(  32,    7), S(  29,    6),
    S(  -4,    1), S(  28,    0), S(  27,    0), S(  26,    2), S(  26,    2), S(  27,    0), S(  28,    0), S(  -4,    1),
    S( -26,    3), S(  -8,   -1), S(  15,    5), S(  23,   -2), S(  23,   -2), S(  15,    5), S(  -8,   -1), S( -26,    3),
    S( -44,   -1), S( -12,   -3), S( -14,    0), S(   0,   -3), S(   0,   -3), S( -14,    0), S( -12,   -3), S( -44,   -1),
    S( -52,   -8), S( -20,   -6), S( -15,  -11), S( -13,   -7), S( -13,   -7), S( -15,  -11), S( -20,   -6), S( -52,   -8),
    S( -68,   -2), S( -14,  -13), S( -16,   -6), S( -10,   -6), S( -10,   -6), S( -16,   -6), S( -14,  -13), S( -68,   -2),
    S( -27,  -18), S( -28,    3), S(  -5,   -1), S(  12,   -5), S(  12,   -5), S(  -5,   -1), S( -28,    3), S( -27,  -18),
]);

#[rustfmt::skip]
pub static QUEEN_PST: SquareMap<EScore> = SquareMap::visual([
    S(  -3,   -6), S(   4,   12), S(  15,   22), S(  25,   26), S(  25,   26), S(  15,   22), S(   4,   12), S(  -3,   -6),
    S(   0,  -19), S( -32,   12), S(  22,   24), S(   8,   30), S(   8,   30), S(  22,   24), S( -32,   12), S(   0,  -19),
    S(  23,  -15), S(  20,   -2), S(  19,   17), S(  24,   41), S(  24,   41), S(  19,   17), S(  20,   -2), S(  23,  -15),
    S(  -1,    2), S( -20,   32), S(   3,   20), S(  -9,   47), S(  -9,   47), S(   3,   20), S( -20,   32), S(  -1,    2),
    S(  -4,  -12), S(  -7,   21), S(  -8,   22), S( -13,   43), S( -13,   43), S(  -8,   22), S(  -7,   21), S(  -4,  -12),
    S(  -9,  -12), S(  12,  -27), S(  -5,   11), S(  -5,    4), S(  -5,    4), S(  -5,   11), S(  12,  -27), S(  -9,  -12),
    S( -31,  -32), S(  -6,  -32), S(  23,  -49), S(   2,  -18), S(   2,  -18), S(  23,  -49), S(  -6,  -32), S( -31,  -32),
    S( -16,  -37), S( -21,  -34), S( -18,  -34), S(  11,  -56), S(  11,  -56), S( -18,  -34), S( -21,  -34), S( -16,  -37),
]);

#[rustfmt::skip]
pub static KING_PST: SquareMap<EScore> = SquareMap::visual([
    S(  28,  -37), S(  23,  -17), S(  11,   -6), S(  -2,  -15), S(  -2,  -15), S(  11,   -6), S(  23,  -17), S(  28,  -37),
    S(  22,   -6), S(  15,   15), S(   7,   25), S(  -2,   15), S(  -2,   15), S(   7,   25), S(  15,   15), S(  22,   -6),
    S(  14,    4), S(  20,   34), S(  10,   36), S( -11,   17), S( -11,   17), S(  10,   36), S(  20,   34), S(  14,    4),
    S(  -7,   -8), S(  -5,   25), S( -14,   32), S( -28,   30), S( -28,   30), S( -14,   32), S(  -5,   25), S(  -7,   -8),
    S( -30,  -21), S( -19,    3), S( -36,   26), S( -58,   35), S( -58,   35), S( -36,   26), S( -19,    3), S( -30,  -21),
    S( -13,  -16), S(  -4,    1), S( -36,   18), S( -57,   29), S( -57,   29), S( -36,   18), S(  -4,    1), S( -13,  -16),
    S(  47,  -35), S(  36,  -17), S( -17,    6), S( -56,   18), S( -56,   18), S( -17,    6), S(  36,  -17), S(  47,  -35),
    S(  55,  -76), S(  72,  -49), S(   0,  -25), S(  34,  -46), S(  34,  -46), S(   0,  -25), S(  72,  -49), S(  55,  -76),
]);

#[derive(Clone, Debug)]
pub struct Eval {
    material: [PieceMap<u8>; 2],
    pst: [PieceMap<EScore>; 2],

    pub trace: Trace,
}

impl From<&Position> for Eval {
    fn from(pos: &Position) -> Self {
        let mut material: [PieceMap<u8>; 2] = Default::default();
        for sq in Bitboard::all() {
            if let Some(piece) = pos.find_piece(sq) {
                let side = if pos.pieces(Side::Black).contains(sq) {
                    Side::Black
                } else {
                    Side::White
                };
                material[side as usize][piece] += 1;
            }
        }

        let mut pst = <[PieceMap<EScore>; 2]>::default();

        for piece in Piece::all() {
            pst[Side::White as usize][piece] = pst_for_piece::<true>(pos, piece);
            pst[Side::Black as usize][piece] = pst_for_piece::<false>(pos, piece);
        }

        Self {
            material,
            pst,
            trace: Trace::default(),
        }
    }
}

impl Eval {
    pub fn evaluate(&mut self, pos: &Position) -> Score {
        let mut score = S(0, 0);

        score += self.material::<true>() - self.material::<false>();
        score += self.pst::<true>() - self.pst::<false>();

        #[cfg(feature = "tune")]
        {
            self.trace_incrementals(pos);
            self.trace.eval = score;
        }

        let score = interpolate(score, self.phase());

        if pos.side_to_move() == Side::White {
            score
        } else {
            -score
        }
    }

    fn material<const WHITE: bool>(&self) -> EScore {
        let mut score = S(0, 0);
        let side = Side::white(WHITE);
        for (_piece, count, value) in self.material[side as usize].zip(&PIECE_VALUES).iter() {
            score += i32::from(*count) * value;
        }

        score
    }

    fn pst<const WHITE: bool>(&self) -> EScore {
        let mut score = S(0, 0);
        let side = Side::white(WHITE);

        score += self.pst[side as usize][Piece::Pawn];
        score += self.pst[side as usize][Piece::Knight];
        score += self.pst[side as usize][Piece::Bishop];
        score += self.pst[side as usize][Piece::Rook];
        score += self.pst[side as usize][Piece::Queen];
        score += self.pst[side as usize][Piece::King];

        score
    }

    fn phase(&mut self) -> i16 {
        let mut phase = 0;
        for (_piece, count, weight) in self.material[0].zip(&PHASE_WEIGTHS).iter() {
            phase += i16::from(*count) * weight;
        }

        for (_piece, count, weight) in self.material[1].zip(&PHASE_WEIGTHS).iter() {
            phase += i16::from(*count) * weight;
        }

        self.trace.phase = phase;

        phase
    }

    // `position` is the position _before_ the move was made
    pub fn make_move(&mut self, position: &Position, mov: Move) {
        let side = position.side_to_move() as usize;
        let us = position.pieces(position.side_to_move());

        // Update material
        if let Some(piece) = mov.capture {
            self.material[1 - side][piece] -= 1;
        }

        if let Some(piece) = mov.promotion {
            self.material[side][Piece::Pawn] -= 1;
            self.material[side][piece] += 1;
        }

        // Update piece-square tables
        let from = mov.from.normalize(position.side_to_move());
        let to = mov.to.normalize(position.side_to_move());
        match mov.piece {
            p @ Piece::Pawn => self.pst[side][p] -= PAWN_PST[from],
            p @ Piece::Knight => self.pst[side][p] -= KNIGHT_PST[from],
            p @ Piece::Bishop => self.pst[side][p] -= BISHOP_PST[from],
            p @ Piece::Rook => self.pst[side][p] -= ROOK_PST[from],
            p @ Piece::Queen => self.pst[side][p] -= QUEEN_PST[from],
            p @ Piece::King => self.pst[side][p] -= KING_PST[from],
        }

        match mov.promotion.unwrap_or(mov.piece) {
            p @ Piece::Pawn => self.pst[side][p] += PAWN_PST[to],
            p @ Piece::Knight => self.pst[side][p] += KNIGHT_PST[to],
            p @ Piece::Bishop => self.pst[side][p] += BISHOP_PST[to],
            p @ Piece::Rook => self.pst[side][p] += ROOK_PST[to],
            p @ Piece::Queen => self.pst[side][p] += QUEEN_PST[to],
            Piece::King => {
                // Castling
                if (us & position.rooks()) & mov.to {
                    match mov.to.file().cmp(&mov.from.file()) {
                        std::cmp::Ordering::Less => {
                            // queenside
                            let a1 = Square::from_file_rank(File::A, Rank::One);
                            let c1 = Square::from_file_rank(File::C, Rank::One);
                            let d1 = Square::from_file_rank(File::D, Rank::One);
                            self.pst[side][Piece::King] += KING_PST[c1];
                            self.pst[side][Piece::Rook] -= ROOK_PST[a1];
                            self.pst[side][Piece::Rook] += ROOK_PST[d1];
                        }
                        std::cmp::Ordering::Greater => {
                            // kingside
                            let h1 = Square::from_file_rank(File::H, Rank::One);
                            let g1 = Square::from_file_rank(File::G, Rank::One);
                            let f1 = Square::from_file_rank(File::F, Rank::One);
                            self.pst[side][Piece::King] += KING_PST[g1];
                            self.pst[side][Piece::Rook] -= ROOK_PST[h1];
                            self.pst[side][Piece::Rook] += ROOK_PST[f1];
                        }
                        std::cmp::Ordering::Equal => {}
                    }
                } else {
                    self.pst[side][Piece::King] += KING_PST[to]
                }
            }
        }

        let ep = mov
            .to
            .backward(position.side_to_move())
            .filter(|_| mov.en_passant)
            .map(|sq| sq.normalize(!position.side_to_move()));
        match mov.capture {
            None | Some(Piece::King) => {}
            Some(p @ Piece::Pawn) => {
                if let Some(ep) = ep {
                    self.pst[1 - side][p] -= PAWN_PST[ep];
                } else {
                    self.pst[1 - side][p] -= PAWN_PST[mov.to.normalize(!position.side_to_move())];
                }
            }
            Some(p @ Piece::Knight) => {
                self.pst[1 - side][p] -= KNIGHT_PST[mov.to.normalize(!position.side_to_move())]
            }
            Some(p @ Piece::Bishop) => {
                self.pst[1 - side][p] -= BISHOP_PST[mov.to.normalize(!position.side_to_move())]
            }
            Some(p @ Piece::Rook) => {
                self.pst[1 - side][p] -= ROOK_PST[mov.to.normalize(!position.side_to_move())]
            }
            Some(p @ Piece::Queen) => {
                self.pst[1 - side][p] -= QUEEN_PST[mov.to.normalize(!position.side_to_move())]
            }
        }
    }

    // `position` is the position _before_ the move was made
    pub fn unmake_move(&mut self, position: &Position, mov: Move) {
        let side = position.side_to_move() as usize;
        let us = position.pieces(position.side_to_move());

        // Update material
        if let Some(piece) = mov.capture {
            self.material[1 - side][piece] += 1;
        }

        if let Some(piece) = mov.promotion {
            self.material[side][Piece::Pawn] += 1;
            self.material[side][piece] -= 1;
        }

        // Update piece-square tables
        let from = mov.from.normalize(position.side_to_move());
        let to = mov.to.normalize(position.side_to_move());
        match mov.piece {
            p @ Piece::Pawn => self.pst[side][p] += PAWN_PST[from],
            p @ Piece::Knight => self.pst[side][p] += KNIGHT_PST[from],
            p @ Piece::Bishop => self.pst[side][p] += BISHOP_PST[from],
            p @ Piece::Rook => self.pst[side][p] += ROOK_PST[from],
            p @ Piece::Queen => self.pst[side][p] += QUEEN_PST[from],
            p @ Piece::King => self.pst[side][p] += KING_PST[from],
        }

        match mov.promotion.unwrap_or(mov.piece) {
            p @ Piece::Pawn => self.pst[side][p] -= PAWN_PST[to],
            p @ Piece::Knight => self.pst[side][p] -= KNIGHT_PST[to],
            p @ Piece::Bishop => self.pst[side][p] -= BISHOP_PST[to],
            p @ Piece::Rook => self.pst[side][p] -= ROOK_PST[to],
            p @ Piece::Queen => self.pst[side][p] -= QUEEN_PST[to],
            Piece::King => {
                // Castling
                if (us & position.rooks()) & mov.to {
                    match mov.to.file().cmp(&mov.from.file()) {
                        std::cmp::Ordering::Less => {
                            // queenside
                            let a1 = Square::from_file_rank(File::A, Rank::One);
                            let c1 = Square::from_file_rank(File::C, Rank::One);
                            let d1 = Square::from_file_rank(File::D, Rank::One);
                            self.pst[side][Piece::King] -= KING_PST[c1];
                            self.pst[side][Piece::Rook] += ROOK_PST[a1];
                            self.pst[side][Piece::Rook] -= ROOK_PST[d1];
                        }
                        std::cmp::Ordering::Greater => {
                            // kingside
                            let h1 = Square::from_file_rank(File::H, Rank::One);
                            let g1 = Square::from_file_rank(File::G, Rank::One);
                            let f1 = Square::from_file_rank(File::F, Rank::One);
                            self.pst[side][Piece::King] -= KING_PST[g1];
                            self.pst[side][Piece::Rook] += ROOK_PST[h1];
                            self.pst[side][Piece::Rook] -= ROOK_PST[f1];
                        }
                        std::cmp::Ordering::Equal => {}
                    }
                } else {
                    self.pst[side][Piece::King] -= KING_PST[to]
                }
            }
        }

        let ep = mov
            .to
            .backward(position.side_to_move())
            .filter(|_| mov.en_passant)
            .map(|sq| sq.normalize(!position.side_to_move()));
        match mov.capture {
            None | Some(Piece::King) => {}
            Some(p @ Piece::Pawn) => {
                if let Some(ep) = ep {
                    self.pst[1 - side][p] += PAWN_PST[ep];
                } else {
                    self.pst[1 - side][p] += PAWN_PST[mov.to.normalize(!position.side_to_move())];
                }
            }
            Some(p @ Piece::Knight) => {
                self.pst[1 - side][p] += KNIGHT_PST[mov.to.normalize(!position.side_to_move())]
            }
            Some(p @ Piece::Bishop) => {
                self.pst[1 - side][p] += BISHOP_PST[mov.to.normalize(!position.side_to_move())]
            }
            Some(p @ Piece::Rook) => {
                self.pst[1 - side][p] += ROOK_PST[mov.to.normalize(!position.side_to_move())]
            }
            Some(p @ Piece::Queen) => {
                self.pst[1 - side][p] += QUEEN_PST[mov.to.normalize(!position.side_to_move())]
            }
        }
    }

    #[cfg(feature = "tune")]
    fn trace_incrementals(&mut self, pos: &Position) {
        let w = Side::White as usize;
        let b = Side::Black as usize;
        self.trace.material.0[w] = self.material[w].map(|x| *x as i8);
        self.trace.material.0[b] = self.material[b].map(|x| *x as i8);

        for side in [Side::White, Side::Black] {
            for sq in pos.pawns() & pos.pieces(side) {
                self.trace.pawn_pst.inner[side as usize][sq.normalize(side)] += 1;
            }
            for sq in pos.knights() & pos.pieces(side) {
                self.trace.knight_pst.inner[side as usize][sq.normalize(side)] += 1;
            }
            for sq in pos.bishops() & pos.pieces(side) {
                self.trace.bishop_pst.inner[side as usize][sq.normalize(side)] += 1;
            }
            for sq in pos.rooks() & pos.pieces(side) {
                self.trace.rook_pst.inner[side as usize][sq.normalize(side)] += 1;
            }
            for sq in pos.queens() & pos.pieces(side) {
                self.trace.queen_pst.inner[side as usize][sq.normalize(side)] += 1;
            }
            for sq in pos.kings() & pos.pieces(side) {
                self.trace.king_pst.inner[side as usize][sq.normalize(side)] += 1;
            }
        }
    }
}

fn pst_for_piece<const WHITE: bool>(position: &Position, piece: Piece) -> EScore {
    let mut score = S(0, 0);
    let side = Side::white(WHITE);
    match piece {
        Piece::Pawn => {
            for sq in position.pieces(side) & position.pawns() {
                score += PAWN_PST[sq.normalize(side)];
            }
        }
        Piece::Knight => {
            for sq in position.pieces(side) & position.knights() {
                score += KNIGHT_PST[sq.normalize(side)];
            }
        }
        Piece::Bishop => {
            for sq in position.pieces(side) & position.bishops() {
                score += BISHOP_PST[sq.normalize(side)];
            }
        }
        Piece::Rook => {
            for sq in position.pieces(side) & position.rooks() {
                score += ROOK_PST[sq.normalize(side)];
            }
        }
        Piece::Queen => {
            for sq in position.pieces(side) & position.queens() {
                score += QUEEN_PST[sq.normalize(side)];
            }
        }
        Piece::King => {
            for sq in position.pieces(side) & position.kings() {
                score += KING_PST[sq.normalize(side)];
            }
        }
    }

    score
}

#[allow(non_snake_case)]
pub const fn S(mg: i16, eg: i16) -> EScore {
    ((eg as u32) << 16) as EScore + mg as EScore
}

pub const fn mg(s: EScore) -> i16 {
    (s & 0xFFFF) as i16
}

pub const fn eg(s: EScore) -> i16 {
    ((s + 0x8000) as u32 >> 16) as i16
}

const PHASE_WEIGTHS: PieceMap<i16> = PieceMap::new([0, 3, 3, 5, 9, 0]);

fn interpolate(score: EScore, phase: i16) -> Score {
    let mg = i32::from(mg(score));
    let eg = i32::from(eg(score));
    let phase = i32::from(phase);

    ((mg * phase + eg * (62 - phase)) / 62) as Score
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::{prop_assume, proptest};

    proptest! {
        #[test]
        fn mg(mg: i16, eg: i16) {
            assert_eq!(super::mg(S(mg, eg)), mg)
        }

        #[test]
        fn eg(mg: i16, eg: i16) {
            assert_eq!(super::eg(S(mg, eg)), eg)
        }

        #[test]
        fn addition(mg1: i16, eg1: i16, mg2: i16, eg2: i16) {
            prop_assume!(mg1.checked_add(mg2).is_some());
            let a = S(mg1, eg1);
            let b = S(mg2, eg2);
            let add = S(mg1 + mg2, eg1 + eg2);
            assert_eq!(a + b, add);
        }

        #[test]
        fn subtraction(mg1: i16, eg1: i16, mg2: i16, eg2: i16) {
            prop_assume!(mg1.checked_sub(mg2).is_some());
            let a = S(mg1, eg1);
            let b = S(mg2, eg2);
            let add = S(mg1 - mg2, eg1 - eg2);
            assert_eq!(a - b, add);
        }
    }
}
