use crate::{
    hash::Hash,
    movegen::{bishop_from, rook_from, Move, KNIGHT_ATTACKS},
    position::Position,
    tune::Trace,
    types::{Bitboard, File, Piece, PieceMap, Rank, Side, Square, SquareMap},
};

pub type Score = i16;
pub(crate) type EScore = i32;

#[rustfmt::skip]
pub const PIECE_VALUES: PieceMap<EScore> = PieceMap::new([
    S( 100,  155),
    S( 402,  313),
    S( 442,  346),
    S( 542,  625),
    S(1099, 1197),
    S(   0,    0),
]);

#[rustfmt::skip]
pub static PAWN_PST: SquareMap<EScore> = SquareMap::visual([
    S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0),
    S(  15,  211), S(  61,  197), S(  50,  171), S(  55,  155), S(  55,  155), S(  50,  171), S(  61,  197), S(  15,  211),
    S( -41,    6), S( -11,    7), S(  17,  -19), S(   4,  -48), S(   4,  -48), S(  17,  -19), S( -11,    7), S( -41,    6),
    S( -35,  -24), S(   8,  -27), S(   4,  -41), S(  16,  -55), S(  16,  -55), S(   4,  -41), S(   8,  -27), S( -35,  -24),
    S( -40,  -38), S(  -6,  -34), S(  -3,  -47), S(   8,  -55), S(   8,  -55), S(  -3,  -47), S(  -6,  -34), S( -40,  -38),
    S( -29,  -49), S(   6,  -45), S(   0,  -50), S(  -9,  -42), S(  -9,  -42), S(   0,  -50), S(   6,  -45), S( -29,  -49),
    S( -39,  -47), S(  15,  -49), S( -14,  -40), S( -33,  -31), S( -33,  -31), S( -14,  -40), S(  15,  -49), S( -39,  -47),
    S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0), S(   0,    0),
]);

#[rustfmt::skip]
pub static KNIGHT_PST: SquareMap<EScore> = SquareMap::visual([
    S(-100,  -72), S( -36,  -40), S( -40,  -11), S(  -4,  -15), S(  -4,  -15), S( -40,  -11), S( -36,  -40), S(-100,  -72),
    S( -47,  -22), S( -23,    1), S(  62,  -16), S(  12,   12), S(  12,   12), S(  62,  -16), S( -23,    1), S( -47,  -22),
    S( -10,  -19), S(  63,  -11), S(  46,   23), S(  69,   16), S(  69,   16), S(  46,   23), S(  63,  -11), S( -10,  -19),
    S(  10,   -1), S(  14,   21), S(  27,   38), S(  31,   44), S(  31,   44), S(  27,   38), S(  14,   21), S(  10,   -1),
    S(  -5,    1), S(  11,   14), S(  15,   37), S(  18,   42), S(  18,   42), S(  15,   37), S(  11,   14), S(  -5,    1),
    S( -16,   -5), S(   6,    1), S(  12,   11), S(  14,   30), S(  14,   30), S(  12,   11), S(   6,    1), S( -16,   -5),
    S( -15,  -24), S( -27,   -1), S(  -1,    0), S(  15,    3), S(  15,    3), S(  -1,    0), S( -27,   -1), S( -15,  -24),
    S( -49,  -35), S(  -5,  -33), S( -35,    1), S( -12,    5), S( -12,    5), S( -35,    1), S(  -5,  -33), S( -49,  -35),
]);

#[rustfmt::skip]
pub static BISHOP_PST: SquareMap<EScore> = SquareMap::visual([
    S( -28,  -12), S( -18,   -9), S( -42,  -11), S( -23,   -8), S( -23,   -8), S( -42,  -11), S( -18,   -9), S( -28,  -12),
    S( -49,    2), S(   4,   -3), S(  -1,    0), S(  -1,   -7), S(  -1,   -7), S(  -1,    0), S(   4,   -3), S( -49,    2),
    S( -21,    8), S(  26,   -3), S(  32,    0), S(  25,   -5), S(  25,   -5), S(  32,    0), S(  26,   -3), S( -21,    8),
    S( -11,    3), S(   3,    4), S(  13,    9), S(  31,   11), S(  31,   11), S(  13,    9), S(   3,    4), S( -11,    3),
    S(  -4,    0), S(  -1,    2), S(   0,   11), S(  19,   13), S(  19,   13), S(   0,   11), S(  -1,    2), S(  -4,    0),
    S(   3,   -4), S(  12,    2), S(  21,    3), S(   5,   13), S(   5,   13), S(  21,    3), S(  12,    2), S(   3,   -4),
    S(   3,  -13), S(  34,  -13), S(  12,   -3), S(   7,    0), S(   7,    0), S(  12,   -3), S(  34,  -13), S(   3,  -13),
    S( -27,   -3), S(  -9,    4), S(  -3,    1), S( -13,    8), S( -13,    8), S(  -3,    1), S(  -9,    4), S( -27,   -3),
]);

#[rustfmt::skip]
pub static ROOK_PST: SquareMap<EScore> = SquareMap::visual([
    S(   5,   10), S(  11,    8), S(   0,   14), S(  37,    6), S(  37,    6), S(   0,   14), S(  11,    8), S(   5,   10),
    S(  26,    4), S(  21,    7), S(  68,   -4), S(  65,   -6), S(  65,   -6), S(  68,   -4), S(  21,    7), S(  26,    4),
    S( -11,    2), S(  23,   -2), S(  18,   -3), S(  14,   -2), S(  14,   -2), S(  18,   -3), S(  23,   -2), S( -11,    2),
    S( -36,    7), S( -20,    0), S(   6,    4), S(   7,   -3), S(   7,   -3), S(   6,    4), S( -20,    0), S( -36,    7),
    S( -48,    6), S( -19,    0), S( -18,    3), S( -11,    0), S( -11,    0), S( -18,    3), S( -19,    0), S( -48,    6),
    S( -44,    0), S( -20,   -1), S( -10,   -7), S( -13,   -3), S( -13,   -3), S( -10,   -7), S( -20,   -1), S( -44,    0),
    S( -50,    4), S(  -8,   -8), S(  -4,   -4), S(  -1,   -2), S(  -1,   -2), S(  -4,   -4), S(  -8,   -8), S( -50,    4),
    S(  -9,  -13), S( -11,    0), S(  13,   -6), S(  23,  -11), S(  23,  -11), S(  13,   -6), S( -11,    0), S(  -9,  -13),
]);

#[rustfmt::skip]
pub static QUEEN_PST: SquareMap<EScore> = SquareMap::visual([
    S(  -1,   -5), S(   4,   14), S(  16,   23), S(  28,   26), S(  28,   26), S(  16,   23), S(   4,   14), S(  -1,   -5),
    S(   0,  -22), S( -42,   17), S(  18,   23), S(   3,   35), S(   3,   35), S(  18,   23), S( -42,   17), S(   0,  -22),
    S(  19,  -21), S(  16,   -5), S(  16,   16), S(  19,   44), S(  19,   44), S(  16,   16), S(  16,   -5), S(  19,  -21),
    S(  -7,    2), S( -21,   34), S(  -3,   22), S( -15,   52), S( -15,   52), S(  -3,   22), S( -21,   34), S(  -7,    2),
    S(  -6,  -15), S( -14,   24), S(  -6,   22), S( -11,   43), S( -11,   43), S(  -6,   22), S( -14,   24), S(  -6,  -15),
    S( -12,  -13), S(   8,  -27), S(  -1,   10), S(   1,    0), S(   1,    0), S(  -1,   10), S(   8,  -27), S( -12,  -13),
    S( -27,  -30), S(   0,  -37), S(  19,  -45), S(  18,  -33), S(  18,  -33), S(  19,  -45), S(   0,  -37), S( -27,  -30),
    S( -14,  -40), S(  -9,  -37), S(  -6,  -35), S(  13,  -42), S(  13,  -42), S(  -6,  -35), S(  -9,  -37), S( -14,  -40),
]);

#[rustfmt::skip]
pub static KING_PST: SquareMap<EScore> = SquareMap::visual([
    S(  27,  -41), S(  24,  -19), S(  11,   -5), S(  -2,  -19), S(  -2,  -19), S(  11,   -5), S(  24,  -19), S(  27,  -41),
    S(  22,   -9), S(  16,   12), S(   8,   25), S(   1,   18), S(   1,   18), S(   8,   25), S(  16,   12), S(  22,   -9),
    S(  14,   -7), S(  25,   25), S(  15,   32), S(  -8,   19), S(  -8,   19), S(  15,   32), S(  25,   25), S(  14,   -7),
    S(  -8,  -23), S(  -5,    9), S( -11,   24), S( -27,   28), S( -27,   28), S( -11,   24), S(  -5,    9), S(  -8,  -23),
    S( -33,  -30), S( -14,   -4), S( -34,   22), S( -65,   35), S( -65,   35), S( -34,   22), S( -14,   -4), S( -33,  -30),
    S( -10,  -21), S(   4,    0), S( -28,   20), S( -52,   33), S( -52,   33), S( -28,   20), S(   4,    0), S( -10,  -21),
    S(  23,  -31), S(  13,   -8), S( -29,   18), S( -43,   25), S( -43,   25), S( -29,   18), S(  13,   -8), S(  23,  -31),
    S(  47,  -63), S(  60,  -32), S(  13,   -9), S(  46,  -26), S(  46,  -26), S(  13,   -9), S(  60,  -32), S(  47,  -63),
]);

#[rustfmt::skip]
pub static KNIGHT_MOBILITY: [EScore; 9] = [
    S( -40,  -17), S( -21,  -20), S( -10,   -4), S(  -7,    3), S(   3,    4), S(   9,   10), S(  16,    7), S(  20,    6),
    S(  26,   -4),
];

#[rustfmt::skip]
pub static BISHOP_MOBILITY: [EScore; 14] = [
    S( -40,  -52), S( -26,  -48), S( -13,  -36), S( -10,  -22), S(  -6,  -10), S(   0,   -1), S(   4,    4), S(   5,   10),
    S(  11,   14), S(  13,   12), S(  16,   13), S(  19,   12), S(   3,   11), S(   6,   19),
];

#[rustfmt::skip]
pub static ROOK_MOBILITY: [EScore; 15] = [
    S( -39,  -66), S( -27,  -57), S( -26,  -33), S( -23,  -24), S( -21,  -13), S( -16,   -7), S(  -9,   -2), S(  -2,   -1),
    S(   2,    5), S(  11,    5), S(  21,    9), S(  27,   10), S(  37,   15), S(  44,   16), S(  24,   20),
];

#[rustfmt::skip]
pub const DOUBLED_PAWN: EScore = S(  -5,  -35);

#[rustfmt::skip]
pub const WEAK_PAWN: EScore = S(  -3,   -8);

#[rustfmt::skip]
pub static PASSED_PAWN_ON_RANK: [EScore; 8] = [
    S(   0,    0), S(   0,   -5), S(  -8,    0), S( -16,   24), S(  -1,   55), S(  22,  114), S(   0,    0), S(   0,    0),
];

#[rustfmt::skip]
pub static PAWN_SHIELD_KING_FILE: [EScore; 8] = [
    S(   0,    0), S(  28,    4), S(  20,    8), S(   4,   -1), S(  -1,  -11), S( -10,  -12), S(  -1,   -6), S( -38,   18),
];

#[rustfmt::skip]
pub static PAWN_SHIELD_OTHER_FILE: [EScore; 8] = [
    S(  26,    8), S(  18,   -2), S(   1,    6), S(   0,   -5), S(  -7,   -9), S(  -5,   -6), S(  -2,   -3), S( -32,   13),
];

#[rustfmt::skip]
pub static PAWN_STORM: [EScore; 8] = [
    S(   7,   37), S( -13,   40), S( -33,    1), S(   1,  -10), S(   9,  -13), S(  14,  -19), S(   7,  -18), S(   4,  -15),
];

const SF_BASE: i16 = 128;
const SF_PAWNLESS: i16 = 64;

#[derive(Clone, Debug)]
pub struct Eval {
    material: [PieceMap<u8>; 2],
    pst: [PieceMap<EScore>; 2],

    pk_table: PkTable,

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
            pk_table: PkTable::new(),
            trace: Trace::default(),
        }
    }
}

impl Eval {
    pub fn evaluate(&mut self, pos: &Position, pk_hash: Hash) -> Score {
        let mut score = S(0, 0);

        score += self.material::<true>() - self.material::<false>();
        score += self.mobility::<true>(pos) - self.mobility::<false>(pos);
        score += self.pst::<true>() - self.pst::<false>();

        // Check Pawn-King table match
        #[cfg(not(feature = "tune"))]
        {
            let pawns = pos.pawns();
            let white = pawns & pos.pieces(Side::White);
            if let Some(entry) = self.pk_table.get(pk_hash, pawns, white) {
                score += entry.eval;
            } else {
                let pawn_score = self.pawns::<true>(pos) - self.pawns::<false>(pos);
                score += pawn_score;
                // Store in Pawn-King table
                let pawns = pos.pawns();
                let white = pawns & pos.pieces(Side::White);
                let entry = PkEntry {
                    pawns,
                    white,
                    eval: pawn_score,
                };
                self.pk_table.insert(pk_hash, entry);
            }
        }

        #[cfg(feature = "tune")]
        {
            let _ = pk_hash;
            score += self.pawns::<true>(pos) - self.pawns::<false>(pos);
        }

        score += self.kings::<true>(pos) - self.kings::<false>(pos);

        let sf = self.scale_factor(score);

        #[cfg(feature = "tune")]
        {
            self.trace_incrementals(pos);
            self.trace.eval = score;
            self.trace.sf = f32::from(sf) / f32::from(SF_BASE);
        }

        let score = scale(score, sf);
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

    fn mobility<const WHITE: bool>(&mut self, pos: &Position) -> EScore {
        let mut score = S(0, 0);
        let side = Side::white(WHITE);
        let us = pos.pieces(side);

        let defended_by_their_pawns = pos.attacks(!side, Piece::Pawn);
        for knight in pos.knights() & pos.pieces(side) {
            let attacked = KNIGHT_ATTACKS[knight];
            let mobility = attacked & !us & !defended_by_their_pawns;
            let mobility = mobility.popcount();

            score += KNIGHT_MOBILITY[mobility];

            #[cfg(feature = "tune")]
            {
                self.trace.knight_mobility.inner[side as usize][mobility] += 1;
            }
        }

        for bishop in pos.bishops() & pos.pieces(side) {
            let attacked = bishop_from(bishop, pos.all_pieces());
            let mobility = attacked & !us;
            let mobility = mobility.popcount();

            score += BISHOP_MOBILITY[mobility];

            #[cfg(feature = "tune")]
            {
                self.trace.bishop_mobility.inner[side as usize][mobility] += 1;
            }
        }

        for rook in pos.rooks() & pos.pieces(side) {
            let attacked = rook_from(rook, pos.all_pieces());
            let mobility = attacked & !us;
            let mobility = mobility.popcount();

            score += ROOK_MOBILITY[mobility];

            #[cfg(feature = "tune")]
            {
                self.trace.rook_mobility.inner[side as usize][mobility] += 1;
            }
        }

        score
    }

    fn pawns<const WHITE: bool>(&mut self, pos: &Position) -> EScore {
        let mut score = S(0, 0);
        let side = Side::white(WHITE);
        let us = pos.pieces(side);
        let them = pos.pieces(!side);

        let potential_weak_pawns = pos.pawns() & us // our pawns which
            & pos.attacks(!side, Piece::Pawn).backward(1, side) // cannot be pushed safely, and
            & !pos.attacks(!side, Piece::Pawn); // cannot capture

        for pawn in pos.pawns() & us {
            let file = pawn.file().as_bb();
            let corridor = PAWN_CORRIDOR[side as usize][pawn];
            let frontspan = file & corridor;

            let doubled = (frontspan & pos.pawns() & us).at_least_one();
            let weak = potential_weak_pawns.contains(pawn)
                && (PAWN_CORRIDOR[1 - side as usize][pawn] & pos.pawns() & us).is_empty();
            let passed = (corridor & pos.pawns() & them).is_empty();

            if doubled {
                score += DOUBLED_PAWN;
                #[cfg(feature = "tune")]
                {
                    self.trace.doubled_pawn.inner[side as usize] += 1;
                }
            }

            if weak {
                score += WEAK_PAWN;
                #[cfg(feature = "tune")]
                {
                    self.trace.weak_pawn.inner[side as usize] += 1;
                }
            }

            if passed && !doubled {
                let normalized_rank = pawn.normalize(side).rank() as usize;

                // Any pawn on rank 7 is a passer. No need to score or trace (which could lead to
                // issues when tuning at the same time as the pawn PST, because it traces the same
                // condition).
                // normalized_rank starts at 0 for rank 1, therefore we need to compare with 6.
                if normalized_rank < 6 {
                    score += PASSED_PAWN_ON_RANK[normalized_rank];
                    #[cfg(feature = "tune")]
                    {
                        self.trace.passed_pawn_on_rank.inner[side as usize][normalized_rank] += 1;
                    }
                }
            }
        }

        score
    }

    fn kings<const WHITE: bool>(&mut self, pos: &Position) -> EScore {
        let mut score = S(0, 0);
        let side = Side::white(WHITE);
        let us = pos.pieces(side);
        let them = pos.pieces(!side);

        let king_sq = pos.king_sq(side);

        let file = king_sq.file();
        let (center_neighbor_file, non_center_neighbor_file) = match file {
            File::A => (File::C, File::B),
            File::B => (File::C, File::A),
            File::C => (File::D, File::B),
            File::D => (File::E, File::C),
            File::E => (File::D, File::F),
            File::F => (File::E, File::G),
            File::G => (File::F, File::H),
            File::H => (File::F, File::G),
        };

        // Pawn shield
        {
            let our_pawns = pos.pawns() & us;
            let our_pawns_in_front =
                our_pawns.backward(king_sq.normalize(side).rank() as usize, side);
            let normalized_our_pawns_in_front = if WHITE {
                our_pawns_in_front
            } else {
                our_pawns_in_front.flip()
            };

            let pawn_shield = [file, center_neighbor_file, non_center_neighbor_file].map(|file| {
                let in_front = file.as_bb() & normalized_our_pawns_in_front;
                let pawn = in_front.lsb_sq();
                let distance = pawn.map_or(7, |pawn| pawn.rank() as usize);
                distance
            });

            score += PAWN_SHIELD_KING_FILE[pawn_shield[0]];
            score += PAWN_SHIELD_OTHER_FILE[pawn_shield[1]];
            score += PAWN_SHIELD_OTHER_FILE[pawn_shield[2]];

            #[cfg(feature = "tune")]
            {
                self.trace.pawn_shield_king_file.inner[side as usize][pawn_shield[0]] += 1;
                self.trace.pawn_shield_other_file.inner[side as usize][pawn_shield[1]] += 1;
                self.trace.pawn_shield_other_file.inner[side as usize][pawn_shield[2]] += 1;
            }
        }

        // Pawn storm
        {
            let their_pawns = pos.pawns() & them;
            let their_pawns_in_front =
                their_pawns.backward(king_sq.normalize(side).rank() as usize, side);
            let normalized_their_pawns_in_front = if WHITE {
                their_pawns_in_front
            } else {
                their_pawns_in_front.flip()
            };

            let pawn_storm = [file, center_neighbor_file, non_center_neighbor_file].map(|file| {
                let in_front = file.as_bb() & normalized_their_pawns_in_front;
                let pawn = in_front.lsb_sq();
                let distance = pawn.map_or(7, |pawn| pawn.rank() as usize);
                distance
            });

            score += PAWN_STORM[pawn_storm[0]];
            score += PAWN_STORM[pawn_storm[1]];
            score += PAWN_STORM[pawn_storm[2]];

            #[cfg(feature = "tune")]
            {
                self.trace.pawn_storm.inner[side as usize][pawn_storm[0]] += 1;
                self.trace.pawn_storm.inner[side as usize][pawn_storm[1]] += 1;
                self.trace.pawn_storm.inner[side as usize][pawn_storm[2]] += 1;
            }
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

    fn scale_factor(&mut self, score: EScore) -> i16 {
        let mut sf = SF_BASE;

        if eg(score) > 0 && self.material[Side::White as usize][Piece::Pawn] == 0
            || eg(score) < 0 && self.material[Side::Black as usize][Piece::Pawn] == 0
        {
            sf = SF_PAWNLESS;
        }

        sf
    }

    pub fn non_pawn_material(&self, side: Side) -> u8 {
        let mut result = 0;
        result += self.material[side as usize][Piece::Knight];
        result += self.material[side as usize][Piece::Bishop];
        result += self.material[side as usize][Piece::Rook];
        result += self.material[side as usize][Piece::Queen];

        result
    }

    pub fn highest_value_piece(&self, side: Side) -> Option<Piece> {
        for piece in [
            Piece::Queen,
            Piece::Rook,
            Piece::Bishop,
            Piece::Knight,
            Piece::Pawn,
        ] {
            if self.material[side as usize][piece] > 0 {
                return Some(piece);
            }
        }

        None
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

    pub fn make_nullmove(&mut self) {}
    pub fn unmake_nullmove(&mut self) {}

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

fn scale(score: EScore, scale: i16) -> EScore {
    let eg = i32::from(eg(score));
    let eg = (eg * i32::from(scale)) / i32::from(SF_BASE);
    S(mg(score), eg as i16)
}

fn interpolate(score: EScore, phase: i16) -> Score {
    let mg = i32::from(mg(score));
    let eg = i32::from(eg(score));
    let phase = i32::from(phase);

    ((mg * phase + eg * (62 - phase)) / 62) as Score
}

static PAWN_CORRIDOR: [SquareMap<Bitboard>; 2] = [
    // White
    SquareMap::new([
        // Rank 1
        Bitboard::new(0x03_03_03_03_03_03_03_00),
        Bitboard::new(0x07_07_07_07_07_07_07_00),
        Bitboard::new(0x0E_0E_0E_0E_0E_0E_0E_00),
        Bitboard::new(0x1C_1C_1C_1C_1C_1C_1C_00),
        Bitboard::new(0x38_38_38_38_38_38_38_00),
        Bitboard::new(0x70_70_70_70_70_70_70_00),
        Bitboard::new(0xE0_E0_E0_E0_E0_E0_E0_00),
        Bitboard::new(0xC0_C0_C0_C0_C0_C0_C0_00),
        // Rank 2
        Bitboard::new(0x03_03_03_03_03_03_00_00),
        Bitboard::new(0x07_07_07_07_07_07_00_00),
        Bitboard::new(0x0E_0E_0E_0E_0E_0E_00_00),
        Bitboard::new(0x1C_1C_1C_1C_1C_1C_00_00),
        Bitboard::new(0x38_38_38_38_38_38_00_00),
        Bitboard::new(0x70_70_70_70_70_70_00_00),
        Bitboard::new(0xE0_E0_E0_E0_E0_E0_00_00),
        Bitboard::new(0xC0_C0_C0_C0_C0_C0_00_00),
        // Rank 3
        Bitboard::new(0x03_03_03_03_03_00_00_00),
        Bitboard::new(0x07_07_07_07_07_00_00_00),
        Bitboard::new(0x0E_0E_0E_0E_0E_00_00_00),
        Bitboard::new(0x1C_1C_1C_1C_1C_00_00_00),
        Bitboard::new(0x38_38_38_38_38_00_00_00),
        Bitboard::new(0x70_70_70_70_70_00_00_00),
        Bitboard::new(0xE0_E0_E0_E0_E0_00_00_00),
        Bitboard::new(0xC0_C0_C0_C0_C0_00_00_00),
        // Rank 4
        Bitboard::new(0x03_03_03_03_00_00_00_00),
        Bitboard::new(0x07_07_07_07_00_00_00_00),
        Bitboard::new(0x0E_0E_0E_0E_00_00_00_00),
        Bitboard::new(0x1C_1C_1C_1C_00_00_00_00),
        Bitboard::new(0x38_38_38_38_00_00_00_00),
        Bitboard::new(0x70_70_70_70_00_00_00_00),
        Bitboard::new(0xE0_E0_E0_E0_00_00_00_00),
        Bitboard::new(0xC0_C0_C0_C0_00_00_00_00),
        // Rank 5
        Bitboard::new(0x03_03_03_00_00_00_00_00),
        Bitboard::new(0x07_07_07_00_00_00_00_00),
        Bitboard::new(0x0E_0E_0E_00_00_00_00_00),
        Bitboard::new(0x1C_1C_1C_00_00_00_00_00),
        Bitboard::new(0x38_38_38_00_00_00_00_00),
        Bitboard::new(0x70_70_70_00_00_00_00_00),
        Bitboard::new(0xE0_E0_E0_00_00_00_00_00),
        Bitboard::new(0xC0_C0_C0_00_00_00_00_00),
        // Rank 6
        Bitboard::new(0x03_03_00_00_00_00_00_00),
        Bitboard::new(0x07_07_00_00_00_00_00_00),
        Bitboard::new(0x0E_0E_00_00_00_00_00_00),
        Bitboard::new(0x1C_1C_00_00_00_00_00_00),
        Bitboard::new(0x38_38_00_00_00_00_00_00),
        Bitboard::new(0x70_70_00_00_00_00_00_00),
        Bitboard::new(0xE0_E0_00_00_00_00_00_00),
        Bitboard::new(0xC0_C0_00_00_00_00_00_00),
        // Rank 7
        Bitboard::new(0x03_00_00_00_00_00_00_00),
        Bitboard::new(0x07_00_00_00_00_00_00_00),
        Bitboard::new(0x0E_00_00_00_00_00_00_00),
        Bitboard::new(0x1C_00_00_00_00_00_00_00),
        Bitboard::new(0x38_00_00_00_00_00_00_00),
        Bitboard::new(0x70_00_00_00_00_00_00_00),
        Bitboard::new(0xE0_00_00_00_00_00_00_00),
        Bitboard::new(0xC0_00_00_00_00_00_00_00),
        // Rank 8
        Bitboard::new(0),
        Bitboard::new(0),
        Bitboard::new(0),
        Bitboard::new(0),
        Bitboard::new(0),
        Bitboard::new(0),
        Bitboard::new(0),
        Bitboard::new(0),
    ]),
    // Black
    SquareMap::new([
        // Rank 1
        Bitboard::new(0x0),
        Bitboard::new(0x0),
        Bitboard::new(0x0),
        Bitboard::new(0x0),
        Bitboard::new(0x0),
        Bitboard::new(0x0),
        Bitboard::new(0x0),
        Bitboard::new(0x0),
        // Rank 2
        Bitboard::new(0x00_00_00_00_00_00_00_03),
        Bitboard::new(0x00_00_00_00_00_00_00_07),
        Bitboard::new(0x00_00_00_00_00_00_00_0E),
        Bitboard::new(0x00_00_00_00_00_00_00_1C),
        Bitboard::new(0x00_00_00_00_00_00_00_38),
        Bitboard::new(0x00_00_00_00_00_00_00_70),
        Bitboard::new(0x00_00_00_00_00_00_00_E0),
        Bitboard::new(0x00_00_00_00_00_00_00_C0),
        // Rank 3
        Bitboard::new(0x00_00_00_00_00_00_03_03),
        Bitboard::new(0x00_00_00_00_00_00_07_07),
        Bitboard::new(0x00_00_00_00_00_00_0E_0E),
        Bitboard::new(0x00_00_00_00_00_00_1C_1C),
        Bitboard::new(0x00_00_00_00_00_00_38_38),
        Bitboard::new(0x00_00_00_00_00_00_70_70),
        Bitboard::new(0x00_00_00_00_00_00_E0_E0),
        Bitboard::new(0x00_00_00_00_00_00_C0_C0),
        // Rank 4
        Bitboard::new(0x00_00_00_00_00_03_03_03),
        Bitboard::new(0x00_00_00_00_00_07_07_07),
        Bitboard::new(0x00_00_00_00_00_0E_0E_0E),
        Bitboard::new(0x00_00_00_00_00_1C_1C_1C),
        Bitboard::new(0x00_00_00_00_00_38_38_38),
        Bitboard::new(0x00_00_00_00_00_70_70_70),
        Bitboard::new(0x00_00_00_00_00_E0_E0_E0),
        Bitboard::new(0x00_00_00_00_00_C0_C0_C0),
        // Rank 5
        Bitboard::new(0x00_00_00_00_03_03_03_03),
        Bitboard::new(0x00_00_00_00_07_07_07_07),
        Bitboard::new(0x00_00_00_00_0E_0E_0E_0E),
        Bitboard::new(0x00_00_00_00_1C_1C_1C_1C),
        Bitboard::new(0x00_00_00_00_38_38_38_38),
        Bitboard::new(0x00_00_00_00_70_70_70_70),
        Bitboard::new(0x00_00_00_00_E0_E0_E0_E0),
        Bitboard::new(0x00_00_00_00_C0_C0_C0_C0),
        // Rank 6
        Bitboard::new(0x00_00_00_03_03_03_03_03),
        Bitboard::new(0x00_00_00_07_07_07_07_07),
        Bitboard::new(0x00_00_00_0E_0E_0E_0E_0E),
        Bitboard::new(0x00_00_00_1C_1C_1C_1C_1C),
        Bitboard::new(0x00_00_00_38_38_38_38_38),
        Bitboard::new(0x00_00_00_70_70_70_70_70),
        Bitboard::new(0x00_00_00_E0_E0_E0_E0_E0),
        Bitboard::new(0x00_00_00_C0_C0_C0_C0_C0),
        // Rank 7
        Bitboard::new(0x00_00_03_03_03_03_03_03),
        Bitboard::new(0x00_00_07_07_07_07_07_07),
        Bitboard::new(0x00_00_0E_0E_0E_0E_0E_0E),
        Bitboard::new(0x00_00_1C_1C_1C_1C_1C_1C),
        Bitboard::new(0x00_00_38_38_38_38_38_38),
        Bitboard::new(0x00_00_70_70_70_70_70_70),
        Bitboard::new(0x00_00_E0_E0_E0_E0_E0_E0),
        Bitboard::new(0x00_00_C0_C0_C0_C0_C0_C0),
        // Rank 8
        Bitboard::new(0x00_03_03_03_03_03_03_03),
        Bitboard::new(0x00_07_07_07_07_07_07_07),
        Bitboard::new(0x00_0E_0E_0E_0E_0E_0E_0E),
        Bitboard::new(0x00_1C_1C_1C_1C_1C_1C_1C),
        Bitboard::new(0x00_38_38_38_38_38_38_38),
        Bitboard::new(0x00_70_70_70_70_70_70_70),
        Bitboard::new(0x00_E0_E0_E0_E0_E0_E0_E0),
        Bitboard::new(0x00_C0_C0_C0_C0_C0_C0_C0),
    ]),
];

const PAWN_KING_TABLE_SIZE: usize = 2048;

#[derive(Clone, Debug)]
struct PkTable {
    entries: Vec<PkEntry>,
}

impl PkTable {
    fn new() -> Self {
        Self {
            entries: vec![PkEntry::default(); PAWN_KING_TABLE_SIZE],
        }
    }

    fn get(&self, pk_hash: Hash, pawns: Bitboard, white: Bitboard) -> Option<&PkEntry> {
        let i = pk_hash as usize % PAWN_KING_TABLE_SIZE;
        let entry = &self.entries[i];
        if entry.pawns == pawns && entry.white == white {
            Some(entry)
        } else {
            None
        }
    }

    fn insert(&mut self, pk_hash: Hash, entry: PkEntry) {
        let i = pk_hash as usize % PAWN_KING_TABLE_SIZE;
        self.entries[i] = entry;
    }
}

#[derive(Copy, Clone, Debug, Default)]
struct PkEntry {
    pawns: Bitboard,
    white: Bitboard,
    eval: EScore,
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

    #[test]
    fn pawn_corridor() {
        for side in [Side::White, Side::Black] {
            for sq in Bitboard::all() {
                let mut front_span = sq.as_bb().forward(1, side);
                front_span |= front_span.forward(1, side);
                front_span |= front_span.forward(2, side);
                front_span |= front_span.forward(4, side);

                let corridor = front_span.west(1) | front_span | front_span.east(1);

                assert_eq!(
                    PAWN_CORRIDOR[side as usize][sq], corridor,
                    "Wrong pawn corridor; side {side:?} sq {sq}"
                );
            }
        }
    }
}
