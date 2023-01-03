use std::cmp::Ordering;

use crate::{
    movegen::Move,
    position::{Position, CASTLING_MASKS},
    random::{Fill, Xoshiro},
    types::{File, Piece, PieceMap, Side, Square, SquareMap},
};

pub type Hash = u64;

#[derive(Clone, Debug)]
pub struct Hashes {
    pieces: PieceMap<SquareMap<Hash>>,
    color: [SquareMap<Hash>; 2],
    en_passant: [Hash; 8],
    castling: [Hash; 4],
    side_to_move: Hash,
}

impl Hashes {
    pub fn new() -> Self {
        let mut rng = Xoshiro::new([
            0x42620fbcbf960935,
            0xf4b815ff1964debd,
            0xba5b1af0bbeb11a4,
            0x2cbc33307dd9a87a,
        ]);

        let mut pieces = PieceMap::<SquareMap<Hash>>::default();
        pieces.fill(&mut rng);

        let mut color = <[SquareMap<Hash>; 2]>::default();
        color.fill(&mut rng);

        let mut en_passant = <[Hash; 8]>::default();
        en_passant.fill(&mut rng);

        let mut castling = <[Hash; 4]>::default();
        castling.fill(&mut rng);

        let mut side_to_move = Hash::default();
        side_to_move.fill(&mut rng);

        Self {
            pieces,
            color,
            en_passant,
            castling,
            side_to_move,
        }
    }

    pub fn make_move(&self, pos: &Position, mov: Move) -> Hash {
        let mut hash = 0;
        let side = pos.side_to_move() as usize;
        let us = pos.pieces(pos.side_to_move());

        // Remove current castling hash
        let castling = pos.castling();
        if castling.white_kingside() {
            hash ^= self.castling[0];
        }
        if castling.white_queenside() {
            hash ^= self.castling[1];
        }
        if castling.black_kingside() {
            hash ^= self.castling[2];
        }
        if castling.black_queenside() {
            hash ^= self.castling[3];
        }

        // Remove current en passant hash
        if let Some(ep) = pos.en_passant_file() {
            hash ^= self.en_passant[ep as usize];
        }

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
            hash ^= self.pieces[Piece::Rook][mov.to];
            hash ^= self.color[side][mov.to];

            // Remove king
            hash ^= self.pieces[Piece::King][mov.from];
            hash ^= self.color[side][mov.from];

            // Add rook
            hash ^= self.pieces[Piece::Rook][rook];
            hash ^= self.color[side][rook];

            // Add king
            hash ^= self.pieces[Piece::King][king];
            hash ^= self.color[side][king];
        } else {
            // Non-castling moves
            if let Some(capture) = mov.capture {
                if let Some(ep) = mov
                    .to
                    .backward(pos.side_to_move())
                    .filter(|_| mov.en_passant)
                {
                    // Remove their pawn
                    hash ^= self.pieces[Piece::Pawn][ep];
                    hash ^= self.color[1 - side][ep];
                } else {
                    // Remove their piece
                    hash ^= self.pieces[capture][mov.to];
                    hash ^= self.color[1 - side][mov.to];
                }
            }

            // Remove our piece
            hash ^= self.pieces[mov.piece][mov.from];
            hash ^= self.color[side][mov.from];

            // Add our piece
            hash ^= self.pieces[mov.promotion.unwrap_or(mov.piece)][mov.to];
            hash ^= self.color[side][mov.to];
        }

        let mut castling = pos.castling();
        castling.mask(CASTLING_MASKS[mov.from]);
        castling.mask(CASTLING_MASKS[mov.to]);
        if castling.white_kingside() {
            hash ^= self.castling[0];
        }
        if castling.white_queenside() {
            hash ^= self.castling[1];
        }
        if castling.black_kingside() {
            hash ^= self.castling[2];
        }
        if castling.black_queenside() {
            hash ^= self.castling[3];
        }

        if mov.piece == Piece::Pawn
            && mov.from.rank() == pos.side_to_move().initial_pawn_rank()
            && mov.to.rank() == pos.side_to_move().double_step_pawn_rank()
        {
            hash ^= self.en_passant[mov.from.file() as usize];
        }

        hash ^= self.side_to_move;

        hash
    }

    pub fn make_nullmove(&self, pos: &Position) -> Hash {
        let mut hash = 0;

        // Remove current en passant hash
        if let Some(ep) = pos.en_passant_file() {
            hash ^= self.en_passant[ep as usize];
        }

        hash ^= self.side_to_move;

        hash
    }

    pub fn compute_hash_for_position(&self, position: &Position) -> Hash {
        let mut hash = Hash::default();

        for sq in position.pawns() {
            hash ^= self.pieces[Piece::Pawn][sq];
        }

        for sq in position.knights() {
            hash ^= self.pieces[Piece::Knight][sq];
        }

        for sq in position.bishops() {
            hash ^= self.pieces[Piece::Bishop][sq];
        }

        for sq in position.rooks() {
            hash ^= self.pieces[Piece::Rook][sq];
        }

        for sq in position.queens() {
            hash ^= self.pieces[Piece::Queen][sq];
        }

        for sq in position.kings() {
            hash ^= self.pieces[Piece::King][sq];
        }

        for side in [Side::White, Side::Black].into_iter() {
            for sq in position.pieces(side) {
                hash ^= self.color[side as usize][sq];
            }
        }

        if let Some(file) = position.en_passant_file() {
            hash ^= self.en_passant[file as usize];
        }

        let castling = position.castling();
        if castling.white_kingside() {
            hash ^= self.castling[0];
        }
        if castling.white_queenside() {
            hash ^= self.castling[1];
        }
        if castling.black_kingside() {
            hash ^= self.castling[2];
        }
        if castling.black_queenside() {
            hash ^= self.castling[3];
        }

        if position.side_to_move() == Side::White {
            hash ^= self.side_to_move;
        }

        hash
    }
}
