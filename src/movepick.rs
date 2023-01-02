use crate::{
    movegen::{
        generate_bishop_moves, generate_king_moves, generate_knight_moves, generate_pawn_moves,
        generate_queen_moves, generate_rook_moves, Move, MoveVec,
    },
    position::Position,
    types::{Bitboard, Square},
};

enum Stage {
    TtMove,
    GenerateGoodNoisy,
    GoodNoisy,
    Killers,
    GenerateQuiet,
    Quiet,
    GenerateBadNoisy,
    BadNoisy,
    End,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum MoveType {
    TtMove,
    GoodNoisy,
    Killer,
    Quiet,
    BadNoisy,
}

pub struct MovePicker {
    stage: Stage,
    tt_move: Option<Move>,
    moves: MoveVec,
    index: usize,
    killer_moves: [Option<Move>; 2],
}

impl MovePicker {
    pub fn new(tt_move: Option<Move>, killer_moves: [Option<Move>; 2]) -> Self {
        Self {
            stage: Stage::TtMove,
            tt_move,
            moves: MoveVec::new(),
            index: 0,
            killer_moves,
        }
    }

    pub fn next(&mut self, position: &Position) -> Option<(MoveType, Move)> {
        loop {
            match self.stage {
                Stage::TtMove => {
                    self.stage = Stage::GenerateGoodNoisy;
                    if self.tt_move.is_some() {
                        return self.tt_move.map(|mov| (MoveType::TtMove, mov));
                    }

                    continue;
                }
                Stage::GenerateGoodNoisy => {
                    self.moves.clear();

                    let side = position.side_to_move();
                    generate_pawn_moves(
                        position,
                        position.pieces(!side)
                            | side.promotion_rank().as_bb()
                            | position
                                .en_passant_sq()
                                .map_or(Bitboard::empty(), Square::as_bb),
                        &mut self.moves,
                    );
                    generate_knight_moves(position, position.pieces(!side), &mut self.moves);
                    generate_bishop_moves(position, position.pieces(!side), &mut self.moves);
                    generate_rook_moves(position, position.pieces(!side), &mut self.moves);
                    generate_queen_moves(position, position.pieces(!side), &mut self.moves);
                    generate_king_moves(position, position.pieces(!side), &mut self.moves);

                    self.moves.retain(|mov| position.see_ge(*mov, 0));

                    self.stage = Stage::GoodNoisy;
                    self.index = 0;
                    continue;
                }
                Stage::GoodNoisy => {
                    if let Some(mov) = self.moves.get(self.index) {
                        self.index += 1;
                        return Some((MoveType::GoodNoisy, *mov));
                    }

                    //self.stage = Stage::GenerateQuiet;
                    self.stage = Stage::Killers;
                    self.index = 0;
                    continue;
                }
                Stage::Killers => {
                    if let Some(mov) = self.killer_moves.get(self.index).and_then(Option::as_ref) {
                        self.index += 1;
                        if position.is_move_pseudo_legal(*mov) {
                            return Some((MoveType::Killer, *mov));
                        } else {
                            continue;
                        }
                    }

                    self.stage = Stage::GenerateQuiet;
                    continue;
                }
                Stage::GenerateQuiet => {
                    self.moves.clear();

                    let side = position.side_to_move();
                    generate_pawn_moves(
                        position,
                        !(position.pieces(!side)
                            | side.promotion_rank().as_bb()
                            | position
                                .en_passant_sq()
                                .map_or(Bitboard::empty(), Square::as_bb)),
                        &mut self.moves,
                    );
                    generate_knight_moves(position, !position.pieces(!side), &mut self.moves);
                    generate_bishop_moves(position, !position.pieces(!side), &mut self.moves);
                    generate_rook_moves(position, !position.pieces(!side), &mut self.moves);
                    generate_queen_moves(position, !position.pieces(!side), &mut self.moves);
                    generate_king_moves(position, !position.pieces(!side), &mut self.moves);

                    self.stage = Stage::Quiet;
                    self.index = 0;
                    continue;
                }
                Stage::Quiet => {
                    if let Some(mov) = self.moves.get(self.index) {
                        self.index += 1;
                        return Some((MoveType::Quiet, *mov));
                    }

                    self.stage = Stage::GenerateBadNoisy;
                    continue;
                }
                Stage::GenerateBadNoisy => {
                    self.moves.clear();

                    let side = position.side_to_move();
                    generate_pawn_moves(
                        position,
                        position.pieces(!side)
                            | side.promotion_rank().as_bb()
                            | position
                                .en_passant_sq()
                                .map_or(Bitboard::empty(), Square::as_bb),
                        &mut self.moves,
                    );
                    generate_knight_moves(position, position.pieces(!side), &mut self.moves);
                    generate_bishop_moves(position, position.pieces(!side), &mut self.moves);
                    generate_rook_moves(position, position.pieces(!side), &mut self.moves);
                    generate_queen_moves(position, position.pieces(!side), &mut self.moves);
                    generate_king_moves(position, position.pieces(!side), &mut self.moves);

                    self.moves.retain(|mov| !position.see_ge(*mov, 0));

                    self.stage = Stage::BadNoisy;
                    self.index = 0;
                    continue;
                }
                Stage::BadNoisy => {
                    if let Some(mov) = self.moves.get(self.index) {
                        self.index += 1;
                        return Some((MoveType::BadNoisy, *mov));
                    }

                    self.stage = Stage::End;
                    continue;
                }
                Stage::End => return None,
            }
        }
    }
}
