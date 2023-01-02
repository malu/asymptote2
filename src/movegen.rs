use crate::magic::{BISHOP_MAGIC_DATA, ROOK_MAGIC_DATA};
use crate::position::Position;
use crate::types::{Bitboard, File, Piece, Rank, Side, Square, SquareMap};
use arrayvec::ArrayVec;

pub type MoveVec = ArrayVec<Move, 256>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Move {
    pub from: Square,
    pub to: Square,
    pub piece: Piece,
    pub capture: Option<Piece>,
    pub promotion: Option<Piece>,
    pub en_passant: bool,
}

impl Move {
    pub fn from_syzygy_move(mov: fathom_syzygy::Move, position: &Position) -> Option<Self> {
        let from = Square::from_u8(mov.from.into())?;
        let to = Square::from_u8(mov.to.into())?;
        let piece = position.find_piece(from)?;
        let capture = position.find_piece(to);
        let promotion = match mov.promote {
            fathom_syzygy::PromotionPiece::None => None,
            fathom_syzygy::PromotionPiece::Knight => Some(Piece::Knight),
            fathom_syzygy::PromotionPiece::Bishop => Some(Piece::Bishop),
            fathom_syzygy::PromotionPiece::Rook => Some(Piece::Rook),
            fathom_syzygy::PromotionPiece::Queen => Some(Piece::Queen),
        };
        let en_passant = mov.en_passant;

        Some(Self {
            from,
            to,
            piece,
            capture,
            promotion,
            en_passant,
        })
    }
}

pub fn generate_pawn_moves(position: &Position, targets: Bitboard, moves: &mut MoveVec) {
    let side = position.side_to_move();
    let us = position.pieces(side);
    let them = position.pieces(!side);
    let pawns = position.pawns() & us;
    let all_pieces = position.all_pieces();

    let skip_rank = side.skip_rank().as_bb();
    let promotion_rank = side.promotion_rank();

    let make_quiet_pawn_move = |from, to| Move {
        from,
        to,
        piece: Piece::Pawn,
        capture: None,
        promotion: None,
        en_passant: false,
    };

    let make_capture_pawn_move = |from, to, capture| Move {
        from,
        to,
        piece: Piece::Pawn,
        capture,
        promotion: None,
        en_passant: false,
    };

    let single_step_targets = pawns.forward(1, side);
    for single_step in single_step_targets & targets & !all_pieces {
        let from = single_step.backward(side).unwrap();
        let to = single_step;

        if to.rank() != promotion_rank {
            moves.push(make_quiet_pawn_move(from, to));
        } else {
            for &promotion in PROMOTION_PIECES.iter() {
                moves.push(Move {
                    from,
                    to,
                    piece: Piece::Pawn,
                    capture: None,
                    promotion: Some(promotion),
                    en_passant: false,
                });
            }
        }
    }

    let double_step_targets = (single_step_targets & skip_rank & !all_pieces).forward(1, side);
    for double_step in double_step_targets & targets & !all_pieces {
        let from = double_step
            .backward(side)
            .and_then(|sq| sq.backward(side))
            .unwrap();
        let to = double_step;

        moves.push(make_quiet_pawn_move(from, to));
    }

    const PROMOTION_PIECES: [Piece; 4] = [Piece::Queen, Piece::Knight, Piece::Rook, Piece::Bishop];
    let captures_west = single_step_targets.west(1) & them;
    for capture in captures_west & targets {
        let from = capture.backward(side).unwrap().east(1).unwrap();
        let to = capture;
        let capture = position.find_piece(to);
        if to.rank() != promotion_rank {
            moves.push(make_capture_pawn_move(from, to, capture));
        } else {
            for &promotion in PROMOTION_PIECES.iter() {
                moves.push(Move {
                    from,
                    to,
                    piece: Piece::Pawn,
                    capture,
                    promotion: Some(promotion),
                    en_passant: false,
                });
            }
        }
    }

    let captures_east = single_step_targets.east(1) & them;
    for capture in captures_east & targets {
        let from = capture.backward(side).unwrap().west(1).unwrap();
        let to = capture;
        let capture = position.find_piece(to);
        if to.rank() != promotion_rank {
            moves.push(make_capture_pawn_move(from, to, capture));
        } else {
            for &promotion in PROMOTION_PIECES.iter() {
                moves.push(Move {
                    from,
                    to,
                    piece: Piece::Pawn,
                    capture,
                    promotion: Some(promotion),
                    en_passant: false,
                });
            }
        }
    }

    if let Some(en_passant_file) = position.en_passant_file() {
        let en_passant_rank = match side {
            Side::White => Rank::Six,
            Side::Black => Rank::Three,
        };
        let en_passant_square = Square::from_file_rank(en_passant_file, en_passant_rank);

        if (single_step_targets.east(1) & targets) & en_passant_square {
            let from = en_passant_square.backward(side).unwrap().west(1).unwrap();
            let to = en_passant_square;
            moves.push(Move {
                from,
                to,
                piece: Piece::Pawn,
                capture: Some(Piece::Pawn),
                promotion: None,
                en_passant: true,
            });
        }

        if (single_step_targets.west(1) & targets) & en_passant_square {
            let from = en_passant_square.backward(side).unwrap().east(1).unwrap();
            let to = en_passant_square;
            moves.push(Move {
                from,
                to,
                piece: Piece::Pawn,
                capture: Some(Piece::Pawn),
                promotion: None,
                en_passant: true,
            });
        }
    }
}

impl std::fmt::Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.from, self.to)?;
        if let Some(promotion) = self.promotion {
            write!(f, "{}", promotion)?;
        }

        Ok(())
    }
}

pub static KNIGHT_ATTACKS: SquareMap<Bitboard> = SquareMap::new([
    // rank 1
    Bitboard::new(0x00_00_00_00_00_02_04_00),
    Bitboard::new(0x00_00_00_00_00_05_08_00),
    Bitboard::new(0x00_00_00_00_00_0A_11_00),
    Bitboard::new(0x00_00_00_00_00_14_22_00),
    Bitboard::new(0x00_00_00_00_00_28_44_00),
    Bitboard::new(0x00_00_00_00_00_50_88_00),
    Bitboard::new(0x00_00_00_00_00_A0_10_00),
    Bitboard::new(0x00_00_00_00_00_40_20_00),
    // rank 2
    Bitboard::new(0x00_00_00_00_02_04_00_04),
    Bitboard::new(0x00_00_00_00_05_08_00_08),
    Bitboard::new(0x00_00_00_00_0A_11_00_11),
    Bitboard::new(0x00_00_00_00_14_22_00_22),
    Bitboard::new(0x00_00_00_00_28_44_00_44),
    Bitboard::new(0x00_00_00_00_50_88_00_88),
    Bitboard::new(0x00_00_00_00_A0_10_00_10),
    Bitboard::new(0x00_00_00_00_40_20_00_20),
    // rank 3
    Bitboard::new(0x00_00_00_02_04_00_04_02),
    Bitboard::new(0x00_00_00_05_08_00_08_05),
    Bitboard::new(0x00_00_00_0A_11_00_11_0A),
    Bitboard::new(0x00_00_00_14_22_00_22_14),
    Bitboard::new(0x00_00_00_28_44_00_44_28),
    Bitboard::new(0x00_00_00_50_88_00_88_50),
    Bitboard::new(0x00_00_00_A0_10_00_10_A0),
    Bitboard::new(0x00_00_00_40_20_00_20_40),
    // rank 4
    Bitboard::new(0x00_00_02_04_00_04_02_00),
    Bitboard::new(0x00_00_05_08_00_08_05_00),
    Bitboard::new(0x00_00_0A_11_00_11_0A_00),
    Bitboard::new(0x00_00_14_22_00_22_14_00),
    Bitboard::new(0x00_00_28_44_00_44_28_00),
    Bitboard::new(0x00_00_50_88_00_88_50_00),
    Bitboard::new(0x00_00_A0_10_00_10_A0_00),
    Bitboard::new(0x00_00_40_20_00_20_40_00),
    // rank 5
    Bitboard::new(0x00_02_04_00_04_02_00_00),
    Bitboard::new(0x00_05_08_00_08_05_00_00),
    Bitboard::new(0x00_0A_11_00_11_0A_00_00),
    Bitboard::new(0x00_14_22_00_22_14_00_00),
    Bitboard::new(0x00_28_44_00_44_28_00_00),
    Bitboard::new(0x00_50_88_00_88_50_00_00),
    Bitboard::new(0x00_A0_10_00_10_A0_00_00),
    Bitboard::new(0x00_40_20_00_20_40_00_00),
    // rank 6
    Bitboard::new(0x02_04_00_04_02_00_00_00),
    Bitboard::new(0x05_08_00_08_05_00_00_00),
    Bitboard::new(0x0A_11_00_11_0A_00_00_00),
    Bitboard::new(0x14_22_00_22_14_00_00_00),
    Bitboard::new(0x28_44_00_44_28_00_00_00),
    Bitboard::new(0x50_88_00_88_50_00_00_00),
    Bitboard::new(0xA0_10_00_10_A0_00_00_00),
    Bitboard::new(0x40_20_00_20_40_00_00_00),
    // rank 7
    Bitboard::new(0x04_00_04_02_00_00_00_00),
    Bitboard::new(0x08_00_08_05_00_00_00_00),
    Bitboard::new(0x11_00_11_0A_00_00_00_00),
    Bitboard::new(0x22_00_22_14_00_00_00_00),
    Bitboard::new(0x44_00_44_28_00_00_00_00),
    Bitboard::new(0x88_00_88_50_00_00_00_00),
    Bitboard::new(0x10_00_10_A0_00_00_00_00),
    Bitboard::new(0x20_00_20_40_00_00_00_00),
    // rank 8
    Bitboard::new(0x00_04_02_00_00_00_00_00),
    Bitboard::new(0x00_08_05_00_00_00_00_00),
    Bitboard::new(0x00_11_0A_00_00_00_00_00),
    Bitboard::new(0x00_22_14_00_00_00_00_00),
    Bitboard::new(0x00_44_28_00_00_00_00_00),
    Bitboard::new(0x00_88_50_00_00_00_00_00),
    Bitboard::new(0x00_10_A0_00_00_00_00_00),
    Bitboard::new(0x00_20_40_00_00_00_00_00),
]);

pub fn generate_knight_moves(position: &Position, targets: Bitboard, moves: &mut MoveVec) {
    let side = position.side_to_move();
    let us = position.pieces(side);

    for knight in position.knights() & us {
        let targets = targets & KNIGHT_ATTACKS[knight] & !us;
        for target in targets {
            moves.push(Move {
                from: knight,
                to: target,
                piece: Piece::Knight,
                capture: position.find_piece(target),
                promotion: None,
                en_passant: false,
            });
        }
    }
}

pub fn generate_bishop_moves(position: &Position, targets: Bitboard, moves: &mut MoveVec) {
    let side = position.side_to_move();
    let us = position.pieces(side);
    let all = position.all_pieces();

    for bishop in position.bishops() & us {
        let targets = targets & bishop_from(bishop, all) & !us;
        for target in targets {
            moves.push(Move {
                from: bishop,
                to: target,
                piece: Piece::Bishop,
                capture: position.find_piece(target),
                promotion: None,
                en_passant: false,
            });
        }
    }
}

pub fn bishop_from(from: Square, blockers: Bitboard) -> Bitboard {
    unsafe { BISHOP_MAGIC_DATA.lookup(from, blockers) }
}

pub fn generate_rook_moves(position: &Position, targets: Bitboard, moves: &mut MoveVec) {
    let side = position.side_to_move();
    let us = position.pieces(side);
    let all = position.all_pieces();

    for rook in position.rooks() & us {
        let targets = targets & rook_from(rook, all) & !us;
        for target in targets {
            moves.push(Move {
                from: rook,
                to: target,
                piece: Piece::Rook,
                capture: position.find_piece(target),
                promotion: None,
                en_passant: false,
            });
        }
    }
}

pub fn rook_from(from: Square, blockers: Bitboard) -> Bitboard {
    unsafe { ROOK_MAGIC_DATA.lookup(from, blockers) }
}

pub fn generate_queen_moves(position: &Position, targets: Bitboard, moves: &mut MoveVec) {
    let side = position.side_to_move();
    let us = position.pieces(side);
    let all = position.all_pieces();

    for queen in position.queens() & us {
        let targets = targets & (bishop_from(queen, all) | rook_from(queen, all)) & !us;
        for target in targets {
            moves.push(Move {
                from: queen,
                to: target,
                piece: Piece::Queen,
                capture: position.find_piece(target),
                promotion: None,
                en_passant: false,
            });
        }
    }
}

pub const KING_ATTACKS: SquareMap<Bitboard> = SquareMap::new([
    // rank 1
    Bitboard::new(0x00_00_00_00_00_00_03_02),
    Bitboard::new(0x00_00_00_00_00_00_07_05),
    Bitboard::new(0x00_00_00_00_00_00_0E_0A),
    Bitboard::new(0x00_00_00_00_00_00_1C_14),
    Bitboard::new(0x00_00_00_00_00_00_38_28),
    Bitboard::new(0x00_00_00_00_00_00_70_50),
    Bitboard::new(0x00_00_00_00_00_00_E0_A0),
    Bitboard::new(0x00_00_00_00_00_00_C0_40),
    // rank 2
    Bitboard::new(0x00_00_00_00_00_03_02_03),
    Bitboard::new(0x00_00_00_00_00_07_05_07),
    Bitboard::new(0x00_00_00_00_00_0E_0A_0E),
    Bitboard::new(0x00_00_00_00_00_1C_14_1C),
    Bitboard::new(0x00_00_00_00_00_38_28_38),
    Bitboard::new(0x00_00_00_00_00_70_50_70),
    Bitboard::new(0x00_00_00_00_00_E0_A0_E0),
    Bitboard::new(0x00_00_00_00_00_C0_40_C0),
    // rank 3
    Bitboard::new(0x00_00_00_00_03_02_03_00),
    Bitboard::new(0x00_00_00_00_07_05_07_00),
    Bitboard::new(0x00_00_00_00_0E_0A_0E_00),
    Bitboard::new(0x00_00_00_00_1C_14_1C_00),
    Bitboard::new(0x00_00_00_00_38_28_38_00),
    Bitboard::new(0x00_00_00_00_70_50_70_00),
    Bitboard::new(0x00_00_00_00_E0_A0_E0_00),
    Bitboard::new(0x00_00_00_00_C0_40_C0_00),
    // rank 4
    Bitboard::new(0x00_00_00_03_02_03_00_00),
    Bitboard::new(0x00_00_00_07_05_07_00_00),
    Bitboard::new(0x00_00_00_0E_0A_0E_00_00),
    Bitboard::new(0x00_00_00_1C_14_1C_00_00),
    Bitboard::new(0x00_00_00_38_28_38_00_00),
    Bitboard::new(0x00_00_00_70_50_70_00_00),
    Bitboard::new(0x00_00_00_E0_A0_E0_00_00),
    Bitboard::new(0x00_00_00_C0_40_C0_00_00),
    // rank 5
    Bitboard::new(0x00_00_03_02_03_00_00_00),
    Bitboard::new(0x00_00_07_05_07_00_00_00),
    Bitboard::new(0x00_00_0E_0A_0E_00_00_00),
    Bitboard::new(0x00_00_1C_14_1C_00_00_00),
    Bitboard::new(0x00_00_38_28_38_00_00_00),
    Bitboard::new(0x00_00_70_50_70_00_00_00),
    Bitboard::new(0x00_00_E0_A0_E0_00_00_00),
    Bitboard::new(0x00_00_C0_40_C0_00_00_00),
    // rank 6
    Bitboard::new(0x00_03_02_03_00_00_00_00),
    Bitboard::new(0x00_07_05_07_00_00_00_00),
    Bitboard::new(0x00_0E_0A_0E_00_00_00_00),
    Bitboard::new(0x00_1C_14_1C_00_00_00_00),
    Bitboard::new(0x00_38_28_38_00_00_00_00),
    Bitboard::new(0x00_70_50_70_00_00_00_00),
    Bitboard::new(0x00_E0_A0_E0_00_00_00_00),
    Bitboard::new(0x00_C0_40_C0_00_00_00_00),
    // rank 7
    Bitboard::new(0x03_02_03_00_00_00_00_00),
    Bitboard::new(0x07_05_07_00_00_00_00_00),
    Bitboard::new(0x0E_0A_0E_00_00_00_00_00),
    Bitboard::new(0x1C_14_1C_00_00_00_00_00),
    Bitboard::new(0x38_28_38_00_00_00_00_00),
    Bitboard::new(0x70_50_70_00_00_00_00_00),
    Bitboard::new(0xE0_A0_E0_00_00_00_00_00),
    Bitboard::new(0xC0_40_C0_00_00_00_00_00),
    // rank 8
    Bitboard::new(0x02_03_00_00_00_00_00_00),
    Bitboard::new(0x05_07_00_00_00_00_00_00),
    Bitboard::new(0x0A_0E_00_00_00_00_00_00),
    Bitboard::new(0x14_1C_00_00_00_00_00_00),
    Bitboard::new(0x28_38_00_00_00_00_00_00),
    Bitboard::new(0x50_70_00_00_00_00_00_00),
    Bitboard::new(0xA0_E0_00_00_00_00_00_00),
    Bitboard::new(0x40_C0_00_00_00_00_00_00),
]);

const CASTLING_BLOCKERS_WHITE_QUEENSIDE: Bitboard = Bitboard::new(0x00_00_00_00_00_00_00_0E);
const CASTLING_BLOCKERS_WHITE_KINGSIDE: Bitboard = Bitboard::new(0x00_00_00_00_00_00_00_60);
const CASTLING_BLOCKERS_BLACK_QUEENSIDE: Bitboard = Bitboard::new(0x0E_00_00_00_00_00_00_00);
const CASTLING_BLOCKERS_BLACK_KINGSIDE: Bitboard = Bitboard::new(0x60_00_00_00_00_00_00_00);

// Castling moves are always generated, regardless of the `target` bitboard.
// TODO change this if possible
pub fn generate_king_moves(position: &Position, targets: Bitboard, moves: &mut MoveVec) {
    let side = position.side_to_move();
    let us = position.pieces(side);
    let all = position.all_pieces();

    for king in position.kings() & us {
        let targets = targets & KING_ATTACKS[king] & !us;
        for target in targets {
            moves.push(Move {
                from: king,
                to: target,
                piece: Piece::King,
                capture: position.find_piece(target),
                promotion: None,
                en_passant: false,
            });
        }
    }

    // TODO FRC
    if side == Side::White
        && position.castling().white_queenside()
        && (all & CASTLING_BLOCKERS_WHITE_QUEENSIDE).is_empty()
    {
        // No need to check king position. Having castling rights means the king hasn't moved yet.
        let from = position.king_sq(side);
        let to = Square::from_file_rank(File::A, Rank::One);
        moves.push(Move {
            from,
            to,
            piece: Piece::King,
            capture: None,
            promotion: None,
            en_passant: false,
        });
    }

    if side == Side::White
        && position.castling().white_kingside()
        && (all & CASTLING_BLOCKERS_WHITE_KINGSIDE).is_empty()
    {
        // No need to check king position. Having castling rights means the king hasn't moved yet.
        let from = position.king_sq(side);
        let to = Square::from_file_rank(File::H, Rank::One);
        moves.push(Move {
            from,
            to,
            piece: Piece::King,
            capture: None,
            promotion: None,
            en_passant: false,
        });
    }

    if side == Side::Black
        && position.castling().black_queenside()
        && (all & CASTLING_BLOCKERS_BLACK_QUEENSIDE).is_empty()
    {
        // No need to check king position. Having castling rights means the king hasn't moved yet.
        let from = position.king_sq(side);
        let to = Square::from_file_rank(File::A, Rank::Eight);
        moves.push(Move {
            from,
            to,
            piece: Piece::King,
            capture: None,
            promotion: None,
            en_passant: false,
        });
    }

    if side == Side::Black
        && position.castling().black_kingside()
        && (all & CASTLING_BLOCKERS_BLACK_KINGSIDE).is_empty()
    {
        // No need to check king position. Having castling rights means the king hasn't moved yet.
        let from = position.king_sq(side);
        let to = Square::from_file_rank(File::H, Rank::Eight);
        moves.push(Move {
            from,
            to,
            piece: Piece::King,
            capture: None,
            promotion: None,
            en_passant: false,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pawn_moves_starting_position() {
        let position = Position::default();
        let mut moves = MoveVec::new();
        generate_pawn_moves(&position, Bitboard::all(), &mut moves);
        assert_eq!(moves.len(), 16);
    }

    #[test]
    fn knight_moves() {
        for (sq, bb) in KNIGHT_ATTACKS.iter() {
            let mut expected = Bitboard::default();
            expected |= sq.as_bb().forward(2, Side::White).west(1);
            expected |= sq.as_bb().forward(2, Side::White).east(1);
            expected |= sq.as_bb().forward(1, Side::White).west(2);
            expected |= sq.as_bb().forward(1, Side::White).east(2);
            expected |= sq.as_bb().backward(1, Side::White).west(2);
            expected |= sq.as_bb().backward(1, Side::White).east(2);
            expected |= sq.as_bb().backward(2, Side::White).west(1);
            expected |= sq.as_bb().backward(2, Side::White).east(1);

            assert_eq!(
                expected,
                *bb,
                "offending square: {} (exp: {:x}, act: {:x})",
                sq,
                expected.as_u64(),
                bb.as_u64()
            );
        }
    }

    #[test]
    fn king_moves() {
        for (sq, bb) in KING_ATTACKS.iter() {
            let mut expected = Bitboard::default();
            expected |= sq.as_bb().forward(1, Side::White).west(1);
            expected |= sq.as_bb().forward(1, Side::White);
            expected |= sq.as_bb().forward(1, Side::White).east(1);
            expected |= sq.as_bb().west(1);
            expected |= sq.as_bb().east(1);
            expected |= sq.as_bb().backward(1, Side::White).west(1);
            expected |= sq.as_bb().backward(1, Side::White);
            expected |= sq.as_bb().backward(1, Side::White).east(1);

            assert_eq!(
                expected,
                *bb,
                "offending square: {} (exp: {:x}, act: {:x})",
                sq,
                expected.as_u64(),
                bb.as_u64()
            );
        }
    }
}
