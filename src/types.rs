use crate::random::{Fill, Xoshiro};

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct Bitboard(u64);

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct Square(u8);

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Piece {
    Pawn = 0,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl Piece {
    pub unsafe fn from_u8_unchecked(p: u8) -> Self {
        match p {
            0 => Piece::Pawn,
            1 => Piece::Knight,
            2 => Piece::Bishop,
            3 => Piece::Rook,
            4 => Piece::Queen,
            5 => Piece::King,
            _ => std::hint::unreachable_unchecked(),
        }
    }

    pub fn as_char(self) -> char {
        match self {
            Piece::Pawn => 'p',
            Piece::Knight => 'n',
            Piece::Bishop => 'b',
            Piece::Rook => 'r',
            Piece::Queen => 'q',
            Piece::King => 'k',
        }
    }

    pub(crate) fn all() -> [Piece; 6] {
        [
            Piece::Pawn,
            Piece::Knight,
            Piece::Bishop,
            Piece::Rook,
            Piece::Queen,
            Piece::King,
        ]
    }
}

impl std::fmt::Display for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_char())
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Side {
    White = 0,
    Black,
}

impl Side {
    pub const fn white(white: bool) -> Self {
        if white {
            Side::White
        } else {
            Side::Black
        }
    }

    pub const fn initial_pawn_rank(self) -> Rank {
        match self {
            Side::White => Rank::Two,
            Side::Black => Rank::Seven,
        }
    }

    pub const fn skip_rank(self) -> Rank {
        match self {
            Side::White => Rank::Three,
            Side::Black => Rank::Six,
        }
    }

    pub const fn double_step_pawn_rank(self) -> Rank {
        match self {
            Side::White => Rank::Four,
            Side::Black => Rank::Five,
        }
    }

    pub const fn en_passant_rank(self) -> Rank {
        match self {
            Side::White => Rank::Six,
            Side::Black => Rank::Three,
        }
    }

    pub const fn promotion_rank(self) -> Rank {
        match self {
            Side::White => Rank::Eight,
            Side::Black => Rank::One,
        }
    }
}

impl std::ops::Not for Side {
    type Output = Side;

    fn not(self) -> Self {
        match self {
            Side::White => Side::Black,
            Side::Black => Side::White,
        }
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum File {
    A = 0,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
}

impl File {
    const fn index(&self) -> usize {
        *self as usize
    }

    pub const fn new(file: u8) -> Option<Self> {
        match file {
            0 => Some(File::A),
            1 => Some(File::B),
            2 => Some(File::C),
            3 => Some(File::D),
            4 => Some(File::E),
            5 => Some(File::F),
            6 => Some(File::G),
            7 => Some(File::H),
            _ => None,
        }
    }

    pub const fn as_bb(&self) -> Bitboard {
        Bitboard(0x01_01_01_01_01_01_01_01 << self.index())
    }

    pub const fn abs_diff(self, rhs: Self) -> u8 {
        (self as u8).abs_diff(rhs as u8)
    }
}

impl std::fmt::Display for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            File::A => write!(f, "a"),
            File::B => write!(f, "b"),
            File::C => write!(f, "c"),
            File::D => write!(f, "d"),
            File::E => write!(f, "e"),
            File::F => write!(f, "f"),
            File::G => write!(f, "g"),
            File::H => write!(f, "h"),
        }
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Rank {
    One = 0,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
}

impl Rank {
    const fn index(&self) -> usize {
        *self as usize
    }

    pub const fn new(rank: u8) -> Option<Self> {
        match rank {
            0 => Some(Rank::One),
            1 => Some(Rank::Two),
            2 => Some(Rank::Three),
            3 => Some(Rank::Four),
            4 => Some(Rank::Five),
            5 => Some(Rank::Six),
            6 => Some(Rank::Seven),
            7 => Some(Rank::Eight),
            _ => None,
        }
    }

    pub const fn as_bb(&self) -> Bitboard {
        Bitboard(0x00_00_00_00_00_00_00_FF << (8 * self.index()))
    }

    pub fn contains(&self, sq: Square) -> bool {
        *self == sq.rank()
    }

    pub const fn all() -> [Rank; 8] {
        [
            Rank::One,
            Rank::Two,
            Rank::Three,
            Rank::Four,
            Rank::Five,
            Rank::Six,
            Rank::Seven,
            Rank::Eight,
        ]
    }
}

impl std::fmt::Display for Rank {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Rank::One => write!(f, "1"),
            Rank::Two => write!(f, "2"),
            Rank::Three => write!(f, "3"),
            Rank::Four => write!(f, "4"),
            Rank::Five => write!(f, "5"),
            Rank::Six => write!(f, "6"),
            Rank::Seven => write!(f, "7"),
            Rank::Eight => write!(f, "8"),
        }
    }
}

impl Square {
    pub const fn from_file_rank(file: File, rank: Rank) -> Self {
        Square((file.index() + rank.index() * 8) as u8)
    }

    pub const fn as_u8(self) -> u8 {
        self.0
    }

    pub const fn from_u8(sq: u8) -> Option<Self> {
        if sq < 64 {
            Some(Square(sq))
        } else {
            None
        }
    }

    pub const fn as_bb(self) -> Bitboard {
        Bitboard(1 << self.0)
    }

    pub fn file(self) -> File {
        File::new(self.0 & 0b000111).unwrap()
    }

    pub fn rank(self) -> Rank {
        Rank::new((self.0 & 0b111000) >> 3).unwrap()
    }

    pub fn forward(self, side: Side) -> Option<Self> {
        match (side, self.rank()) {
            (Side::White, Rank::Eight) => None,
            (Side::White, _) => Some(Square(self.0 + 8)),
            (Side::Black, Rank::One) => None,
            (Side::Black, _) => Some(Square(self.0 - 8)),
        }
    }

    pub fn backward(self, side: Side) -> Option<Self> {
        self.forward(!side)
    }

    pub fn west(self, files: u8) -> Option<Self> {
        if self.0 & 0b000111 < files {
            return None;
        }

        Some(Square(self.0 - files))
    }

    pub fn east(self, files: u8) -> Option<Self> {
        if self.0 & 0b000111 > 7 - files {
            return None;
        }

        Some(Square(self.0 + files))
    }

    pub fn normalize(self, side: Side) -> Self {
        if side == Side::White {
            self
        } else {
            Square(self.0 ^ 0b111000)
        }
    }
}

impl std::fmt::Display for Square {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{}{}", self.file(), self.rank())
    }
}

impl Bitboard {
    pub const fn new(b: u64) -> Self {
        Bitboard(b)
    }

    pub const fn all() -> Self {
        Bitboard::new(0xFF_FF_FF_FF_FF_FF_FF_FF)
    }

    pub const fn empty() -> Self {
        Bitboard::new(0)
    }

    pub const fn as_u64(&self) -> u64 {
        self.0
    }

    pub const fn is_empty(&self) -> bool {
        self.0 == 0
    }

    pub const fn at_least_one(&self) -> bool {
        self.0 > 0
    }

    pub const fn popcount(&self) -> usize {
        self.0.count_ones() as usize
    }

    pub const fn from_west(files: usize) -> Bitboard {
        if files >= 8 {
            return Bitboard::all();
        }

        let bbs = [
            Bitboard::new(0x00_00_00_00_00_00_00_00),
            Bitboard::new(0x01_01_01_01_01_01_01_01),
            Bitboard::new(0x03_03_03_03_03_03_03_03),
            Bitboard::new(0x07_07_07_07_07_07_07_07),
            Bitboard::new(0x0F_0F_0F_0F_0F_0F_0F_0F),
            Bitboard::new(0x1F_1F_1F_1F_1F_1F_1F_1F),
            Bitboard::new(0x3F_3F_3F_3F_3F_3F_3F_3F),
            Bitboard::new(0x7F_7F_7F_7F_7F_7F_7F_7F),
        ];

        bbs[files]
    }

    pub const fn west(&self, files: usize) -> Bitboard {
        let mask = Self::from_west(files);

        Bitboard((self.0 & !mask.0) >> files)
    }

    pub const fn from_east(files: usize) -> Bitboard {
        if files >= 8 {
            return Bitboard::all();
        }

        let bbs = [
            Bitboard::new(0x00_00_00_00_00_00_00_00),
            Bitboard::new(0x80_80_80_80_80_80_80_80),
            Bitboard::new(0xC0_C0_C0_C0_C0_C0_C0_C0),
            Bitboard::new(0xE0_E0_E0_E0_E0_E0_E0_E0),
            Bitboard::new(0xF0_F0_F0_F0_F0_F0_F0_F0),
            Bitboard::new(0xF8_F8_F8_F8_F8_F8_F8_F8),
            Bitboard::new(0xFC_FC_FC_FC_FC_FC_FC_FC),
            Bitboard::new(0xFE_FE_FE_FE_FE_FE_FE_FE),
        ];

        bbs[files]
    }

    pub const fn east(&self, files: usize) -> Bitboard {
        let mask = Self::from_east(files);

        Bitboard((self.0 & !mask.0) << files)
    }

    pub const fn forward(&self, ranks: usize, side: Side) -> Bitboard {
        match side {
            Side::White => Bitboard::new(self.0 << (8 * ranks)),
            Side::Black => Bitboard::new(self.0 >> (8 * ranks)),
        }
    }

    pub const fn backward(&self, ranks: usize, side: Side) -> Bitboard {
        match side {
            Side::White => Bitboard::new(self.0 >> (8 * ranks)),
            Side::Black => Bitboard::new(self.0 << (8 * ranks)),
        }
    }

    pub const fn contains(&self, sq: Square) -> bool {
        self.0 & sq.as_bb().0 > 0
    }

    pub const fn lsb_bb(self) -> Self {
        Bitboard(self.0 & !(self.0 - 1))
    }
}

impl std::ops::BitAnd for Bitboard {
    type Output = Bitboard;

    fn bitand(self, rhs: Bitboard) -> Bitboard {
        Bitboard(self.0 & rhs.0)
    }
}

impl std::ops::BitAndAssign for Bitboard {
    fn bitand_assign(&mut self, rhs: Bitboard) {
        self.0 &= rhs.0;
    }
}

impl std::ops::BitAnd<Square> for Bitboard {
    type Output = bool;

    fn bitand(self, rhs: Square) -> bool {
        self.contains(rhs)
    }
}

impl std::ops::BitOr for Bitboard {
    type Output = Bitboard;

    fn bitor(self, rhs: Bitboard) -> Bitboard {
        Bitboard(self.0 | rhs.0)
    }
}

impl std::ops::BitOrAssign for Bitboard {
    fn bitor_assign(&mut self, rhs: Bitboard) {
        self.0 |= rhs.0;
    }
}

impl std::ops::BitOrAssign<Square> for Bitboard {
    fn bitor_assign(&mut self, rhs: Square) {
        *self |= rhs.as_bb();
    }
}

impl std::ops::BitXor for Bitboard {
    type Output = Bitboard;

    fn bitxor(self, rhs: Bitboard) -> Bitboard {
        Bitboard(self.0 ^ rhs.0)
    }
}

impl std::ops::BitXorAssign for Bitboard {
    fn bitxor_assign(&mut self, rhs: Bitboard) {
        self.0 ^= rhs.0;
    }
}

impl std::ops::BitXorAssign<Square> for Bitboard {
    fn bitxor_assign(&mut self, rhs: Square) {
        *self ^= rhs.as_bb();
    }
}

impl std::ops::Not for Bitboard {
    type Output = Bitboard;

    fn not(self) -> Bitboard {
        Bitboard(!self.0)
    }
}

impl IntoIterator for Bitboard {
    type Item = Square;
    type IntoIter = BitboardIterator;

    fn into_iter(self) -> Self::IntoIter {
        BitboardIterator { bb: self }
    }
}

pub struct BitboardIterator {
    bb: Bitboard,
}

impl Iterator for BitboardIterator {
    type Item = Square;

    #[inline]
    fn next(&mut self) -> Option<Square> {
        if self.bb.is_empty() {
            return None;
        }

        // Conversion to u8 is safe since there're at most 63 trailing zeros because the bitboard
        // is not empty.
        let sq = Square(self.bb.0.trailing_zeros() as u8);
        self.bb.0 &= self.bb.0 - 1;
        Some(sq)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct SquareMap<T> {
    inner: [T; 64],
}

impl<T> SquareMap<T> {
    pub const fn new(data: [T; 64]) -> Self {
        SquareMap { inner: data }
    }

    pub fn iter(&self) -> impl Iterator<Item = (Square, &T)> {
        self.inner
            .iter()
            .enumerate()
            .map(|(sq, value)| (Square(sq as u8), value))
    }
}

impl<T: Copy> SquareMap<T> {
    pub const fn visual(mut data: [T; 64]) -> Self {
        let mut rank = 0;
        while rank < 4 {
            let mut file = 0;
            while file < 8 {
                let t = data[8 * rank + file];
                data[8 * rank + file] = data[8 * (7 - rank) + file];
                data[8 * (7 - rank) + file] = t;
                file += 1;
            }
            rank += 1;
        }
        Self { inner: data }
    }
}

impl<T: Copy + Default> Default for SquareMap<T> {
    fn default() -> Self {
        Self::new([T::default(); 64])
    }
}

impl<T: Fill> Fill for SquareMap<T> {
    fn fill(&mut self, rand: &mut Xoshiro) {
        self.inner.fill(rand);
    }
}

impl<T> std::ops::Index<Square> for SquareMap<T> {
    type Output = T;

    fn index(&self, square: Square) -> &T {
        // SAFETY: square.0 < 64
        unsafe { self.inner.get_unchecked(square.0 as usize) }
    }
}

impl<T> std::ops::IndexMut<Square> for SquareMap<T> {
    fn index_mut(&mut self, square: Square) -> &mut T {
        // SAFETY: square.0 < 64
        unsafe { self.inner.get_unchecked_mut(square.0 as usize) }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PieceMap<T> {
    inner: [T; 6],
}

impl<T> PieceMap<T> {
    pub const fn new(data: [T; 6]) -> Self {
        PieceMap { inner: data }
    }

    pub fn iter(&self) -> impl Iterator<Item = (Piece, &T)> {
        self.inner
            .iter()
            .enumerate()
            .map(|(p, value)| (unsafe { Piece::from_u8_unchecked(p as u8) }, value))
    }

    pub const fn index_const(&self, piece: Piece) -> &T {
        &self.inner[piece as usize]
    }

    pub const fn zip<'a, U>(&'a self, rhs: &'a PieceMap<U>) -> PieceMapZip<'a, T, U> {
        PieceMapZip {
            left: self,
            right: rhs,
        }
    }

    pub fn map<F, U>(&self, f: F) -> PieceMap<U>
    where
        F: Fn(&T) -> U,
    {
        PieceMap::new([
            f(&self.inner[0]),
            f(&self.inner[1]),
            f(&self.inner[2]),
            f(&self.inner[3]),
            f(&self.inner[4]),
            f(&self.inner[5]),
        ])
    }
}

impl<T: Fill> Fill for PieceMap<T> {
    fn fill(&mut self, rand: &mut Xoshiro) {
        self.inner.fill(rand);
    }
}

impl<T> std::ops::Index<Piece> for PieceMap<T> {
    type Output = T;

    fn index(&self, piece: Piece) -> &T {
        unsafe { self.inner.get_unchecked(piece as usize) }
    }
}

impl<T> std::ops::IndexMut<Piece> for PieceMap<T> {
    fn index_mut(&mut self, piece: Piece) -> &mut T {
        unsafe { self.inner.get_unchecked_mut(piece as usize) }
    }
}

pub struct PieceMapZip<'a, T, U> {
    left: &'a PieceMap<T>,
    right: &'a PieceMap<U>,
}

impl<'a, T, U> PieceMapZip<'a, T, U> {
    pub fn iter(&self) -> impl Iterator<Item = (Piece, &T, &U)> {
        self.left
            .inner
            .iter()
            .zip(self.right.inner.iter())
            .enumerate()
            .map(|(p, (left, right))| (unsafe { Piece::from_u8_unchecked(p as u8) }, left, right))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_bitboards() {
        assert_eq!(File::A.as_bb(), Bitboard(0x01_01_01_01_01_01_01_01));
        assert_eq!(File::E.as_bb(), Bitboard(0x10_10_10_10_10_10_10_10));
    }

    #[test]
    fn rank_bitboards() {
        assert_eq!(Rank::One.as_bb(), Bitboard(0x00_00_00_00_00_00_00_FF));
        assert_eq!(Rank::Five.as_bb(), Bitboard(0x00_00_00_FF_00_00_00_00));
    }

    #[test]
    fn from_west() {
        let a = File::A.as_bb();
        let b = File::B.as_bb();
        let c = File::C.as_bb();

        assert_eq!(Bitboard::from_west(2), a | b);
        assert_eq!(Bitboard::from_west(3), a | b | c);
        assert_eq!(Bitboard::from_west(8), Bitboard::all());
    }

    #[test]
    fn from_east() {
        let f = File::F.as_bb();
        let g = File::G.as_bb();
        let h = File::H.as_bb();

        assert_eq!(Bitboard::from_east(2), g | h);
        assert_eq!(Bitboard::from_east(3), f | g | h);
        assert_eq!(Bitboard::from_east(8), Bitboard::all());
    }

    #[test]
    fn test_square_west_east() {
        let sq = Square::from_file_rank(File::D, Rank::Four);
        assert_eq!(
            sq.west(2).unwrap(),
            Square::from_file_rank(File::B, Rank::Four)
        );
        assert_eq!(
            sq.east(2).unwrap(),
            Square::from_file_rank(File::F, Rank::Four)
        );
        assert_eq!(sq.west(4), None);
        assert_eq!(sq.east(5), None);
    }
}
