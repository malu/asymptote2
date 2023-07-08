use parking_lot::Mutex;
use std::cell::UnsafeCell;

use crate::{
    hash::Hash,
    movegen::Move,
    position::Position,
    thread::{tb_loss_in, tb_win_in},
    types::{Piece, Square},
};

pub struct TranspositionTable {
    data: UnsafeCell<Vec<Bucket>>,
    num_buckets: usize,
    generation: u8,
    locks: Vec<Mutex<()>>,
}

// SAFETY: Access to `data` is guarded by Mutexes in `locks`
unsafe impl Send for TranspositionTable {}
unsafe impl Sync for TranspositionTable {}

impl TranspositionTable {
    pub fn new(mb: u64) -> Self {
        let num_buckets = usize::try_from(mb).unwrap() * 1024 * 1024 / 64;
        let num_locks = std::cmp::min(2048, num_buckets);

        Self {
            data: vec![Bucket::default(); num_buckets].into(),
            num_buckets,
            generation: 0,
            locks: std::iter::repeat_with(|| Mutex::new(()))
                .take(num_locks)
                .collect(),
        }
    }

    pub fn next_generation(&mut self) {
        self.generation = self.generation.wrapping_add(1);
    }

    #[cfg(target_pointer_width = "64")]
    fn index(&self, hash: Hash) -> usize {
        let mul = hash as u128 * self.num_buckets as u128;
        let hi = mul.wrapping_shr(64);
        let mask = usize::MAX as u128;
        (hi & mask) as usize
    }

    #[cfg(target_pointer_width = "64")]
    fn lock_index(&self, bucket_index: usize) -> usize {
        let mul = bucket_index as u128 * self.locks.len() as u128;
        let hi = mul.wrapping_shr(64);
        let mask = usize::MAX as u128;
        (hi & mask) as usize
    }

    pub fn usage_permille(&self) -> usize {
        let _guards: Vec<_> = self.locks.iter().map(Mutex::lock).collect();
        let data = unsafe { &mut *self.data.get() };
        let n = std::cmp::min(1000, self.num_buckets);
        let mut found = 0;
        for bucket in &data[0..n] {
            for entry in &bucket.0 {
                if entry.generation == self.generation && !entry.is_vacant() {
                    found += 1;
                }
            }
        }

        found * 1000 / (n * BUCKET_SIZE)
    }

    pub fn insert(&self, new_entry: Entry) {
        self.with_bucket(new_entry.hash, |bucket| {
            let smallhash = (new_entry.hash & 0xFFFF) as u16;

            let mut replace = 0;
            let mut depth_entry: Option<(usize, i8)> = None;
            let mut age_entry: Option<(usize, u8)> = None;
            for (i, entry) in bucket.0.iter().enumerate() {
                if entry.is_vacant() || entry.hash == smallhash {
                    replace = i;
                    break;
                }

                if entry.depth < depth_entry.map(|e| e.1).unwrap_or(i8::MAX)
                    && entry.depth < new_entry.depth
                {
                    depth_entry = Some((i, entry.depth));
                }

                let age = self.generation.wrapping_sub(entry.generation);
                if age > age_entry.map(|e| e.1).unwrap_or(u8::MIN) {
                    age_entry = Some((i, age));
                }
            }

            let replace = if replace > 0 {
                replace
            } else if let Some(age) = age_entry {
                age.0
            } else if let Some(depth) = depth_entry {
                depth.0
            } else {
                0
            };

            bucket.0[replace] = BucketEntry {
                hash: smallhash,
                generation: self.generation,
                best_move: encode_move(new_entry.best_move),
                score: new_entry.score,
                depth: new_entry.depth,
                bound: new_entry.bound,
            }
        })
    }

    pub fn get(&self, hash: Hash, position: &Position) -> Option<Entry> {
        self.with_bucket(hash, |bucket| {
            let smallhash = (hash & 0xFFFF) as u16;
            for entry in bucket.0.iter_mut() {
                if !entry.is_vacant() && entry.hash == smallhash {
                    let best_move = decode_move(position, entry.best_move)?;
                    entry.generation = self.generation;
                    return Some(Entry {
                        hash,
                        best_move,
                        score: entry.score,
                        depth: entry.depth,
                        bound: entry.bound,
                    });
                }
            }

            None
        })
    }

    pub fn prefetch(&self, hash: Hash) {
        #[cfg(all(target_arch = "x86_64", target_feature = "sse"))]
        {
            let index = self.index(hash);
            unsafe {
                let data = &mut *self.data.get();
                let addr = data.as_ptr().offset(index as isize);
                std::arch::x86_64::_mm_prefetch(addr as *const i8, std::arch::x86_64::_MM_HINT_ET1);
            }
        }
        #[cfg(not(all(target_arch = "x86_64", target_feature = "sse")))]
        {
            let _ = hash;
        }
    }

    fn with_bucket<F, T>(&self, hash: Hash, f: F) -> T
    where
        F: FnOnce(&mut Bucket) -> T,
        T: 'static,
    {
        let index = self.index(hash);
        let lock_index = self.lock_index(index);
        let _guard = self.locks[lock_index].lock();
        let data = unsafe { &mut *self.data.get() };
        let bucket = &mut data[index];
        f(bucket)
    }
}

#[derive(Copy, Clone, Debug, Default)]
#[repr(align(64))]
struct Bucket([BucketEntry; BUCKET_SIZE]);

const BUCKET_SIZE: usize = 4;

#[derive(Copy, Clone, Debug, Default)]
#[repr(align(16))]
struct BucketEntry {
    hash: u16,
    generation: u8,
    best_move: u16,
    score: Score,
    depth: i8,
    bound: u8,
}

impl BucketEntry {
    const fn is_vacant(self) -> bool {
        self.best_move == 0
    }
}

pub const LOWER_BOUND: u8 = 1;
pub const UPPER_BOUND: u8 = 2;
pub const EXACT_BOUND: u8 = 3;

#[derive(Copy, Clone)]
pub struct Entry {
    pub hash: Hash,
    pub best_move: Move,
    pub score: Score,
    pub depth: i8,
    pub bound: u8,
}

const PROMOTION_KNIGHT: u16 = 0 << 12;
const PROMOTION_BISHOP: u16 = 1 << 12;
const PROMOTION_ROOK: u16 = 2 << 12;
const PROMOTION_QUEEN: u16 = 3 << 12;
const PROMOTION_MASK: u16 = 3 << 12;

fn encode_move(mov: Move) -> u16 {
    let mut result = 0;
    result |= mov.from.as_u8() as u16;
    result |= (mov.to.as_u8() as u16) << 6;
    match mov.promotion {
        Some(Piece::Knight) => result |= PROMOTION_KNIGHT,
        Some(Piece::Bishop) => result |= PROMOTION_BISHOP,
        Some(Piece::Rook) => result |= PROMOTION_ROOK,
        Some(Piece::Queen) => result |= PROMOTION_QUEEN,
        _ => {}
    }

    result
}

fn decode_move(position: &Position, mov: u16) -> Option<Move> {
    let from = Square::from_u8((mov & 0x3f) as u8)?;
    let piece = position.find_piece(from)?;
    let to = Square::from_u8((mov >> 6 & 0x3f) as u8)?;
    let capture = position
        .find_piece(to)
        .filter(|_| !position.pieces(position.side_to_move()).contains(to));
    let promotion = if piece == Piece::Pawn && position.side_to_move().promotion_rank().contains(to)
    {
        match mov & PROMOTION_MASK {
            PROMOTION_KNIGHT => Some(Piece::Knight),
            PROMOTION_BISHOP => Some(Piece::Bishop),
            PROMOTION_ROOK => Some(Piece::Rook),
            PROMOTION_QUEEN => Some(Piece::Queen),
            _ => None,
        }
    } else {
        None
    };

    let en_passant = piece == Piece::Pawn && capture.is_none() && from.file() != to.file();

    Some(Move {
        from,
        to,
        piece,
        capture,
        promotion,
        en_passant,
    })
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct Score(i16);

impl Score {
    pub fn from_score(score: i16, ply: i16) -> Self {
        if score > tb_win_in(128) {
            Self(score + ply)
        } else if score < tb_loss_in(128) {
            Self(score - ply)
        } else {
            Self(score)
        }
    }

    pub fn to_score(self, ply: i16) -> i16 {
        if self.0 > tb_win_in(128) {
            self.0 - ply
        } else if self.0 < tb_loss_in(128) {
            self.0 + ply
        } else {
            self.0
        }
    }
}
