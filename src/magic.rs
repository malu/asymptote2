use crate::{
    random::{Fill, Xoshiro},
    types::{Bitboard, File, Rank, Side, Square, SquareMap},
};
use static_init::dynamic;

const SHIFT_MASK: u64 = 0xF8_00_00_00_00_00_00_00;

#[derive(Clone)]
pub struct MagicData<const N: usize> {
    attacks: [Bitboard; N],
    magics: SquareMap<Magic>,
}

impl<const N: usize> MagicData<N> {
    pub fn lookup(&self, square: Square, occupied: Bitboard) -> Bitboard {
        self.attacks[self.magics[square].index(occupied)]
    }
}

#[dynamic(0)]
pub static BISHOP_MAGIC_DATA: MagicData<5248> = initialize_bishop_attacks();
#[dynamic(0)]
pub static ROOK_MAGIC_DATA: MagicData<102400> = initialize_rook_attacks();

#[derive(Copy, Clone, Debug)]
struct Magic {
    magic: u64,
    mask: Bitboard,
    offset: u32,
}

impl Magic {
    const fn new() -> Self {
        Self {
            magic: 0,
            mask: Bitboard::empty(),
            offset: 0,
        }
    }

    fn index(&self, occupied: Bitboard) -> usize {
        let shift = self.magic.wrapping_shr(56) as u32;
        self.offset as usize
            + ((occupied & self.mask).as_u64().wrapping_mul(self.magic)).wrapping_shr(shift)
                as usize
    }
}

fn initialize_bishop_attacks<const N: usize>() -> MagicData<N> {
    let mut result = MagicData::<N> {
        attacks: [Bitboard::empty(); N],
        magics: SquareMap::new([Magic::new(); 64]),
    };

    let border = File::A.as_bb() | File::H.as_bb() | Rank::One.as_bb() | Rank::Eight.as_bb();

    let mut rng = Xoshiro::new([
        0x843C2A98E07DD949,
        0xB58B4DD98D0E8A97,
        0x5D9057620F0B9E7D,
        0x43A6A86B0FDF188D,
    ]);

    let mut offset = 0;

    for from in Bitboard::all() {
        let mask = bishop_from(from, Bitboard::empty()) & !border;
        let bits = mask.popcount() as u64;
        let shift = 64 - bits;

        let mut occ = Bitboard::empty();
        let mut size = 0;

        let mut occupancy = Vec::with_capacity(1 << size);
        let mut reference = Vec::with_capacity(1 << size);

        loop {
            occupancy.push(occ);
            reference.push(bishop_from(from, occ));
            size += 1;
            occ = Bitboard::new(occ.as_u64().wrapping_sub(mask.as_u64())) & mask;
            if occ.is_empty() {
                break;
            }
        }

        // search for magics
        let mut magic = Sparse::<3>::default();
        magic.fill(&mut rng);
        let mut entry = Magic {
            magic: magic.0 & !SHIFT_MASK | shift.wrapping_shl(56),
            mask,
            offset: offset as u32,
        };

        let mut last_used = vec![0; size];

        let mut tries = 1;

        'search_magic: loop {
            for i in 0..size {
                let index = entry.index(occupancy[i]);
                let attacks = result.attacks[index];
                if attacks != reference[i] && last_used[index - offset] == tries {
                    // retry
                    magic.fill(&mut rng);
                    entry.magic = magic.0 & !SHIFT_MASK | shift.wrapping_shl(56);
                    tries += 1;
                    continue 'search_magic;
                }

                result.attacks[index] = reference[i];
                last_used[index - offset] = tries;
            }

            break;
        }

        result.magics[from] = entry;
        offset += size;
    }

    result
}

fn bishop_from(from: Square, blockers: Bitboard) -> Bitboard {
    let empty = !blockers;

    let mut propagators_ne = empty;
    let mut propagators_se = empty;
    let mut propagators_sw = empty;
    let mut propagators_nw = empty;
    let mut reachable_ne = Bitboard::new(0);
    let mut reachable_se = Bitboard::new(0);
    let mut reachable_sw = Bitboard::new(0);
    let mut reachable_nw = Bitboard::new(0);
    reachable_ne |= from;
    reachable_se |= from;
    reachable_sw |= from;
    reachable_nw |= from;

    reachable_ne |= reachable_ne.forward(1, Side::White).east(1) & propagators_ne;
    propagators_ne &= propagators_ne.forward(1, Side::White).east(1);
    reachable_ne |= reachable_ne.forward(2, Side::White).east(2) & propagators_ne;
    propagators_ne &= propagators_ne.forward(2, Side::White).east(2);
    reachable_ne |= reachable_ne.forward(4, Side::White).east(4) & propagators_ne;

    reachable_se |= reachable_se.backward(1, Side::White).east(1) & propagators_se;
    propagators_se &= propagators_se.backward(1, Side::White).east(1);
    reachable_se |= reachable_se.backward(2, Side::White).east(2) & propagators_se;
    propagators_se &= propagators_se.backward(2, Side::White).east(2);
    reachable_se |= reachable_se.backward(4, Side::White).east(4) & propagators_se;

    reachable_sw |= reachable_sw.backward(1, Side::White).west(1) & propagators_sw;
    propagators_sw &= propagators_sw.backward(1, Side::White).west(1);
    reachable_sw |= reachable_sw.backward(2, Side::White).west(2) & propagators_sw;
    propagators_sw &= propagators_sw.backward(2, Side::White).west(2);
    reachable_sw |= reachable_sw.backward(4, Side::White).west(4) & propagators_sw;

    reachable_nw |= reachable_nw.forward(1, Side::White).west(1) & propagators_nw;
    propagators_nw &= propagators_nw.forward(1, Side::White).west(1);
    reachable_nw |= reachable_nw.forward(2, Side::White).west(2) & propagators_nw;
    propagators_nw &= propagators_nw.forward(2, Side::White).west(2);
    reachable_nw |= reachable_nw.forward(4, Side::White).west(4) & propagators_nw;

    reachable_ne.forward(1, Side::White).east(1)
        | reachable_se.backward(1, Side::White).east(1)
        | reachable_sw.backward(1, Side::White).west(1)
        | reachable_nw.forward(1, Side::White).west(1)
}

fn initialize_rook_attacks<const N: usize>() -> MagicData<N> {
    let mut result = MagicData::<N> {
        attacks: [Bitboard::empty(); N],
        magics: SquareMap::new([Magic::new(); 64]),
    };

    let border_files = File::A.as_bb() | File::H.as_bb();
    let border_ranks = Rank::One.as_bb() | Rank::Eight.as_bb();

    let mut rng = Xoshiro::new([
        0x853D68F73550AB43,
        0x64B633464DCF9ADD,
        0x21CA27EC6903A7B8,
        0xEFFFC1960C7167C6,
    ]);

    let mut offset = 0;

    for from in Bitboard::all() {
        let mask = !from.as_bb()
            & ((from.file().as_bb() & !border_ranks) | (from.rank().as_bb() & !border_files));
        let bits = mask.popcount() as u64;
        let shift = 64 - bits;

        let mut occ = Bitboard::empty();
        let mut size = 0;

        let mut occupancy = Vec::with_capacity(1 << bits);
        let mut reference = Vec::with_capacity(1 << bits);

        loop {
            occupancy.push(occ);
            reference.push(rook_from(from, occ));
            size += 1;
            occ = Bitboard::new(occ.as_u64().wrapping_sub(mask.as_u64())) & mask;
            if occ.is_empty() {
                break;
            }
        }

        // search for magics
        let mut magic = Sparse::<3>::default();
        let mut entry = Magic {
            magic: magic.0 & !SHIFT_MASK | shift.wrapping_shl(56),
            mask,
            offset: offset as u32,
        };

        let mut last_used = vec![0; size];

        let mut tries = 1;

        'search_magic: loop {
            for i in 0..size {
                let index = entry.index(occupancy[i]);
                let attacks = result.attacks[index];
                if attacks != reference[i] && last_used[index - offset] == tries {
                    // retry
                    magic.fill(&mut rng);
                    entry.magic = magic.0 & !SHIFT_MASK | shift.wrapping_shl(56);
                    tries += 1;
                    continue 'search_magic;
                }

                result.attacks[index] = reference[i];
                last_used[index - offset] = tries;
            }

            break;
        }

        result.magics[from] = entry;
        offset += size;
    }

    result
}

fn rook_from(from: Square, blockers: Bitboard) -> Bitboard {
    let empty = !blockers;

    let mut propagators_n = empty;
    let mut propagators_e = empty;
    let mut propagators_s = empty;
    let mut propagators_w = empty;
    let mut reachable_n = Bitboard::new(0);
    let mut reachable_e = Bitboard::new(0);
    let mut reachable_s = Bitboard::new(0);
    let mut reachable_w = Bitboard::new(0);
    reachable_n |= from;
    reachable_e |= from;
    reachable_s |= from;
    reachable_w |= from;

    reachable_n |= reachable_n.forward(1, Side::White) & propagators_n;
    propagators_n &= propagators_n.forward(1, Side::White);
    reachable_n |= reachable_n.forward(2, Side::White) & propagators_n;
    propagators_n &= propagators_n.forward(2, Side::White);
    reachable_n |= reachable_n.forward(4, Side::White) & propagators_n;

    reachable_e |= reachable_e.east(1) & propagators_e;
    propagators_e &= propagators_e.east(1);
    reachable_e |= reachable_e.east(2) & propagators_e;
    propagators_e &= propagators_e.east(2);
    reachable_e |= reachable_e.east(4) & propagators_e;

    reachable_s |= reachable_s.backward(1, Side::White) & propagators_s;
    propagators_s &= propagators_s.backward(1, Side::White);
    reachable_s |= reachable_s.backward(2, Side::White) & propagators_s;
    propagators_s &= propagators_s.backward(2, Side::White);
    reachable_s |= reachable_s.backward(4, Side::White) & propagators_s;

    reachable_w |= reachable_w.west(1) & propagators_w;
    propagators_w &= propagators_w.west(1);
    reachable_w |= reachable_w.west(2) & propagators_w;
    propagators_w &= propagators_w.west(2);
    reachable_w |= reachable_w.west(4) & propagators_w;

    reachable_n.forward(1, Side::White)
        | reachable_e.east(1)
        | reachable_s.backward(1, Side::White)
        | reachable_w.west(1)
}

#[derive(Copy, Clone, Debug, Default)]
struct Sparse<const N: usize>(u64);

impl<const N: usize> Fill for Sparse<N> {
    fn fill(&mut self, rand: &mut Xoshiro) {
        if N > 0 {
            self.0 = rand.gen();
        }

        for _ in 1..N {
            self.0 &= rand.gen();
        }
    }
}
