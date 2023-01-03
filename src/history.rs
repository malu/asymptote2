use crate::{
    movegen::Move,
    types::{Side, SquareMap},
};

const DECAY: i32 = 512;

#[derive(Clone, Default)]
pub struct History {
    from_to: [SquareMap<SquareMap<i16>>; 2],
}

impl History {
    pub fn get_score(&self, side: Side, mov: Move) -> i16 {
        self.from_to[side as usize][mov.from][mov.to]
    }

    fn change_score(&mut self, side: Side, depth: i8, mov: Move) {
        // keep the sign of depth, but square the absolute value
        let depth = i16::from(depth);
        let change = depth * depth.abs();
        let change = change.clamp(-400, 400);

        let entry = &mut self.from_to[side as usize][mov.from][mov.to];
        let decay = *entry as i32 * change.abs() as i32 / DECAY;
        *entry += 32 * change - decay as i16;
    }

    pub fn increase_score(&mut self, side: Side, mov: Move, depth: i8) {
        self.change_score(side, depth, mov);
    }

    pub fn decrease_score(&mut self, side: Side, moves: &[Move], depth: i8) {
        for mov in moves {
            self.change_score(side, -depth, *mov);
        }
    }
}
