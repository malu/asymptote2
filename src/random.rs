pub struct Xoshiro {
    state: [u64; 4],
}

impl Xoshiro {
    pub fn new(state: [u64; 4]) -> Self {
        Xoshiro { state }
    }

    pub fn gen(&mut self) -> u64 {
        let result = self.state[1].wrapping_mul(5).rotate_left(7).wrapping_mul(9);
        let t = self.state[1] << 17;

        self.state[2] ^= self.state[0];
        self.state[3] ^= self.state[1];
        self.state[1] ^= self.state[2];
        self.state[0] ^= self.state[3];

        self.state[2] ^= t;
        self.state[3] = self.state[3].rotate_left(45);

        result
    }

    pub fn fill<T: Fill>(&mut self, v: &mut T) {
        v.fill(self)
    }
}

pub trait Fill {
    fn fill(&mut self, rand: &mut Xoshiro);
}

impl Fill for u64 {
    fn fill(&mut self, rand: &mut Xoshiro) {
        *self = rand.gen();
    }
}

impl<const N: usize, T: Fill> Fill for [T; N] {
    fn fill(&mut self, rand: &mut Xoshiro) {
        self.iter_mut().for_each(|v| v.fill(rand))
    }
}
