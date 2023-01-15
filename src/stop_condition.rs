use crate::types::Side;

#[derive(Clone, Debug)]
pub struct TimeManager {
    stop_condition: StopCondition,
    start: std::time::Instant,
    buffer_ms: u128,
    side: Side,
}

impl TimeManager {
    pub fn new(side: Side, stop_condition: StopCondition) -> Self {
        Self {
            stop_condition,
            start: std::time::Instant::now(),
            buffer_ms: 10,
            side,
        }
    }

    pub fn start(&mut self, stop_condition: StopCondition) {
        self.start = std::time::Instant::now();
        self.stop_condition = stop_condition;
    }

    pub fn should_start_another_iteration(&self, depth: i8) -> bool {
        match self.stop_condition {
            StopCondition::Infinite => true,
            StopCondition::Movetime { limit_ms } => {
                let elapsed_ms = self.elapsed_ms();
                elapsed_ms < limit_ms
            }
            StopCondition::Depth { limit } => depth <= limit,
            StopCondition::Dynamic {
                white_base_ms,
                white_increment_ms: _,
                black_base_ms,
                black_increment_ms: _,
                moves_to_go,
            } => {
                let elapsed_ms = self.elapsed_ms();
                let base = if self.side == Side::White {
                    white_base_ms.unwrap_or(0)
                } else {
                    black_base_ms.unwrap_or(0)
                };
                let to_go = moves_to_go.unwrap_or(40);
                elapsed_ms < base / to_go / 2
            }
        }
    }

    pub fn should_end_now(&self) -> bool {
        match self.stop_condition {
            StopCondition::Infinite => false,
            StopCondition::Movetime { limit_ms } => {
                let elapsed_ms = self.elapsed_ms();
                elapsed_ms > limit_ms
            }
            StopCondition::Depth { .. } => false,
            StopCondition::Dynamic {
                white_base_ms,
                white_increment_ms: _,
                black_base_ms,
                black_increment_ms: _,
                moves_to_go,
            } => {
                let elapsed_ms = self.elapsed_ms();
                let base = if self.side == Side::White {
                    white_base_ms.unwrap_or(0)
                } else {
                    black_base_ms.unwrap_or(0)
                };
                let to_go = moves_to_go.unwrap_or(40);
                elapsed_ms > std::cmp::min(2 * base / to_go, base)
            }
        }
    }

    fn elapsed_ms(&self) -> u128 {
        self.start.elapsed().as_millis() + self.buffer_ms
    }
}

#[derive(Copy, Clone, Debug)]
pub enum StopCondition {
    Infinite,
    Movetime {
        limit_ms: u128,
    },
    Depth {
        limit: i8,
    },
    Dynamic {
        white_base_ms: Option<u128>,
        white_increment_ms: Option<u128>,
        black_base_ms: Option<u128>,
        black_increment_ms: Option<u128>,
        moves_to_go: Option<u128>,
    },
}
