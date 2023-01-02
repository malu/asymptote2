use crate::{
    eval::Eval,
    movegen::Move,
    position::{perft, Position, STARTING_POSITION_FEN},
    random::Xoshiro,
    stop_condition::StopCondition,
    thread::Thread,
    tt::TranspositionTable,
    tune::Tuner,
    types::{File, Piece, Rank, Square},
};
use std::{
    io::BufRead,
    num::ParseIntError,
    sync::atomic::{AtomicBool, Ordering},
    time::UNIX_EPOCH,
};
use thiserror::Error;

pub fn run() {
    let mut original_position =
        Position::from_fen(STARTING_POSITION_FEN).expect("Starting FEN is valid");
    let mut current_position =
        Position::from_fen(STARTING_POSITION_FEN).expect("Starting FEN is valid");
    let mut moves = Vec::new();

    let mut tt = TranspositionTable::new(8);
    let mut threads = 1;
    let abort = AtomicBool::new(false);

    let mut fathom: Option<fathom_syzygy::Fathom> = fathom_syzygy::Fathom::new("")
        .map_err(|e| println!("Failed to initialize fathom syzygy library: {e}"))
        .ok();

    let mut line = String::new();
    let mut cached_line = None;
    loop {
        match cached_line.take() {
            Some(cached) => line = cached,
            None => {
                line.clear();
                std::io::stdin().read_line(&mut line).expect("read line");
            }
        };
        let cmd = Command::try_from(line.as_str());

        match cmd {
            Ok(Command::Go(stop_condition)) => {
                abort.store(false, Ordering::SeqCst);

                tt.next_generation();
                let (root_prober, prober) =
                    fathom.as_mut().map(|fathom| fathom.get_probers()).unzip();
                let mut thread = Thread::from_position_and_moves(
                    &original_position,
                    &moves,
                    &tt,
                    &abort,
                    prober,
                );
                thread.threads_total = threads;

                std::thread::scope(|s| {
                    for i in 1..threads {
                        let mut thread = thread.clone();
                        thread.id = i;

                        std::thread::Builder::new()
                            .name(format!("Helper thread {}", i))
                            .spawn_scoped(s, move || {
                                thread.get_move(StopCondition::Infinite, None);
                            })
                            .unwrap();
                    }

                    let builder = std::thread::Builder::new().name("Main thread".to_owned());
                    let main = builder
                        .spawn_scoped(s, || {
                            let mut best_move =
                                match thread.get_move(stop_condition.into(), root_prober) {
                                    Some(mov) => mov,
                                    None => {
                                        println!("info string Failed to get best move");
                                        return;
                                    }
                                };
                            abort.store(true, Ordering::SeqCst);

                            if best_move.piece == Piece::King
                                && best_move.capture.is_none()
                                && current_position.rooks().contains(best_move.to)
                            {
                                match best_move.from.file().cmp(&best_move.to.file()) {
                                    std::cmp::Ordering::Less => {
                                        best_move.to =
                                            Square::from_file_rank(File::G, best_move.to.rank());
                                    }
                                    std::cmp::Ordering::Greater => {
                                        best_move.to =
                                            Square::from_file_rank(File::C, best_move.to.rank());
                                    }
                                    _ => {}
                                }
                            }

                            println!("bestmove {best_move}");
                        })
                        .unwrap();

                    loop {
                        line.clear();
                        std::io::stdin().read_line(&mut line).expect("read line");
                        let cmd = Command::try_from(line.as_str());

                        if main.is_finished() {
                            cached_line = Some(line.clone());
                            break;
                        }

                        match cmd {
                            Ok(Command::Stop) => {
                                abort.store(true, Ordering::SeqCst);
                                main.join().unwrap();
                                break;
                            }
                            _ => {
                                eprintln!(
                                    "Command '{line}' unsupported during search",
                                    line = line.trim()
                                );
                            }
                        }
                    }
                })
            }
            Ok(Command::IsReady) => {
                println!("readyok");
            }
            Ok(Command::SetOption { name, value }) => match name.to_lowercase().as_str() {
                "hash" => {
                    let mb = value.parse().unwrap();
                    tt = TranspositionTable::new(mb);
                }
                "syzygypath" => {
                    fathom = fathom.and_then(|fathom| {
                        fathom
                            .reload(value)
                            .map_err(|e| {
                                println!("Failed to reinitialize fathom syzygy library: {e}")
                            })
                            .ok()
                    });
                }
                "threads" => {
                    threads = value.parse().unwrap();
                }
                _ => {}
            },
            Ok(Command::Stop) => {
                eprintln!("No search running");
            }
            Ok(Command::Position {
                position: new_position,
                moves: new_moves,
            }) => {
                original_position = Position::from_fen(new_position.as_fen()).unwrap();
                current_position = original_position.clone();
                moves.clear();
                for mov in new_moves {
                    match parse_uci_move(&current_position, mov) {
                        Some(mov) => {
                            current_position = current_position.make_move(mov);
                            moves.push(mov);
                        }
                        None => {
                            eprintln!("Unparseable move '{mov}'");
                            break;
                        }
                    }
                }
            }
            Ok(Command::Uci) => {
                println!("id name Asymptote v2");
                println!("option name Hash type spin default 8 min 1 max 65536");
                println!("option name Threads type spin default 1 min 1 max 256");
                println!("option name SyzygyPath type string default <empty>");
                println!("uciok");
            }
            Ok(Command::UciNewGame) => {}
            Ok(Command::Eval) => {
                let eval = Eval::from(&current_position).evaluate(&current_position);
                println!("Eval: {} centipawns", eval);
            }
            Ok(Command::Move(mov)) => match parse_uci_move(&current_position, mov) {
                Some(mov) => {
                    current_position = current_position.make_move(mov);
                    moves.push(mov);
                }
                None => {
                    eprintln!("Unparseable move '{mov}'");
                    break;
                }
            },
            Ok(Command::Perft { depth }) => {
                perft(&current_position, depth);
            }
            Ok(Command::Tune { filename }) => {
                if !cfg!(feature = "tune") {
                    eprintln!("Build with the 'tune' feature to enable tuning.");
                    continue;
                }

                let f = std::fs::File::open(filename).unwrap();
                let reader = std::io::BufReader::new(f);
                let mut positions = Vec::new();
                if filename.ends_with(".epd") {
                    for line in reader.lines() {
                        let line = line.unwrap();
                        let (fen, result) = line.split_once("c9").unwrap();
                        let result = match result.trim().trim_end_matches(";") {
                            "\"1-0\"" => 1.,
                            "\"0-1\"" => 0.,
                            "\"1/2-1/2\"" => 0.5,
                            r => {
                                eprintln!("Unrecognized result: {r}");
                                continue;
                            }
                        };
                        positions.push((Position::from_fen(fen.trim()).unwrap(), result));
                    }
                } else {
                    eprintln!("Unsupported file format");
                    continue;
                }

                let mut tuner = Tuner::new(&positions);

                let initial_error = tuner.total_error();
                println!("Total error: {initial_error:>8.6}");

                let mut last_printed_error = initial_error;
                let mut best = initial_error;

                let mut f = 5.;

                let mut rng = Xoshiro::new([
                    0x2d53e935047d3ab2,
                    0xcea3ee621ceba1bd,
                    0xf54e14086cabda4b,
                    (UNIX_EPOCH.elapsed().unwrap().as_secs() as u64).wrapping_shl(32)
                        ^ (UNIX_EPOCH.elapsed().unwrap().subsec_nanos() as u64),
                ]);

                for i in 1.. {
                    tuner.shuffle_traces(&mut rng);
                    tuner.step(f);

                    let error = tuner.total_error();
                    if i % 20 == 0 {
                        tuner.print();
                        println!(
                            "Total error: {:>8.6}  ({:>8.6}%)  (total {:>8.6}%)",
                            error,
                            100. * (error - last_printed_error) / last_printed_error,
                            100. * (error - initial_error) / initial_error
                        );
                        last_printed_error = error;
                        if error > best {
                            f /= 2.;
                            if f < 0.000001 {
                                break;
                            }
                            println!("Decreased learning rate. Now: {:.6}", f);
                        }

                        best = error;
                    }
                }

                tuner.print();
            }
            Err(CommandParseError::UnknownCommand(cmd)) => eprintln!("Unknown command '{cmd}'"),
            Err(e) => panic!("Error: {e}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Command<'a> {
    Go(UciStopCondition),
    IsReady,
    Position {
        position: UciPosition<'a>,
        moves: Vec<&'a str>,
    },
    SetOption {
        name: &'a str,
        value: &'a str,
    },
    Stop,
    Uci,
    UciNewGame,
    // Rest are non-UCI commands
    Eval,
    Move(&'a str),
    Perft {
        depth: usize,
    },
    Tune {
        filename: &'a str,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum UciPosition<'a> {
    Default,
    Fen(&'a str),
}

impl UciPosition<'_> {
    fn as_fen(&self) -> &str {
        match self {
            UciPosition::Default => STARTING_POSITION_FEN,
            UciPosition::Fen(fen) => fen,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum UciStopCondition {
    Infinite,
    Movetime {
        ms: u64,
    },
    Depth {
        ply: i8,
    },
    Dynamic {
        wtime: Option<u64>,
        winc: Option<u64>,
        btime: Option<u64>,
        binc: Option<u64>,
        moves_to_go: Option<u64>,
    },
}

#[derive(Debug, Error, PartialEq, Eq)]
enum StopConditionParseError {
    #[error("Unknown stop condition: {0}")]
    Unknown(String),
    #[error("Failed to parse integer: {0}")]
    ParseIntError(#[from] std::num::ParseIntError),
}

impl<'a> TryFrom<&'a str> for UciStopCondition {
    type Error = StopConditionParseError;

    fn try_from(input: &'a str) -> Result<Self, Self::Error> {
        if input.starts_with("infinite") {
            Ok(UciStopCondition::Infinite)
        } else if input.starts_with("movetime") {
            let input = input.trim_start_matches("movetime").trim();
            let ms = input.parse()?;
            Ok(UciStopCondition::Movetime { ms })
        } else if input.starts_with("depth") {
            let input = input.trim_start_matches("depth").trim();
            let ply = input.parse()?;
            Ok(UciStopCondition::Depth { ply })
        } else {
            // dynamic
            let mut wtime = None;
            let mut winc = None;
            let mut btime = None;
            let mut binc = None;
            let mut moves_to_go = None;

            for key_value in input
                .split_ascii_whitespace()
                .collect::<Vec<_>>()
                .chunks_exact(2)
            {
                match key_value {
                    ["wtime", value] => wtime = Some(value.parse()?),
                    ["winc", value] => winc = Some(value.parse()?),
                    ["btime", value] => btime = Some(value.parse()?),
                    ["binc", value] => binc = Some(value.parse()?),
                    ["movestogo", value] => moves_to_go = Some(value.parse()?),
                    _ => {}
                }
            }

            Ok(UciStopCondition::Dynamic {
                wtime,
                winc,
                btime,
                binc,
                moves_to_go,
            })
        }
    }
}

impl From<UciStopCondition> for StopCondition {
    fn from(stop_condition: UciStopCondition) -> Self {
        match stop_condition {
            UciStopCondition::Infinite => StopCondition::Infinite,
            UciStopCondition::Movetime { ms } => StopCondition::Movetime {
                limit_ms: ms.into(),
            },
            UciStopCondition::Depth { ply } => StopCondition::Depth { limit: ply.into() },
            UciStopCondition::Dynamic {
                wtime,
                winc,
                btime,
                binc,
                moves_to_go,
            } => StopCondition::Dynamic {
                white_base_ms: wtime.map(u128::from),
                white_increment_ms: winc.map(u128::from),
                black_base_ms: btime.map(u128::from),
                black_increment_ms: binc.map(u128::from),
                moves_to_go: moves_to_go.map(u128::from),
            },
        }
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
enum CommandParseError {
    #[error("Unknown command: {0}")]
    UnknownCommand(String),
    #[error("Invalid perft depth: {0}")]
    InvalidPerftDepth(ParseIntError),
    #[error("Invalid position: {0}")]
    InvalidPosition(String),
    #[error("Stop condition: {0}")]
    StopCondition(#[from] StopConditionParseError),
}

impl<'a> TryFrom<&'a str> for Command<'a> {
    type Error = CommandParseError;

    fn try_from(input: &str) -> Result<Command, Self::Error> {
        let input = input.trim();
        if input.starts_with("go ") {
            let input = input.trim_start_matches("go").trim_start();
            let stop_condition = UciStopCondition::try_from(input)?;
            Ok(Command::Go(stop_condition))
        } else if input.starts_with("isready") {
            Ok(Command::IsReady)
        } else if input.starts_with("setoption ") {
            let input = input
                .trim_start_matches("setoption")
                .trim_start()
                .trim_start_matches("name")
                .trim_start();

            let (name, value) = input.split_once(" value ").unwrap_or((input, ""));
            let name = name.trim();
            let value = value.trim();

            Ok(Command::SetOption { name, value })
        } else if input.starts_with("stop") {
            Ok(Command::Stop)
        } else if input.starts_with("position ") {
            let input = input.trim_start_matches("position").trim_start();
            let (position, moves) = input.split_once(" moves ").unwrap_or((input, ""));

            let position = if position == "startpos" {
                UciPosition::Default
            } else if position.starts_with("fen") {
                UciPosition::Fen(position.trim_start_matches("fen "))
            } else {
                return Err(CommandParseError::InvalidPosition(position.to_owned()));
            };

            let moves = moves.split_whitespace().collect();

            Ok(Command::Position { position, moves })
        } else if input.starts_with("ucinewgame") {
            Ok(Command::UciNewGame)
        } else if input.starts_with("uci") {
            Ok(Command::Uci)

        // The following are non-UCI commands
        } else if input.starts_with("eval") {
            Ok(Command::Eval)
        } else if input.starts_with("move ") {
            let input = input.trim_start_matches("move ");
            Ok(Command::Move(input))
        } else if input.starts_with("perft ") {
            let depth = input
                .trim_start_matches("perft ")
                .parse::<usize>()
                .map_err(CommandParseError::InvalidPerftDepth)?;

            Ok(Command::Perft { depth })
        } else if input.starts_with("tune ") {
            let input = input.trim_start_matches("tune ");
            Ok(Command::Tune { filename: input })
        } else {
            Err(CommandParseError::UnknownCommand(input.to_owned()))
        }
    }
}

fn parse_uci_move(position: &Position, input: &str) -> Option<Move> {
    let bytes = input.as_bytes();
    let from_file = bytes.get(0).copied().and_then(parse_file)?;
    let from_rank = bytes.get(1).copied().and_then(parse_rank)?;
    let to_file = bytes.get(2).copied().and_then(parse_file)?;
    let to_rank = bytes.get(3).copied().and_then(parse_rank)?;
    let promotion = bytes.get(4).copied().and_then(parse_promotion_piece);

    let from = Square::from_file_rank(from_file, from_rank);
    let to = Square::from_file_rank(to_file, to_rank);

    let piece = position.find_piece(from)?;
    let capture = position.find_piece(to);

    let en_passant = piece == Piece::Pawn && from_file != to_file && capture.is_none();
    let capture = if en_passant {
        Some(Piece::Pawn)
    } else {
        capture
    };

    // Internally, castling moves are encoded as the king moving to the rook square. UCI encodes
    // them as the king taking two steps in one direction.
    let (to, capture) = if piece == Piece::King && from_file.abs_diff(to_file) > 1 {
        // In FRC/Chess960, UCI will encode them as the king moving to the rook square, too. We
        // need to clear the captured field in that case.
        if position.pieces(position.side_to_move()).contains(to) {
            (to, None)
        } else if from_file < to_file {
            // Non-FRC kingside castle
            (Square::from_file_rank(File::H, from_rank), None)
        } else if from_file > to_file {
            // Non-FRC queenside castle
            (Square::from_file_rank(File::A, from_rank), None)
        } else {
            return None;
        }
    } else {
        (
            to,
            capture.filter(|_| !position.pieces(position.side_to_move()).contains(to)),
        )
    };

    Some(Move {
        from,
        to,
        piece,
        capture,
        promotion,
        en_passant,
    })
}

fn parse_file(c: u8) -> Option<File> {
    match c {
        b'a' => Some(File::A),
        b'b' => Some(File::B),
        b'c' => Some(File::C),
        b'd' => Some(File::D),
        b'e' => Some(File::E),
        b'f' => Some(File::F),
        b'g' => Some(File::G),
        b'h' => Some(File::H),
        _ => None,
    }
}

fn parse_rank(c: u8) -> Option<Rank> {
    match c {
        b'1' => Some(Rank::One),
        b'2' => Some(Rank::Two),
        b'3' => Some(Rank::Three),
        b'4' => Some(Rank::Four),
        b'5' => Some(Rank::Five),
        b'6' => Some(Rank::Six),
        b'7' => Some(Rank::Seven),
        b'8' => Some(Rank::Eight),
        _ => None,
    }
}

fn parse_promotion_piece(c: u8) -> Option<Piece> {
    match c {
        b'k' => Some(Piece::Knight),
        b'b' => Some(Piece::Bishop),
        b'r' => Some(Piece::Rook),
        b'q' => Some(Piece::Queen),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use test_case::test_case;

    use super::{Command, CommandParseError, UciPosition};

    #[test_case("perft 5" => Ok(Command::Perft { depth: 5 }); "perft 5")]
    #[test_case("position startpos" => Ok(Command::Position { position: UciPosition::Default, moves: vec![] }); "position default")]
    #[test_case("position startpos moves d2d4 d7d5 c2c4" => Ok(Command::Position { position: UciPosition::Default, moves: vec!["d2d4", "d7d5", "c2c4"] }); "position default and moves")]
    #[test_case("position fen r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - moves e1g1" => Ok(Command::Position { position: UciPosition::Fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -"), moves: vec!["e1g1"] }); "position fen and moves")]
    fn parse_command(input: &str) -> Result<Command, CommandParseError> {
        input.try_into()
    }
}
