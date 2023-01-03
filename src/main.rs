#![allow(dead_code)]

mod bench;
mod eval;
mod hash;
mod history;
mod magic;
mod movegen;
mod movepick;
mod position;
mod random;
mod stop_condition;
mod thread;
mod tt;
mod tune;
mod types;
mod uci;

fn main() {
    let mut args = std::env::args().skip(1);
    match args.next().as_deref() {
        None => uci::run(),
        Some("bench") => bench::run(),
        Some(arg) => eprintln!("Unknown command `{arg}`"),
    }
}
