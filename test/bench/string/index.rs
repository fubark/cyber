use std::time::{Instant};

fn main() {
    let str = "abcdefghijklmnopqrstuvwxyz123456".repeat(1000000) + "waldo";

    let start = Instant::now();
    let mut idx = 0;
    for _ in 0..50 {
        idx = str.find("waldo").unwrap();
    }
    println!("idx: {} ms: {}", idx, start.elapsed().as_millis());
}