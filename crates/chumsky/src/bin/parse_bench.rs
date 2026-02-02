use beancount_chumsky::parse_str;
use std::env;
use std::fs;

fn main() {
  let mut args = env::args().skip(1);
  let input_path = args
    .next()
    .unwrap_or_else(|| "examples/example.beancount".to_string());
  let iters: usize = args.next().and_then(|val| val.parse().ok()).unwrap_or(1000);

  let input = fs::read_to_string(&input_path).expect("read input file");

  for _ in 0..iters {
    let _ = parse_str(&input, &input_path).expect("parse");
  }
}
