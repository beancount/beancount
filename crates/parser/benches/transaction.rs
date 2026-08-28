use beancount_parser::parse_lossy as parse_chumsky;
use criterion::{Criterion, black_box, criterion_group, criterion_main};

const TRANSACTION: &str = r#"2013-06-22 * "La Colombe" "Buying coffee"  ^ee89ada94a39
  Expenses:Coffee         5 USD
  Assets:US:Cash
"#;
const BEAN_EXAMPLE: &str = include_str!("../../../examples/example.beancount");

fn bench_transaction(c: &mut Criterion) {
  let mut group = c.benchmark_group("transaction_parse");

  group.bench_function("chumsky", |b| {
    b.iter(|| {
      parse_chumsky(black_box(TRANSACTION));
    })
  });

  group.finish();
}

fn bench_example(c: &mut Criterion) {
  let mut group = c.benchmark_group("bean_example_parse");

  group.bench_function("chumsky", |b| {
    b.iter(|| {
      parse_chumsky(black_box(BEAN_EXAMPLE));
    })
  });

  group.finish();
}

criterion_group!(benches, bench_transaction, bench_example);
criterion_main!(benches);
