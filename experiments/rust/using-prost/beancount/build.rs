use std::io::Result;

fn main() -> Result<()> {
    prost_build::compile_protos(
        &[
            "../../protos/beancount/ccore/number.proto",
            "../../protos/beancount/ccore/date.proto",
            "../../protos/beancount/ccore/precision.proto",
            "../../protos/beancount/ccore/data.proto",
            "../../protos/beancount/cparser/inter.proto",
            "../../protos/beancount/cparser/options.proto",
            "../../protos/beancount/cparser/ledger.proto",
        ],
        &["../../protos"],
    )?;
    Ok(())
}
