use std::path::PathBuf;

use beancount_parser_py::parse_string;
use pyo3::prelude::*;
use pyo3::types::PyList;

fn add_project_root_to_sys_path(py: Python<'_>) -> PyResult<()> {
    let root: PathBuf = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .map(PathBuf::from)
        .expect("project root");
    let code = format!("import sys; sys.path.insert(0, r\"{}\")", root.to_string_lossy());
    py.run(&code, None, None)
}

#[test]
fn parses_cost_spec_into_costspec() {
    pyo3::prepare_freethreaded_python();
    Python::with_gil(|py| -> PyResult<()> {
        add_project_root_to_sys_path(py)?;

        let input = vec![
            "2014-06-05 * \"Payee\" \"Narr\"",
            "  Assets:Investing      30 HOOL {40 USD}",
            "  Assets:Other",
            "",
            "2014-06-06 *",
            "  Assets:Investing      2 HOOL {{120 USD}}",
            "  Assets:Other",
            "",
        ]
        .join("\n");

        let (entries, errors, _opts) = parse_string(py, &input, Some("<memory>"))?;
        let errors: Bound<'_, PyList> = errors.bind(py).downcast_into()?;
        assert_eq!(errors.len(), 0);

        let entries: Bound<'_, PyList> = entries.bind(py).downcast_into()?;
        assert_eq!(entries.len(), 2);

        let txn1 = entries.get_item(0)?;
        let postings1: Bound<'_, PyList> = txn1.getattr("postings")?.downcast_into()?;
        let posting1 = postings1.get_item(0)?;
        let cost1 = posting1.getattr("cost")?;
        let cost1_cls: String = cost1.getattr("__class__")?.getattr("__name__")?.extract()?;
        assert_eq!(cost1_cls, "CostSpec");
        let number_per1 = cost1.getattr("number_per")?;
        assert_eq!(number_per1.str()?.to_str()?, "40");
        let number_total1 = cost1.getattr("number_total")?;
        assert!(number_total1.is_none());
        let currency1: String = cost1.getattr("currency")?.extract()?;
        assert_eq!(currency1, "USD");
        assert!(cost1.getattr("date")?.is_none());
        assert!(cost1.getattr("label")?.is_none());
        assert!(!cost1.getattr("merge")?.extract::<bool>()?);

        let txn2 = entries.get_item(1)?;
        let postings2: Bound<'_, PyList> = txn2.getattr("postings")?.downcast_into()?;
        let posting2 = postings2.get_item(0)?;
        let cost2 = posting2.getattr("cost")?;
        let number_per2 = cost2.getattr("number_per")?;
        assert_eq!(number_per2.str()?.to_str()?, "0");
        let number_total2 = cost2.getattr("number_total")?;
        assert_eq!(number_total2.str()?.to_str()?, "120");
        let currency2: String = cost2.getattr("currency")?.extract()?;
        assert_eq!(currency2, "USD");

        Ok(())
    })
    .unwrap();
}
