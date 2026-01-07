use beancount_parser::{ast::Directive, parse_str};

#[test]
fn parses_posting_cost_spec() {
    let input = [
        "2014-01-01 open Assets:Investing",
        "2014-01-01 open Assets:Other",
        "",
        "2014-06-05 *",
        "  Assets:Investing      30 HOOL {40 USD}",
        "  Assets:Other",
        "",
        "2014-06-05 *",
        "  Assets:Investing      -20 HOOL {40 USD}",
        "  Assets:Other",
        "",
    ]
    .join("\n");

    let directives = parse_str(&input, "input.bean").expect("parse failed");
    assert_eq!(directives.len(), 4);

    // First transaction
    let txn1 = match &directives[2] {
        Directive::Transaction(t) => t,
        other => panic!("expected transaction, got {other:?}"),
    };
    assert_eq!(txn1.postings.len(), 2);
    let p1 = &txn1.postings[0];
    assert_eq!(p1.account, "Assets:Investing");
    let amt1 = p1.amount.as_ref().expect("amount");
    assert_eq!(amt1.raw, "30 HOOL");
    assert_eq!(
        amt1.number,
        beancount_parser::ast::NumberExpr::Literal("30")
    );
    assert_eq!(amt1.currency, Some("HOOL"));
    let cost1 = p1.cost_spec.as_ref().expect("cost_spec");
    let cost_amount = cost1.amount.as_ref().expect("cost amount");
    assert_eq!(
        cost_amount.per,
        Some(beancount_parser::ast::NumberExpr::Literal("40"))
    );
    assert_eq!(cost_amount.total, None);
    assert_eq!(cost_amount.currency, Some("USD"));
    assert!(!cost1.merge);
    assert!(!cost1.is_total);
    assert_eq!(cost1.date, None);
    assert_eq!(cost1.label, None);
    assert_eq!(p1.price_operator, None);
    assert_eq!(p1.price_annotation, None);

    let p1b = &txn1.postings[1];
    assert_eq!(p1b.account, "Assets:Other");
    assert!(p1b.amount.is_none());
    assert_eq!(p1b.cost_spec, None);

    // Second transaction
    let txn2 = match &directives[3] {
        Directive::Transaction(t) => t,
        other => panic!("expected transaction, got {other:?}"),
    };
    assert_eq!(txn2.postings.len(), 2);
    let p2 = &txn2.postings[0];
    let amt2 = p2.amount.as_ref().expect("amount");
    assert_eq!(amt2.raw, "-20 HOOL");
    assert_eq!(
        amt2.number,
        beancount_parser::ast::NumberExpr::Literal("-20")
    );
    assert_eq!(amt2.currency, Some("HOOL"));
    let cost2 = p2.cost_spec.as_ref().expect("cost_spec");
    let cost_amount = cost2.amount.as_ref().expect("cost amount");
    assert_eq!(
        cost_amount.per,
        Some(beancount_parser::ast::NumberExpr::Literal("40"))
    );
    assert_eq!(cost_amount.total, None);
    assert_eq!(cost_amount.currency, Some("USD"));
    assert!(!cost2.merge);
    assert!(!cost2.is_total);
    assert_eq!(cost2.date, None);
    assert_eq!(cost2.label, None);
    assert_eq!(p2.price_operator, None);
    assert_eq!(p2.price_annotation, None);
}
