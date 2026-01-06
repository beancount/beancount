use beancount_parser::{
    ast::{Directive, Span},
    parse_str,
};

#[test]
fn parses_transaction_with_inline_link_and_postings() {
    let input = vec![
        "2013-06-22 * \"La Colombe\" \"Buying coffee\"  ^ee89ada94a39",
        "  Expenses:Coffee         5 USD",
        "  Assets:US:Cash",
        "",
    ]
    .join("\n");

    let directives = parse_str(&input.as_str(), "input.bean").expect("parse failed");
    assert_eq!(directives.len(), 1);

    let txn = match &directives[0] {
        Directive::Transaction(txn) => txn,
        other => panic!("expected transaction, got {other:?}"),
    };

    assert_eq!(txn.meta.filename, "input.bean");
    assert_eq!(txn.meta.line, 1);
    assert_eq!(txn.meta.column, 1);
    assert_eq!(txn.span, Span::from_range(0, input.len()));

    assert_eq!(txn.date, "2013-06-22");
    assert_eq!(txn.txn, Some("*"));
    assert_eq!(txn.payee.as_deref(), Some("La Colombe"));
    assert_eq!(txn.narration.as_deref(), Some("Buying coffee"));
    assert_eq!(txn.tags_links, Some("^ee89ada94a39"));
    assert!(txn.tags.is_empty());
    assert_eq!(txn.links.to_vec(), vec!["ee89ada94a39"]);
    assert_eq!(txn.comment, None);
    assert_eq!(txn.tags_links_lines.to_vec(), vec!["^ee89ada94a39"]);
    assert!(txn.comments.is_empty());
    assert!(txn.key_values.is_empty());

    assert_eq!(txn.postings.len(), 2);

    let p1_line = "  Expenses:Coffee         5 USD\n";
    let p1_start = input.find(p1_line).expect("p1 start");
    let p1_end = p1_start + p1_line.len();
    let p1 = &txn.postings[0];
    assert_eq!(p1.meta.line, 2);
    assert_eq!(p1.meta.column, 1);
    assert_eq!(p1.span, Span::from_range(p1_start, p1_end));
    assert_eq!(p1.opt_flag, None);
    assert_eq!(p1.account, "Expenses:Coffee");
    let p1_amount = p1.amount.as_ref().expect("p1 amount");
    assert_eq!(p1_amount.raw, "5 USD");
    assert_eq!(p1_amount.number, "5");
    assert_eq!(p1_amount.currency, "USD");
    assert_eq!(p1.cost_spec, None);
    assert_eq!(p1.price_operator, None);
    assert_eq!(p1.price_annotation, None);
    assert_eq!(p1.comment, None);

    let p2_line = "  Assets:US:Cash\n";
    let p2_start = input.find(p2_line).expect("p2 start");
    let p2_end = p2_start + p2_line.len();
    let p2 = &txn.postings[1];
    assert_eq!(p2.meta.line, 3);
    assert_eq!(p2.meta.column, 1);
    assert_eq!(p2.span, Span::from_range(p2_start, p2_end));
    assert_eq!(p2.opt_flag, None);
    assert_eq!(p2.account, "Assets:US:Cash");
    assert!(p2.amount.is_none());
    assert_eq!(p2.cost_spec, None);
    assert_eq!(p2.price_operator, None);
    assert_eq!(p2.price_annotation, None);
    assert_eq!(p2.comment, None);
}

#[test]
fn parses_and_sorts_tags_and_links() {
    let input = vec![
        "2013-06-22 * \"Payee\" \"Narr\"  #b ^z #a ^a #b",
        "  Assets:Cash 1 USD",
        "",
    ]
    .join("\n");

    let directives = parse_str(&input, "input.bean").expect("parse failed");
    assert_eq!(directives.len(), 1);

    let txn = match &directives[0] {
        Directive::Transaction(txn) => txn,
        other => panic!("expected transaction, got {other:?}"),
    };

    assert_eq!(txn.payee.as_deref(), Some("Payee"));
    assert_eq!(txn.narration.as_deref(), Some("Narr"));
    assert_eq!(txn.tags.to_vec(), vec!["a", "b"]);
    assert_eq!(txn.links.to_vec(), vec!["a", "z"]);
    assert_eq!(txn.tags_links_lines.to_vec(), vec!["#b ^z #a ^a #b"]);
}
