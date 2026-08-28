use chumsky::prelude::*;

use crate::{Error, ast};

use super::common::{line_end, not_eol_parser};

pub(super) fn raw_block_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> + 'src {
  let raw_line = not_eol_parser()
    .repeated()
    .at_least(1)
    .to_slice()
    .filter(|line: &&str| {
      let trimmed = line.trim_start();
      !(trimmed.starts_with('#') || trimmed.starts_with('^'))
    })
    .boxed();

  raw_line
    .clone()
    .then(line_end().ignore_then(raw_line.clone()).repeated())
    .then(choice((just("\r\n").to(2usize), just('\n').to(1usize))).or_not())
    .map_with(move |((_, _), trailing_nl), e| {
      // Build from the contiguous slice; exclude an optional trailing newline.
      let span: SimpleSpan = e.span();
      let consumed_extra = trailing_nl.unwrap_or(0);
      let end = span.end.saturating_sub(consumed_extra);
      let span = ast::Span::from_range(span.start, end);

      let consumed = e.slice();
      let text = &consumed[..consumed.len().saturating_sub(consumed_extra)];

      ast::Directive::Raw(ast::Raw { span, text })
    })
    .boxed()
}
