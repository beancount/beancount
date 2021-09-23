#include "beancount/cparser/parser.h"
#include "beancount/cparser/scanner.h"
#include "beancount/cparser/builder.h"
#include "beancount/cparser/ledger.h"
#include "beancount/cparser/test_utils.h"

#include <algorithm>
#include <cassert>
#include <limits>
#include <memory>
#include <string>
#include <vector>

#include "absl/strings/string_view.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "reflex/input.h"

namespace beancount {
namespace {

using std::pair;
using std::string;
using std::vector;
using std::cout;
using std::endl;
using std::string_view;

struct CompareOptions {
  bool partial = false;
  bool no_dedent = false;
  bool leave_lineno = false;
  bool print_input = false;
  bool debug = false;
};


// Evaluate all the expressions in a single directive.
void ReduceExpressions(Ledger* ledger,
                       decimal::Context& context,
                       beancount::Directive* directive) {
  if (!directive->has_transaction())
    return;
  for (auto& posting : *directive->mutable_transaction()->mutable_postings()) {
    if (posting.has_spec()) {
      auto* spec = posting.mutable_spec();
      if (spec->has_units()) {
        parser::ReduceExpression(spec->mutable_units(), context, false);
      }

      if (spec->has_cost()) {
        auto* cost = spec->mutable_cost();
        if (cost->has_per_unit()) {
          parser::ReduceExpression(cost->mutable_per_unit(), context, false);
        }
        if (cost->has_total()) {
          parser::ReduceExpression(cost->mutable_total(), context, false);
        }
      }

      if (spec->has_price()) {
        auto* price = spec->mutable_price();
        parser::ReduceExpression(price, context, false);

        // Prices may not be negative. Check and issue an error if found; fix up
        // the price to its absolute value and continue.
        //
        // TODO(blais): Delay this to post-parsing. Expressions may reduce to a
        // negative number and that would be an error too.
        if (price->has_number()) {
          decimal::Decimal dec = ProtoToDecimal(price->number());
          if (dec.sign() == -1) {
            // TODO(blais): Move all the number processing to post-parsing.
            AddError(ledger,
                     "Negative prices are not allowed "
                     "(see http://furius.ca/beancount/doc/bug-negative-prices "
                     "for workaround)", directive->location());
            // Invert and continue.
            DecimalToProto(-dec, false, price->mutable_number());
          }
        }
      }
    }
  }
}

std::unique_ptr<Ledger> ExpectParse(const std::string& input_string,
                                    const std::string& expected_string,
                                    const CompareOptions& options = {}) {
  string clean_string = options.no_dedent ? input_string : StripAndDedent(input_string);
  if (options.print_input) {
    std::cout << "clean_string = >>>>>" << clean_string << "<<<<<" << std::endl;
  }
  auto ledger = parser::ParseString(clean_string, "<string>", 0, options.debug);

  // Reduce all expressions in a given contxt.
  decimal::Context context = decimal::context;
  context.prec(28);
  using namespace std::placeholders;
  std::for_each(ledger->directives.begin(), ledger->directives.end(),
                std::bind(&ReduceExpressions, ledger.get(), context, _1));

  ClearLineNumbers(ledger.get(), options.leave_lineno);
  auto ledger_proto = LedgerToProto(*ledger);

  // Compare the whole protos.
  inter::Ledger expected_proto;
  if (!google::protobuf::TextFormat::ParseFromString(
          expected_string,
          &expected_proto)) {
    throw std::domain_error("Could not parse expected proto.");
  }
  EXPECT_TRUE(EqualsMessages(expected_proto, *ledger_proto, options.partial));

  // If comparing partial, ensure that we compare the errors non-partial.
  if (options.partial) {
    expected_proto.clear_directives();
    expected_proto.clear_options();
    expected_proto.clear_info();
    ledger_proto->clear_directives();
    ledger_proto->clear_options();
    ledger_proto->clear_info();
    EXPECT_TRUE(EqualsMessages(expected_proto, *ledger_proto, {}));
  }

  return ledger;
}


TEST(ParserTest, TestBasicTesting) {
  auto ledger = ExpectParse(R"(

    2014-01-27 * "UNION MARKET"
      Liabilities:US:Amex:BlueCash    -22.02 USD
      Expenses:Food:Grocery            22.02 USD

  )", R"(
    directives {
      date { year: 2014 month: 1 day: 27 }
      transaction {
        flag: "*"
        narration: "UNION MARKET"
        postings {
          account: "Liabilities:US:Amex:BlueCash"
          spec { units { number { exact: "-22.02" } currency: "USD" } }
        }
        postings {
          account: "Expenses:Food:Grocery"
          spec { units { number { exact: "22.02" } currency: "USD" } }
        }
      }
    }
  )");
  EXPECT_EQ(1, ledger->directives.size());
}

TEST(TestParserEntryTypes, TransactionOneString) {
  ExpectParse(R"(
    2013-05-18 * "Nice dinner at Mermaid Inn"
      Expenses:Restaurant         100 USD
      Assets:US:Cash             -100 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Nice dinner at Mermaid Inn"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:Cash"
          spec { units { number { exact: "-100" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestParserEntryTypes, TransactionTwoStrings) {
  ExpectParse(R"(
    2013-05-18 * "Mermaid Inn" "Nice dinner"
      Expenses:Restaurant         100 USD
      Assets:US:Cash             -100 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        payee: "Mermaid Inn"
        narration: "Nice dinner"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:Cash"
          spec { units { number { exact: "-100" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestParserEntryTypes, TransactionThreeStrings) {
  ExpectParse(R"(
    2013-05-18 * "Mermaid Inn" "Nice dinner" "With Caroline"
      Expenses:Restaurant         100 USD
      Assets:US:Cash             -100 USD
  )", R"(
    errors {
      message: "Syntax error, unexpected STRING, expecting EOL or TAG or LINK"
    }
  )");
}

TEST(TestParserEntryTypes, TransactionWithTxnKeyword) {
  ExpectParse(R"(
    2013-05-18 txn "Nice dinner at Mermaid Inn"
      Expenses:Restaurant         100 USD
      Assets:US:Cash             -100 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Nice dinner at Mermaid Inn"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:Cash"
          spec { units { number { exact: "-100" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestParserEntryTypes, Balance) {
  ExpectParse(R"(
    2013-05-18 balance Assets:US:BestBank:Checking  200 USD
    2013-05-18 balance Assets:US:BestBank:Checking  200 ~ 0.002 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      balance {
        account: "Assets:US:BestBank:Checking"
        amount { number { exact: "200" } currency: "USD" }
      }
    }
    directives {
      date { year: 2013 month: 5 day: 18 }
      balance {
        account: "Assets:US:BestBank:Checking"
        amount { number { exact: "200" } currency: "USD" }
        tolerance { exact: "0.002" }
      }
    }
  )");
}

TEST(TestParserEntryTypes, BalanceWithCost) {
  ExpectParse(R"(
    2013-05-18 balance Assets:Investments  10 MSFT {45.30 USD}
  )", R"(
    errors {
      message: "Syntax error, unexpected LCURL, expecting EOL or TAG or LINK"
    }
  )");
}

TEST(TestParserEntryTypes, Open1) {
  ExpectParse(R"(
    2013-05-18 open Assets:US:BestBank:Checking
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      open {
        account: "Assets:US:BestBank:Checking"
      }
    }
  )");
}

TEST(TestParserEntryTypes, Open2) {
  ExpectParse(R"(
    2013-05-18 open Assets:US:BestBank:Checking   USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      open {
        account: "Assets:US:BestBank:Checking"
        currencies: "USD"
      }
    }
  )");
}

TEST(TestParserEntryTypes, Open3) {
  ExpectParse(R"(
    2013-05-18 open Assets:Cash   USD,CAD,EUR
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      open {
        account: "Assets:Cash"
        currencies: "USD"
        currencies: "CAD"
        currencies: "EUR"
      }
    }
  )");
}

TEST(TestParserEntryTypes, Open4) {
  ExpectParse(R"(
    2013-05-18 open Assets:US:Vanguard:VIIPX  VIIPX  "STRICT"
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      open {
        account: "Assets:US:Vanguard:VIIPX"
        currencies: "VIIPX"
        booking: STRICT
      }
    }
  )");
}

TEST(TestParserEntryTypes, Open5) {
  ExpectParse(R"(
    2013-05-18 open Assets:US:Vanguard:VIIPX    "STRICT"
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      open {
        account: "Assets:US:Vanguard:VIIPX"
        booking: STRICT
      }
    }
  )");
}

TEST(TestParserEntryTypes, Close) {
  ExpectParse(R"(
    2013-05-18 close Assets:US:BestBank:Checking
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      close {
        account: "Assets:US:BestBank:Checking"
      }
    }
  )");
}

TEST(TestParserEntryTypes, Commodity) {
  ExpectParse(R"(
    2013-05-18 commodity MSFT
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      commodity {
        currency: "MSFT"
      }
    }
  )");
}

TEST(TestParserEntryTypes, Pad) {
  ExpectParse(R"(
    2013-05-18 pad Assets:US:BestBank:Checking  Equity:Opening-Balances
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      pad {
        account: "Assets:US:BestBank:Checking"
        source_account: "Equity:Opening-Balances"
      }
    }
  )");
}

TEST(TestParserEntryTypes, Event) {
  ExpectParse(R"(
    2013-05-18 event "location" "New York, USA"

    ;; Test empty event.
    2013-05-18 event "location" ""
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      event {
        type: "location"
        description: "New York, USA"
      }
    }
    directives {
      date { year: 2013 month: 5 day: 18 }
      event {
        type: "location"
        description: ""
      }
    }
  )");
}

TEST(TestParserEntryTypes, Query) {
  ExpectParse(R"(
    2013-05-18 query "cash" "SELECT SUM(position) WHERE currency = 'USD'"
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      query {
        name: "cash"
        query_string: "SELECT SUM(position) WHERE currency = \'USD\'"
      }
    }
  )");
}

TEST(TestParserEntryTypes, Note) {
  ExpectParse(R"(
    2013-05-18 note Assets:US:BestBank:Checking  "Blah, di blah."
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      note {
        account: "Assets:US:BestBank:Checking"
        comment: "Blah, di blah."
      }
    }
  )");
}

TEST(TestParserEntryTypes, Document) {
  ExpectParse(R"(
    2013-05-18 document Assets:US:BestBank:Checking "/Accounting/statement.pdf"
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      document {
        account: "Assets:US:BestBank:Checking"
        filename: "/Accounting/statement.pdf"
      }
    }
  )");
}

TEST(TestParserEntryTypes, Price) {
  ExpectParse(R"(
    2013-05-18 price USD   1.0290 CAD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      price {
        currency: "USD"
        amount { number { exact: "1.0290" } currency: "CAD" }
      }
    }
  )");
}

TEST(TestParserEntryTypes, Custom) {
  ExpectParse(R"(
    2013-05-18 custom "budget" "weekly < 1000.00 USD" 2016-02-28 TRUE 43.03 USD 23
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      custom {
        type: "budget"
        values {
          text: "weekly < 1000.00 USD"
        }
        values {
          date { year: 2016 month: 2 day: 28 }
        }
        values {
          boolean: true
        }
        values {
          number { exact: "43.03" }
        }
        values {
          currency: "USD"
        }
        values {
          number { exact: "23" }
        }
      }
    }
  )");
}

//------------------------------------------------------------------------------
// TestWhitespace

TEST(TestWhitespace, IndentError0) {
  // TODO(blais): Figure out if we can turn these two errors to single one.
  ExpectParse(R"(
    2020-07-28 open Assets:Foo
      2020-07-28 open Assets:Bar
  )", R"(
    directives {
      date { year: 2020 month: 7 day: 28 }
      open {
        account: "Assets:Bar"
      }
    }
    errors {
      message: "Syntax error, unexpected DATE, expecting DEDENT or TAG or LINK or KEY"
      location { lineno: 2 }
    }
    errors {
      message: "Syntax error, unexpected DEDENT"
      location { lineno: 3 }
    }
  )", { .leave_lineno = true });
}

TEST(TestWhitespace, IndentError1) {
  // TODO(blais): Figure out if we can turn these two errors to single one.
  ExpectParse(R"(
    2020-07-28 open Assets:Foo

      2020-07-28 open Assets:Bar
  )", R"(
    directives {
      date { year: 2020 month: 7 day: 28 }
      open {
        account: "Assets:Foo"
      }
    }
    directives {
      date { year: 2020 month: 7 day: 28 }
      open {
        account: "Assets:Bar"
      }
    }
    errors {
      message: "Syntax error, unexpected INDENT"
      location { lineno: 3 }
    }
    errors {
      message: "Syntax error, unexpected DEDENT"
      location { lineno: 4 }
    }
  )", { .leave_lineno = true });
}

//------------------------------------------------------------------------------
// TestParserComplete

TEST(TestParserComplete, TransactionSinglePostingAtZero) {
  // TODO(blais): Figure out if we can turn these two errors to single one.
  ExpectParse(R"(
    2013-05-18 * "Nice dinner at Mermaid Inn"
      Expenses:Restaurant         0 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Nice dinner at Mermaid Inn"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "0" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestParserComplete, TransactionImbalanceFromSinglePosting) {
  // TODO(blais): Figure out if we can turn these two errors to single one.
  ExpectParse(R"(
    2013-05-18 * "Nice dinner at Mermaid Inn"
      Expenses:Restaurant         100 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Nice dinner at Mermaid Inn"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
      }
    }
  )");
}

//------------------------------------------------------------------------------
// TestUglyBugs

TEST(TestParserComplete, Empty1) {
  ExpectParse("", R"(
  )");
}

TEST(TestParserComplete, Empty2) {
  ExpectParse(R"(

  )", R"(
  )");
}

TEST(TestParserComplete, Comment) {
  ExpectParse(R"(
    ;; This is some comment.
  )", R"(
  )");
}

TEST(TestParserComplete, ExtraWhitespaceNote) {
  // TODO(blais): We could do better here, in the original, the directive is produced.
  ExpectParse(R"(
    2013-07-11 note Assets:Cash "test"
      ;;
  )", R"(
    errors {
      message: "Syntax error, unexpected EOL, expecting DEDENT or TAG or LINK or KEY"
    }
  )");
}

TEST(TestParserComplete, ExtraWhitespaceTransaction) {
  // TODO(blais): Fix this, in the original, there's no error.
  ExpectParse(absl::StrCat(
    "2013-05-18 * \"Nice dinner at Mermaid Inn\"\n",
    "  Expenses:Restaurant         100 USD\n",
    "  Assets:US:Cash\n",
    "  \n",
    ";; End of file"), R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Nice dinner at Mermaid Inn"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:Cash"
        }
      }
    }
  )", { .no_dedent = true });
}

TEST(TestParserComplete, ExtraWhitespaceComment) {
  ExpectParse(absl::StrCat(
    "2013-05-18 * \"Nice dinner at Mermaid Inn\"\n",
    "  Expenses:Restaurant         100 USD\n",
    "  Assets:US:Cash\n",
    "  ;;"), R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Nice dinner at Mermaid Inn"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:Cash"
        }
      }
    }
  )", { .no_dedent = true });
}

TEST(TestParserComplete, ExtraWhitespaceCommentIndented) {
  ExpectParse(absl::StrCat(
    "2013-05-18 * \"Nice dinner at Mermaid Inn\"\n",
    "  Expenses:Restaurant         100 USD\n",
    "  Assets:US:Cash\n",
    "    ;;"), R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Nice dinner at Mermaid Inn"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:Cash"
        }
      }
    }
  )", { .no_dedent = true });
}

TEST(TestParserComplete, IndentEOF) {
  ExpectParse("\t", R"(
  )", { .no_dedent = true });
}

TEST(TestParserComplete, CommentEOF) {
  ExpectParse("; comment", R"(
  )", { .no_dedent = true });
}

TEST(TestParserComplete, NoEmptyLines) {
  ExpectParse(R"(
    2013-05-01 open Assets:Cash   USD,CAD,EUR
    2013-05-02 close Assets:US:BestBank:Checking
    2013-05-03 pad Assets:US:BestBank:Checking  Equity:Opening-Balances
    2013-05-04 event "location" "New York, USA"
    2013-05-05 * "Payee" "Narration"
      Assets:US:BestBank:Checking   100.00 USD
      Assets:Cash                  -100.00 USD
    2013-05-06 note Assets:US:BestBank:Checking  "Blah, di blah."
    2013-05-07 price USD   1.0290 CAD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 1 }
      open {
        account: "Assets:Cash"
        currencies: "USD"
        currencies: "CAD"
        currencies: "EUR"
      }
    }
    directives {
      date { year: 2013 month: 5 day: 2 }
      close {
        account: "Assets:US:BestBank:Checking"
      }
    }
    directives {
      date { year: 2013 month: 5 day: 3 }
      pad {
        account: "Assets:US:BestBank:Checking"
        source_account: "Equity:Opening-Balances"
      }
    }
    directives {
      date { year: 2013 month: 5 day: 4 }
      event {
        type: "location"
        description: "New York, USA"
      }
    }
    directives {
      date { year: 2013 month: 5 day: 5 }
      transaction {
        flag: "*"
        payee: "Payee"
        narration: "Narration"
        postings {
          account: "Assets:US:BestBank:Checking"
          spec { units { number { exact: "100.00" } currency: "USD" } }
        }
        postings {
          account: "Assets:Cash"
          spec { units { number { exact: "-100.00" } currency: "USD" } }
        }
      }
    }
    directives {
      date { year: 2013 month: 5 day: 6 }
      note {
        account: "Assets:US:BestBank:Checking"
        comment: "Blah, di blah."
      }
    }
    directives {
      date { year: 2013 month: 5 day: 7 }
      price {
        currency: "USD"
        amount { number { exact: "1.0290" } currency: "CAD" }
      }
    }
  )");
}

//------------------------------------------------------------------------------
// TestComment

TEST(TestComment, CommentBeforeTransaction) {
  ExpectParse(R"(
    ; Hi
    2015-06-07 *
      Assets:Cash   1 USD
      Assets:Cash   -1 USD
  )", R"(
    directives {
      date { year: 2015 month: 6 day: 7 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Cash"
          spec { units { number { exact: "1" } currency: "USD" } }
        }
        postings {
          account: "Assets:Cash"
          spec { units { number { exact: "-1" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestComment, CommentAfterTransaction) {
  ExpectParse(R"(
    2015-06-07 *
      Assets:Cash   1 USD
      Assets:Cash   -1 USD
    ; Hi
  )", R"(
    directives {
      date { year: 2015 month: 6 day: 7 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Cash"
          spec { units { number { exact: "1" } currency: "USD" } }
        }
        postings {
          account: "Assets:Cash"
          spec { units { number { exact: "-1" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestComment, CommentBetweenPostings) {
  ExpectParse(R"(
    2015-06-07 *
      Assets:Cash   1 USD
      ; Hi
      Assets:Cash   -1 USD
  )", R"(
    directives {
      date { year: 2015 month: 6 day: 7 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Cash"
          spec { units { number { exact: "1" } currency: "USD" } }
        }
        postings {
          account: "Assets:Cash"
          spec { units { number { exact: "-1" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestComment, CommentAfterPostings) {
  ExpectParse(R"(
    2015-06-07 *
      Assets:Cash   1 USD    ; Hi
      Assets:Cash   -1 USD
  )", R"(
    directives {
      date { year: 2015 month: 6 day: 7 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Cash"
          spec { units { number { exact: "1" } currency: "USD" } }
        }
        postings {
          account: "Assets:Cash"
          spec { units { number { exact: "-1" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestComment, CommentAfterTransactionStart) {
  ExpectParse(R"(
    2015-06-07 *     ; Hi
      Assets:Cash   1 USD
      Assets:Cash   -1 USD
  )", R"(
    directives {
      date { year: 2015 month: 6 day: 7 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Cash"
          spec { units { number { exact: "1" } currency: "USD" } }
        }
        postings {
          account: "Assets:Cash"
          spec { units { number { exact: "-1" } currency: "USD" } }
        }
      }
    }
  )");
}

//------------------------------------------------------------------------------
// TestPushPopTag

TEST(TestPushPopTag, TagLeftUnclosed) {
  ExpectParse(R"(
    pushtag #trip-to-nowhere
  )", R"(
    errors {
      message: "Unbalanced pushed tag: \'trip-to-nowhere\'"
    }
  )");
}

TEST(TestPushPopTag, PopInvalidTag) {
  ExpectParse(R"(
    poptag #trip-to-nowhere
  )", R"(
    errors {
      message: "Attempting to pop absent tag: \'trip-to-nowhere\'"
    }
  )");
}

//------------------------------------------------------------------------------
// TestPushPopMeta

TEST(TestPushPopMeta, PushmetaNormal) {
  ExpectParse(R"(
    pushmeta location: "Lausanne, Switzerland"

    2015-06-07 * "Something"
      Assets:Something   1 USD
      Assets:Something  -1 USD

    popmeta location:
  )", R"(
    directives {
      date { year: 2015 month: 6 day: 7 }
      meta {
        kv { key: "location" value { text: "Lausanne, Switzerland" } }
      }
      transaction {
        flag: "*"
        narration: "Something"
        postings {
          account: "Assets:Something"
          spec { units { number { exact: "1" } currency: "USD" } }
        }
        postings {
          account: "Assets:Something"
          spec { units { number { exact: "-1" } currency: "USD" } }
        }
      }
    }
  )");
}

// TODO(blais): In v2, the list of metavalues is a map and Paris overshadows
// Lausanne, they aren't both present.
TEST(TestPushPopMeta, PushmetaShadow) {
  ExpectParse(R"(
    pushmeta location: "Lausanne, Switzerland"

    2015-06-07 * "Something"
      location: "Paris, France"
      Assets:Something   1 USD
      Assets:Something  -1 USD

    popmeta location:
  )", R"(
    directives {
      date { year: 2015 month: 6 day: 7 }
      meta {
        kv { key: "location" value { text: "Lausanne, Switzerland" } }
        kv { key: "location" value { text: "Paris, France" } }
      }
      transaction {
        flag: "*"
        narration: "Something"
        postings {
          account: "Assets:Something"
          spec { units { number { exact: "1" } currency: "USD" } }
        }
        postings {
          account: "Assets:Something"
          spec { units { number { exact: "-1" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestPushPopMeta, PushmetaOverride) {
  ExpectParse(R"(
    pushmeta location: "Lausanne, Switzerland"

    2015-06-01 * "Something"
      Assets:Something   1 USD
      Assets:Something  -1 USD

    pushmeta location: "Paris, France"

    2015-06-02 * "Something"
      Assets:Something   1 USD
      Assets:Something  -1 USD

    popmeta location:
    popmeta location:
  )", R"(
    directives {
      date { year: 2015 month: 6 day: 1 }
      meta {
        kv { key: "location" value { text: "Lausanne, Switzerland" } }
      }
      transaction {
        flag: "*"
        narration: "Something"
        postings {
          account: "Assets:Something"
          spec { units { number { exact: "1" } currency: "USD" } }
        }
        postings {
          account: "Assets:Something"
          spec { units { number { exact: "-1" } currency: "USD" } }
        }
      }
    }
    directives {
      date { year: 2015 month: 6 day: 2 }
      meta {
        kv { key: "location" value { text: "Paris, France" } }
      }
      transaction {
        flag: "*"
        narration: "Something"
        postings {
          account: "Assets:Something"
          spec { units { number { exact: "1" } currency: "USD" } }
        }
        postings {
          account: "Assets:Something"
          spec { units { number { exact: "-1" } currency: "USD" } }
        }
      }
    }  )");
}

TEST(TestPushPopMeta, PushmetaInvalidPop) {
  ExpectParse(R"(
    popmeta location:
  )", R"(
    errors {
      message: "Attempting to pop absent metadata key: \'location\'"
    }
  )");
}

TEST(TestPushPopMeta, PushmetaForgotten) {
  ExpectParse(R"(
    pushmeta location: "Lausanne, Switzerland"
  )", R"(
    errors {
      message: "Unbalanced metadata key \'location\' has leftover metadata"
    }
  )");
}



//------------------------------------------------------------------------------
// TestMultipleLines

TEST(TestMultipleLines, MultilineNarration) {
  ExpectParse(R"(
    2014-07-11 * "Hello one line
    and yet another,
    and why not another!"
      Expenses:Restaurant         100 USD
      Assets:Cash                -100 USD
  )", R"(
    directives {
      date { year: 2014 month: 7 day: 11 }
      transaction {
        flag: "*"
        narration: "Hello one line\nand yet another,\nand why not another!"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:Cash"
          spec { units { number { exact: "-100" } currency: "USD" } }
        }
      }
    }
  )");
}

//------------------------------------------------------------------------------
// TestSyntaxErrors
//
// Test syntax errors that occur within the parser. One of our goals is to
// recover and report without ever bailing out with an exception.

TEST(TestSyntaxErrors, SingleErrorTokenAtTopLevel) {
  // Note: This is a single invalid token.
  ExpectParse(R"(
    Error:*:Token
  )", R"(
    errors {
      message: "Syntax error, unexpected invalid token"
    }
  )");
}

TEST(TestSyntaxErrors, ErrorInTransactionLine) {
  ExpectParse(R"(
    2013-05-01 open Assets:US:Cash

    2013-05-02 * "Dinner" A:*:B
      Expenses:Restaurant               100 USD
      Assets:US:Cash                   -100 USD

    2013-05-03 balance Assets:US:Cash  -100 USD
  )", R"(
     directives {
       date { year: 2013 month: 5 day: 1 }
       open { account: "Assets:US:Cash" }
     }
     directives {
       date { year: 2013 month: 5 day: 03 }
       balance {
         account: "Assets:US:Cash"
         amount { number { exact: "-100" } currency: "USD" }
       }
     }
     errors {
       message: "Syntax error, unexpected CURRENCY, expecting EOL or TAG or LINK"
       location { lineno: 3 }
     }
  )", { .leave_lineno = true });
}

TEST(TestSyntaxErrors, ErrorInPosting) {
  ExpectParse(R"(
    2013-05-01 open Assets:US:Cash

    2013-05-02 * "Dinner"
      Expenses:Resta(urant              100 USD
      Assets:US:Cash                   -100 USD

    2013-05-03 balance Assets:US:Cash  -100 USD
  )", R"(
     directives {
       date { year: 2013 month: 5 day: 1 }
       open { account: "Assets:US:Cash" }
     }
     directives {
       date { year: 2013 month: 5 day: 03 }
       balance {
         account: "Assets:US:Cash"
         amount { number { exact: "-100" } currency: "USD" }
       }
     }
     errors {
       message: "Syntax error, unexpected invalid token, expecting PLUS or MINUS or LPAREN or NUMBER"
       location { lineno: 4 }
     }
  )", { .leave_lineno = true });
}

TEST(TestSyntaxErrors, NoFinalNewline) {
  // Note: We would have to explicitly have to simulate and dedents here for
  // this to work and this is surprisingly tricky and error prone. See
  // {fab24459d79d} Prefer not to handle.
  ExpectParse(absl::StrCat(
    "2014-11-02 *\n",
    "  Assets:Something   1 USD\n",
    "  Assets:Other      -1 USD"),
  R"(
    errors {
      message: "Syntax error, unexpected DEDENT, expecting EOL or ATAT or AT"
    }
  )", { .no_dedent = true });
}

// TODO(blais): Test all the other error cases.

//------------------------------------------------------------------------------
// TestParserOptions

TEST(TestParserOptions, OptionSingleValue) {
  ExpectParse(R"(
    option "title" "Super Rich"
  )", R"(
    options {
      title: "Super Rich"
    }
  )");
}

TEST(TestParserOptions, OptionListValue) {
  ExpectParse(R"(
    option "documents" "/path/docs/a"
    option "documents" "/path/docs/b"
    option "documents" "/path/docs/c"
  )", R"(
    options {
      documents: "/path/docs/a"
      documents: "/path/docs/b"
      documents: "/path/docs/c"
    }
  )");
}

TEST(TestParserOptions, ToleranceMapValue) {
  ExpectParse(R"(
    option "inferred_tolerance_default" "*:0"
    option "inferred_tolerance_default" "USD:0.05"
    option "inferred_tolerance_default" "JPY:0.5"
  )", R"(
    options {
      inferred_tolerance_default { key: "*" value: "0" }
      inferred_tolerance_default { key: "JPY" value: "0.5" }
      inferred_tolerance_default { key: "USD" value: "0.05" }
    }
  )");
}

TEST(TestParserOptions, InvalidOption) {
  ExpectParse(R"(
    option "bladibla_invalid" "Some value"
  )", R"(
    errors {
      message: "Invalid option: \'bladibla_invalid\'"
    }
  )");
}

TEST(TestParserOptions, ReadonlyOption) {
  ExpectParse(R"(
    option "filename" "gniagniagniagniagnia"
  )", R"(
    errors {
      message: "Invalid option: \'filename\'"
    }
  )");
}

TEST(TestParserOptions, UnaryOption) {
  ExpectParse(R"(
    options "inferred_tolerance_default { key: 'USD' value: '0.00001' }"
    options "account_types {
      assets: 'Actifs'
      liabilities: 'Passifs'
    }"
  )", R"(
    options {
      account_types {
        assets: 'Actifs'
        liabilities: 'Passifs'
      }
      inferred_tolerance_default { key: 'USD' value: '0.00001' }
    }
  )");
}

TEST(TestParserOptions, LegacyTranslations) {
  ExpectParse(R"(
    option "name_assets" "Actifs"
    option "name_liabilities" "Passifs"
  )", R"(
    options {
      account_types {
        assets: 'Actifs'
        liabilities: 'Passifs'
      }
    }
  )");
}

TEST(TestParserOptions, InvalidAccountNames) {
  ExpectParse(R"(
    option "name_assets" "Actifs"
    option "name_liabilities" "Passifs"
    2021-09-19 open Assets:US:Checking
  )", R"(
    directives {
      date { year: 2021 month: 9 day: 19 }
      open { account: "Assets:US:Checking" }
    }
    errors {
      message: "Invalid account name: \'Assets:US:Checking\'"
    }
    options {
      account_types {
        assets: 'Actifs'
        liabilities: 'Passifs'
      }
    }
  )");
}

//------------------------------------------------------------------------------
// TestParserInclude

TEST(TestParserInclude, ParseNonExisting) {
  auto ledger = parser::ParseFile("/some/bullshit/filename.beancount");
  auto ledger_proto = LedgerToProto(*ledger);
  for (auto& error : *ledger_proto->mutable_errors()) {
    error.clear_location();
  }
  EXPECT_TRUE(EqualsMessages(*ledger_proto, R"(
    errors {
      message: "An IO error has occurred."
    }
  )", false));
}

TEST(TestParserInclude, IncludeAbsolute) {
  ExpectParse(R"(
    include "/some/absolute/filename.beancount"
  )", R"(
    info {
      include: "/some/absolute/filename.beancount"
    }
  )");
}

TEST(TestParserInclude, IncludeRelativeFromString) {
  ExpectParse(R"(
    include "some/relative/filename.beancount"
  )", R"(
    info {
      include: "some/relative/filename.beancount"
    }
  )");
}

//------------------------------------------------------------------------------
// TestParserPlugin

TEST(TestParserPlugin, Plugin) {
  ExpectParse(R"(
    plugin "beancount.plugin.unrealized"
  )", R"(
    info {
      plugin {
        name: "beancount.plugin.unrealized"
      }
    }
  )");
}

TEST(TestParserPlugin, PluginWithConfig) {
  ExpectParse(R"(
    plugin "beancount.plugin.unrealized" "Unrealized"
  )", R"(
    info {
      plugin {
        name: "beancount.plugin.unrealized"
        config: "Unrealized"
      }
    }
  )");
}

TEST(TestParserPlugin, PluginAsOption) {
  ExpectParse(R"(
    option "plugin" "beancount.plugin.unrealized"
  )", R"(
    errors {
      message: "Invalid option: \'plugin\'"
    }
  )");
}

//------------------------------------------------------------------------------
// TestDisplayContextOptions

TEST(TestDisplayContextOptions, RenderCommasNo) {
  ExpectParse(R"(
    option "render_commas" "0"
  )", R"(
    options {
      render_commas: false
    }
  )");
}

TEST(TestDisplayContextOptions, RenderCommasYes) {
  ExpectParse(R"(
    option "render_commas" "1"
  )", R"(
    options {
      render_commas: true
    }
  )");
}

TEST(TestDisplayContextOptions, RenderCommasYes2) {
  ExpectParse(R"(
    option "render_commas" "true"
  )", R"(
    options {
      render_commas: true
    }
  )");
}

TEST(TestDisplayContextOptions, RenderCommasError) {
  ExpectParse(R"(
    option "render_commas" "TRUE"
  )", R"(
    errors {
      message: "Could not parse and set option 'render_commas' with value 'TRUE'; ignored."
    }
  )");
}

//------------------------------------------------------------------------------
// TestMiscOptions

TEST(TestMiscOptions, PluginProcessingModeDefault) {
  ExpectParse(R"(
    option "plugin_processing_mode" "DEFAULT"
  )", R"(
    options {
      plugin_processing_mode: DEFAULT
    }
  )");
}

TEST(TestMiscOptions, PluginProcessingModeRaw) {
  ExpectParse(R"(
    option "plugin_processing_mode" "RAW"
  )", R"(
    options {
      plugin_processing_mode: RAW
    }
  )");
}

TEST(TestMiscOptions, PluginProcessingModeInvalid) {
  ExpectParse(R"(
    option "plugin_processing_mode" "INVALID"
  )", R"(
    errors {
      message: "Could not parse and set option 'plugin_processing_mode' with value 'INVALID'; ignored."
    }
  )");
}

//------------------------------------------------------------------------------
// TestDeprecatedOptions

TEST(TestDeprecatedOptions, DeprecatedPlugin) {
  ExpectParse(R"(
    option "plugin" "beancount.plugins.module_name"
  )", R"(
    errors {
      message: "Invalid option: 'plugin'"
    }
  )");
}

TEST(TestDeprecatedOptions, DeprecatedOption) {
  ExpectParse(R"(
    option "allow_pipe_separator" "TRUE"
  )", R"(
    errors {
      message: "Invalid option: 'allow_pipe_separator'"
    }
  )");
}

//------------------------------------------------------------------------------
// TestParserLinks

TEST(TestParserLinks, ParseLinks) {
  ExpectParse(R"(
    2013-05-18 * "Something something" ^38784734873
      Expenses:Restaurant         100 USD
      Assets:US:Cash             -100 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      links: "38784734873"
      transaction {
        flag: "*"
        narration: "Something something"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:Cash"
          spec { units { number { exact: "-100" } currency: "USD" } }
        }
      }
    }
  )");
}

//------------------------------------------------------------------------------
// TestTransactions

TEST(TestTransactions, Simple1) {
  ExpectParse(R"(
    2013-05-18 * "Nice dinner at Mermaid Inn"
      Expenses:Restaurant         100 USD
      Assets:US:Cash             -100 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Nice dinner at Mermaid Inn"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:Cash"
          spec { units { number { exact: "-100" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestTransactions, Simple2) {
  ExpectParse(R"(
    2013-05-18 * "Nice dinner at Mermaid Inn"
      Expenses:Restaurant         100 USD
      Assets:US:Cash             -100 USD

    2013-05-20 * "Duane Reade" "Toothbrush"
      Expenses:BathroomSupplies         4 USD
      Assets:US:BestBank:Checking      -4 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Nice dinner at Mermaid Inn"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:Cash"
          spec { units { number { exact: "-100" } currency: "USD" } }
        }
      }
    }
    directives {
      date { year: 2013 month: 5 day: 20 }
      transaction {
        flag: "*"
        payee: "Duane Reade"
        narration: "Toothbrush"
        postings {
          account: "Expenses:BathroomSupplies"
          spec { units { number { exact: "4" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:BestBank:Checking"
          spec { units { number { exact: "-4" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestTransactions, EmptyNarration) {
  ExpectParse(R"(
    2013-05-18 * ""
      Expenses:Restaurant         100 USD
      Assets:US:Cash             -100 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:Cash"
          spec { units { number { exact: "-100" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestTransactions, NoNarration) {
  ExpectParse(R"(
    2013-05-18 *
      Expenses:Restaurant         100 USD
      Assets:US:Cash             -100 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:Cash"
          spec { units { number { exact: "-100" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestTransactions, PayeeNoNarration) {
  ExpectParse(R"(
    2013-05-18 * "Mermaid Inn"
      Expenses:Restaurant         100 USD
      Assets:US:Cash             -100 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Mermaid Inn"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:Cash"
          spec { units { number { exact: "-100" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestTransactions, TooManyStrings) {
  ExpectParse(R"(
    2013-05-18 * "A" "B" "C"
      Expenses:Restaurant         100 USD
      Assets:US:Cash             -100 USD
  )", R"(
    errors {
      message: "Syntax error, unexpected STRING, expecting EOL or TAG or LINK"
    }
  )");
}

TEST(TestTransactions, LinkAndThenTag) {
  ExpectParse(R"(
    2014-04-20 * "Money from CC" ^610fa7f17e7a #trip
      Expenses:Restaurant         100 USD
      Assets:US:Cash             -100 USD
  )", R"(
    directives {
      date { year: 2014 month: 4 day: 20 }
      tags: "trip"
      links: "610fa7f17e7a"
      transaction {
        flag: "*"
        narration: "Money from CC"
        postings {
          account: "Expenses:Restaurant"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:US:Cash"
          spec { units { number { exact: "-100" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestTransactions, TagThenLink) {
  ExpectParse(R"(
    2014-04-20 * #trip "Money from CC" ^610fa7f17e7a
      Expenses:Restaurant         100 USD
      Assets:US:Cash             -100 USD
  )", R"(
    errors {
      message: "Syntax error, unexpected STRING, expecting EOL or TAG or LINK"
    }
  )");
}

TEST(TestTransactions, ZeroPrices) {
  ExpectParse(R"(
    2014-04-20 * "Like a conversion entry"
      Equity:Conversions         100 USD @ 0 XFER
      Equity:Conversions         101 CAD @ 0 XFER
      Equity:Conversions         102 AUD @ 0 XFER
  )", R"(
    directives {
      date { year: 2014 month: 4 day: 20 }
      transaction {
        flag: "*"
        narration: "Like a conversion entry"
        postings {
          account: "Equity:Conversions"
          spec { units { number { exact: "100" } currency: "USD" }
                 price { number { exact: "0" } currency: "XFER" } }
        }
        postings {
          account: "Equity:Conversions"
          spec { units { number { exact: "101" } currency: "CAD" }
                 price { number { exact: "0" } currency: "XFER" } }
        }
        postings {
          account: "Equity:Conversions"
          spec { units { number { exact: "102" } currency: "AUD" }
                 price { number { exact: "0" } currency: "XFER" } }
        }
      }
    }
  )");
}

TEST(TestTransactions, ZeroUnits) {
  // Note: Zero amount is caught only at booking time.
  ExpectParse(R"(
    2014-04-20 * "Zero number of units"
      Assets:Investment         0 HOOL {500.00 USD}
      Assets:Cash               0 USD
  )", R"(
    directives {
      date { year: 2014 month: 4 day: 20 }
      transaction {
        flag: "*"
        narration: "Zero number of units"
        postings {
          account: "Assets:Investment"
          spec { units { number { exact: "0" } currency: "HOOL" }
                 cost { per_unit { number { exact: "500.00" } } currency: "USD" } }
        }
        postings {
          account: "Assets:Cash"
          spec { units { number { exact: "0" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestTransactions, ZeroCosts) {
  ExpectParse(R"(
    2014-04-20 * "Like a conversion entry"
      Assets:Investment         10 HOOL {0 USD}
      Assets:Cash                0 USD
  )", R"(
    directives {
      date { year: 2014 month: 4 day: 20 }
      transaction {
        flag: "*"
        narration: "Like a conversion entry"
        postings {
          account: "Assets:Investment"
          spec { units { number { exact: "10" } currency: "HOOL" }
                 cost { per_unit { number { exact: "0" } } currency: "USD" } }
        }
        postings {
          account: "Assets:Cash"
          spec { units { number { exact: "0" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestTransactions, Imbalance) {
  ExpectParse(R"(
    2014-04-20 * "Busted!"
      Assets:Checking         100 USD
      Assets:Checking         -99 USD
  )", R"(
    directives {
      date { year: 2014 month: 4 day: 20 }
      transaction {
        flag: "*"
        narration: "Busted!"
        postings {
          account: "Assets:Checking"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:Checking"
          spec { units { number { exact: "-99" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestTransactions, NoPostings) {
  ExpectParse(R"(
    2014-07-17 * "(JRN) INTRA-ACCOUNT TRANSFER" ^795422780
  )", R"(
    directives {
      date { year: 2014 month: 7 day: 17 }
      links: "795422780"
      transaction {
        flag: "*"
        narration: "(JRN) INTRA-ACCOUNT TRANSFER"
      }
    }
  )");
}

TEST(TestTransactions, BlankLineNotAllowed) {
  ExpectParse(R"(
    2014-04-20 * "Busted!"
      Assets:Checking         100 USD

      Assets:Checking         -99 USD
  )", R"(
    directives {
      date { year: 2014 month: 4 day: 20 }
      transaction {
        flag: "*"
        narration: "Busted!"
        postings {
          account: "Assets:Checking"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
      }
    }
    errors {
      message: "Syntax error, unexpected INDENT"
    }
  )");
}

TEST(TestTransactions, BlankLineWithSpacesNotAllowed) {
  // Note: This resulted in an error in v2. The difference is due to how
  // processing of indents is improved in v3.
  ExpectParse(absl::StrCat(
            "2014-04-20 * \"Busted!\"\n",
            "  Assets:Checking         100 USD\n",
            "  \n",  // This is not allowed.
            "  Assets:Checking         -99 USD\n"),
  R"(
    directives {
      date { year: 2014 month: 4 day: 20 }
      transaction {
        flag: "*"
        narration: "Busted!"
        postings {
          account: "Assets:Checking"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:Checking"
          spec { units { number { exact: "-99" } currency: "USD" } }
        }
      }
    }
  )", { .no_dedent = true });
}

TEST(TestTransactions, TagsAfterFirstLine) {
  ExpectParse(R"(
    2014-04-20 * "Links and tags on subsequent lines" #basetag ^baselink
      #tag1
      ^link1
      #tag2
      ^link2
      Assets:Checking         100 USD
      Assets:Checking         -99 USD
  )", R"(
    directives {
      date { year: 2014 month: 4 day: 20 }
      meta {
        kv { value { tag: "tag1" } }
        kv { value { link: "link1" } }
        kv { value { tag: "tag2" } }
        kv { value { link: "link2" } }
      }
      tags: "basetag"
      links: "baselink"
      transaction {
        flag: "*"
        narration: "Links and tags on subsequent lines"
        postings {
          account: "Assets:Checking"
          spec { units { number { exact: "100" } currency: "USD" } }
        }
        postings {
          account: "Assets:Checking"
          spec { units { number { exact: "-99" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestTransactions, MultipleTagsLinksOnMetadataLine) {
  ExpectParse(R"(
    2014-04-20 * "Links and tags on subsequent lines" #basetag ^baselink
      #tag1 #tag2 ^link1
      Assets:Checking         100 USD
      Assets:Checking         -99 USD
  )", R"(
    errors {
      message: "Syntax error, unexpected TAG, expecting EOL"
    }
  )");
}

TEST(TestTransactions, TagsAfterFirstPosting) {
  ExpectParse(R"(
    2014-04-20 * "Links and tags on subsequent lines" #basetag ^baselink
      Assets:Checking         100 USD
      #tag1
      Assets:Checking         -99 USD
  )", R"(
    errors {
      message: "Syntax error, unexpected TAG, expecting ACCOUNT"
    }
  )");
}

//------------------------------------------------------------------------------
// TestParseLots

TEST(TestParseLots, CostNone) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL   45.23 USD
      Assets:Invest:Cash  -45.23 USD
  )", R"(
    directives {
      date { year: 2014 month: 1 day: 1 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "45.23" } currency: "USD" } }
        }
        postings {
          account: "Assets:Invest:Cash"
          spec { units { number { exact: "-45.23" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestParseLots, CostEmpty) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL   20 AAPL {}
      Assets:Invest:Cash  -20 AAPL
  )", R"(
    directives {
      date { year: 2014 month: 1 day: 1 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "20" } currency: "AAPL" }
                 cost { } }
        }
        postings {
          account: "Assets:Invest:Cash"
          spec { units { number { exact: "-20" } currency: "AAPL" } }
        }
      }
    }
  )");
}

TEST(TestParseLots, CostAmount) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL      20 AAPL {45.23 USD}
      Assets:Invest:Cash  -90.46 USD
  )", R"(
    directives {
      date { year: 2014 month: 1 day: 1 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "20" } currency: "AAPL" }
                 cost { per_unit { number { exact: "45.23" } } currency: "USD" } }
        }
        postings {
          account: "Assets:Invest:Cash"
          spec { units { number { exact: "-90.46" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestParseLots, CostDate) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL   20 AAPL {2014-12-26}
      Assets:Invest:Cash  -20 AAPL
  )", R"(
    directives {
      date { year: 2014 month: 1 day: 1 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "20" } currency: "AAPL" }
                 cost { date { year: 2014 month: 12 day: 26 } } }
        }
        postings {
          account: "Assets:Invest:Cash"
          spec { units { number { exact: "-20" } currency: "AAPL" } }
        }
      }
    }
  )");
}

TEST(TestParseLots, CostLabel) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL   20 AAPL {"d82d55a0dbe8"}
      Assets:Invest:Cash  -20 AAPL
  )", R"(
    directives {
      date { year: 2014 month: 1 day: 1 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "20" } currency: "AAPL" }
                 cost { label: "d82d55a0dbe8" } }
        }
        postings {
          account: "Assets:Invest:Cash"
          spec { units { number { exact: "-20" } currency: "AAPL" } }
        }
      }
    }
  )");
}

TEST(TestParseLots, CostMerge) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL   20 AAPL {*}
      Assets:Invest:Cash  -20 AAPL
  )", R"(
    directives {
      date { year: 2014 month: 1 day: 1 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "20" } currency: "AAPL" }
                 cost { merge_cost: true } }
        }
        postings {
          account: "Assets:Invest:Cash"
          spec { units { number { exact: "-20" } currency: "AAPL" } }
        }
      }
    }
  )");
}

TEST(TestParseLots, CostTwoComponents) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL    1 AAPL {45.23 USD, 2014-12-26}
      Assets:Invest:AAPL    1 AAPL {2014-12-26, 45.23 USD}
      Assets:Invest:AAPL    1 AAPL {45.23 USD, "d82d55a0dbe8"}
      Assets:Invest:AAPL    1 AAPL {"d82d55a0dbe8", 45.23 USD}
      Assets:Invest:AAPL    1 AAPL {2014-12-26, "d82d55a0dbe8"}
      Assets:Invest:AAPL    1 AAPL {"d82d55a0dbe8", 2014-12-26}
  )", R"(
    directives {
      date { year: 2014 month: 1 day: 1 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "1" } currency: "AAPL" }
                 cost {
            per_unit { number { exact: "45.23" } }
            currency: "USD"
            date { year: 2014 month: 12 day: 26 }
          } }
        }
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "1" } currency: "AAPL" }
                 cost {
            per_unit { number { exact: "45.23" } }
            currency: "USD"
            date { year: 2014 month: 12 day: 26 }
          } }
        }
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "1" } currency: "AAPL" }
                 cost { per_unit { number { exact: "45.23" } } currency: "USD"
                        label: "d82d55a0dbe8" } }
        }
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "1" } currency: "AAPL" }
                 cost { per_unit { number { exact: "45.23" } } currency: "USD"
                        label: "d82d55a0dbe8" } }
        }
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "1" } currency: "AAPL" }
                 cost { date { year: 2014 month: 12 day: 26 } label: "d82d55a0dbe8" } }
        }
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "1" } currency: "AAPL" }
                 cost { date { year: 2014 month: 12 day: 26 } label: "d82d55a0dbe8" } }
        }
      }
    }
  )");
}

TEST(TestParseLots, CostThreeComponents) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL    1 AAPL {45.23 USD, 2014-12-26, "d82d55a0dbe8"}
      Assets:Invest:AAPL    1 AAPL {2014-12-26, 45.23 USD, "d82d55a0dbe8"}
      Assets:Invest:AAPL    1 AAPL {45.23 USD, "d82d55a0dbe8", 2014-12-26}
      Assets:Invest:AAPL    1 AAPL {2014-12-26, "d82d55a0dbe8", 45.23 USD}
      Assets:Invest:AAPL    1 AAPL {"d82d55a0dbe8", 45.23 USD, 2014-12-26}
      Assets:Invest:AAPL    1 AAPL {"d82d55a0dbe8", 2014-12-26, 45.23 USD}
  )", R"(
    directives {
      date { year: 2014 month: 1 day: 1 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "1" } currency: "AAPL" }
                 cost {
            per_unit { number { exact: "45.23" } }
            currency: "USD"
            date { year: 2014 month: 12 day: 26 }
            label: "d82d55a0dbe8"
          } }
        }
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "1" } currency: "AAPL" }
                 cost {
            per_unit { number { exact: "45.23" } }
            currency: "USD"
            date { year: 2014 month: 12 day: 26 }
            label: "d82d55a0dbe8"
          } }
        }
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "1" } currency: "AAPL" }
                 cost {
            per_unit { number { exact: "45.23" } }
            currency: "USD"
            date { year: 2014 month: 12 day: 26 }
            label: "d82d55a0dbe8"
          } }
        }
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "1" } currency: "AAPL" }
                 cost {
            per_unit { number { exact: "45.23" } }
            currency: "USD"
            date { year: 2014 month: 12 day: 26 }
            label: "d82d55a0dbe8"
          } }
        }
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "1" } currency: "AAPL" }
                 cost {
            per_unit { number { exact: "45.23" } }
            currency: "USD"
            date { year: 2014 month: 12 day: 26 }
            label: "d82d55a0dbe8"
          } }
        }
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "1" } currency: "AAPL" }
                 cost {
            per_unit { number { exact: "45.23" } }
            currency: "USD"
            date { year: 2014 month: 12 day: 26 }
            label: "d82d55a0dbe8"
          } }
        }
      }
    }
  )");
}

TEST(TestParseLots, CostRepeated) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL       1 AAPL {45.23 USD, 45.24 USD}
      Assets:Invest:Cash  -45.23 USD
  )", R"(
    errors {
      message: "Duplicate `per_unit` cost spec field."
    }
  )");
}

TEST(TestParseLots, CostRepeatedDate) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL    1 AAPL {45.23 USD, 2014-12-26, 2014-12-27}
      Assets:Invest:Cash   -1 AAPL
  )", R"(
    errors {
      message: "Duplicate `date` cost spec field."
    }
  )");
}

TEST(TestParseLots, CostRepeatedLabel) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL       1 AAPL {"aaa", "bbb", 45.23 USD}
      Assets:Invest:Cash  -45.23 USD
  )", R"(
    errors {
      message: "Duplicate `label` cost spec field."
    }
  )");
}

TEST(TestParseLots, CostRepeatedMerge) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL       1 AAPL {*, *}
      Assets:Invest:Cash  -45.23 USD
  )", R"(
    errors {
      message: "Duplicate `merge_cost` cost spec field."
    }
  )");
}

TEST(TestParseLots, CostBothCosts) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL       10 AAPL {45.23 # 9.95 USD}
      Assets:Invest:Cash  -110.36 USD
  )", R"(
    directives {
      date { year: 2014 month: 1 day: 1 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "10" } currency: "AAPL" }
                 cost {
            per_unit { number { exact: "45.23" } }
            total { number { exact: "9.95" } }
            currency: "USD"
          } }
        }
        postings {
          account: "Assets:Invest:Cash"
          spec { units { number { exact: "-110.36" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestParseLots, CostTotalCostOnly) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL      10 AAPL {# 9.95 USD}
      Assets:Invest:Cash  -19.90 USD
  )", R"(
    directives {
      date { year: 2014 month: 1 day: 1 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Invest:AAPL"
          spec { units { number { exact: "10" } currency: "AAPL" }
                 cost {
            total { number { exact: "9.95" } }
            currency: "USD"
          } }
        }
        postings {
          account: "Assets:Invest:Cash"
          spec { units { number { exact: "-19.90" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestParseLots, CostEmptyComponents) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL      10 AAPL {, 100.0 USD, , }
      Assets:Invest:Cash  -19.90 USD
  )", R"(
    errors {
      message: "Syntax error, unexpected COMMA, expecting HASH"
    }
  )");
}

TEST(TestParseLots, CostTotalEmptyTotal) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL      20 AAPL {45.23 # USD}
      Assets:Invest:Cash  -45.23 USD
  )", R"(
    directives {
      date { year: 2014 month: 1 day: 1 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Invest:AAPL"
          spec {
            units { number { exact: "20" } currency: "AAPL" }
            cost { per_unit { number { exact: "45.23" } } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Invest:Cash"
          spec {
            units { number { exact: "-45.23" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestParseLots, CostTotalJustCurrency) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL   20 AAPL {USD}
      Assets:Invest:AAPL   20 AAPL { # USD}
      Assets:Invest:Cash    0 USD
  )", R"(
    directives {
      date { year: 2014 month: 1 day: 1 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Invest:AAPL"
          spec {
            units { number { exact: "20" } currency: "AAPL" }
            cost { currency: "USD" }
          }
        }
        postings {
          account: "Assets:Invest:AAPL"
          spec {
            units { number { exact: "20" } currency: "AAPL" }
            cost { currency: "USD" }
          }
        }
        postings {
          account: "Assets:Invest:Cash"
          spec {
            units { number { exact: "0" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestParseLots, CostWithSlashes) {
  ExpectParse(R"(
    2014-01-01 *
      Assets:Invest:AAPL      1.1 AAPL {45.23 USD / 2015-07-16 / "blabla"}
      Assets:Invest:Cash   -45.23 USD
  )", R"(
    errors {
      message: "Syntax error, unexpected SLASH, expecting RCURL or COMMA"
    }
  )");
}

//------------------------------------------------------------------------------
// TestCurrencies

TEST(TestCurrencies, ParseCurrencies) {
  ExpectParse(R"(
    2014-01-19 open Assets:Underscore    DJ_EURO
    2014-01-19 open Assets:Period        DJ.EURO
    2014-01-19 open Assets:Apostrophe    DJ'EURO
    2014-01-19 open Assets:Numbers       EURO123
    2014-01-19 open Assets:Futures       /LOX21_211204_P100.25
  )", R"(
    directives { date { year: 2014 month: 1 day: 19 }
      open { account: "Assets:Underscore" currencies: "DJ_EURO" } }
    directives { date { year: 2014 month: 1 day: 19 }
      open { account: "Assets:Period" currencies: "DJ.EURO" } }
    directives { date { year: 2014 month: 1 day: 19 }
      open { account: "Assets:Apostrophe" currencies: "DJ\'EURO" } }
    directives { date { year: 2014 month: 1 day: 19 }
      open { account: "Assets:Numbers" currencies: "EURO123" } }
    directives { date { year: 2014 month: 1 day: 19 }
      open { account: "Assets:Futures" currencies: "/LOX21_211204_P100.25" } }
  )");
}

TEST(TestCurrencies, DifferentCostAndPriceCurrency) {
  ExpectParse(R"(
    2018-03-21 * "Convert MR to KrisFlyer"
      Assets:Test                -100 MR {0.0075 USD} @ 1 KRISFLYER
      Assets:Krisflyer            100 KRISFLYER
  )", R"(
    directives {
      # ..
    }
    errors {
      message: "Cost and price currencies must match: USD != KRISFLYER"
    }
  )", { .partial = true });
}

//------------------------------------------------------------------------------
// TestTotalsAndSigns

TEST(TestTotalsAndSigns, ZeroAmount) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT      0 MSFT {200.00 USD}
      Assets:Investments:Cash      0 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Investments:MSFT"
          spec {
            units { number { exact: "0" } currency: "MSFT" }
            cost { per_unit { number { exact: "200.00" } } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Investments:Cash"
          spec {
            units { number { exact: "0" } currency: "USD" }
          }
        }
      }
    }
  )");
  // Should produce no errors.
}

TEST(TestTotalsAndSigns, ZeroCost) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT      -10 MSFT {0.00 USD}
      Assets:Investments:Cash     0.00 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Investments:MSFT"
          spec {
            units { number { exact: "-10" } currency: "MSFT" }
            cost { per_unit { number { exact: "0.00" } } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Investments:Cash"
          spec {
            units { number { exact: "0.00" } currency: "USD" }
          }
        }
      }
    }
  )");
  // Should produce no errors.
}

TEST(TestTotalsAndSigns, CostNegative) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT      -10 MSFT {-200.00 USD}
      Assets:Investments:Cash  2000.00 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Investments:MSFT"
          spec {
            units { number { exact: "-10" } currency: "MSFT" }
            cost { per_unit { number { exact: "-200.00" } } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Investments:Cash"
          spec {
            units { number { exact: "2000.00" } currency: "USD" }
          }
        }
      }
    }
  )");
  // # Should produce no errors.
  // Note: This error is caught only at booking time.
}

TEST(TestTotalsAndSigns, TotalCost) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT      10 MSFT {{2,000 USD}}
      Assets:Investments:Cash  -20000 USD

    2013-05-18 * ""
      Assets:Investments:MSFT      10 MSFT {{2000 USD, 2014-02-25}}
      Assets:Investments:Cash  -20000 USD

    2013-06-01 * ""
      Assets:Investments:MSFT      -10 MSFT {{2,000 USD}}
      Assets:Investments:Cash    20000 USD
  )", R"(
    directives {
      transaction {
        postings {
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            cost { total { number { exact: "2000" } } currency: "USD" }
          }
        }
        postings {
          spec { units { number { exact: "-20000" } currency: "USD" } }
        }
      }
    }
    directives {
      transaction {
        postings {
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            cost {
              total { number { exact: "2000" } } currency: "USD"
              date { year: 2014 month: 2 day: 25 }
            }
          }
        }
        postings {
          spec {
            units { number { exact: "-20000" } currency: "USD" }
          }
        }
      }
    }
    directives {
      transaction {
        postings {
          spec {
            units { number { exact: "-10" } currency: "MSFT" }
            cost { total { number { exact: "2000" } } currency: "USD" }
          }
        }
        postings {
          spec {
            units { number { exact: "20000" } currency: "USD" }
          }
        }
      }
    }
  )", { .partial = true });
}

TEST(TestTotalsAndSigns, TotalCostInvalid) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT      10 MSFT {{100 # 2,000 USD}}
      Assets:Investments:Cash  -20000 USD
  )", R"(
    directives {
      # ...
      transaction {
        postings {
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            cost { total { number { exact: "2000" } } currency: "USD" }
          }
        }
        postings {
          spec { units { number { exact: "-20000" } currency: "USD" } }
        }
      }
    }
    errors {
      message: "Per-unit cost may not be specified using total cost syntax: \'per_unit {\n  number {\n    exact: \"100\"\n  }\n}\ntotal {\n  number {\n    exact: \"2000\"\n  }\n}\ncurrency: \"USD\"\n\'; ignoring per-unit cost"
    }
  )", { .partial = true });
}

TEST(TestTotalsAndSigns, TotalCostNegative) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT      -10 MSFT {{-200.00 USD}}
      Assets:Investments:Cash   200.00 USD
  )", R"(
    directives {
      transaction {
        postings {
          spec {
            units { number { exact: "-10" } currency: "MSFT" }
            cost { total { number { exact: "-200.00" } } currency: "USD" }
          }
        }
        postings {
          # ...
        }
      }
    }
  )", { .partial = true });
  // Should produce no errors.
  // Note: This error is caught only at booking time.
}

TEST(TestTotalsAndSigns, PriceNegative) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT      -10 MSFT @ -200.00 USD
      Assets:Investments:Cash  2000.00 USD
  )", R"(
    directives {
      transaction {
        postings {
          spec {
            units { number { exact: "-10" } currency: "MSFT" }
            price { number { exact: "200.00" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Investments:Cash"
          spec {
            units { number { exact: "2000.00" } currency: "USD" }
          }
        }
      }
    }
    errors {
      message: 'Negative prices are not allowed (see http://furius.ca/beancount/doc/bug-negative-prices for workaround)'
    }
  )", { .partial = true });
}

TEST(TestTotalsAndSigns, TotalPricePositive) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT        10 MSFT @@ 2000.00 USD
      Assets:Investments:Cash  -2000.00 USD
  )", R"(
    directives {
      transaction {
        postings {
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            price { number { exact: "2000.00" } currency: "USD" is_total: true }
          }
        }
        postings {
          # ...
        }
      }
    }
  )", { .partial = true });
}

TEST(TestTotalsAndSigns, TotalPriceNegative) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT       -10 MSFT @@ 2000.00 USD
      Assets:Investments:Cash  20000.00 USD
  )", R"(
    directives {
      transaction {
        postings {
          spec {
            units { number { exact: "-10" } currency: "MSFT" }
            price { number { exact: "2000.00" } currency: "USD" is_total: true }
          }
        }
        postings {
          # ...
        }
      }
    }
  )", { .partial = true });
}

TEST(TestTotalsAndSigns, TotalPriceInverted) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT         10 MSFT @@ -2000.00 USD
      Assets:Investments:Cash   20000.00 USD
  )", R"(
    directives {
      transaction {
        postings {
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            price { number { exact: "2000.00" } currency: "USD" is_total: true }
          }
        }
        postings {
          # ...
        }
      }
    }
    errors {
      message: 'Negative prices are not allowed (see http://furius.ca/beancount/doc/bug-negative-prices for workaround)'
    }
  )", { .partial = true });
}

TEST(TestTotalsAndSigns, TotalPriceWithMissing) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT            MSFT @@ 2000.00 USD
      Assets:Investments:Cash   20000.00 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Investments:MSFT"
          spec {
            units { currency: "MSFT" }
            price { number { exact: "2000.00" } currency: "USD" is_total: true }
          }
        }
        postings {
          account: "Assets:Investments:Cash"
          spec { units { number { exact: "20000.00" } currency: "USD" } }
        }
      }
    }
    errors {
      message: "Total price on a posting without units: number {\n  exact: \"2000.00\"\n}\ncurrency: \"USD\"\nis_total: true\n."
    }
  )");
}

//------------------------------------------------------------------------------
// TestBalance

TEST(TestBalance, TotalPrice) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT      10 MSFT @@ 2000 USD
      Assets:Investments:Cash  -20000 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Investments:MSFT"
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            price { number { exact: "2000" } currency: "USD" is_total: true }
          }
        }
        postings {
          account: "Assets:Investments:Cash"
          spec { units { number { exact: "-20000" } currency: "USD" } }
        }
      }
    }
  )");
}

TEST(TestBalance, TotalCost) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT      10 MSFT {{2,000 USD}}
      Assets:Investments:Cash  -20000 USD

    2013-05-18 * ""
      Assets:Investments:MSFT      10 MSFT {{2000 USD, 2014-02-25}}
      Assets:Investments:Cash  -20000 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Investments:MSFT"
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            cost { total { number { exact: "2000" } } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Investments:Cash"
          spec {
            units { number { exact: "-20000" } currency: "USD" }
          }
        }
      }
    }
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Investments:MSFT"
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            cost {
              total { number { exact: "2000" } }
              currency: "USD"
              date { year: 2014 month: 2 day: 25 }
            }
          }
        }
        postings {
          account: "Assets:Investments:Cash"
          spec { units { number { exact: "-20000" } currency: "USD" } }
        }
      }
    }
  )");
}

//------------------------------------------------------------------------------
// TestMetaData

TEST(TestMetaData, MetadataTransactionBegin) {
  ExpectParse(R"(
    2013-05-18 * ""
      test: "Something"
      Assets:Investments:MSFT      10 MSFT @@ 2000 USD
      Assets:Investments:Cash  -20000 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      meta {
        kv { key: "test" value { text: "Something" } }
      }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Investments:MSFT"
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            price { number { exact: "2000" } currency: "USD" is_total: true }
          }
        }
        postings {
          account: "Assets:Investments:Cash"
          spec {
            units { number { exact: "-20000" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestMetaData, MetadataTransactionMiddle) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT      10 MSFT @@ 2000 USD
        test: "Something"
      Assets:Investments:Cash  -20000 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        postings {
          meta {
            kv { key: "test" value { text: "Something" } }
          }
          account: "Assets:Investments:MSFT"
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            price { number { exact: "2000" } currency: "USD" is_total: true }
          }
        }
        postings {
          account: "Assets:Investments:Cash"
          spec {
            units { number { exact: "-20000" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestMetaData, MetadataTransactionEnd) {
  ExpectParse(R"(
    2013-05-18 * ""
      Assets:Investments:MSFT      10 MSFT @@ 2000 USD
      Assets:Investments:Cash  -20000 USD
        test: "Something"
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Investments:MSFT"
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            price { number { exact: "2000" } currency: "USD" is_total: true }
          }
        }
        postings {
          meta {
            kv { key: "test" value { text: "Something" } }
          }
          account: "Assets:Investments:Cash"
          spec {
            units { number { exact: "-20000" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestMetaData, MetadataTransactionMany) {
  ExpectParse(R"(
    2013-05-18 * ""
      test1: "Something"
      Assets:Investments:MSFT      10 MSFT @@ 2000 USD
        test2: "has"
        test3: "to"
      Assets:Investments:Cash  -20000 USD
        test4: "come"
        test5: "from"
        test6: "this"
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      meta {
        kv { key: "test1" value { text: "Something" } }
      }
      transaction {
        flag: "*"
        postings {
          meta {
            kv { key: "test2" value { text: "has" } }
            kv { key: "test3" value { text: "to" } }
          }
          account: "Assets:Investments:MSFT"
          spec {
            units { number { exact: "10" } currency: "MSFT" }
            price { number { exact: "2000" } currency: "USD" is_total: true } }
        }
        postings {
          meta {
            kv { key: "test4" value { text: "come" } }
            kv { key: "test5" value { text: "from" } }
            kv { key: "test6" value { text: "this" } }
          }
          account: "Assets:Investments:Cash"
          spec {
            units { number { exact: "-20000" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestMetaData, MetadataTransactionIndented) {
  ExpectParse(R"(
    2013-05-18 * ""
        test1: "Something"
      Assets:Investments:MSFT      10 MSFT @@ 2000 USD
        test2: "has"
        test3: "to"
      Assets:Investments:Cash  -20000 USD
        test4: "come"
        test5: "from"
        test6: "this"
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      meta {
        kv { key: "test1" value { text: "Something" } }
      }
      transaction {
        flag: "*"
      }
    }
    errors {
      message: "Syntax error, unexpected ACCOUNT"
    }
)");
  // This is a difference with the Python version; indentation is more strict in
  // the RE-Flex/C++ version and this generates an error.
}

TEST(TestMetaData, MetadataTransactionRepeated) {
  ExpectParse(R"(
    2013-05-18 * ""
      test: "Bananas"
      test: "Apples"
      test: "Oranges"
      Assets:Investments   100 USD
        test: "Mangos"
        test: "Pineapple"
      Income:Investments  -100 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      meta {
        kv { key: "test" value { text: "Bananas" } }
        kv { key: "test" value { text: "Apples" } }
        kv { key: "test" value { text: "Oranges" } }
      }
      transaction {
        flag: "*"
        postings {
          meta {
            kv { key: "test" value { text: "Mangos" } }
            kv { key: "test" value { text: "Pineapple" } }
          }
          account: "Assets:Investments"
          spec {
            units { number { exact: "100" } currency: "USD" }
          }
        }
        postings {
          account: "Income:Investments"
          spec {
            units { number { exact: "-100" } currency: "USD" }
          }
        }
      }
    }
    errors {
      message: "Duplicate metadata key: \'test\' with value \'text: \"Pineapple\"\n\'"
    }
    errors {
      message: "Duplicate metadata key: \'test\' with value \'text: \"Apples\"\n\'"
    }
    errors {
      message: "Duplicate metadata key: \'test\' with value \'text: \"Oranges\"\n\'"
    }
  )");
  // The C++ version tolerates multiple identical keys.
  // This is a harmless difference with the Python version.
}

TEST(TestMetaData, MetadataEmpty) {
  ExpectParse(R"(
    2013-05-18 * "blabla"
      oranges:
      bananas:

    2013-05-19 open Assets:Something
      apples:
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      meta {
        kv { key: "oranges" value {} }
        kv { key: "bananas" value {} }
      }
      transaction {
        flag: "*"
        narration: "blabla"
      }
    }
    directives {
      date { year: 2013 month: 5 day: 19 }
      meta {
        kv { key: "apples" value {} }
      }
      open {
        account: "Assets:Something"
      }
    }
  )");
}

TEST(TestMetaData, MetadataOther) {
  ExpectParse(R"(
    2013-01-01 open Equity:Other

    2013-01-01 open Assets:Investments
      test1: "Something"
      test2: "Something"

    2014-01-01 close Assets:Investments
      test1: "Something"

    2013-01-10 note Assets:Investments "Bla"
      test1: "Something"

    2013-01-31 pad Assets:Investments Equity:Other
      test1: "Something"

    2013-02-01 balance Assets:Investments  111.00 USD
      test1: "Something"

    2013-03-01 event "location" "Nowhere"
      test1: "Something"

    2013-03-01 document Assets:Investments "/path/to/something.pdf"
      test1: "Something"

    2013-03-01 price  HOOL  500 USD
      test1: "Something"
  )", R"(
    directives {
      date { year: 2013 month: 1 day: 1 }
      open {
        account: "Equity:Other"
      }
    }
    directives {
      date { year: 2013 month: 1 day: 1 }
      meta {
        kv { key: "test1" value { text: "Something" } }
        kv { key: "test2" value { text: "Something" } }
      }
      open {
        account: "Assets:Investments"
      }
    }
    directives {
      date { year: 2014 month: 1 day: 1 }
      meta {
        kv { key: "test1" value { text: "Something" } }
      }
      close {
        account: "Assets:Investments"
      }
    }
    directives {
      date { year: 2013 month: 1 day: 10 }
      meta {
        kv { key: "test1" value { text: "Something" } }
      }
      note {
        account: "Assets:Investments"
        comment: "Bla"
      }
    }
    directives {
      date { year: 2013 month: 1 day: 31 }
      meta {
        kv { key: "test1" value { text: "Something" } }
      }
      pad {
        account: "Assets:Investments"
        source_account: "Equity:Other"
      }
    }
    directives {
      date { year: 2013 month: 2 day: 1 }
      meta {
        kv { key: "test1" value { text: "Something" } }
      }
      balance {
        account: "Assets:Investments"
        amount { number { exact: "111.00" } currency: "USD" }
      }
    }
    directives {
      date { year: 2013 month: 3 day: 1 }
      meta {
        kv { key: "test1" value { text: "Something" } }
      }
      event {
        type: "location"
        description: "Nowhere"
      }
    }
    directives {
      date { year: 2013 month: 3 day: 1 }
      meta {
        kv { key: "test1" value { text: "Something" } }
      }
      document {
        account: "Assets:Investments"
        filename: "/path/to/something.pdf"
      }
    }
    directives {
      date { year: 2013 month: 3 day: 1 }
      meta {
        kv { key: "test1" value { text: "Something" } }
      }
      price {
        currency: "HOOL"
        amount { number { exact: "500" } currency: "USD" }
      }
    }
  )");
}

TEST(TestMetaData, MetadataDataTypes) {
  ExpectParse(R"(
    2013-05-18 * ""
      string: "Something"
      account: Assets:Investments:Cash
      date: 2012-01-01
      currency: HOOL
      tag: #trip-florida
      number: 345.67
      amount: 345.67 USD
      boolt: TRUE
      boolf: FALSE
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      meta {
        kv { key: "string" value { text: "Something" } }
        kv { key: "account" value { account: "Assets:Investments:Cash" } }
        kv { key: "date" value { date { year: 2012 month: 1 day: 1 } } }
        kv { key: "currency" value { currency: "HOOL" } }
        kv { key: "tag" value { tag: "trip-florida" } }
        kv { key: "number" value { number { exact: "345.67" } } }
        kv { key: "amount" value { amount { number { exact: "345.67" } currency: "USD" } } }
        kv { key: "boolt" value { boolean: true } }
        kv { key: "boolf" value { boolean: false } }
      }
      transaction {
        flag: "*"
      }
    }
  )");
}

TEST(TestMetaData, MetadataKeySyntax) {
  ExpectParse(R"(
    2013-05-18 * ""
      nameoncard: "Jim"
      nameOnCard: "Joe"
      name-on-card: "Bob"
      name_on_card: "John"
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      meta {
        kv { key: "nameoncard" value { text: "Jim" } }
        kv { key: "nameOnCard" value { text: "Joe" } }
        kv { key: "name-on-card" value { text: "Bob" } }
        kv { key: "name_on_card" value { text: "John" } }
      }
      transaction {
        flag: "*"
      }
    }
  )");
}

//------------------------------------------------------------------------------
// TestArithmetic

TEST(TestArithmetic, NumberExprAdd) {
  ExpectParse(R"(
    2013-05-18 * "Test"
      Assets:Something    12 + 3 USD
      Assets:Something   7.5 + 3.1 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Test"
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "15" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "10.6" } currency: "USD" }
          }
        }
      }
    }


  )");
}

TEST(TestArithmetic, NumberExprSubtract) {
  ExpectParse(R"(
    2013-05-18 * "Test"
      Assets:Something    12 - 3 USD
      Assets:Something   7.5 - 3.1 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Test"
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "9" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "4.4" } currency: "USD" }
          }
        }
      }
    }

  )");
}

TEST(TestArithmetic, NumberExprMultiply) {
  ExpectParse(R"(
    2013-05-18 * "Test"
      Assets:Something    12 * 3 USD
      Assets:Something   7.5 * 3.1 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Test"
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "36" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "23.25" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestArithmetic, NumberExprDivide) {
  ExpectParse(R"(
    2013-05-18 * "Test"
      Assets:Something    12 / 3 USD
      Assets:Something   7.5 / 3 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Test"
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "4" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "2.5" } currency: "USD" }
          }
        }
      }
    }


  )");
}

TEST(TestArithmetic, NumberExprNegative) {
  ExpectParse(R"(
    2013-05-18 * "Test"
      Assets:Something    -12 USD
      Assets:Something   -7.5 USD
      Assets:Something   - 7.5 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Test"
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "-12" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "-7.5" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "-7.5" } currency: "USD" }
          }
        }
      }
    }


  )");
}

TEST(TestArithmetic, NumberExprPositive) {
  ExpectParse(R"(
    2013-05-18 * "Test"
      Assets:Something    +12 USD
      Assets:Something   -7.5 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Test"
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "12" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "-7.5" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestArithmetic, NumberExprPrecedence) {
  ExpectParse(R"(
    2013-05-18 * "Test"
      Assets:Something   2 * 3 + 4 USD
      Assets:Something   2 + 3 * 4 USD
      Assets:Something   2 + -3 * 4 USD
      Assets:Something   (2 + -3) * 4 USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Test"
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "10" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "14" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "-10" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "-4" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestArithmetic, NumberExprGroups) {
  ExpectParse(R"(
    2013-05-18 * "Test"
      Assets:Something   (2 + -3) * 4 USD
      Assets:Something   2 * (2 + -3) USD
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Test"
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "-4" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "-2" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestArithmetic, NumberExprDifferentPlaces) {
  ExpectParse(R"(
    2013-05-18 * "Test"
      Assets:Something   -(3 * 4) HOOL {120.01 * 2.1 USD} @ 134.02 * 2.1 USD
      Assets:Something   1000000 USD ;; No balance checks.
    2014-01-01 balance Assets:Something  3 * 4 * 120.01 * 2.1  USD
      number: -(5662.23 + 22.3)
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Test"
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "-12" } currency: "HOOL" }
            cost { per_unit { number { exact: "252.021" } } currency: "USD" }
            price { number { exact: "281.442" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Something"
          spec {
            units { number { exact: "1000000" } currency: "USD" }
          }
        }
      }
    }
    directives {
      date { year: 2014 month: 1 day: 1 }
      meta {
        kv { key: "number" value { number { exact: "-5684.53" } } }
      }
      balance {
        account: "Assets:Something"
        amount { number { exact: "3024.252" } currency: "USD" }
      }
    }
  )");
}

//------------------------------------------------------------------------------
// TestLexerAndParserErrors

// There are a number of different paths where errors may occur. This test case
// intends to exercise them all. This docstring explains how it all works (it's
// a bit complicated).
//
// Expectations: The parser.parse_*() functions never raise exceptions, they
// always produce a list of error messages on the builder objects. This is by
// design: we have a single way to produce and report errors.
//
// The lexer may fail in two different ways:
//
// * Invalid Token: yylex() is invoked by the parser... the lexer rules are
//   processed and all fail and the default rule of the lexer gets called at
//   {bf253a29a820}. The lexer immediately switches to its INVALID subparser to
//   chomp the rest of the invalid token's unrecognized characters until it
//   hits some whitespace (see {bba169a1d35a}). A new LexerError is created and
//   accumulated by calling the build_lexer_error() method on the builder
//   object using this text (see {0e31aeca3363}). The lexer then returns a
//   LEX_ERROR token to the parser, which fails as below.
//
// * Lexer Builder Exception: The lexer recognizes a valid token and invokes a
//   callback on the builder using BUILD_LEX(). However, an exception is raised
//   in that Python code ({3cfb2739349a}). The exception's error text is saved
//   and this is used to build an error on the lexer builder at
//   build_lexer_error().
//
// For both the errors above, when the parser receives the LEX_ERROR token, it
// is an unexpected token (because none of the rules handle it), so it calls
// yyerror() {ca6aab8b9748}. Because the lexer has already registered an error
// in the error list, in yyerror() we simply ignore it and do nothing (see
// {ca6aab8b9748}).
//
// Error recovery then proceeds by successive reductions of the "error" grammar
// rules which discards all tokens until a valid rule can be reduced again
// ({3d95e55b654e}). Note that Bison issues a single call to yyerror() and
// keeps reducing invalid "error" rules silently until another one succeeds. We
// ignore the directive and restart parsing from that point on. If the error
// occurred on a posting in progress of being parsed, because the "error" rule
// is not a valid posting of a transaction, that transaction will not be
// produced and thus is ignore, which is what we want.
//
// The parser may itself also encounter two similar types of errors:
//
// * Syntax Error: There is an error in the grammar of the input. yyparse() is
//   called automatically by the generated code and we call
//   build_grammar_error() to register the error. Error recovery proceeds
//   similarly to what was described previously.
//
// * Grammar Builder Exception: A grammar rule is reduced successfully, a
//   builder method is invoked and raises a Python exception. A macro in the
//   code that invokes this method is used to catch this error and calls
//   build_grammar_error_from_exception() to register an error and makes the
//   parser issue an error with YYERROR (see {05bb0fb60e86}).
//
// We never call YYABORT anywhere so the yyparse() function should never return
// anything else than 0. If it does, we translate that into a Python
// RuntimeError exception, or a MemoryError exception (if yyparse() ran out of
// memory), see {459018e2905c}.

TEST(TestLexerAndParserErrors, LexerInvalidToken) {
  ExpectParse(R"(
    2000-01-01 open ) USD
  )", R"(
    errors {
      message: "Syntax error, unexpected RPAREN, expecting ACCOUNT"
    }
  )");
}


TEST(TestLexerAndParserErrors, LexerInvalidTokenRecovery) {
  ExpectParse(R"(
    2000-01-01 open ) USD
    2000-01-02 open Assets:Something
  )", R"(
    directives {
      date {
        year: 2000
        month: 1
        day: 2
      }
      open {
        account: "Assets:Something"
      }
    }
    errors {
      message: "Syntax error, unexpected RPAREN, expecting ACCOUNT"
    }
  )");
}

TEST(TestLexerAndParserErrors, LexerException) {
  ExpectParse(R"(
    2000-13-32 open Assets:Something
  )", R"(
    errors {
      message: "Syntax error, unexpected invalid token"
    }
  )");
  // TODO(blais): This does trigger a date error via YYUNDEF, but somehow I
  // haven't been able to surface the error message through parse.rcc's logging
  // function. It should be 'month must be in 1..12'
}

TEST(TestLexerAndParserErrors, LexerExceptionRecovery) {
  ExpectParse(R"(
    2000-13-32 open Assets:Something
    2000-01-02 open Assets:Working
  )", R"(
    directives {
      date {
        year: 2000
        month: 1
        day: 2
      }
      open {
        account: "Assets:Working"
      }
    }
    errors {
      message: "Syntax error, unexpected invalid token"
    }
  )");
  // TODO(blais): This does trigger a date error via YYUNDEF, but somehow I
  // haven't been able to surface the error message through parse.rcc's logging
  // function. It should be 'month must be in 1..12'
}

TEST(TestLexerAndParserErrors, LexerErrorsInPostings) {
  ExpectParse(R"(

    2000-01-02 *
      Assets:Working  `
      Assets:Working  11.11 USD
      Assets:Working  22.22 USD

    2000-01-02 *
      Assets:Working  11.11 USD
      Assets:Working  `
      Assets:Working  22.22 USD

    2000-01-02 *
      Assets:Working  11.11 USD
      Assets:Working  22.22 USD
      Assets:Working  `

    2000-01-02 *
      Assets:Working  )
      Assets:Working  11.11 USD
      Assets:Working  22.22 USD

    2000-01-02 *
      Assets:Working  11.11 USD
      Assets:Working  )
      Assets:Working  22.22 USD

    2000-01-02 *
      Assets:Working  11.11 USD
      Assets:Working  22.22 USD
      Assets:Working  )

  )", R"(
    # Note: no transaction is produced.

    errors {
      message: "Syntax error, unexpected invalid token, expecting EOL or ATAT or AT"
    }
    errors {
      message: "Syntax error, unexpected invalid token, expecting EOL or ATAT or AT"
    }
    errors {
      message: "Syntax error, unexpected invalid token, expecting EOL or ATAT or AT"
    }
    errors {
      message: "Syntax error, unexpected RPAREN, expecting EOL or ATAT or AT"
    }
    errors {
      message: "Syntax error, unexpected RPAREN, expecting EOL or ATAT or AT"
    }
    errors {
      message: "Syntax error, unexpected RPAREN, expecting EOL or ATAT or AT"
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarSyntaxError) {
  ExpectParse(R"(
    2000-01-01 open open
  )", R"(
    errors {
      message: "Syntax error, unexpected OPEN, expecting ACCOUNT"
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarSyntaxErrorRecovery) {
  ExpectParse(R"(
    2000-01-01 open Assets:Before
    2000-01-02 open open
    2000-01-03 open Assets:After
  )", R"(
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
    directives {
      date { year: 2000 month: 1 day: 3 }
      open { account: "Assets:After" }
    }
    errors {
      message: "Syntax error, unexpected OPEN, expecting ACCOUNT"
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarSyntaxErrorRecovery2) {
  ExpectParse(R"(
    2000-01-01 open open
    2000-01-02 open Assets:Something
  )", R"(
    directives {
      date { year: 2000 month: 1 day: 2 }
      open { account: "Assets:Something" }
    }
    errors {
      message: "Syntax error, unexpected OPEN, expecting ACCOUNT"
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarSyntaxErrorMultiple) {
  ExpectParse(R"(
    2000-01-01 open Assets:Before
    2000-01-02 open open
    2000-01-03 open Assets:After
    2000-01-04 close close
    2000-01-05 close Assets:After
  )", R"(
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
    directives {
      date { year: 2000 month: 1 day: 3 }
      open { account: "Assets:After" }
    }
    directives {
      date { year: 2000 month: 1 day: 5 }
      close { account: "Assets:After" }
    }
    errors {
      message: "Syntax error, unexpected OPEN, expecting ACCOUNT"
    }
    errors {
      message: "Syntax error, unexpected CLOSE, expecting ACCOUNT"
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsPushtag) {
  ExpectParse(R"(
    2000-01-01 open Assets:Before
    pushtag #sometag
    2010-01-01 close Assets:Before
  )", R"(
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      tags: "sometag"
      close { account: "Assets:Before" }
    }
    errors {
      message: "Unbalanced pushed tag: \'sometag\'"
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsPoptag) {
  ExpectParse(R"(
    2000-01-01 open Assets:Before
    poptag #sometag
    2010-01-01 close Assets:Before
  )", R"(
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    errors {
      message: "Attempting to pop absent tag: \'sometag\'"
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsOption) {
  ExpectParse(R"(
    2000-01-01 open Assets:Before
    option "operating_currency" "CAD"
    2010-01-01 close Assets:Before
  )", R"(
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    options {
      operating_currency: "CAD"
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsInclude) {
  ExpectParse(R"(
    2000-01-01 open Assets:Before
    include "answer.beancount"
    2010-01-01 close Assets:Before
  )", R"(
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    info {
      include: "answer.beancount"
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsPlugin) {
  ExpectParse(R"(
    2000-01-01 open Assets:Before
    plugin "answer.beancount"
    2010-01-01 close Assets:Before
  )", R"(
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    info {
      plugin { name: "answer.beancount" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsAmount) {
  ExpectParse(R"(
    2000-01-01 open Assets:Before
    2001-02-02 balance Assets:Before    23.00 USD
    2010-01-01 close Assets:Before
  )", R"(
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
    directives {
      date { year: 2001 month: 2 day: 2 }
      balance {
        account: "Assets:Before"
        amount { number { exact: "23.00" } currency: "USD" }
      }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsCompoundAmount) {
  ExpectParse(R"(
    2000-01-01 open Assets:Before
    2001-02-02 *
      Assets:Before   10.00 HOOL {100.00 # 9.95 USD}
      Assets:After   -100.00 USD
    2010-01-01 close Assets:Before
  )", R"(
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
    directives {
      date { year: 2001 month: 2 day: 2 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Before"
          spec {
            units { number { exact: "10.00" } currency: "HOOL" }
            cost { per_unit { number { exact: "100.00" } }
                   total { number { exact: "9.95" } }
                   currency: "USD" }
          }
        }
        postings {
          account: "Assets:After"
          spec {
            units { number { exact: "-100.00" } currency: "USD" }
          }
        }
      }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsLotCostDate) {
  ExpectParse(R"(
    2000-01-01 open Assets:Before
    2001-02-02 *
      Assets:Before   10.00 HOOL {100.00 USD}
      Assets:After   -100.00 USD
    2010-01-01 close Assets:Before
  )", R"(
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
    directives {
      date { year: 2001 month: 2 day: 2 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Before"
          spec {
            units { number { exact: "10.00" } currency: "HOOL" }
            cost { per_unit { number { exact: "100.00" } } currency: "USD" }
          }
        }
        postings {
          account: "Assets:After"
          spec {
            units { number { exact: "-100.00" } currency: "USD" }
          }
        }
      }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsOpen) {
  ExpectParse(R"(
    2010-01-01 balance Assets:Before  1 USD
    2000-01-01 open Assets:Before
    2010-01-01 close Assets:Before
  )", R"(
    directives {
      date { year: 2010 month: 1 day: 1 }
      balance {
        account: "Assets:Before"
        amount { number { exact: "1" } currency: "USD" }
      }
    }
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsClose) {
  ExpectParse(R"(
    2010-01-01 balance Assets:Before  1 USD
    2010-01-01 close Assets:Before
    2000-01-01 open Assets:Before
  )", R"(
    directives {
      date { year: 2010 month: 1 day: 1 }
      balance {
        account: "Assets:Before"
        amount { number { exact: "1" } currency: "USD" }
      }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsCommodity) {
  ExpectParse(R"(
    2010-01-01 close Assets:Before
    2010-01-01 commodity USD
    2000-01-01 open Assets:Before
  )", R"(
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      commodity { currency: "USD" }
    }
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsPad) {
  ExpectParse(R"(
    2010-01-01 close Assets:Before
    2010-01-01 pad Assets:Before Assets:After
    2000-01-01 open Assets:Before
  )", R"(
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      pad {
        account: "Assets:Before"
        source_account: "Assets:After"
      }
    }
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsBalance) {
  ExpectParse(R"(
    2010-01-01 close Assets:Before
    2010-01-01 balance Assets:Before 100 USD
    2000-01-01 open Assets:Before
  )", R"(
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      balance {
        account: "Assets:Before"
        amount { number { exact: "100" } currency: "USD" }
      }
    }
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsEvent) {
  ExpectParse(R"(
    2010-01-01 close Assets:Before
    2010-01-01 event "location" "New York, NY"
    2000-01-01 open Assets:Before
  )", R"(
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      event {
        type: "location"
        description: "New York, NY"
      }
    }
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsPrice) {
  ExpectParse(R"(
    2010-01-01 close Assets:Before
    2010-01-01 price HOOL 20 USD
    2000-01-01 open Assets:Before
  )", R"(
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      price {
        currency: "HOOL"
        amount { number { exact: "20" } currency: "USD" }
      }
    }
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsNote) {
  ExpectParse(R"(
    2010-01-01 close Assets:Before
    2010-01-01 note Assets:Before "Something something"
    2000-01-01 open Assets:Before
  )", R"(
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      note {
        account: "Assets:Before"
        comment: "Something something"
      }
    }
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsDocument) {
  ExpectParse(R"(
    2010-01-01 close Assets:Before
    2010-01-01 document Assets:Before "/path/to/document.png"
    2000-01-01 open Assets:Before
  )", R"(
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      document {
        account: "Assets:Before"
        filename: "/path/to/document.png"
      }
    }
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsKeyValue) {
  ExpectParse(R"(
    2010-01-01 close Assets:Before
    2010-01-01 commodity HOOL
      key: "Value"
    2000-01-01 open Assets:Before
  )", R"(
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      meta {
        kv { key: "key" value { text: "Value" } }
      }
      commodity { currency: "HOOL" }
    }
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsPosting) {
  ExpectParse(R"(
    2010-01-01 close Assets:Before
    2010-01-01 *
      Assets:Before   100.00 USD
      Assets:After   -100.00 USD
    2000-01-01 open Assets:Before
  )", R"(
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Before"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:After"
          spec {
            units { number { exact: "-100.00" } currency: "USD" }
          }
        }
      }
    }
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsTagLinkNew) {
  ExpectParse(R"(
    2010-01-01 close Assets:Before
    2010-01-01 * "Payee" "Narration"
      Assets:Before   100.00 USD
      Assets:After   -100.00 USD
    2000-01-01 open Assets:Before
  )", R"(
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      transaction {
        flag: "*"
        payee: "Payee"
        narration: "Narration"
        postings {
          account: "Assets:Before"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:After"
          spec {
            units { number { exact: "-100.00" } currency: "USD" }
          }
        }
      }
    }
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsTagLinkTag) {
  ExpectParse(R"(
    2010-01-01 close Assets:Before
    2010-01-01 * "Payee" "Narration" #sometag
      Assets:Before   100.00 USD
      Assets:After   -100.00 USD
    2000-01-01 open Assets:Before
  )", R"(
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      tags: "sometag"
      transaction {
        flag: "*"
        payee: "Payee"
        narration: "Narration"
        postings {
          account: "Assets:Before"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:After"
          spec {
            units { number { exact: "-100.00" } currency: "USD" }
          }
        }
      }
    }
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsTagLinkLink) {
  ExpectParse(R"(
    2010-01-01 close Assets:Before
    2010-01-01 * "Payee" "Narration" ^somelink
      Assets:Before   100.00 USD
      Assets:After   -100.00 USD
    2000-01-01 open Assets:Before
  )", R"(
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      links: "somelink"
      transaction {
        flag: "*"
        payee: "Payee"
        narration: "Narration"
        postings {
          account: "Assets:Before"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:After"
          spec {
            units { number { exact: "-100.00" } currency: "USD" }
          }
        }
      }
    }
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsTagLinkPipe) {
  ExpectParse(R"(
    2010-01-01 close Assets:Before
    2010-01-01 * "Payee" "Narration"
      Assets:Before   100.00 USD
      Assets:After   -100.00 USD
    2000-01-01 open Assets:Before
  )", R"(
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      transaction {
        flag: "*"
        payee: "Payee"
        narration: "Narration"
        postings {
          account: "Assets:Before"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:After"
          spec {
            units { number { exact: "-100.00" } currency: "USD" }
          }
        }
      }
    }
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
  )");
}

TEST(TestLexerAndParserErrors, GrammarExceptionsTransaction) {
  ExpectParse(R"(
    2010-01-01 close Assets:Before
    2010-01-01 * "Payee" "Narration"
      Assets:Before   100.00 USD
      Assets:After   -100.00 USD
    2000-01-01 open Assets:Before
  )", R"(
    directives {
      date { year: 2010 month: 1 day: 1 }
      close { account: "Assets:Before" }
    }
    directives {
      date { year: 2010 month: 1 day: 1 }
      transaction {
        flag: "*"
        payee: "Payee"
        narration: "Narration"
        postings {
          account: "Assets:Before"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:After"
          spec {
            units { number { exact: "-100.00" } currency: "USD" }
          }
        }
      }
    }
    directives {
      date { year: 2000 month: 1 day: 1 }
      open { account: "Assets:Before" }
    }
  )");
}

//------------------------------------------------------------------------------
// TestIncompleteInputs

TEST(TestIncompleteInputs, UnitsFull) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     100.00 USD
      Assets:Account2    -100.00 USD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "-100.00" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, UnitsMissing) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     100.00 USD
      Assets:Account2
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Account2"
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, UnitsMissingNumber) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     100.00 USD
      Assets:Account2            USD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, UnitsMissingCurrency) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     100.00 USD
      Assets:Account2    -100.00
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "-100.00" } }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, UnitsMissingWithCost) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     {300.00 USD}
      Assets:Account2    -600.00 USD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*" postings { account: "Assets:Account1"
          spec {
            cost { per_unit { number { exact: "300.00" } } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "-600.00" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, UnitsMissingNumberWithCost) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1            HOOL {300.00 USD}
      Assets:Account2    -600.00 USD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings { account: "Assets:Account1"
          spec {
            units { currency: "HOOL" }
            cost { per_unit { number { exact: "300.00" } } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "-600.00" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, UnitsMissingCurrencyWithCost) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1      10        {300.00 USD}
      Assets:Account2    -600.00 USD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "10" } }
            cost { per_unit { number { exact: "300.00" } } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "-600.00" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, UnitsMissingWithPrice) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account2                @ 1.2 USD
      Assets:Account1     100.00 USD @
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account2"
          spec {
            price { number { exact: "1.2" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
            price {}
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, UnitsMissingNumberWithPrice) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account2            CAD @ 1.2 USD
      Assets:Account1     100.00 USD @
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account2"
          spec {
            units { currency: "CAD" }
            price { number { exact: "1.2" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
            price {}
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, UnitsMissingCurrencyWithPrice) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account2     120.00     @ 1.2 USD
      Assets:Account1     100.00 USD @
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } }
            price { number { exact: "1.2" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
            price {}
          }
        }
      }
    }
  )");
}

//
// Price
//

TEST(TestIncompleteInputs, PriceNone) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     100.00 USD
      Assets:Account2     120.00 CAD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } currency: "CAD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, PriceMissing) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     100.00 USD @
      Assets:Account2     120.00 CAD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
            price {}
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } currency: "CAD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, PriceMissingNumber) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     100.00 USD @ CAD
      Assets:Account2     120.00 CAD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
            price { currency: "CAD" }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } currency: "CAD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, PriceMissingCurrency) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     100.00 USD @ 1.2
      Assets:Account2     120.00 CAD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "100.00" } currency: "USD" }
            price { number { exact: "1.2" } }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } currency: "CAD" }
          }
        }
      }
    }
  )");
}

//
// Cost
//

TEST(TestIncompleteInputs, CostFull) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     2 HOOL {150 # 5 USD}
      Assets:Account2     120.00 USD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "2" } currency: "HOOL" }
            cost {
              per_unit { number { exact: "150" } }
              total { number { exact: "5" } }
              currency: "USD"
            }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, CostMissingNumberPer) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     2 HOOL {# 5 USD}
      Assets:Account2     120.00 USD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "2" } currency: "HOOL" }
            cost {
              total { number { exact: "5" } }
               currency: "USD"
            }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, CostMissingNumberTotal) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     2 HOOL {150 # USD}
      Assets:Account2     120.00 USD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "2" } currency: "HOOL" }
            cost {
              per_unit { number { exact: "150" } }
              currency: "USD"
            }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, CostNoNumberTotal) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     2 HOOL {150 USD}
      Assets:Account2     120.00 USD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "2" } currency: "HOOL" }
            cost {
              per_unit { number { exact: "150" } }
              currency: "USD"
            }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, CostMissingNumbers) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     2 HOOL {USD}
      Assets:Account2     120.00 USD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "2" } currency: "HOOL" }
            cost {
              currency: "USD"
            }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, CostMissingCurrency) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     2 HOOL {150}
      Assets:Account2     120.00 USD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "2" } currency: "HOOL" }
            cost {
              per_unit { number { exact: "150" } }
            }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } currency: "USD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, CostEmpty) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     2 HOOL {}
      Assets:Account2     120.00 CAD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "2" } currency: "HOOL" }
            cost {}
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } currency: "CAD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, CostEmptyWithOther) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     2 HOOL {2015-09-21, "blablabla"}
      Assets:Account2     120.00 CAD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "2" } currency: "HOOL" }
            cost {
              date { year: 2015 month: 9 day: 21 }
              label: "blablabla"
            }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } currency: "CAD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, CostMissingBasis) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     2 HOOL {2015-09-21, "blablabla"}
      Assets:Account2     120.00 CAD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "2" } currency: "HOOL" }
            cost {
              date { year: 2015 month: 9 day: 21 }
              label: "blablabla"
            }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } currency: "CAD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, CostAverage) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     2 HOOL {*}
      Assets:Account2     120.00 CAD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "2" } currency: "HOOL" }
            cost {
              merge_cost: true
            }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } currency: "CAD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, CostAverageMissingBasis) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     2 HOOL {*, 2015-09-21, "blablabla"}
      Assets:Account2     120.00 CAD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "2" } currency: "HOOL" }
            cost {
              date { year: 2015 month: 9 day: 21 }
              label: "blablabla"
              merge_cost: true
            }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } currency: "CAD" }
          }
        }
      }
    }
  )");
}

TEST(TestIncompleteInputs, CostAverageWithOther) {
  ExpectParse(R"(
    2010-05-28 *
      Assets:Account1     2 HOOL {*, 100.00 CAD, 2015-09-21, "blablabla"}
      Assets:Account2     120.00 CAD
  )", R"(
    directives {
      date { year: 2010 month: 5 day: 28 }
      transaction {
        flag: "*"
        postings {
          account: "Assets:Account1"
          spec {
            units { number { exact: "2" } currency: "HOOL" }
            cost {
              per_unit { number { exact: "100.00" } }
              currency: "CAD"
              date { year: 2015 month: 9 day: 21 }
              label: "blablabla"
              merge_cost: true
            }
          }
        }
        postings {
          account: "Assets:Account2"
          spec {
            units { number { exact: "120.00" } currency: "CAD" }
          }
        }
      }
    }
  )");
}

//------------------------------------------------------------------------------
// TestMisc

TEST(TestMisc, CommentInPostings) {
  ExpectParse(R"(
    2017-06-27 * "Bitcoin network fee"
      ; Account: Pocket money
      Expenses:Crypto:NetworkFees           0.00082487 BTC
      Assets:Crypto:Bitcoin                -0.00082487 BTC
  )", R"(
    directives {
      date { year: 2017 month: 6 day: 27 }
      transaction {
        flag: "*"
        narration: "Bitcoin network fee"
        postings {
          account: "Expenses:Crypto:NetworkFees"
          spec {
            units { number { exact: "0.00082487" } currency: "BTC" }
          }
        }
        postings {
          account: "Assets:Crypto:Bitcoin"
          spec {
            units { number { exact: "-0.00082487" } currency: "BTC" }
          }
        }
      }
    }
  )");
}

TEST(TestMisc, CommentInPostingsInvalid) {
  ExpectParse(R"(
    2017-06-27 * "Bitcoin network fee"
      Expenses:Crypto:NetworkFees           0.00082487 BTC
    ; Account: Pocket money
      Assets:Crypto:Bitcoin                -0.00082487 BTC
  )", R"(
    directives {
      date { year: 2017 month: 6 day: 27 }
      transaction {
        flag: "*"
        narration: "Bitcoin network fee"
        postings {
          account: "Expenses:Crypto:NetworkFees"
          spec {
            units { number { exact: "0.00082487" } currency: "BTC" }
          }
        }
      }
    }
    errors {
      message: "Syntax error, unexpected INDENT"
    }
  )");
}

//------------------------------------------------------------------------------
// TestDocument

TEST(TestDocument, DocumentNoTagsLinks) {
  ExpectParse(R"(
    2013-05-18 document Assets:US:BestBank:Checking "/Accounting/statement.pdf"
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      document {
        account: "Assets:US:BestBank:Checking"
        filename: "/Accounting/statement.pdf"
      }
    }
  )");
}

TEST(TestDocument, DocumentTags) {
  ExpectParse(R"(
    pushtag #something
    2013-05-18 document Assets:US:BestBank:Checking "/Accounting/statement.pdf" #else
    poptag #something
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      tags: "something"
      tags: "else"
      document {
        account: "Assets:US:BestBank:Checking"
        filename: "/Accounting/statement.pdf"
      }
    }
  )");
}

TEST(TestDocument, DocumentLinks) {
  ExpectParse(R"(
    2013-05-18 document Assets:US:BestBank:Checking "/statement.pdf" ^something
  )", R"(
    directives {
      date { year: 2013 month: 5 day: 18 }
      links: "something"
      document {
        account: "Assets:US:BestBank:Checking"
        filename: "/statement.pdf"
      }
    }
  )");
}

// TODO(blais): Don't forget to explicitly test for each of the error conditions
// raised in the scanner.

}  // namespace
}  // namespace beancount
