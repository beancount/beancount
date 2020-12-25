#include "beancount/cparser/parser.h"
#include "beancount/cparser/scanner.h"
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

// TODO(blais): When symbol_name() is made public, make a << operator on the symbol_type.
using std::pair;
using std::string;
using std::vector;
using std::cout;
using std::endl;
using std::string_view;

std::unique_ptr<Ledger> ExpectParse(const std::string& input_string,
                                    const std::string& expected_string,
                                    bool no_dedent=false,
                                    bool leave_lineno=false,
                                    bool print_input=false) {
  string clean_string = no_dedent ? input_string : StripAndDedent(input_string);
  if (print_input) {
    std::cout << "clean_string = >>>>>" << clean_string << "<<<<<" << std::endl;
  }
  auto ledger = parser::ParseString(clean_string, "<string>");
  ClearLineNumbers(ledger.get(), leave_lineno);
  auto ledger_proto = LedgerToProto(*ledger);
  EXPECT_TRUE(EqualsMessages(*ledger_proto, expected_string));
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
          units { number { exact: "-22.02" } currency: "USD" }
        }
        postings {
          account: "Expenses:Food:Grocery"
          units { number { exact: "22.02" } currency: "USD" }
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
          units { number { exact: "100" } currency: "USD" }
        }
        postings {
          account: "Assets:US:Cash"
          units { number { exact: "-100" } currency: "USD" }
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
          units { number { exact: "100" } currency: "USD" }
        }
        postings {
          account: "Assets:US:Cash"
          units { number { exact: "-100" } currency: "USD" }
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
      message: "Syntax error, unexpected STRING, expecting end of file or EOL or TAG or LINK"
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
          units { number { exact: "100" } currency: "USD" }
        }
        postings {
          account: "Assets:US:Cash"
          units { number { exact: "-100" } currency: "USD" }
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
  message: "Syntax error, unexpected LCURL, expecting end of file or EOL or TAG or LINK"
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
        amount {
          number {
            exact: "1.0290"
          }
          currency: "CAD"
        }
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
          amount { number { exact: "43.03" } currency: "USD" }
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
  )", false, true);
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
  )", false, true);
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
          units { number { exact: "0" } currency: "USD" }
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
          units { number { exact: "100" } currency: "USD" }
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
          units { number { exact: "100" } currency: "USD" }
        }
        postings {
          account: "Assets:US:Cash"
        }
      }
    }
  )", true);
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
          units { number { exact: "100" } currency: "USD" }
        }
        postings {
          account: "Assets:US:Cash"
        }
      }
    }
  )", true, false, true);
}

TEST(TestParserComplete, IndentEOF) {
  ExpectParse("\t", R"(
  )", true);
}

TEST(TestParserComplete, CommentEOF) {
  ExpectParse("; comment", R"(
  )", true);
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
          units { number { exact: "100.00" } currency: "USD" }
        }
        postings {
          account: "Assets:Cash"
          units { number { exact: "-100.00" } currency: "USD" }
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
          units { number { exact: "1" } currency: "USD" }
        }
        postings {
          account: "Assets:Cash"
          units { number { exact: "-1" } currency: "USD" }
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

  )");
// TODO(blais): should be valid
}

}  // namespace
}  // namespace beancount
