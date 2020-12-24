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
                               bool dedent=true) {
  string clean_string = dedent ? StripAndDedent(input_string) : input_string;
  auto ledger = parser::ParseString(clean_string, "<string>");
  ClearLineNumbers(ledger.get());
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

//------------------------------------------------------------------------------
// TestParserEntryTypes.Transaction

TEST(TestParserEntryTypes, EntryTransactionOneString) {
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

TEST(TestParserEntryTypes, EntryTransactionTwoStrings) {
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

TEST(TestParserEntryTypes, EntryTransactionThreeStrings) {
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

TEST(TestParserEntryTypes, EntryTransactionWithTxnKeyword) {
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

//------------------------------------------------------------------------------
// TestParserEntryTypes.Balance

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

//------------------------------------------------------------------------------
// TestParserEntryTypes.Open

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

//------------------------------------------------------------------------------
// TestParserEntryTypes.Close

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

//------------------------------------------------------------------------------
// TestParserEntryTypes.Commodity

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

//------------------------------------------------------------------------------
// TestParserEntryTypes.Pad

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

//------------------------------------------------------------------------------
// TestParserEntryTypes.Event

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

 //------------------------------------------------------------------------------
// TestParserEntryTypes.Query

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

//------------------------------------------------------------------------------
// TestParserEntryTypes.Note

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

  //------------------------------------------------------------------------------
// TestParserEntryTypes.Document

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

//------------------------------------------------------------------------------
// TestParserEntryTypes.Price

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

  //------------------------------------------------------------------------------
// TestParserEntryTypes.Custom

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

}  // namespace
}  // namespace beancount
