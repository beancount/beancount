#include "beancount/cparser/parser.h"
#include "beancount/cparser/scanner.h"
#include "beancount/cparser/test_utils.h"

#if 0
#include "beancount/compile.h"
#include "beancount/data.pb.h"
#include "beancount/test_utils.h"
#endif

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

void ExpectParse(const std::string& input_string,
                 const std::string& expected_string,
                 bool dedent=true) {
  string clean_string = dedent ? StripAndDedent(input_string) : input_string;
  auto ledger = parser::ParseString(clean_string, "<string>");
  EXPECT_EQ(1, ledger->directives.size());
  ClearLineNumbers(ledger.get());
  EXPECT_TRUE(EqualsMessages(*ledger->directives.front(), expected_string));
}

TEST(ParserTest, TestBasic) {
  ExpectParse(R"(

    2014-01-27 * "UNION MARKET"
      Liabilities:US:Amex:BlueCash    -22.02 USD
      Expenses:Food:Grocery            22.02 USD

  )", R"(
    date {
      year: 2014
      month: 1
      day: 27
    }
    transaction {
      flag: "*"
      narration: "UNION MARKET"
      postings {
        account: "Liabilities:US:Amex:BlueCash"
        units {
          number { exact: "-22.02" }
          currency: "USD"
        }
      }
      postings {
        account: "Expenses:Food:Grocery"
        units {
          number { exact: "22.02" }
          currency: "USD"
        }
      }
    }
  )");
}

}  // namespace
}  // namespace beancount
