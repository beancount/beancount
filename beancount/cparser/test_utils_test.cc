#include "beancount/cparser/test_utils.h"
#include "beancount/ccore/data.pb.h"

#include <string>

#include "absl/strings/str_split.h"
#include "gmock/gmock.h"
#include "google/protobuf/text_format.h"
#include "google/protobuf/util/message_differencer.h"
#include "gtest/gtest.h"
#include "gmock/gmock.h"

namespace beancount {
namespace {
using std::string;
using std::vector;
using google::protobuf::TextFormat;
using google::protobuf::util::MessageDifferencer;
using testing::ElementsAre;

TEST(TestUtilsTest, StripAndDedent) {
  string input = u8R"(

    left
       indented
      less

  )";
  string output = StripAndDedent(input);
  vector<string> output_lines = absl::StrSplit(output, "\n");
  EXPECT_THAT(output_lines, ElementsAre("", "left", "   indented", "  less", "", ""));
}

TEST(TestUtilsTest, CompareMessages) {
  beancount::Amount amount;
  amount.mutable_number()->set_exact("144000");
  amount.set_currency("USD");
  EXPECT_TRUE(EqualsMessages(amount, R"(
    number { exact: "144000" }
    currency: "USD"
  )", false));
}

TEST(TestUtilsTest, ClearLineNumbers) {
  auto txn = new Directive();
  google::protobuf::TextFormat::ParseFromString(R"(
    location {
      lineno: 10
      lineno_end: 10
    }
    transaction {
      postings {
        location {
          lineno: 10
          lineno_end: 10
        }
      }
      postings {
        location {
          lineno: 10
          lineno_end: 10
        }
      }
    }
  )", txn);

  auto error = new Error();
  google::protobuf::TextFormat::ParseFromString(R"(
    location {
      lineno: 10
      lineno_end: 10
    }
  )", error);

  Ledger ledger;
  ledger.directives.push_back(txn);
  ledger.errors.push_back(error);

  ClearLineNumbers(&ledger);

  EXPECT_TRUE(EqualsMessages(*ledger.directives.front(), R"(
    transaction {
      postings {
      }
      postings {
      }
    }
  )", false));
}

}  // namespace
}  // namespace beancount
