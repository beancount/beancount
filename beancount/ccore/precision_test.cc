// Copyright (C) 2021  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/precision.h"

#include <string>
#include <vector>

#include "beancount/ccore/precision.pb.h"
#include "beancount/cparser/test_utils.h"

#include "google/protobuf/text_format.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace beancount {
namespace {

void ParseDirective(const std::string& value, Directive* proto){
  if (!google::protobuf::TextFormat::ParseFromString(value, proto)) {
    throw std::domain_error("Could not parse expected proto.");
  }
}

TEST(TestPrecisionStats, Basic) {
  PrecisionStatsAccum accum;
  Directive dir1;
  ParseDirective(R"(
      date { year: 2013 month: 5 day: 18 }
      transaction {
        flag: "*"
        narration: "Nice dinner at Mermaid Inn"
        postings {
          account: "Assets:ForUnits"
          spec {
            units { number { exact: "100" } currency: "USD" }
          }
        }
        postings {
          account: "Assets:ForCost"
          spec {
            units { number { exact: "-2" } currency: "HOOL" }
            cost {
              per_unit { number { exact: "101" } }
              total { number { exact: "102" } }
              currency: "USD"
            }
          }
        }
        postings {
          account: "Assets:ForPrice"
          spec {
            units { number { exact: "2" } currency: "HOOL" }
            price { number { exact: "103" } currency: "USD" }
          }
        }
      }
  )", &dir1);
  UpdateStatistics(dir1, &accum);

  PrecisionStats stats;
  accum.Serialize(&stats);
  EXPECT_TRUE(EqualsMessages(stats, R"(
    pairs {
      quote: "HOOL"
      has_sign: true
      max_integer_digits: 1
      exponent_mode: 0
      exponent_max: 0
    }
    pairs {
      quote: "USD"
      has_sign: false
      max_integer_digits: 3
      exponent_mode: 0
      exponent_max: 0
    }
    pairs {
      quote: "USD"
      base: "HOOL"
      has_sign: false
      max_integer_digits: 3
      exponent_mode: 0
      exponent_max: 0
    }
  )"));
}

TEST(TestPrecisionStats, Distribution) {
  PrecisionStatsAccum accum;
  Directive dir1;
  std::vector<std::string> values = {
    "1e2", "100", "100.0", "100.0000", "100.0", "100.0",
  };
  for (const auto& value : values) {
    auto* units = (dir1.mutable_transaction()->add_postings()->mutable_spec()
                   ->mutable_units());
    units->mutable_number()->set_exact(value);
    units->set_currency("USD");
  }
  UpdateStatistics(dir1, &accum);

  PrecisionStats stats;
  accum.Serialize(&stats);
  EXPECT_TRUE(EqualsMessages(stats, R"(
    pairs {
      quote: "USD"
      has_sign: false
      max_integer_digits: 3
      exponent_mode: 1
      exponent_max: 4
    }
  )"));
}









}  // namespace
}  // namespace beancount
