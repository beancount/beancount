// A translation of the corresponding Python test.
// (First experiment with doing this; perhaps we'll stick with the Python-level testing.)
//
// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/std_utils.h"

#include <string>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace beancount {
namespace {
using std::string;
using std::vector;

TEST(TestStdUtils, Capitalize) {
  EXPECT_EQ("Weirdo", Capitalize("weirdo"));
  EXPECT_EQ("Weirdo", Capitalize("Weirdo"));
  EXPECT_EQ(" weirdo", Capitalize(" weirdo"));
  EXPECT_EQ("123", Capitalize("123"));
  EXPECT_EQ("", Capitalize(""));
}

}  // namespace
}  // namespace beancount
