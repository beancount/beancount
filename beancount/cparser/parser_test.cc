#include "beancount/cparser/parser.h"
#include "beancount/cparser/scanner.h"

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

#if 0
// Run the parser and check that its proto output equivalent matches.
bool CheckParse(string_view input, string_view expected_proto, int line_offset = 0) {
  string clean_string = StripAndDedent(input);
  auto actual_db = ParseString(clean_string, nullptr, line_offset);
  ClearLineNumbers(actual_db.get());
  return CompareMessages(*actual_db, expected_proto);
}

// // Serious wizardry in effect to get the line at the start of macro expansion.
// #define EXPECT_PARSE (SAVE_LINE RUN_CHECK
// #define SAVE_LINE { int lineno = __LINE__;
// #define RUN_CHECK(...) EXPECT_TRUE(CheckParse(__VA_ARGS__, lineno)); } )

#define EXPECT_PARSE(input, expected_proto)             \
  EXPECT_TRUE(CheckParse(input, expected_proto));

// Test just one item.
TEST(ParserTest, OneItem) {
  EXPECT_PARSE(u8R"(
    Conquer the world.
  )", u8R"(
    type {type: "item" contents: "Item type" flavor: LAZY}
    object {id {type: "item"} contents: "Conquer the world."}
  )");
}
#endif

}  // namespace
}  // namespace beancount
