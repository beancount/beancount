#include "beancount/cparser/parser.h"
#include "beancount/cparser/scanner.h"

#include <algorithm>
#include <cassert>
#include <limits>
#include <string>
#include <vector>

#include "gtest/gtest.h"
#include "reflex/input.h"

namespace beancount {
namespace {

using std::pair;
using std::string;
using std::vector;
using testing::Eq;

// TODO(blais): Implement scanner tests here.

/// // TODO(blais): When symbol_name() is made public, make a << operator on the symbol_type.
///
/// using symbol = parser::Parser::symbol_kind;
/// using symbol_type = parser::Parser::symbol_kind::symbol_kind_type;
/// typedef vector<pair<symbol_type, string>> symbol_pairs;
///
/// // Read the given string in 'input_string' and return a scanned list of (token,
/// // name) pairs.
/// symbol_pairs Tokenize(const string& input_string) {
///   // Ensure the string is converted to UTF8 by using reflex::Input and
///   // instantiate a scanner.
///   string clean_string = StripAndDedent(input_string);
///   reflex::Input input(clean_string);
///   scanner::Scanner scanner(input, std::cout);
///
///   // Yield and accumulate all the tokens and their matched text.
///   symbol_pairs symbols;
///   while (true) {
///     auto token = scanner.lex();
///     if (token.type_get() == symbol::S_YYEOF) {
///       break;
///     }
///     // Note: the matcher will always return the full pattern matched, not the
///     // token value. Be aware of this when debugging.
///     symbols.push_back({token.type_get(), scanner.matcher().text()});
///   }
///   return symbols;
/// }
///
/// // Test a single item, by itself, as simple as can be.
/// TEST(ScannerTest, SimpleItem) {
///   const auto symbols = Tokenize(u8R"(
///     Conquer the world
///   )");
///   EXPECT_EQ(symbols, symbol_pairs({
///         {symbol::S_WORD, "Conquer"},
///         {symbol::S_WORD, "the"},
///         {symbol::S_WORD, "world"},
///         {symbol::S_EOL, "\n"}}));
/// }
///
/// // Test one level of indent, with different spaces.
/// TEST(ScannerTest, OneIndent) {
///   vector<pair<string, string>> tests{
///     {u8R"(
///       Conquer
///        the world
///      )", " "},
///     {u8R"(
///       Conquer
///         the world
///      )", "  "},
///     {u8R"(
///       Conquer
///          the world
///      )", "   "},
///     {u8R"(
///       Conquer
///              the world
///      )", "       "},
///   };
///   for (const auto& pair : tests) {
///     const auto symbols = Tokenize(pair.first);
///     EXPECT_EQ(symbols, symbol_pairs({
///           {symbol::S_WORD, "Conquer"},
///           {symbol::S_EOL, "\n"},
///           {symbol::S_INDENT, pair.second},
///           {symbol::S_WORD, "the"},
///           {symbol::S_WORD, "world"},
///           {symbol::S_EOL, "\n"}}));
///   }
/// }
///
/// // Test multiple levels of indent.
/// TEST(ScannerTest, MultipleIndents) {
///   const auto symbols = Tokenize(u8R"(
///     Conquer
///        the
///              world
///                 and
///                   all
///                    the
///                               rest
///   )");
///   EXPECT_EQ(symbols, symbol_pairs({
///         {symbol::S_WORD, "Conquer"},
///         {symbol::S_EOL, "\n"},
///         {symbol::S_INDENT, "   "},
///         {symbol::S_WORD, "the"},
///         {symbol::S_EOL, "\n"},
///         {symbol::S_INDENT, "         "},
///         {symbol::S_WORD, "world"},
///         {symbol::S_EOL, "\n"},
///         {symbol::S_INDENT, "            "},
///         {symbol::S_WORD, "and"},
///         {symbol::S_EOL, "\n"},
///         {symbol::S_INDENT, "              "},
///         {symbol::S_WORD, "all"},
///         {symbol::S_EOL, "\n"},
///         {symbol::S_INDENT, "               "},
///         {symbol::S_WORD, "the"},
///         {symbol::S_EOL, "\n"},
///         {symbol::S_INDENT, "                          "},
///         {symbol::S_WORD, "rest"},
///         {symbol::S_EOL, "\n"},
///  }));
/// }
///
/// // Test command reserved words.
/// TEST(ScannerTest, Commands) {
///   const auto symbols = Tokenize(u8R"(
///     /type/some /lazytype/thing /macro /import
///   )");
///   EXPECT_EQ(symbols, symbol_pairs({
///         {symbol::S_TYPE, "/type/some"},
///         {symbol::S_LAZYTYPE, "/lazytype/thing"},
///         {symbol::S_MACRO, "/macro"},
///         {symbol::S_IMPORT, "/import"},
///         {symbol::S_EOL, "\n"}}));
/// }
///
/// // Test comment syntax.
/// TEST(ScannerTest, Comments) {
///   const auto symbols = Tokenize(u8R"(
///     # Conquer the world
///   )");
///   EXPECT_EQ(symbols, symbol_pairs({
///         {symbol::S_COMMENT, "# Conquer the world"},
///         {symbol::S_EOL, "\n"}}));
/// }
///
/// // Test indented comments.
/// TEST(ScannerTest, IndentedComments) {
///   const auto symbols = Tokenize(u8R"(
///     ### Conquer the world
///        ## Conquer another world
///   )");
///   EXPECT_EQ(symbols, symbol_pairs({
///         {symbol::S_COMMENT, "### Conquer the world"},
///         {symbol::S_EOL, "\n"},
///         {symbol::S_COMMENT, "   ## Conquer another world"},
///         {symbol::S_EOL, "\n"}}));
/// }
///
/// // Test object references.
/// TEST(ScannerTest, Ref) {
///   const auto symbols = Tokenize(u8R"(
///     a/Conquer ar/the obj/world
///   )");
///   EXPECT_EQ(symbols, symbol_pairs({
///         {symbol::S_REF, "a/Conquer"},
///         {symbol::S_REF, "ar/the"},
///         {symbol::S_REF, "obj/world"},
///         {symbol::S_EOL, "\n"}}));
/// }
///
/// // Test object reference with some punctuation after.
/// TEST(ScannerTest, RefPunctAfter) {
///   const auto symbols = Tokenize(u8R"(
///     With u/caroline.
///   )");
///   EXPECT_EQ(symbols, symbol_pairs({
///         {symbol::S_WORD, "With"},
///         {symbol::S_REF, "u/caroline"},
///         {symbol::S_WORD, "."},
///         {symbol::S_EOL, "\n"}}));
/// }
///
/// // Test lazy object references.
/// TEST(ScannerTest, LazyRef) {
///   const auto symbols = Tokenize(u8R"(
///     a/ ar/ obj/
///   )");
///   EXPECT_EQ(symbols, symbol_pairs({
///         {symbol::S_AUTOREF, "a/"},
///         {symbol::S_AUTOREF, "ar/"},
///         {symbol::S_AUTOREF, "obj/"},
///         {symbol::S_EOL, "\n"}}));
/// }
///
/// // Test valid type declaration.
/// TEST(ScannerTest, ValidType) {
///   const auto symbols = Tokenize(u8R"(
///     /type/task Task.
///   )");
///   EXPECT_EQ(symbols, symbol_pairs({
///         {symbol::S_TYPE, "/type/task"},
///         {symbol::S_WORD, "Task."},
///         {symbol::S_EOL, "\n"}}));
/// }


}  // namespace
}  // namespace beancount
