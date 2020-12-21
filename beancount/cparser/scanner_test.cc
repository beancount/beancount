#include "beancount/cparser/parser.h"
#include "beancount/cparser/scanner.h"
#include "beancount/cparser/test_utils.h"

#include <algorithm>
#include <cassert>
#include <limits>
#include <string>
#include <vector>
#include <tuple>

#include "gtest/gtest.h"
#include "reflex/input.h"

namespace beancount {
namespace {

using std::pair;
using std::string;
using std::vector;
using std::tuple;
using testing::Eq;

// TODO(blais): Implement scanner tests here.

/// // TODO(blais): When symbol_name() is made public, make a << operator on the symbol_type.

using symbol = parser::Parser::symbol_kind;
using symbol_type = parser::Parser::symbol_kind::symbol_kind_type;
typedef tuple<symbol_type, int, string> symbol_tuple;
typedef vector<symbol_tuple> symbol_tuples;

#if 0
std::ostream& operator<<(std::ostream& os, const symbol_type& sym) {
  os << parser::Parser::symbol_name(sym);
  return os;
}
#endif

#if 0
// Print a sequence of actual test tokens.
void PrintTokens(const symbol_tuples& tokens) {
  for (const auto& token : tokens) {
    std::cout << std::get<0>(token) << " "
              << std::get<1>(token) << " "
              << std::get<2>(token) << std::endl;
  }
}
#endif

// Read the given string in 'input_string' and return a scanned list of (token,
// name) pairs.
symbol_tuples Tokenize(const string& input_string, bool dedent=true) {
  // Ensure the string is converted to UTF8 by using reflex::Input and
  // instantiate a scanner.
  string clean_string = dedent ? StripAndDedent(input_string) : input_string;
  reflex::Input input(clean_string);
  scanner::Scanner scanner(input, std::cout);

  // Yield and accumulate all the tokens and their matched text.
  symbol_tuples symbols;
  while (true) {
    auto token = scanner.lex();
    if (token.type_get() == symbol::S_YYEOF) {
      break;
    }
    // Note: the matcher will always return the full pattern matched, not the
    // token value. Be aware of this when debugging.
    //
    // Also, you could include the error message for YYerror here. We ignore it
    // for now.
    int lineno = scanner.matcher().lineno();
    symbols.push_back({token.type_get(), lineno, scanner.matcher().text()});
  }
  return symbols;
}

// Basic test checking various tokens.
TEST(ScannerTest, BasicTokens) {
  const char* test = u8R"(
    2013-05-18 2014-01-02 2014/01/02
    Assets:US:Bank:Checking
    Liabilities:US:Bank:Credit
    Other:Bank
    USD HOOL TEST_D TEST_3 TEST-D TEST-3 NT
    "Nice dinner at Mermaid Inn"
    ""
    123 123.45 123.456789 -123 -123.456789
    #sometag123
    ^sometag123
    somekey:
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_DATE,     1, "2013-05-18"},
        {symbol::S_DATE,     1, "2014-01-02"},
        {symbol::S_DATE,     1, "2014/01/02"},
        {symbol::S_EOL,      1, "\n"},
        {symbol::S_ACCOUNT,  2, "Assets:US:Bank:Checking"},
        {symbol::S_EOL,      2, "\n"},
        {symbol::S_ACCOUNT,  3, "Liabilities:US:Bank:Credit"},
        {symbol::S_EOL,      3, "\n"},
        {symbol::S_ACCOUNT,  4, "Other:Bank"},
        {symbol::S_EOL,      4, "\n"},
        {symbol::S_CURRENCY, 5, "USD"},
        {symbol::S_CURRENCY, 5, "HOOL"},
        {symbol::S_CURRENCY, 5, "TEST_D"},
        {symbol::S_CURRENCY, 5, "TEST_3"},
        {symbol::S_CURRENCY, 5, "TEST-D"},
        {symbol::S_CURRENCY, 5, "TEST-3"},
        {symbol::S_CURRENCY, 5, "NT"},
        {symbol::S_EOL,      5, "\n"},
        {symbol::S_STRING,   6, "\"Nice dinner at Mermaid Inn\""},
        {symbol::S_EOL,      6, "\n"},
        {symbol::S_STRING,   7, "\"\""},
        {symbol::S_EOL,      7, "\n"},
        {symbol::S_NUMBER,   8, "123"},
        {symbol::S_NUMBER,   8, "123.45"},
        {symbol::S_NUMBER,   8, "123.456789"},
        {symbol::S_MINUS,    8, "-"},
        {symbol::S_NUMBER,   8, "123"},
        {symbol::S_MINUS,    8, "-"},
        {symbol::S_NUMBER,   8, "123.456789"},
        {symbol::S_EOL,      8, "\n"},
        {symbol::S_TAG,      9, "#sometag123"},
        {symbol::S_EOL,      9, "\n"},
        {symbol::S_LINK,     10, "^sometag123"},
        {symbol::S_EOL,      10, "\n"},
        {symbol::S_KEY,      11, "somekey"},
        {symbol::S_COLON,    11, ":"},
        {symbol::S_EOL,      11, "\n"},
      }));
}

TEST(ScannerTest, UnicodeAccountName) {
  const char* test = u8R"(
    Other:Bank
    Óthяr:Bあnk
    abc1:abc1
    ΑβγⅠ:ΑβγⅠ
    ابجا:ابجا
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_ACCOUNT, 1, u8"Other:Bank"},
        {symbol::S_EOL,     1, u8"\n"},
        //
        {symbol::S_ACCOUNT, 2, u8"Óthяr:Bあnk"},
        {symbol::S_EOL,     2, u8"\n"},
        //
        {symbol::S_KEY,     3, u8"abc1"},
        {symbol::S_COLON,   3, u8":"},
        {symbol::S_YYerror, 3, u8"abc1"},
        {symbol::S_EOL,     3, u8"\n"},
        //
        {symbol::S_ACCOUNT, 4, u8"ΑβγⅠ:ΑβγⅠ"},
        {symbol::S_EOL,     4, u8"\n"},
        //
        {symbol::S_ACCOUNT, 5, u8"ابجا:ابجا"},
        {symbol::S_EOL,     5, u8"\n"},
      }));
}

TEST(ScannerTest, Indent) {
  const char* test = u8R"(
    2014-07-05 *
      Equity:Something
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_DATE,     1, "2014-07-05"},
        {symbol::S_ASTERISK, 1, "*"},
        {symbol::S_EOL,      1, "\n"},
        //
        {symbol::S_INDENT,   2, "  "},
        {symbol::S_ACCOUNT,  2, "Equity:Something"},
        {symbol::S_EOL,      2, "\n"},
        //
        {symbol::S_DEDENT,   3, ""},
      }));
}

TEST(ScannerTest, CommaCurrencies) {
  const char* test = u8R"(
    USD,CAD,AUD
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_CURRENCY, 1, "USD"},
        {symbol::S_COMMA,    1, ","},
        {symbol::S_CURRENCY, 1, "CAD"},
        {symbol::S_COMMA,    1, ","},
        {symbol::S_CURRENCY, 1, "AUD"},
        {symbol::S_EOL,      1, "\n"},
      }));
}

TEST(ScannerTest, NumberOkay) {
  const char* test = u8R"(
    1001 USD
    1002.00 USD
    -1001 USD
    -1002.00 USD
    +1001 USD
    +1002.00 USD
    1,001 USD
    1,002.00 USD
    -1,001 USD
    -1,002.00 USD
    +1,001 USD
    +1,002.00 USD
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_NUMBER,   1, "1001"},
        {symbol::S_CURRENCY, 1, "USD"},
        {symbol::S_EOL,      1, "\n"},
        {symbol::S_NUMBER,   2, "1002.00"},
        {symbol::S_CURRENCY, 2, "USD"},
        {symbol::S_EOL,      2, "\n"},
        {symbol::S_MINUS,    3, "-"},
        {symbol::S_NUMBER,   3, "1001"},
        {symbol::S_CURRENCY, 3, "USD"},
        {symbol::S_EOL,      3, "\n"},
        {symbol::S_MINUS,    4, "-"},
        {symbol::S_NUMBER,   4, "1002.00"},
        {symbol::S_CURRENCY, 4, "USD"},
        {symbol::S_EOL,      4, "\n"},
        {symbol::S_PLUS,     5, "+"},
        {symbol::S_NUMBER,   5, "1001"},
        {symbol::S_CURRENCY, 5, "USD"},
        {symbol::S_EOL,      5, "\n"},
        {symbol::S_PLUS,     6, "+"},
        {symbol::S_NUMBER,   6, "1002.00"},
        {symbol::S_CURRENCY, 6, "USD"},
        {symbol::S_EOL,      6, "\n"},
        {symbol::S_NUMBER,   7, "1,001"},
        {symbol::S_CURRENCY, 7, "USD"},
        {symbol::S_EOL,      7, "\n"},
        {symbol::S_NUMBER,   8, "1,002.00"},
        {symbol::S_CURRENCY, 8, "USD"},
        {symbol::S_EOL,      8, "\n"},
        {symbol::S_MINUS,    9, "-"},
        {symbol::S_NUMBER,   9, "1,001"},
        {symbol::S_CURRENCY, 9, "USD"},
        {symbol::S_EOL,      9, "\n"},
        {symbol::S_MINUS,    10, "-"},
        {symbol::S_NUMBER,   10, "1,002.00"},
        {symbol::S_CURRENCY, 10, "USD"},
        {symbol::S_EOL,      10, "\n"},
        {symbol::S_PLUS,     11, "+"},
        {symbol::S_NUMBER,   11, "1,001"},
        {symbol::S_CURRENCY, 11, "USD"},
        {symbol::S_EOL,      11, "\n"},
        {symbol::S_PLUS,     12, "+"},
        {symbol::S_NUMBER,   12, "1,002.00"},
        {symbol::S_CURRENCY, 12, "USD"},
        {symbol::S_EOL,      12, "\n"},
      }));
}

TEST(ScannerTest, NumberSpace) {
  const char* test = u8R"(
    - 1002.00 USD
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_MINUS,    1, "-"},
        {symbol::S_NUMBER,   1, "1002.00"},
        {symbol::S_CURRENCY, 1, "USD"},
        {symbol::S_EOL,      1, "\n"},
      }));
}

TEST(ScannerTest, NumberDots) {
  const char* test = u8R"(
    1.234.00 USD
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_NUMBER,   1, "1.234"},
        {symbol::S_YYerror,  1, ".00"},
        {symbol::S_CURRENCY, 1, "USD"},
        {symbol::S_EOL,      1, "\n"},
      }));
}

TEST(ScannerTest, NumberNoInteger) {
  const char* test = u8R"(
    .2347 USD
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_YYerror,  1, ".2347"},
        {symbol::S_CURRENCY, 1, "USD"},
        {symbol::S_EOL,      1, "\n"},
      }));
}

TEST(ScannerTest, CurrencyNumber) {
  const char* test = u8R"(
    555.00 CAD.11
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_NUMBER,   1, "555.00"},
        {symbol::S_CURRENCY, 1, "CAD.11"},
        {symbol::S_EOL,      1, "\n"},
      }));
}

TEST(ScannerTest, CurrencyDash) {
  const char* test = u8R"(
    TEST-DA
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_CURRENCY, 1, "TEST-DA"},
        {symbol::S_EOL,      1, "\n"},
      }));
}

TEST(ScannerTest, BadDate) {
  const char* test = u8R"(
    2013-12-98
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_YYerror, 1, "2013-12-98"},
        {symbol::S_EOL,     1, "\n"},
      }));
}

TEST(ScannerTest, DateFollowedByNumber) {
  const char* test = u8R"(
    2013-12-228
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_YYerror, 1, "2013-12-228"},
        {symbol::S_EOL,     1, "\n"},
      }));
}

TEST(ScannerTest, SingleLetterAccount) {
  const char* test = u8R"(
    Assets:A
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_ACCOUNT, 1, "Assets:A"},
        {symbol::S_EOL,     1, "\n"},
      }));
}

TEST(ScannerTest, AccountNamesWithNumbers) {
  const char* test = u8R"(
    Assets:Vouchers:99Ranch
    Assets:99Test
    Assets:signals
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_ACCOUNT, 1, "Assets:Vouchers:99Ranch"},
        {symbol::S_EOL,     1, "\n"},
        {symbol::S_ACCOUNT, 2, "Assets:99Test"},
        {symbol::S_EOL,     2, "\n"},
        {symbol::S_YYerror, 3, "Assets:signals"},
        {symbol::S_EOL,     3, "\n"},
      }));
}

TEST(ScannerTest, AccountNamesWithDash) {
  const char* test = u8R"(
    Equity:Beginning-Balances
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_ACCOUNT, 1, "Equity:Beginning-Balances"},
        {symbol::S_EOL,     1, "\n"},
      }));
}

TEST(ScannerTest, InvalidDirective) {
  const char* test = u8R"(
    2008-03-01 check Assets:BestBank:Savings 2340.19 USD
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_DATE,     1, "2008-03-01"},
        {symbol::S_YYerror,  1, "check"},
        {symbol::S_ACCOUNT,  1, "Assets:BestBank:Savings"},
        {symbol::S_NUMBER,   1, "2340.19"},
        {symbol::S_CURRENCY, 1, "USD"},
        {symbol::S_EOL,      1, "\n"},
      }));
}

TEST(ScannerTest, Comment) {
  const char* test = u8R"(
    ;; This is a typical error that should get detected for long strings.
    2008-03-01
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_EOL,  1, "\n"},
        {symbol::S_DATE, 2, "2008-03-01"},
        {symbol::S_EOL,  2, "\n"},
      }));
}

TEST(ScannerTest, StringTooLong) {
  char empty[20002];
  std::fill_n(empty, 20000, ' ');
  empty[20000] = '\n';
  empty[20001] = '\0';
  const string test = absl::StrCat(
      ";; This is a typical error that should get detected for long strings.\n",
      "2014-01-01 note Assets:Temporary \"Bla bla\" \"\n",
      "2014-02-01 open Liabilities:US:BankWithLongName:Credit-Card:Account01\n",
      empty,
      "2014-02-02 note Assets:Temporary \"Bla bla\"\n");
  const auto symbols = Tokenize(test, false);
  EXPECT_EQ(symbols.size(), 9);
  EXPECT_EQ(std::get<0>(symbols[5]), symbol::S_YYerror);
}

TEST(ScannerTest, VeryLongNumber) {
  char buffer[2048 + 1];
  std::fill_n(buffer, 2048, '1');
  buffer[2047] = '\n';
  buffer[2048] = '\0';
  const auto symbols = Tokenize(buffer, false);
  EXPECT_EQ(symbols.size(), 2);
  EXPECT_EQ(std::get<0>(symbols[0]), symbol::S_NUMBER);
}

TEST(ScannerTest, VeryLongString) {
  char buffer[262144 + 1];
  std::fill_n(buffer, 262144, 'x');
  buffer[0] = '"';
  buffer[262142] = '"';
  buffer[262143] = '\n';
  buffer[262144] = '\0';
  const auto symbols = Tokenize(buffer, false);
  EXPECT_EQ(symbols.size(), 2);
  EXPECT_EQ(std::get<0>(symbols[0]), symbol::S_YYerror);
}

TEST(ScannerTest, NoFinalNewline) {
  const string test = absl::StrCat(
      "2014-01-01 open Assets:Temporary    ");
  const auto symbols = Tokenize(test, false);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_DATE, 1, "2014-01-01"},
        {symbol::S_OPEN, 1, "open"},
        {symbol::S_ACCOUNT, 1, "Assets:Temporary"},
      }));
}

TEST(ScannerTest, StringEscaped) {
  const char* test = R"(
    "The Great \"Juju\""
    "The Great \t\n\r\f\b"
  )";

  std::cout << "test = '" << test << "'" << std::endl;

  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_STRING, 1, R"("The Great "Juju"__")"},
        {symbol::S_EOL,    1, "\n"},
        {symbol::S_STRING, 2, "\"The Great \t\n\r\f\b_____\""},
        {symbol::S_EOL,    2, "\n"},
      }));
}

TEST(ScannerTest, StringNewline) {
  auto test = absl::StrCat(R"("The Great\nJuju")", "\n");
  const auto symbols = Tokenize(test, false);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_STRING, 1, "\"The Great\nJuju_\""},
        {symbol::S_EOL,    1, "\n"},
      }));
}

// TODO(blais): Change string type to std::string and measure performance.
// Remove padding.


//     def test_string_newline_long(self, tokens, errors):
//         '''
//         "Forty
//         world
//         leaders
//         and
//         hundreds"
//         '''
//         # Note that this test contains an _actual_ newline, not an escape one as
//         # in the previous test. This should allow us to parse multiline strings.
//         self.assertEqual([
//             {symbol::S_EOL, 2, "\n"},
//             ('STRING', 6,
//              b'"Forty\nworld\nleaders\nand\nhundreds"',
//              'Forty\nworld\nleaders\nand\nhundreds'),
//             {symbol::S_EOL, 7, "\n"},
//             ], tokens)
//         self.assertFalse(errors)
//
//     def test_string_newline_toolong(self):
//         # Testing a string that busts the limits.
//         line = 'a' * 127 + '\n'
//         string = '"' + line * 128 + '"\n'
//         builder = lexer.LexBuilder()
//         tokens = list(lexer.lex_iter_string(string, builder))
//         self.assertTrue(tokens[0], 'error')
//         self.assertTrue(tokens[1], 'EOL')
//
//     @lex_tokens
//     def test_popmeta(self, tokens, errors):
//         '''
//         popmeta location:
//         '''
//         self.assertEqual([
//             {symbol::S_EOL, 2, "\n"},
//             {symbol::S_POPMETA, 2, "popmeta"},
//             {symbol::S_KEY, 2, "location"},
//             {symbol::S_COLON, 2, ":"},
//             {symbol::S_EOL, 3, "\n"},
//         ], tokens)
//         self.assertFalse(errors)
//
//     @lex_tokens
//     def test_null_true_false(self, tokens, errors):
//         '''
//         TRUE FALSE NULL
//         '''
//         self.assertEqual([
//             {symbol::S_EOL, 2, "\n"},
//             {symbol::S_BOOL, 2, "TRUE"},
//             {symbol::S_BOOL, 2, "FALSE"},
//             {symbol::S_NONE, 2, "NULL"},
//             {symbol::S_EOL, 3, "\n"},
//         ], tokens)
//         self.assertFalse(errors)
//
//
// class TestIgnoredLines(unittest.TestCase):
//
//     @lex_tokens
//     def test_ignored__long_comment(self, tokens, errors):
//         """
//         ;; Long comment line about something something.
//         """
//         self.assertEqual([
//             {symbol::S_EOL, 2, "\n"},
//             {symbol::S_EOL, 3, "\n"},
//             ], tokens)
//         self.assertFalse(errors)
//
//     @lex_tokens
//     def test_ignored__indented_comment(self, tokens, errors):
//         """
//         option "title" "The Title"
//           ;; Something something.
//         """
//         self.assertEqual([
//             {symbol::S_EOL, 2, "\n"},
//             {symbol::S_OPTION, 2, "option"},
//             {symbol::S_STRING, 2, ""title""},
//             {symbol::S_STRING, 2, ""The Title""},
//             {symbol::S_EOL, 3, "\n"},
//             {symbol::S_INDENT, 3, "  "},
//             {symbol::S_EOL, 4, "\n"},
//         ], tokens)
//         self.assertFalse(errors)
//
//     @lex_tokens
//     def test_ignored__something_else(self, tokens, errors):
//         """
//         Regular prose appearing mid-file which starts with a flag character.
//         """
//         self.assertEqual([
//             {symbol::S_EOL, 2, "\n"},
//             {symbol::S_EOL, 3, "\n"},
//             ], tokens)
//         self.assertFalse(errors)
//
//     @lex_tokens
//     def test_ignored__something_else_non_flag(self, tokens, errors):
//         """
//         Xxx this sentence starts with a non-flag character.
//         """
//         self.assertTrue(errors)
//
//     @lex_tokens
//     def test_ignored__org_mode_title(self, tokens, errors):
//         """
//         * This sentence is an org-mode title.
//         """
//         self.assertEqual([
//             {symbol::S_EOL, 2, "\n"},
//             {symbol::S_EOL, 3, "\n"},
//         ], tokens)
//         self.assertFalse(errors)
//
//     @lex_tokens
//     def test_ignored__org_mode_drawer(self, tokens, errors):
//         """
//         :PROPERTIES:
//         :this: is an org-mode property drawer
//         :END:
//         """
//         self.assertEqual([
//             {symbol::S_EOL, 2, "\n"},
//             {symbol::S_EOL, 3, "\n"},
//             {symbol::S_EOL, 4, "\n"},
//             {symbol::S_EOL, 5, "\n"},
//         ], tokens)
//         self.assertFalse(errors)
//
//
// class TestLexerErrors(unittest.TestCase):
//     """Test lexer error handling.
//     """
//
//     @lex_tokens
//     def test_lexer_invalid_token(self, tokens, errors):
//         """
//           2000-01-01 open ` USD
//         """
//         self.assertEqual([{symbol::S_EOL, 2, "\n"},
//                           ('DATE', 2, b'2000-01-01', datetime.date(2000, 1, 1)),
//                           {symbol::S_OPEN, 2, "open"},
//                           ('error', 2, b'`', None),
//                           {symbol::S_CURRENCY, 2, "USD"},
//                           ('EOL', 3, b'\n', None)], tokens)
//         self.assertEqual(1, len(errors))
//
//     @lex_tokens
//     def test_lexer_exception__recovery(self, tokens, errors):
//         """
//           2000-13-32 open Assets:Something
//
//           2000-01-02 open Assets:Working
//         """
//         self.assertEqual([{symbol::S_EOL, 2, "\n"},
//                           ('error', 2, b'2000-13-32', None),
//                           {symbol::S_OPEN, 2, "open"},
//                           {symbol::S_ACCOUNT, 2, "Assets:Something"},
//                           {symbol::S_EOL, 3, "\n"},
//                           {symbol::S_EOL, 4, "\n"},
//                           ('DATE', 4, b'2000-01-02', datetime.date(2000, 1, 2)),
//                           {symbol::S_OPEN, 4, "open"},
//                           {symbol::S_ACCOUNT, 4, "Assets:Working"},
//                           ('EOL', 5, b'\n', None)], tokens)
//         self.assertEqual(1, len(errors))
//
//     @lex_tokens
//     def test_lexer_exception_DATE(self, tokens, errors):
//         """
//           2000-13-32 open Assets:Something
//         """
//         self.assertEqual([{symbol::S_EOL, 2, "\n"},
//                           ('error', 2, b'2000-13-32', None),
//                           {symbol::S_OPEN, 2, "open"},
//                           {symbol::S_ACCOUNT, 2, "Assets:Something"},
//                           ('EOL', 3, b'\n', None)], tokens)
//         self.assertEqual(1, len(errors))
//
//     def test_lexer_exception_substring_with_quotes(self):
//         test_input = """
//           2016-07-15 query "hotels" "SELECT * WHERE account ~ 'Expenses:Accommodation'"
//         """
//         builder = lexer.LexBuilder()
//         tokens = list(lexer.lex_iter_string(textwrap.dedent(test_input), builder))
//         self.assertEqual([
//             {symbol::S_EOL, 2, "\n"},
//             ('DATE', 2, b'2016-07-15', datetime.date(2016, 7, 15)),
//             {symbol::S_QUERY, 2, "query"},
//             {symbol::S_STRING, 2, ""hotels""},
//             ('STRING', 2, b'"SELECT * WHERE account ~ \'Expenses:Accommodation\'"',
//              'SELECT * WHERE account ~ \'Expenses:Accommodation\''),
//             ('EOL', 3, b'\n', None)], tokens)
//         self.assertEqual(0, len(builder.errors))
//
//
// class TestLexerUnicode(unittest.TestCase):
//
//     test_utf8_string = textwrap.dedent("""
//       2015-05-23 note Assets:Something "a¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼ z"
//     """)
//     expected_utf8_string = "a¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼ z"
//
//     test_latin1_string = textwrap.dedent("""
//       2015-05-23 note Assets:Something "école Floß søllerød"
//     """)
//     expected_latin1_string = "école Floß søllerød"
//
//     # Test providing utf8 bytes to the lexer.
//     def test_bytes_encoded_utf8(self):
//         utf8_bytes = self.test_utf8_string.encode('utf8')
//         builder = lexer.LexBuilder()
//         tokens = list(lexer.lex_iter_string(utf8_bytes, builder))
//
//         # The lexer outputs no errors.
//         self.assertFalse(builder.errors)
//
//         # Check that the lexer correctly parsed the UTF8 string.
//         str_tokens = [token for token in tokens if token[0] == 'STRING']
//         self.assertEqual(self.expected_utf8_string, str_tokens[0][3])
//
//     # Test providing latin1 bytes to the lexer when it is expecting utf8.
//     def test_bytes_encoded_latin1_invalid(self):
//         latin1_bytes = self.test_utf8_string.encode('latin1')
//         builder = lexer.LexBuilder()
//         tokens = list(lexer.lex_iter_string(latin1_bytes, builder))
//
//         # The lexer outputs no errors.
//         self.assertFalse(builder.errors)
//
//         # Check that the lexer failed to convert the string but did not cause
//         # other errors.
//         str_tokens = [token for token in tokens if token[0] == 'STRING']
//         self.assertNotEqual(self.expected_utf8_string, str_tokens[0][3])
//
//     # Test providing latin1 bytes to the lexer with an encoding.
//     def test_bytes_encoded_latin1(self):
//         latin1_bytes = self.test_latin1_string.encode('latin1')
//         builder = lexer.LexBuilder()
//         tokens = list(lexer.lex_iter_string(latin1_bytes, builder, encoding='latin1'))
//
//         # The lexer outputs no errors.
//         self.assertFalse(builder.errors)
//
//         # Check that the lexer correctly parsed the latin1 string.
//         str_tokens = [token for token in tokens if token[0] == 'STRING']
//         self.assertEqual(self.expected_latin1_string, str_tokens[0][3])
//
//     # Test providing utf16 bytes to the lexer when it is expecting utf8.
//     def test_bytes_encoded_utf16_invalid(self):
//         utf16_bytes = self.test_utf8_string.encode('utf16')
//         builder = lexer.LexBuilder()
//         tokens = list(lexer.lex_iter_string(utf16_bytes, builder))
//         self.assertTrue(builder.errors)
//
//
// class TestLexerMisc(unittest.TestCase):
//
//     @lex_tokens
//     def test_valid_commas_in_number(self, tokens, errors):
//         """\
//           45,234.00
//         """
//         self.assertEqual([
//             ('NUMBER', 1, b'45,234.00', D('45234.00')),
//             {symbol::S_EOL, 2, "\n"},
//         ], tokens)
//         self.assertEqual(0, len(errors))
//
//     @lex_tokens
//     def test_invalid_commas_in_integral(self, tokens, errors):
//         """\
//           452,34.00
//         """
//         self.assertEqual(1, len(errors))
//         self.assertEqual([
//             ('error', 1, b'452,34.00', None),
//             {symbol::S_EOL, 2, "\n"},
//         ], tokens)
//
//     @lex_tokens
//     def test_invalid_commas_in_fractional(self, tokens, errors):
//         """\
//           45234.000,000
//         """
//         # Unfortunately this is going to get parsed as two numbers but that will
//         # cause an error downstream in the parser. Nevertheless, keep this test
//         # case here in case eventually we improve the lexer.
//         self.assertEqual(0, len(errors))
//         self.assertEqual([
//             ('NUMBER', 1, b'45234.000', D('45234.000')),
//             {symbol::S_COMMA, 1, ","},
//             ('NUMBER', 1, b'000', D('0')),
//             {symbol::S_EOL, 2, "\n"},
//         ], tokens)
}  // namespace
}  // namespace beancount
