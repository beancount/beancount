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
using std::string;
using symbol = parser::Parser::symbol_kind;
using symbol_type = parser::Parser::symbol_kind::symbol_kind_type;
using testing::Eq;

typedef std::tuple<symbol_type, int, string> symbol_tuple;
typedef std::vector<symbol_tuple> symbol_tuples;

// Print a sequence of actual test tokens.
void PrintTokens(const symbol_tuples& tokens) {
  std::cout << ",--------------------------------" << std::endl;
  for (const auto& token : tokens) {
    std::cout << std::get<1>(token) << " "
              << parser::Parser::symbol_name(std::get<0>(token)) << " "
              << std::get<2>(token) << std::endl;
  }
  std::cout << "`--------------------------------" << std::endl;
}

// Read the given string in 'input_string' and return a scanned list of (token,
// name) pairs.
symbol_tuples Tokenize(const string& input_string, bool no_dedent=false) {
  // Ensure the string is converted to UTF8 by using reflex::Input and
  // instantiate a scanner.
  string clean_string = no_dedent ? input_string : StripAndDedent(input_string);
  reflex::Input input(clean_string);
  scanner::Scanner scanner(input, std::cout);

  // Yield and accumulate all the tokens and their matched text.
  symbol_tuples symbols;
  while (true) {
    try {
      parser::Parser::symbol_type token = scanner.lex();
      auto token_type = token.type_get();
      if (token_type == symbol::S_YYEOF) {
        break;
      }
      // Note: the matcher will always return the full pattern matched, not the
      // token value. Be aware of this when debugging.
      //
      // Also, you could include the error message for YYerror here. We ignore it
      // for now.
      int lineno = scanner.matcher().lineno();

      // Get the original text matched in the input stream, or, if this is a
      // STRING typed token, the resultant parsed string included.
      string value;
      if (token_type == symbol::S_YYerror ||
          token_type == symbol::S_ACCOUNT ||
          token_type == symbol::S_CURRENCY ||
          token_type == symbol::S_STRING ||
          token_type == symbol::S_TAG ||
          token_type == symbol::S_LINK ||
          token_type == symbol::S_KEY) {
        value = token.value.as<std::string>();
      } else {
        value = scanner.matcher().str();
      }

      // Add the symbol to the list.
      symbols.push_back({token.type_get(), lineno, value});
    }
    catch (const parser::Parser::syntax_error& exc) {
      symbols.push_back({
          symbol::S_YYerror,
          scanner.matcher().lineno(),
          absl::StrCat("Syntax error: ", exc.what())});
      scanner.matcher().input();
    }
  }
  return symbols;
}

// Basic test checking various tokens.
TEST(ScannerTest, BasicTokens) {
  const char* test = u8R"(
    2013-05-18 2014-01-02 2014/01/02
    13:18 2013-05-18 12:34:56 2013-05-18 12:34
    Assets:US:Bank:Checking
    Liabilities:US:Bank:Credit
    Other:Bank
    USD HOOL TEST_D TEST_3 TEST-D TEST-3 NT V V12
    /NQH21 /6A /6J8 ABC.TO /3.2
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
        {symbol::S_NUMBER,   2, "13"},
        {symbol::S_COLON,    2, ":"},
        {symbol::S_NUMBER,   2, "18"},
        {symbol::S_DATETIME, 2, "2013-05-18 12:34:56"},
        {symbol::S_DATETIME, 2, "2013-05-18 12:34"},
        {symbol::S_EOL,      2, "\n"},
        {symbol::S_ACCOUNT,  3, "Assets:US:Bank:Checking"},
        {symbol::S_EOL,      3, "\n"},
        {symbol::S_ACCOUNT,  4, "Liabilities:US:Bank:Credit"},
        {symbol::S_EOL,      4, "\n"},
        {symbol::S_ACCOUNT,  5, "Other:Bank"},
        {symbol::S_EOL,      5, "\n"},
        {symbol::S_CURRENCY, 6, "USD"},
        {symbol::S_CURRENCY, 6, "HOOL"},
        {symbol::S_CURRENCY, 6, "TEST_D"},
        {symbol::S_CURRENCY, 6, "TEST_3"},
        {symbol::S_CURRENCY, 6, "TEST-D"},
        {symbol::S_CURRENCY, 6, "TEST-3"},
        {symbol::S_CURRENCY, 6, "NT"},
        {symbol::S_CURRENCY, 6, "V"},
        {symbol::S_CURRENCY, 6, "V12"},
        {symbol::S_EOL,      6, "\n"},
        {symbol::S_CURRENCY, 7, "/NQH21"},
        {symbol::S_CURRENCY, 7, "/6A"},
        {symbol::S_CURRENCY, 7, "/6J8"},
        {symbol::S_CURRENCY, 7, "ABC.TO"},
        {symbol::S_SLASH,    7, "/"},
        {symbol::S_NUMBER,   7, "3.2"},
        {symbol::S_EOL,      7, "\n"},
        {symbol::S_STRING,   8, "Nice dinner at Mermaid Inn"},
        {symbol::S_EOL,      8, "\n"},
        {symbol::S_STRING,   9, ""},
        {symbol::S_EOL,      9, "\n"},
        {symbol::S_NUMBER,   10, "123"},
        {symbol::S_NUMBER,   10, "123.45"},
        {symbol::S_NUMBER,   10, "123.456789"},
        {symbol::S_MINUS,    10, "-"},
        {symbol::S_NUMBER,   10, "123"},
        {symbol::S_MINUS,    10, "-"},
        {symbol::S_NUMBER,   10, "123.456789"},
        {symbol::S_EOL,      10, "\n"},
        {symbol::S_TAG,      11, "sometag123"},
        {symbol::S_EOL,      11, "\n"},
        {symbol::S_LINK,     12, "sometag123"},
        {symbol::S_EOL,      12, "\n"},
        {symbol::S_KEY,      13, "somekey"},
        {symbol::S_COLON,    13, ":"},
        {symbol::S_EOL,      13, "\n"},
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
        {symbol::S_YYUNDEF, 3, u8"abc1"},
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
        {symbol::S_YYUNDEF,  1, ".00"},
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
        {symbol::S_YYUNDEF,  1, ".2347"},
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

TEST(ScannerTest, BadDateInvalidToken) {
  const char* test = u8R"(
    2013-12-98
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_YYUNDEF, 1, "2013-12-98"},
        {symbol::S_EOL,     1, "\n"},
      }));
}

TEST(ScannerTest, BadDateValidButInvalid) {
  const char* test = u8R"(
    2013-15-01
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_YYUNDEF, 1, "2013-15-01"},
        {symbol::S_EOL,     1, "\n"},
      }));
}

TEST(ScannerTest, DateFollowedByNumber) {
  // Because we care about word boundary on the date, this parses as an
  // arithmetic expression.
  const char* test = u8R"(
    2013-12-228
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_NUMBER, 1, "2013"},
        {symbol::S_MINUS,  1, "-"},
        {symbol::S_NUMBER, 1, "12"},
        {symbol::S_MINUS,  1, "-"},
        {symbol::S_NUMBER, 1, "228"},
        {symbol::S_EOL,    1, "\n"},
      }));
}

TEST(ScannerTest, BadTime) {
  const char* test = u8R"(
    99:99
    2000-09-10 99:99
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_NUMBER,  1, "99"},
        {symbol::S_COLON,   1, ":"},
        {symbol::S_NUMBER,  1, "99"},
        {symbol::S_EOL,     1, "\n"},
        {symbol::S_YYUNDEF, 2, "2000-09-10 99:99"},
        {symbol::S_EOL,     2, "\n"},
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
        {symbol::S_YYUNDEF, 3, "Assets:signals"},
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
        {symbol::S_YYUNDEF,  1, "check"},
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
  const auto symbols = Tokenize(test, true);
  EXPECT_EQ(symbols.size(), 9);
  EXPECT_EQ(std::get<0>(symbols[5]), symbol::S_YYUNDEF);
}

TEST(ScannerTest, VeryLongNumber) {
  char buffer[2048 + 1];
  std::fill_n(buffer, 2048, '1');
  buffer[2047] = '\n';
  buffer[2048] = '\0';
  const auto symbols = Tokenize(buffer, true);
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
  const auto symbols = Tokenize(buffer, true);
  EXPECT_EQ(symbols.size(), 2);
  EXPECT_EQ(std::get<0>(symbols[0]), symbol::S_YYUNDEF);
}

TEST(ScannerTest, NoFinalNewline) {
  const string test = absl::StrCat(
      "2014-01-01 open Assets:Temporary    ");
  const auto symbols = Tokenize(test, true);
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
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_STRING, 1, "The Great \"Juju\""},
        {symbol::S_EOL,    1, "\n"},
        {symbol::S_STRING, 2, "The Great \t\n\r\f\b"},
        {symbol::S_EOL,    2, "\n"},
      }));
}

TEST(ScannerTest, StringNewline) {
  auto test = absl::StrCat(R"("The Great\nJuju")", "\n");
  const auto symbols = Tokenize(test, true);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_STRING, 1, "The Great\nJuju"},
        {symbol::S_EOL,    1, "\n"},
      }));
}

TEST(ScannerTest, StringNewlineLong) {
  // Note that this test contains an _actual_ newline, not an escape one as in
  // the previous test. This should allow us to parse multiline strings.
  const char* test = R"(
    "Forty
    world
    leaders
    and
    hundreds"
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
            {symbol::S_STRING, 1, "Forty\nworld\nleaders\nand\nhundreds"},
            {symbol::S_EOL, 5, "\n"},
      }));
}

TEST(ScannerTest, StringNewlineTooLong) {
  // Testing a string that busts the limits.
  char line[128];
  std::fill_n(line, 127, 'a');
  line[127] = '\0';
  std::ostringstream oss;
  for (int ii = 0; ii < 128; ++ii) {
    oss << line << std::endl;
  }
  const auto symbols = Tokenize(oss.str(), true);
  EXPECT_EQ(256, symbols.size());
  EXPECT_EQ(symbol::S_YYUNDEF, std::get<0>(symbols[0]));
  EXPECT_EQ(symbol::S_EOL, std::get<0>(symbols[1]));
}

TEST(ScannerTest, InvalidCharacter) {
  // Testing a string with invalid characters.
  const char* test  = "\x80\n";
  const auto symbols = Tokenize(test, true);
  EXPECT_EQ(symbols, symbol_tuples({
        // This is translated by the exception handler in the Tokenize() routine
        // above. I'm not exactly sure why we find two produced here.
        {symbol::S_YYerror, 1, "Syntax error: Unknown token: ''"},
        {symbol::S_YYerror, 1, "Syntax error: Unknown token: ''"},
      }));
}

TEST(ScannerTest, PopMeta) {
  const char* test = R"(
    popmeta location:
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_POPMETA, 1, "popmeta"},
        {symbol::S_KEY,     1, "location"},
        {symbol::S_COLON,   1, ":"},
        {symbol::S_EOL,     1, "\n"},
      }));
}

TEST(ScannerTest, TrueFalseNull) {
  const char* test = R"(
    TRUE FALSE NULL
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_BOOL, 1, "TRUE"},
        {symbol::S_BOOL, 1, "FALSE"},
        {symbol::S_NONE, 1, "NULL"},
        {symbol::S_EOL,  1, "\n"},
      }));
}

TEST(ScannerTest, ValidCommasInNumber) {
  const char* test = R"(
    45,234.00
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_NUMBER, 1, "45,234.00"},
        {symbol::S_EOL,    1, "\n"},
      }));
}

TEST(ScannerTest, InvalidCommasInInteger) {
  const char* test = R"(
    452,34.00
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_NUMBER, 1, "452,34.00"},
        {symbol::S_EOL,    1, "\n"},
      }));
}

TEST(ScannerTest, InvalidCommasInFractional) {
  // Unfortunately this is going to get parsed as two numbers but that will
  // cause an error downstream in the parser. Nevertheless, keep this test case
  // here in case eventually we improve the lexer.
  const char* test = R"(
    45234.000,000
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_NUMBER, 1, "45234.000"},
        {symbol::S_COMMA,  1, ","},
        {symbol::S_NUMBER, 1, "000"},
        {symbol::S_EOL,    1, "\n"},
      }));
}

// Tests for ignored lines.

TEST(IgnoredLinesTest, TrueFalseNull) {
  const char* test = R"(
    ;; Long comment line about something something.
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_EOL, 1, "\n"},
      }));
}

TEST(IgnoredLinesTest, IndentedComment) {
  const char* test = R"(
    option "title" "The Title"
      ;; Something something.
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_OPTION, 1, "option"},
        {symbol::S_STRING, 1, "title"},
        {symbol::S_STRING, 1, "The Title"},
        {symbol::S_EOL, 1, "\n"},
        {symbol::S_INDENT, 2, "  "},
        {symbol::S_EOL, 2, "\n"},
        {symbol::S_DEDENT, 3, ""},
      }));
}

TEST(IgnoredLinesTest, NonCommentIgnored) {
  const char* test = R"(
    Regular prose appearing mid-file.
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_YYUNDEF, 1, "Regular"},
        {symbol::S_YYUNDEF, 1, "prose"},
        {symbol::S_YYUNDEF, 1, "appearing"},
        {symbol::S_YYUNDEF, 1, "mid-file."},
        {symbol::S_EOL, 1, "\n"},
      }));
}

TEST(IgnoredLinesTest, NonCommentNonFlag) {
  const char* test = R"(
    Xxx this sentence starts with a non-flag character.
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols.size(), 9);
  EXPECT_EQ(symbol::S_YYUNDEF, std::get<0>(symbols[0]));
  EXPECT_EQ(symbol::S_YYUNDEF, std::get<0>(symbols[1]));
  EXPECT_EQ(symbol::S_EOL, std::get<0>(symbols.back()));
}

TEST(IgnoredLinesTest, NonCommentOrgModeTitle) {
  const char* test = R"(
    * This sentence is an org-mode title.
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_EOL, 1, "\n"},
      }));
}

TEST(IgnoredLinesTest, NonCommentOrgModeDrawer) {
  const char* test = R"(
    :PROPERTIES:
    :this: is an org-mode property drawer
    :END:
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_EOL, 1, "\n"},
        {symbol::S_EOL, 2, "\n"},
        {symbol::S_EOL, 3, "\n"},
      }));
}

// Test lexer error handling.

TEST(LexerErrorsTest, InvalidToken) {
  const char* test = R"(
    2000-01-01 open ` USD
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_DATE, 1, "2000-01-01"},
        {symbol::S_OPEN, 1, "open"},
        {symbol::S_YYUNDEF, 1, "`"},
        {symbol::S_CURRENCY, 1, "USD"},
        {symbol::S_EOL, 1, "\n"},
      }));
}

TEST(LexerErrorsTest, ErrorRecovery) {
  const char* test = R"(
    2000-13-32 open Assets:Something
    2000-01-02 open Assets:Working
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_YYUNDEF, 1, "2000-13-32"},
        {symbol::S_OPEN,    1, "open"},
        {symbol::S_ACCOUNT, 1, "Assets:Something"},
        {symbol::S_EOL,     1, "\n"},
        {symbol::S_DATE,    2, "2000-01-02"},
        {symbol::S_OPEN,    2, "open"},
        {symbol::S_ACCOUNT, 2, "Assets:Working"},
        {symbol::S_EOL,     2, "\n"},
    }));
}

TEST(LexerErrorsTest, ErrorSubstringWithQuotes) {
  const char* test = R"(
    2016-07-15 query "hotels" "SELECT * WHERE account ~ 'Expenses:Accommodation'"
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_DATE,   1, "2016-07-15"},
        {symbol::S_QUERY,  1, "query"},
        {symbol::S_STRING, 1, "hotels"},
        {symbol::S_STRING, 1, "SELECT * WHERE account ~ \'Expenses:Accommodation\'"},
        {symbol::S_EOL,    1, "\n"},
      }));
}

// Unicode tests.
// TODO(blais): Also test with Latin1 and UTF16 inputs.

TEST(LexerUnicodeTest, EncodedUTF8) {
  const char* test = u8R"(
    2015-05-23 note Assets:Something "a¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼ z"
  )";
  const auto symbols = Tokenize(test);
  EXPECT_EQ(symbols, symbol_tuples({
        {symbol::S_DATE,    1, "2015-05-23"},
        {symbol::S_NOTE,    1, "note"},
        {symbol::S_ACCOUNT, 1, "Assets:Something"},
        {symbol::S_STRING,  1, u8"a¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼ z"},
        {symbol::S_EOL,     1, "\n"},
      }));
}

// Cut-n-paste bits and bobs here as needed.
TEST(PlaceholderTest, DebugPrint) {
#if 0
  const std::string test = absl::StrCat(
    "2013-05-18 * \"Nice dinner at Mermaid Inn\"\n",
    "  Expenses:Restaurant         100 USD\n",
    "  Assets:US:Cash\n",
    "  ;;");
#endif
  const std::string test = R"(
    Account:*:Bla
  )";
  const auto symbols = Tokenize(test, true);
  PrintTokens(symbols);
}

}  // namespace
}  // namespace beancount
