#include "beancount/cparser/test_utils.h"

#include <cassert>
#include <iostream>
#include <limits>
#include <string>
#include <vector>

#include "absl/strings/ascii.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "absl/strings/str_split.h"
#include "google/protobuf/util/message_differencer.h"

namespace beancount {
using google::protobuf::util::MessageDifferencer;
using std::cerr;
using std::endl;
using std::string;

namespace {

// Return the number of whitespace characters leading the string.
inline int CountIndentSpaces(const string& line) {
  int index = 0;
  for (const char c : line) {
    if (c != ' ')
      return index;
    index++;
  }
  return 0;
}

}

string StripAndDedent(std::string_view input_string) {
  if (input_string.empty()) {
    return string(input_string);
  }
  std::vector<string> lines = absl::StrSplit(input_string, "\n");

  // Check that the first two lines are empty and remove them.
  assert(lines.size() >= 2);
  assert(absl::StripAsciiWhitespace(lines.front()).empty());
  assert(absl::StripAsciiWhitespace(lines.back()).empty());
  lines.erase(lines.begin());
  lines.pop_back();

  // Dedent the lines.
  int min_column = std::numeric_limits<int>::max();
  for (const auto& line : lines) {
    if (line.empty()) {
      continue;
    }
    int num_spaces = CountIndentSpaces(line);
    if (num_spaces < min_column) {
      min_column = num_spaces;
    }
  }
  std::vector<string> dedented_lines;
  if (min_column == 0) {
    dedented_lines = std::move(lines);
  } else {
    dedented_lines.reserve(lines.size());
    for (const auto& line : lines) {
      dedented_lines.push_back(line.size() >= min_column ?
                               line.substr(min_column, string::npos) : line);
    }
  }
  return absl::StrCat(absl::StrJoin(dedented_lines, "\n"), "\n");
}

bool EqualsMessages(const google::protobuf::Message& expected,
                    const google::protobuf::Message& actual) {
  bool succ = MessageDifferencer::Equals(expected, actual);
  if (!succ) {
    // Print actual output.
    cerr << ",--------------------------- Actual" << endl;
    cerr << actual.DebugString() << endl;
    cerr << "---------------------------- Expected" << endl;
    cerr << expected.DebugString() << endl;
    cerr << "`-----------------------------------'" << endl;
  }
  return succ;
}

template <typename T>
void ClearLocation(T* parent, bool leave_errors) {
  if (!parent->has_location()) {
    return;
  }
  if (leave_errors) {
    parent->mutable_location()->clear_filename();
    parent->mutable_location()->clear_lineno_end();
  } else {
    parent->clear_location();
  }
}

void ClearLineNumbers(Ledger* ledger, bool leave_errors) {
  for (auto* dir : ledger->directives) {
    ClearLocation(dir, false);
    if (dir->has_transaction()) {
      for (auto& posting : *dir->mutable_transaction()->mutable_postings()) {
        ClearLocation(&posting, false);
      }
    }
  }
  for (auto* error : ledger->errors) {
    ClearLocation(error, leave_errors);
  }
}

}  // namespace beancount
