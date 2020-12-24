#include "beancount/cparser/test_utils.h"
#if 0
#include "beancount/data.pb.h"
#endif

#include <algorithm>
#include <cassert>
#include <iostream>
#include <limits>
#include <string>
#include <vector>

#include "absl/strings/ascii.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "absl/strings/str_split.h"
#include "google/protobuf/text_format.h"
#include "google/protobuf/util/message_differencer.h"

namespace beancount {
using std::string_view;
using google::protobuf::TextFormat;
using google::protobuf::util::MessageDifferencer;
using std::cout;
using std::endl;
using std::pair;
using std::string;
using std::vector;

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

string StripAndDedent(const string_view& input_string) {
  vector<string> lines = absl::StrSplit(input_string, "\n");

  // Check that the first two lines are empty and remove them.
  assert(lines.size() >= 2);
  assert(absl::StripAsciiWhitespace(lines.front()).empty());
  assert(absl::StripAsciiWhitespace(lines.back()).empty());
  lines.erase(lines.begin());
  lines.pop_back();
  // for (auto line : lines) { std::cout << "[" << line << "]" << std::endl; }

  // Dedent the lines.
  int min_spaces = std::numeric_limits<int>::max();
  for (const auto& line : lines) {
    int num_spaces = CountIndentSpaces(line);
    if (num_spaces < min_spaces)
      min_spaces = num_spaces;
  }
  vector<string> dedented_lines;
  dedented_lines.reserve(lines.size());
  for (const auto& line : lines) {
    dedented_lines.push_back(line.substr(min_spaces, string::npos));
  }
  return absl::StrCat(absl::StrJoin(dedented_lines, "\n"), "\n");
}

template <typename T>
bool CompareMessages(const T& actual,
                     std::string_view expected_proto) {
  // TODO(blais): Remove string() when protobuf is upgraded.
  T expected;
  assert(TextFormat::ParseFromString(string(expected_proto), &expected));
  bool succ = MessageDifferencer::Equals(expected, actual);
  if (!succ) {
    // Print actual output.
    cout << ",--------------------------- Actual" << endl;
    cout << actual.DebugString() << endl;
    cout << "---------------------------- Expected" << endl;
    cout << expected.DebugString() << endl;
    cout << "`-----------------------------------'" << endl;
  }
  return succ;
}







#if 0
// Explicit instantiation.
template bool CompareMessages(const beancount::proto::Database& actual,
                              std::string_view expected_proto);

void ClearLineNumbers(proto::Database* db) {
  for (auto& typ : *db->mutable_type()) {
    if (typ.has_lineno()) {
      typ.clear_lineno();
    }
  }
  for (auto& obj : *db->mutable_object()) {
    if (obj.has_lineno()) {
      obj.clear_lineno();
    }
  }
}
#endif

}  // namespace beancount
