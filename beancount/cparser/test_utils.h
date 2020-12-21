#ifndef BEANCOUNT_CPARSER_TEST_UTILS_H_
#define BEANCOUNT_CPARSER_TEST_UTILS_H_

#include <string>

#include "absl/strings/string_view.h"
#include "google/protobuf/message.h"

namespace beancount {

// Ditch the first and last lines and dedent the remaining lines.
// This is useful for writing unit tests.
std::string StripAndDedent(const absl::string_view& input_string);

// Compare the given message with the parsed text proto version.
// Returns true on success.
template <typename T>
bool CompareMessages(const T& message, absl::string_view expected_proto);



#if 0
// Clear all the line number fields. Mutates the proto in place.
// This is useful for testing.
void ClearLineNumbers(proto::Database* db);
#endif

}  //  namespace beancount

#endif  // BEANCOUNT_CPARSER_TEST_UTILS_H_
