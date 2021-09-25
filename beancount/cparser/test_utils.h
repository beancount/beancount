// Common test utilities for the parser.
//
// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#ifndef BEANCOUNT_CPARSER_TEST_UTILS_H_
#define BEANCOUNT_CPARSER_TEST_UTILS_H_

#include "beancount/cparser/ledger.h"

#include <stdexcept>
#include <string>

#include "absl/strings/string_view.h"
#include "google/protobuf/message.h"
#include "google/protobuf/text_format.h"

namespace beancount {

// Ditch the first and last lines and dedent the remaining lines.
// This is useful for writing unit tests.
std::string StripAndDedent(std::string_view input_string);

// Compare two parsed messages.
bool EqualsMessages(const google::protobuf::Message& expected,
                    const google::protobuf::Message& actual,
                    bool partial);

// Compare the given message with the parsed text proto version.
// Returns true on success.
template <typename T>
bool EqualsMessages(const T& actual, std::string_view expected_proto_string,
                    bool partial = false) {
  T expected;
  if (!google::protobuf::TextFormat::ParseFromString(
          // TODO(blais): Remove string() when protobuf is upgraded.
          std::string(expected_proto_string),
          &expected)) {
    throw std::domain_error("Could not parse expected proto.");
  }
  return EqualsMessages(expected, actual, partial);
}

// Clear all the line number fields in the parsed leder. Mutates the protos in
// place. This is useful for testing.
void ClearLineNumbers(Ledger* ledger, bool leave_start=false);

}  //  namespace beancount

#endif  // BEANCOUNT_CPARSER_TEST_UTILS_H_
