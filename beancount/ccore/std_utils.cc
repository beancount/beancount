// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/std_utils.h"

#include "absl/strings/ascii.h"

namespace beancount {

std::string Capitalize(std::string_view s) {
  std::string result(s);
  if (!s.empty()) {
    result[0] = absl::ascii_toupper(result[0]);
  }
  return result;
}

}  // namespace beancount
