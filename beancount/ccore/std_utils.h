// Common definitions in global namespace. This is a small enough codebase, we
// assume the presence of some limited list of common datatypes without having
// to pepper the entire codebase with std prefixes.
//
// Copyright (C) 2013-2016,2020  Martin Blais
// License: "GNU GPLv2"

#ifndef BEANCOUNT_CCORE_STD_UTILS_H_
#define BEANCOUNT_CCORE_STD_UTILS_H_

#include <string>
#include <string_view>

namespace beancount {

// Generic hash for generic pair.
struct pair_hash
{
  template <typename T1, typename T2>
  std::size_t operator()(const std::pair<T1, T2> &pair) const {
    return std::hash<T1>{}(pair.first) ^ std::hash<T2>{}(pair.second);
  }
};

// Creates an uppercase string from a given absl::string_view.
std::string Capitalize(std::string_view s);

}  // namespace beancount

#endif // BEANCOUNT_CCORE_STD_UTILS_H_
