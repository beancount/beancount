// Common definitions in global namespace. This is a small enough codebase, we
// assume the presence of some limited list of common datatypes without having
// to pepper the entire codebase with std prefixes.
//
// Copyright (C) 2013-2016,2020  Martin Blais
// License: "GNU GPLv2"

#ifndef BEANCOUNT_DEFS_H_
#define BEANCOUNT_DEFS_H_

#include <optional>
#include <string>
#include <tuple>
#include <vector>

namespace beancount {

using std::string;
using std::string_view;
using std::vector;
using std::size_t;
using std::optional;
using std::pair;
using std::make_pair;
using std::tuple;
using std::make_tuple;

// Generic hash for generic pair.
struct pair_hash
{
  template <typename T1, typename T2>
  std::size_t operator()(const std::pair<T1, T2> &pair) const {
    return std::hash<T1>{}(pair.first) ^ std::hash<T2>{}(pair.second);
  }
};

}  // namespace beancount

#endif // BEANCOUNT_CCORE_ACCOUNT_H_
