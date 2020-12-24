// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#ifndef BEANCOUNT_CCORE_DATA_H_
#define BEANCOUNT_CCORE_DATA_H_

#include <string>

#include "beancount/ccore/data.pb.h"
#include "beancount/ccore/number.h"
#include "beancount/defs.h"

namespace std {

template<>
struct hash<beancount::Date> {
  size_t operator()(const beancount::Date& date) const {
    return (hash<int32_t>{}(date.year()) ^
            hash<int32_t>{}(date.month()) ^
            hash<int32_t>{}(date.day()));
  }
};

template<>
struct hash<beancount::Cost> {
  size_t operator()(const beancount::Cost& cost) const {
    return (hash<beancount::Number>{}(cost.number()) ^
            hash<std::string>{}(cost.currency()) ^
            hash<beancount::Date>{}(cost.date()) ^
            hash<std::string>{}(cost.label()));
  }
};

}  // namespace std

namespace beancount {

// Custom comparison operators for protos.
bool operator==(const Cost& cost1, const Cost& cost2);

}  // namespace beancount

#endif // BEANCOUNT_CCORE_DATA_H_
