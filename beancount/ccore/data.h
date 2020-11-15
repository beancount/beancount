// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#ifndef _BEANCOUNT_CCORE_DATA_H_
#define _BEANCOUNT_CCORE_DATA_H_

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
    return (hash<beancount::Decimal>{}(cost.number()) ^
            hash<string>{}(cost.currency()) ^
            hash<beancount::Date>{}(cost.date()) ^
            hash<string>{}(cost.label()));
  }
};

}  // namespace std

#endif // _BEANCOUNT_CCORE_DATA_H_
