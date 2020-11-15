// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#ifndef _BEANCOUNT_CCORE_NUMBER_H_
#define _BEANCOUNT_CCORE_NUMBER_H_

#include <string>

#include "beancount/ccore/number.pb.h"
#include "beancount/defs.h"

namespace std {

// Hashing function for Decimal type.
//
// Note: This is not provided by the mpdecimal library, but has an
// implementation in Python's _decimal.c wrappers.
template<>
struct hash<beancount::MpDecimal> {
  size_t operator()(const beancount::MpDecimal& mpd) const {
    uint64_t h = 0;
    for (uint64_t datum : mpd.data()) {
      h ^= hash<uint64_t>{}(datum);
    }
    return (hash<int32_t>{}(mpd.flags()) ^
            hash<int64_t>{}(mpd.exp()) ^
            hash<int64_t>{}(mpd.digits()) ^
            hash<int64_t>{}(mpd.len()) ^
            h);
  }
};

template<>
struct hash<beancount::Decimal> {
  size_t operator()(const beancount::Decimal& number) const {
    return (number.has_exact() ?
            hash<string>{}(number.exact()) :
            hash<beancount::MpDecimal>{}(number.mpd()));
  }
};

}  // namespace std

#endif // _BEANCOUNT_CCORE_NUMBER_H_
