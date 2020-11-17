// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#ifndef _BEANCOUNT_CCORE_NUMBER_H_
#define _BEANCOUNT_CCORE_NUMBER_H_

#include <string>
#include <iosfwd>

#include "beancount/ccore/number.pb.h"
#include "beancount/defs.h"

#include "decimal.hh"

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
struct hash<beancount::Number> {
  size_t operator()(const beancount::Number& number) const {
    return (number.has_exact() ?
            hash<string>{}(number.exact()) :
            hash<beancount::MpDecimal>{}(number.mpd()));
  }
};

}  // namespace std

namespace beancount {

// Deserialize a Number proto to a mpdecimal number.
decimal::Decimal ProtoToDec(const Number& decproto);

// Serialize a mpdecimal number to a Number proto.
Number DecToProto(const decimal::Decimal& decproto);

// Comparison operators for decimal protos.
bool operator==(const Number& number1, const Number& number2);

// Streaming operator for Number proto.
std::ostream& operator<<(std::ostream& os, const Number& self);

}  // namespace beancount

#endif // _BEANCOUNT_CCORE_NUMBER_H_
