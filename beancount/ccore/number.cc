// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/number.h"
#include "beancount/ccore/number.pb.h"

#include <google/protobuf/util/message_differencer.h>

namespace beancount {
using google::protobuf::util::MessageDifferencer;

decimal::Decimal ProtoToDecimal(const Number& proto) {
  decimal::Decimal dec;
  if (proto.has_exact()) {
    // Use text serialization.
    dec = decimal::Decimal(proto.exact());
  } else {
    // Use triple serialization.
    const auto& tp = proto.triple();
    mpd_uint128_triple_t triple;
    triple.tag = static_cast<mpd_triple_class>(tp.tag());
    triple.sign = tp.sign();
    triple.hi = tp.hi();
    triple.lo = tp.lo();
    triple.exp = tp.exp();
    dec = decimal::Decimal(triple);
  }
  return dec;
}

// Convert a mpdecimal number to a Number proto.
Number DecimalToProto(const decimal::Decimal& dec, bool use_triple) {
  // Serialize.
  Number proto;
  DecimalToProto(dec, use_triple, &proto);
  return proto;
}

// Convert a mpdecimal number to a Number proto.
void DecimalToProto(const decimal::Decimal& dec, bool use_triple, Number* proto) {
  // Serialize.
  if (use_triple) {
    // Use triple serialization.
    mpd_uint128_triple_t triple = dec.as_uint128_triple();
    auto* tp = proto->mutable_triple();
    tp->set_tag(triple.tag);
    tp->set_sign(triple.sign);
    tp->set_hi(triple.hi);
    tp->set_lo(triple.lo);
    tp->set_exp(triple.exp);
  } else {
    // Use text serialization.
    proto->set_exact(dec.to_sci());
  }
}

// TODO(blais): Convert to decimals before comparing.
bool operator==(const Number& proto1, const Number& proto2) {
  return MessageDifferencer::Equals(proto1, proto2);
}

std::ostream& operator<<(std::ostream& os, const Number& proto) {
  if (proto.has_exact()) {
    os << proto.exact();
  } else {
    decimal::Decimal dec = ProtoToDecimal(proto);
    os << dec.to_sci(false);
  }
  return os;
}

void CopySansCommas(const char* src, char* buffer, size_t num_chars) {
  char* dst = buffer;
  for (size_t i = 0; i < num_chars; ++i, ++src) {
    if (*src == ',')
      continue;
    *dst++ = *src;
  }
  *dst = '\0';
}

}  // namespace beancount
