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
    dec = decimal::Decimal(proto.exact());
  } else {
    mpd_t* value = dec.get();
    const auto& mpd = proto.mpd();
    value->flags = mpd.flags() & ~MPD_DATAFLAGS;
    value->exp = mpd.exp();
    value->digits = mpd.digits();
    value->len = mpd.len();
    value->alloc = mpd.data().size();
    ///assert(value->data == nullptr);
    // We have no access
    value->data = static_cast<mpd_uint_t*>(
      mpd_alloc(value->alloc, sizeof(*value->data)));
    for (int ii = 0; ii < value->alloc; ++ii) {
      value->data[ii] = mpd.data(ii);
    }
  }
  return dec;
}

// Convert a mpdecimal number to a Number proto.
Number DecimalToProto(const decimal::Decimal& dec) {
  // Serialize.
  Number proto;
  DecimalToProto(dec, &proto);
  return proto;
}

// Convert a mpdecimal number to a Number proto.
void DecimalToProto(const decimal::Decimal& dec, Number* proto) {
  // Serialize.
  {
    auto* mpd = proto->mutable_mpd();
    const mpd_t* value = dec.getconst();
    mpd->set_flags(value->flags & ~MPD_DATAFLAGS);
    mpd->set_exp(value->exp);
    mpd->set_digits(value->digits);
    mpd->set_len(value->len);
    for (int ii = 0; ii < value->alloc; ++ii) {
      mpd->add_data(value->data[ii]);
    }
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

}  // namespace beancount
