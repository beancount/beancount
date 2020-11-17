// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/number.h"
#include "beancount/ccore/number.pb.h"

#include <google/protobuf/util/message_differencer.h>

namespace beancount {
using google::protobuf::util::MessageDifferencer;

decimal::Decimal ProtoToDec(const Decimal& decproto) {
  decimal::Decimal dec;
  if (decproto.has_exact()) {
    dec = decimal::Decimal(decproto.exact());
  } else {
    mpd_t* value = dec.get();
    const auto& mpd = decproto.mpd();
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

// Convert a mpdecimal number to a Decimal proto.
Decimal DecToProto(const decimal::Decimal& dec) {
  // Serialize.
  Decimal decproto;
  {
    auto* mpd = decproto.mutable_mpd();
    const mpd_t* value = dec.getconst();
    mpd->set_flags(value->flags & ~MPD_DATAFLAGS);
    mpd->set_exp(value->exp);
    mpd->set_digits(value->digits);
    mpd->set_len(value->len);
    for (int ii = 0; ii < value->alloc; ++ii) {
      mpd->add_data(value->data[ii]);
    }
  }
  return decproto;
}

// TODO(blais): Convert to decimals before comparing.
bool operator==(const Decimal& obj1, const Decimal& obj2) {
  return MessageDifferencer::Equals(obj1, obj2);
}

std::ostream& operator<<(std::ostream& os, const Decimal& number) {
  if (number.has_exact()) {
    os << number.exact();
  } else {
    decimal::Decimal dec = ProtoToDec(number);
    os << dec.to_sci(false);
  }
  return os;
}

}  // namespace beancount
