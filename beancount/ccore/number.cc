// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/number.h"
#include "beancount/ccore/number.pb.h"

#include <cassert>

#include <google/protobuf/util/message_differencer.h>

namespace beancount {
using google::protobuf::util::MessageDifferencer;

decimal::Decimal ProtoToDecimalOrDie(const Number& proto) {
  absl::StatusOr<decimal::Decimal> sdec = ProtoToDecimal(proto);
  assert(sdec.ok());
  return *sdec;
}

absl::StatusOr<decimal::Decimal> ProtoToDecimal(const Number& proto) {
  decimal::Decimal dec;
  if (proto.has_exact()) {
    // Text deserialization.
    dec = decimal::Decimal(proto.exact());
  } else if (proto.has_mpd()) {
    // mpd_t deserialization.
    const auto& p = proto.mpd();
    auto* mpd = dec.get();
    if (mpd_resize(mpd, p.len(), decimal::context.get()) == 0) {
      return absl::ResourceExhaustedError("Failed to resize Decimal instance.");
    }
    mpd->flags = p.flags();
    mpd->exp = p.exp();
    mpd->digits = p.digits();
    mpd->len = p.len();
    for (int ii = 0; ii < p.len(); ++ii) {
      mpd->data[ii] = p.data(ii);
    }
  } else if (proto.has_triple()) {
    // Triple deserialization.
    const auto& p = proto.triple();
    mpd_uint128_triple_t triple;
    triple.tag = static_cast<mpd_triple_class>(p.tag());
    triple.sign = p.sign();
    triple.hi = p.hi();
    triple.lo = p.lo();
    triple.exp = p.exp();
    dec = decimal::Decimal(triple);
  } else {
    return absl::InvalidArgumentError("Number proto does not have any variant set.");
  }
  return dec;
}

// Convert a mpdecimal number to a Number proto.
absl::StatusOr<Number> DecimalToProto(const decimal::Decimal& dec,
                                      DecimalConversion conversion) {
  Number proto;
  auto status = DecimalToProto(dec, conversion, &proto);
  if (!status.ok()) {
    return status;
  }
  return proto;
}

// Convert a mpdecimal number to a Number proto.
absl::Status DecimalToProto(const decimal::Decimal& dec,
                            DecimalConversion conversion,
                            Number* proto) {
  switch (conversion) {
    case CONV_MPD: {
      // Use mpd directly.
      const mpd_t* mpd = dec.getconst();
      // TODO(blais): Check for error, fallback to mpd_t.
      auto* p = proto->mutable_mpd();
      p->set_flags(mpd->flags);
      p->set_exp(mpd->exp);
      p->set_digits(mpd->digits);
      p->set_len(mpd->len);
      auto* data = p->mutable_data();
      data->Reserve(mpd->len);
      for (int ii = 0; ii < mpd->len; ++ii) {
        data->AddAlreadyReserved(mpd->data[ii]);
      }
    } break;

    case CONV_TRIPLE: {
      // Use triple serialization.
      mpd_uint128_triple_t triple = dec.as_uint128_triple();
      if (triple.tag == MPD_TRIPLE_ERROR) {
        return absl::OutOfRangeError(
            "The coefficient is too large for conversion to triple.");
      }
      auto* p = proto->mutable_triple();
      p->set_tag(triple.tag);
      p->set_sign(triple.sign);
      p->set_hi(triple.hi);
      p->set_lo(triple.lo);
      p->set_exp(triple.exp);
    } break;

    case CONV_STRING: {
      // Use text serialization.
      proto->set_exact(dec.to_sci());
    } break;
  }
  return absl::OkStatus();
}

// TODO(blais): Canonicalize decimal representations before comparing.
bool operator==(const Number& proto1, const Number& proto2) {
  return MessageDifferencer::Equals(proto1, proto2);
}

std::ostream& operator<<(std::ostream& os, const Number& proto) {
  if (proto.has_exact()) {
    os << proto.exact();
  } else {
    auto sdec = ProtoToDecimal(proto);
    if (sdec.ok()) {
      decimal::Decimal dec = *sdec;
      os << dec.to_sci(false);
    } else {
      os << "<ProtoToDecimal - Conversion Error>";
    }
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
