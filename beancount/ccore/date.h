// Date-related utilities.
//
// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#ifndef BEANCOUNT_CCORE_DATE_H_
#define BEANCOUNT_CCORE_DATE_H_

#include "beancount/ccore/date.pb.h"

#include <cassert>

#include "absl/time/civil_time.h"

namespace beancount {

// Convert a date object to an equivalent proto.
inline void DateToProto(const absl::CivilDay& date, Date* proto) {
  assert(proto != nullptr);
  proto->set_year(date.year());
  proto->set_month(date.month());
  proto->set_day(date.day());
}

}  // namespace beancount

#endif  // BEANCOUNT_CCORE_DATE_H_
