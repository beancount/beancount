// Date-related utilities.
//
// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#ifndef BEANCOUNT_CCORE_DATE_H_
#define BEANCOUNT_CCORE_DATE_H_

#include "beancount/ccore/date.pb.h"

#include <cassert>
#include <string_view>

#include "absl/time/civil_time.h"
#include "absl/status/statusor.h"

namespace beancount {

// Convert a date object to an equivalent proto.
inline void DateToProto(const absl::CivilDay& date, Date* proto) {
  assert(proto != nullptr);
  proto->set_year(date.year());
  proto->set_month(date.month());
  proto->set_day(date.day());
}

// Convert date string in YYYY.MM.DD format to (year, month, date).
// Only the first 10 characters are inspected; the string need not be zero-terminated.
absl::StatusOr<absl::CivilDay> ParseDateFromString(const char* string);

// Convert time string in hh::mm or hh:mm:ss format to civil seconds.
absl::StatusOr<absl::CivilSecond> ParseTimeFromString(std::string_view input);

}  // namespace beancount

#endif  // BEANCOUNT_CCORE_DATE_H_
