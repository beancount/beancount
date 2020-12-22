// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/date.h"

#include <cstring>

#include "absl/strings/str_cat.h"

namespace beancount {
namespace {

// Convert ASCII string to an integer. Converts the 'string' string of length
// 'len' to int. The input is assumed to be a valid representation of an integer
// number. No input validation or error checking is performed.
inline int FixedAtoi(const char* string, size_t len) {
    int result = 0;
    for (size_t i = 0; i < len; ++i) {
        result *= 10;
        result += string[i] - '0';
    }
    return result;
}

// Eat up the front of a string's numbers, subject to a bound (we assume it may
// not be zero-terminated), and return the converted number and the end of the
// matched portion. If no characters were able to be matched, the returned
// string points to the string itself and we return 0.
inline int BoundedAtoi(const char* string, size_t length, const char** end) {
  int result = 0;
  for (size_t ii = 0; ii < length; ++ii, ++string) {
    if (*string >= '0' && *string <= '9') {
      result = (result * 10) + (*string - '0');
    } else {
      break;
    }
  }
  *end = string;
  return result;
}

}  // namespace

absl::StatusOr<absl::CivilDay> ParseDateFromString(const char* string) {
  // Build date, parsing from fixed locations.
  int year = FixedAtoi(string, 4);
  int month = FixedAtoi(string + 5, 2);
  int day = FixedAtoi(string + 8, 2);
  auto date = absl::CivilDay{year, month, day};

  // Detect invalid values out of range.
  if (date.year() != year || date.month() != month || date.day() != day) {
    return absl::InvalidArgumentError(
        absl::StrCat("Invalid date: '", std::string_view(string, 10), "'"));
  }

  return date;
}

absl::StatusOr<absl::CivilSecond> ParseTimeFromString(const char* string, size_t length) {
  if (length == 0) {
    length = std::strlen(string);
  }

  const char* end = string + length;
  const char* current = string;
  const char* skip;

  int hour = BoundedAtoi(current, length, &skip);
  if (current == skip || hour >= 24 || skip == end || *skip != ':') {
    return absl::InvalidArgumentError(
        absl::StrCat("Invalid time at hour: '", std::string_view(string, length), "'"));
  }
  current = skip + 1;

  int minute = BoundedAtoi(current, end - current, &skip);
  if (current == skip || minute >= 60) {
    return absl::InvalidArgumentError(
        absl::StrCat("Invalid time at minute: '", std::string_view(string, length), "'"));
  }
  current = skip;

  int second;
  if (current == end || *current != ':') {
    second = 0;
  } else {
    ++current;
    second = BoundedAtoi(current, end - current, &skip);
    if (current == skip || second >= 60) {
      return absl::InvalidArgumentError(
          absl::StrCat("Invalid time at second: '", std::string_view(string, length), "'"));
    }
  }
  auto time = absl::CivilSecond{1970, 1, 1, hour, minute, second};

  return time;
}

}  // namespace beancount
