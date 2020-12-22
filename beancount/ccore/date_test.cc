// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/date.h"

#include "gtest/gtest.h"

namespace beancount {
namespace {

// ParseDateFromString

TEST(ParseDateFromString, ValidDate) {
  auto s = ParseDateFromString("2020-12-22");
  EXPECT_TRUE(s.ok());
  EXPECT_EQ(absl::CivilDay(2020, 12, 22), s.value());
}

TEST(ParseDateFromString, CharactersAfter) {
  auto s = ParseDateFromString("2020-12-22111");
  EXPECT_TRUE(s.ok());
  EXPECT_EQ(absl::CivilDay(2020, 12, 22), s.value());
}

TEST(ParseDateFromString, InvalidMonth) {
  // Out of range.
  auto s = ParseDateFromString("2020-13-22");
  EXPECT_FALSE(s.ok());
}

TEST(ParseDateFromString, InvalidDay) {
  // Out of range.
  auto s = ParseDateFromString("2020-12-32");
  EXPECT_FALSE(s.ok());
}

TEST(ParseDateFromString, InvalidMonthDay) {
  // Within range, but not for the month.
  auto s = ParseDateFromString("2020-11-31");
  EXPECT_FALSE(s.ok());
}

// ParseTimeFromString

TEST(ParseTimeFromString, ValidTime) {
  // Within range, but not for the month.
  auto s = ParseTimeFromString("08:45:13");
  EXPECT_TRUE(s.ok());
  EXPECT_EQ(s.value().hour(), 8);
  EXPECT_EQ(s.value().minute(), 45);
  EXPECT_EQ(s.value().second(), 13);
}

TEST(ParseTimeFromString, SingleCharacters) {
  // Within range, but not for the month.
  auto s = ParseTimeFromString("10:2:1");
  EXPECT_TRUE(s.ok());
  EXPECT_EQ(s.value().hour(), 10);
  EXPECT_EQ(s.value().minute(), 2);
  EXPECT_EQ(s.value().second(), 1);
}

TEST(ParseTimeFromString, NoSeconds) {
  // Within range, but not for the month.
  auto s = ParseTimeFromString("11:21");
  EXPECT_TRUE(s.ok());
  EXPECT_EQ(s.value().hour(), 11);
  EXPECT_EQ(s.value().minute(), 21);
  EXPECT_EQ(s.value().second(), 0);
}

TEST(ParseTimeFromString, InvalidHour) {
  // Within range, but not for the month.
  auto s = ParseTimeFromString("25:00:00");
  EXPECT_FALSE(s.ok());
}

TEST(ParseTimeFromString, InvalidMinute) {
  // Within range, but not for the month.
  auto s = ParseTimeFromString("11:60:00");
  EXPECT_FALSE(s.ok());
}

TEST(ParseTimeFromString, InvalidSecond) {
  // Within range, but not for the month.
  auto s = ParseTimeFromString("11:11:60");
  EXPECT_FALSE(s.ok());
}


}  // namespace
}  // namespace beancount
