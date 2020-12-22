// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/number.h"
#include "beancount/ccore/number.pb.h"

#include <iostream>
// #include <string>
// #include <vector>

#include "decimal.hh"
#include "mpdecimal.h"
// #include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace beancount {
namespace {

TEST(TestNumber, Decimal) {
  Number dec;
  dec.set_exact("123.4567");
  std::cout << dec << std::endl;
}

TEST(TestNumber, CopySansCommas) {
  const char* src = "123,456,678,9AB";
  char dst[20];
  std::fill_n(dst, 20, '\0');
  CopySansCommas(src, dst, 8);
  EXPECT_STREQ("123456", dst);

  std::fill_n(dst, 20, '\0');
  CopySansCommas(src, dst, 7);
  EXPECT_STREQ("123456", dst);
}

}  // namespace
}  // namespace beancount
