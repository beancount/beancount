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

}  // namespace
}  // namespace beancount
