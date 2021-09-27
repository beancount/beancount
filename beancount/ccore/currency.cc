// Copyright (C) 2021  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/currency.h"

#include "absl/strings/str_cat.h"

namespace beancount {

const char* kCurrencyREStr = (
    "(?:" "[A-Z][A-Z0-9\\'\\.\\_\\-]*[A-Z0-9]?\\b"
    "|" "\\/[A-Z0-9\\'\\.\\_\\-]*[A-Z]([A-Z0-9\\'\\.\\_\\-]*[A-Z0-9])?" ")");

re2::RE2 kCurrencyRE(kCurrencyREStr);

re2::RE2 kCurrencyPairRE(absl::StrCat("(", kCurrencyREStr, ")/(", kCurrencyREStr, ")"));

}  // namespace beancount
