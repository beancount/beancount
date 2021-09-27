// Copyright (C) 2021  Martin Blais
// License: "GNU GPLv2"

#ifndef BEANCOUNT_CCORE_CURRENCY_H_
#define BEANCOUNT_CCORE_CURRENCY_H_

#include "re2/re2.h"

namespace beancount {

// Regular expression string that matches a valid currency.
extern const char* kCurrencyREStr;
extern re2::RE2 kCurrencyRE;

// Regular expression string that matches a slash-separated pairs of currencies.
extern re2::RE2 kCurrencyPairRE;

}  // namespace beancount

#endif // BEANCOUNT_CCORE_CURRENCY_H_
