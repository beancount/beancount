// Copyright (C) 2021  Martin Blais
// License: "GNU GPLv2"

#ifndef BEANCOUNT_UTILS_ERRORS_H_
#define BEANCOUNT_UTILS_ERRORS_H_

#include "absl/status/status.h"

#include "absl/strings/string_view.h"

namespace beancount {

// StatusCode for system error.
extern absl::StatusCode kSystemError;

// Return a system error.
absl::Status SystemError(absl::string_view message);

}  // namespace beancount

#endif // BEANCOUNT_CCORE_ACCOUNT_H_
