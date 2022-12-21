// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#include "beancount/utils/errors.h"

#include "absl/strings/str_cat.h"

#ifndef __APPLE__
#  include <error.h>
#else
#  include <errno.h>
#endif

#include <string.h>

namespace beancount {

int kUserErrors =
  int(absl::StatusCode::kDoNotUseReservedForFutureExpansionUseDefaultInSwitchInstead_);

absl::StatusCode kSystemError = absl::StatusCode(kUserErrors + 1);

absl::Status SystemError(absl::string_view message) {
  char buffer[2048];
#ifndef __APPLE__
  description = strerror_r(errno, buffer, 2048);
#else
  int status = strerror_r(errno, buffer, 2048);
  assert(status == 0);
#endif

  return absl::Status(kSystemError, absl::StrCat(message, ": ", buffer));
}

}  // namespace beancount
