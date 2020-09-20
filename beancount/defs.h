// Common definitions in global namespace. This is a small enough codebase, we
// assume the presence of some limited list of common datatypes without having
// to pepper the entire codebase with std prefixes.
//
// Copyright (C) 2013-2016,2020  Martin Blais
// License: "GNU GPLv2"

#ifndef _BEANCOUNT_DEFS_H_
#define _BEANCOUNT_DEFS_H_

namespace beancount {

using std::string;
using std::string_view;
using std::vector;
using std::size_t;
using std::optional;

}  // namespace beancount

#endif // _BEANCOUNT_CCORE_ACCOUNT_H_
