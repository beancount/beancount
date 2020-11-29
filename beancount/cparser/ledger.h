// Parsed and booked state of a ledger. This is the main artifact produced by
// the Beancount core. This is mainly a list of directives, and some side-state.
//
// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#ifndef _BEANCOUNT_CPARSER_LEDGER_H_
#define _BEANCOUNT_CPARSER_LEDGER_H_

#include <memory>
#include <vector>
#include <list>

#include "beancount/cparser/parser.pb.h"
#include "beancount/cparser/options.pb.h"
#include "beancount/ccore/data.pb.h"

namespace beancount {

struct Ledger final {
  ~Ledger();

  // A list of directives, with ownership.
  std::list<beancount::inter::Directive*> directives;

  // A list of errors encountered during parsing and processing.
  std::vector<beancount::Error*> errors;

  // Parsed options.
  std::shared_ptr<options::Options> options;

  // Processing details.
  std::shared_ptr<options::ProcessingInfo> info;
};

}  // namespace beancount

#endif // _BEANCOUNT_CPARSER_LEDGER_H_
