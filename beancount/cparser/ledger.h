// Parsed and booked state of a ledger. This is the main artifact produced by
// the Beancount core. This is mainly a list of directives, and some side-state.
//
// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#ifndef BEANCOUNT_CPARSER_LEDGER_H_
#define BEANCOUNT_CPARSER_LEDGER_H_

#include "beancount/cparser/ledger.pb.h"

#include <memory>
#include <vector>
#include <list>

#include "beancount/cparser/options.pb.h"
#include "beancount/ccore/data.pb.h"

namespace beancount {

// Container for all data produced by the parser, and also by Beancount in
// general (after booking, interpolation, and plugins processing).
//
// Note that this a C++ container; we also have a separate proto container used
// for tests.
//
// TODO(blais): Measure performance and use cases and eliminate one or the
// other.
struct Ledger final {
  ~Ledger();

  // A list of directives, with ownership.
  std::list<beancount::Directive*> directives;

  // A list of errors encountered during parsing and processing.
  std::vector<beancount::Error*> errors;

  // Parsed options.
  std::shared_ptr<options::Options> options;

  // Processing details.
  std::shared_ptr<options::ProcessingInfo> info;
};

// Write ledger content to a text-formatted file.
int WriteToText(const Ledger& ledger, const std::string& filename);

// Convert the class above to a proto version.
std::unique_ptr<inter::Ledger> LedgerToProto(const Ledger& ledger);

// Add an error to the ledger.
void AddError(Ledger* ledger, std::string_view message, const Location& location);

}  // namespace beancount

#endif // BEANCOUNT_CPARSER_LEDGER_H_
