// Precisions stats and display context.
//
// This compilation unit contains utilities to accumulate statistics on
// numerical precision witnessed in an input file and render out numbers based
// on the most common occurrence of precision entered by the user. Moreover, the
// values are overrideable by the user as well, via options. The statistics are
// calculated for each context for which a currency another is used to quote
// another; for example quoting shares of a stock in dollars will be done with 2
// digits of decimal precision, while quoting an exchange rate for some foreign
// currencies in dollars will require 4 digits of decimal precision.
//
// Note: This replaces the DisplayContext implementation from Python.
//
// Copyright (C) 2021  Martin Blais
// License: "GNU GPLv2"

#ifndef BEANCOUNT_CCORE_PRECISION_H_
#define BEANCOUNT_CCORE_PRECISION_H_

#include "beancount/ccore/data.pb.h"
#include "beancount/ccore/precision.pb.h"

#include <list>
#include <string>
#include <utility>
#include <unordered_map>

#include "absl/hash/hash.h"
#include "decimal.hh"

namespace beancount {

// A statistics accumulator.
class PrecisionStatsAccum final {
public:
  // Update counts with the given context.
  void Update(const decimal::Decimal& number,
              std::string_view currency);
  void Update(const decimal::Decimal& number,
              std::string_view quote, std::string_view base);

  // Write out the summarized data to a proto object.
  void Serialize(precision::PrecisionStats* proto) const;

private:
  // A mapping of (quote, base) currencies (for example {"USD", "HOOL"}) to a
  // distribution of {precision, counts}. The key is the context; this is used
  // to tally up the number of times we've seen a precision used from a file.
  typedef std::pair<std::string, std::string> QuoteBase;
  typedef std::unordered_map<QuoteBase, precision::Pair, absl::Hash<QuoteBase>> Stats;
  Stats stats_;
};

// Pull out explicit numbers from a series of directives and accumulate
// statistics on them.
void UpdateStatistics(const Directive& directive, PrecisionStatsAccum* stats);

}  // namespace beancount

#endif // BEANCOUNT_CCORE_PRECISION_H_
