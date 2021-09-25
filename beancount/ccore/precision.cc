// Copyright (C) 2021  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/precision.h"
#include "beancount/ccore/number.h"

#include <algorithm>
#include <iostream>
#include <string_view>

namespace beancount {
using precision::Pair;
using precision::PrecisionStats;

void UpdatePair(const decimal::Decimal& number, Pair* pair_stats) {
  // Update the signs.
  if (number.sign() == -1) {
    pair_stats->set_has_sign(true);
  } else if (!pair_stats->has_has_sign()) {
    pair_stats->set_has_sign(false);
  }

  // Update the precision.
  int64_t exponent = number.exponent();
  (*pair_stats->mutable_exponents())[-exponent]++;

  // Update the maximum number of integral digits.
  int integer_digits = number.getconst()->digits + exponent;
  if (integer_digits > pair_stats->max_integer_digits()) {
    pair_stats->set_max_integer_digits(integer_digits);
  }
}

// Update the exponents fields.
void Summarize(Pair* pair) {
  int exponent_mode = 0;
  int exponent_freq = 0;
  int exponent_max = 0;
  for (const auto& item : pair->exponents()) {
    if (item.second > exponent_freq) {
      exponent_freq = item.second;
      exponent_mode = item.first;
    }
    if (item.first > exponent_max) {
      exponent_max = item.first;
    }
  }
  pair->set_exponent_mode(exponent_mode);
  pair->set_exponent_max(exponent_max);
  pair->clear_exponents();
}

void PrecisionStatsAccum::Update(const decimal::Decimal& number,
                                 std::string_view currency) {
  auto& pair_stats = stats_[QuoteBase(currency, "")];
  pair_stats.set_quote(std::string(currency));
  UpdatePair(number, &pair_stats);
}

void PrecisionStatsAccum::Update(const decimal::Decimal& number,
                                 std::string_view quote, std::string_view base) {
  auto& pair_stats = stats_[QuoteBase(quote, base)];
  pair_stats.set_quote(std::string(quote));
  pair_stats.set_base(std::string(base));
  UpdatePair(number, &pair_stats);
}

void PrecisionStatsAccum::Serialize(PrecisionStats* proto) const {
  assert(proto != nullptr);
  std::vector<Pair*> pairs;
  for (const auto& item : stats_) {
    auto* pair = new Pair();
    pair->CopyFrom(item.second);
    Summarize(pair);
    pairs.push_back(pair);
  }

  // Sort for test stability.
  std::sort(pairs.begin(), pairs.end(), [](const Pair* a, const Pair* b) -> bool {
    if (a->quote() < b->quote())
      return true;
    if (a->quote() > b->quote())
      return false;
    return a->base() < b->base();
  });
  auto* proto_pairs = proto->mutable_pairs();
  for (Pair* pair : pairs) { proto_pairs->AddAllocated(pair); }
}

void UpdateStatistics(const Directive& directive, PrecisionStatsAccum* stats) {
  if (directive.has_transaction()) {
    for (const auto& posting : directive.transaction().postings()) {
      if (!posting.has_spec())
        continue;
      const auto& spec = posting.spec();

      // Witness units.
      if (spec.has_units()) {
        const auto& units = spec.units();
        if (units.has_currency()) {
          if (units.has_number()) {
            decimal::Decimal number = ProtoToDecimal(units.number());
            stats->Update(number, units.currency());
          }

          // Witness cost.
          if (spec.has_cost()) {
            const auto& cost = spec.cost();
            if (cost.has_currency()) {
              const auto& per_unit = cost.per_unit();
              if (per_unit.has_number()) {
                decimal::Decimal number = ProtoToDecimal(per_unit.number());
                stats->Update(number, cost.currency(), units.currency());
              }
              const auto& total = cost.total();
              if (total.has_number()) {
                decimal::Decimal number = ProtoToDecimal(total.number());
                stats->Update(number, cost.currency(), units.currency());
              }
            }
          }

          // Witness price.
          if (spec.has_price()) {
            const auto& price = spec.price();
            if (price.has_number() && price.has_currency()) {
              decimal::Decimal number = ProtoToDecimal(price.number());
              stats->Update(number, price.currency(), units.currency());
            }
          }
        }
      }
    }
  }
  // TODO(blais): Add occurrences of other types here, Price, Balance.
}

}  // namespace beancount
