// Copyright (C) 2021  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/precision.h"
#include "beancount/ccore/number.h"

#include <algorithm>
#include <iostream>
#include <string_view>

namespace beancount {

void UpdatePair(const decimal::Decimal& number, PairStats* pair_stats) {
  // Update the signs.
  if (number.sign() == -1) {
    pair_stats->has_sign = true;
  }

  // Update the precision.
  int64_t exponent = number.exponent();
  pair_stats->exponents[-exponent]++;

  // Update the maximum number of integral digits.
  int integer_digits = number.getconst()->digits + exponent;
  if (integer_digits > pair_stats->max_integer_digits) {
    pair_stats->max_integer_digits = integer_digits;
  }
}

void PrecisionStatsAccum::Update(const decimal::Decimal& number,
                                 std::string_view currency) {
  auto& pair_stats = stats_[QuoteBase(currency, "")];
  UpdatePair(number, &pair_stats);
}

void PrecisionStatsAccum::Update(const decimal::Decimal& number,
                                 std::string_view quote, std::string_view base) {
  auto& pair_stats = stats_[QuoteBase(quote, base)];
  UpdatePair(number, &pair_stats);
}

void PrecisionStatsAccum::Serialize(PrecisionStats* proto) const {
  assert(proto != nullptr);
  std::vector<Pair*> pairs;
  for (const auto& item : stats_) {
    auto* pair = new Pair();
    pair->set_quote(item.first.first);
    if (!item.first.second.empty()) {
      pair->set_base(item.first.second);
    }
    pair->set_has_sign(item.second.has_sign);
    pair->set_max_integer_digits(item.second.max_integer_digits);

    int exponent_mode = 0;
    int exponent_freq = 0;
    int exponent_max = 0;
    for (const auto& eitem : item.second.exponents) {
      if (eitem.second > exponent_freq) {
        exponent_freq = eitem.second;
        exponent_mode = eitem.first;
      }
      if (eitem.first > exponent_max) {
        exponent_max = eitem.first;
      }
    }
    pair->set_exponent_mode(exponent_mode);
    pair->set_exponent_max(exponent_max);

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
