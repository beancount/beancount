// Copyright (C) 2021  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/precision.h"
#include "beancount/ccore/number.h"
#include "beancount/ccore/currency.h"

#include <algorithm>
#include <iostream>
#include <string>
#include <string_view>

#include "absl/strings/str_cat.h"

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

void PrecisionStatsAccum::Update(const Number& number,
                                 std::string_view currency) {
  decimal::Decimal decimal = ProtoToDecimal(number);
  Update(decimal, currency);
}

void PrecisionStatsAccum::Update(const Number& number,
                                 std::string_view quote, std::string_view base) {
  decimal::Decimal decimal = ProtoToDecimal(number);
  Update(decimal, quote, base);
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
            stats->Update(units.number(), units.currency());
          }

          // Witness cost.
          if (spec.has_cost()) {
            const auto& cost = spec.cost();
            if (cost.has_currency()) {
              const auto& per_unit = cost.per_unit();
              if (per_unit.has_number()) {
                stats->Update(per_unit.number(), cost.currency(), units.currency());
              }
              const auto& total = cost.total();
              if (total.has_number()) {
                stats->Update(total.number(), cost.currency(), units.currency());
              }
            }
          }

          // Witness price.
          if (spec.has_price()) {
            const auto& price = spec.price();
            if (price.has_number() && price.has_currency()) {
              stats->Update(price.number(), price.currency(), units.currency());
            }
          }
        }
      }
    }
  } else if (directive.has_price()) {
    const auto& price = directive.price();
    if (price.has_currency() &&
        price.has_amount() &&
        price.amount().has_number() &&
        price.amount().has_currency()) {
      stats->Update(directive.price().amount().number(),
                    price.amount().currency(), price.currency());
    }
  } else if (directive.has_balance()) {
    const auto& balance = directive.balance();
    if (balance.has_amount() &&
        balance.amount().has_number() &&
        balance.amount().has_currency()) {
      stats->Update(directive.balance().amount().number(),
                    balance.amount().currency());
    }
  }
}

precision::Pair* FindPair(precision::PrecisionStats* stats,
                          const std::string& quote, const std::string& base) {
  // Find an existing match.
  for (auto& pair : *stats->mutable_pairs()) {
    if (pair.quote() == quote && pair.base() == base) {
      return &pair;
    }
  }

  // Not found; create one.
  precision::Pair* pair = stats->add_pairs();
  pair->set_quote(quote);
  if (!base.empty()) {
    pair->set_base(base);
  }
  return pair;
}

void OverridePrecisionOptions(const options::Options& options,
                              precision::PrecisionStats* stats,
                              std::vector<std::string>* errors) {
  // We're going to update the mode here.

  /// TODO(blais): Remove
  /// std::cout << "options = " << options.DebugString() << std::endl;
  for (const auto& setting : options.precision()) {
    // Split up the currency pair.
    std::string quote;
    std::string base;
    if (!re2::RE2::FullMatch(setting.first, kCurrencyPairRE, &quote, &base)) {
      if (re2::RE2::FullMatch(setting.first, kCurrencyRE)) {
        quote = setting.first;
      } else {
        if (errors != nullptr) {
          errors->push_back(absl::StrCat("Invalid pattern for precision: '",
                                         setting.first, "'"));
        }
        continue;
      }
    }

    // Update the mode of the pair.
    precision::Pair* pair = FindPair(stats, quote, base);
    try {
      decimal::Decimal number(setting.second);
      pair->set_exponent_mode(-number.exponent());
      if (!pair->has_exponent_max()) {
        pair->set_exponent_max(pair->exponent_mode());
      }
    }
    catch (const decimal::IEEEInvalidOperation&) {
      errors->push_back(absl::StrCat("Invalid number in precision for '",
                                     setting.first, "': '", setting.second, "'"));
      continue;
    }
  }
}

}  // namespace beancount
