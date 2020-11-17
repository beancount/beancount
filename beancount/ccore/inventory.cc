// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/inventory.h"
#include "beancount/ccore/data.h"

// #include <algorithm>
// #include <string>
// #include <initializer_list>
// #include <iostream>

// #include "absl/strings/str_cat.h"
// #include "absl/strings/str_format.h"
// #include "absl/strings/str_join.h"
// #include "absl/strings/str_split.h"
// #include "re2/re2.h"

namespace beancount {

Inventory InventoryFromString(string_view string) {
  Inventory inv;
  // TODO(blais)
  return inv;
}

bool Inventory::operator==(const Inventory& other) const {
  return positions_ == other.positions_;
}


}  // namespace beancount
