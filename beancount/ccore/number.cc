// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/number.h"
#include "beancount/ccore/number.pb.h"

#include <google/protobuf/util/message_differencer.h>

namespace beancount {
using google::protobuf::util::MessageDifferencer;

// TODO(blais): Convert to decimals before comparing.
bool operator==(const Decimal& obj1, const Decimal& obj2) {
  return MessageDifferencer::Equals(obj1, obj2);
}

std::ostream& operator<<(std::ostream& os, const Decimal& self) {
  // TODO(blais): Implement this.
  return os;
}

}  // namespace beancount
