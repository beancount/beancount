// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/data.h"

#include <google/protobuf/util/message_differencer.h>

namespace beancount {
using google::protobuf::util::MessageDifferencer;

bool operator==(const Cost& obj1, const Cost& obj2) {
  return MessageDifferencer::Equals(obj1, obj2);
}

}  // namespace beancount
