#include "beancount/cparser/ledger.h"

namespace beancount {

Ledger::~Ledger() {
  for (auto* directive : directives) {
    delete directive;
  }
  directives.clear();
}

}  // namespace beancount
