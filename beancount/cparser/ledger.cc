#include "beancount/cparser/ledger.h"
#include "beancount/defs.h"

#include "google/protobuf/text_format.h"
#include "google/protobuf/io/zero_copy_stream.h"
#include "google/protobuf/io/zero_copy_stream_impl.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

namespace beancount {
using google::protobuf::io::ZeroCopyOutputStream;
using google::protobuf::io::FileOutputStream;
using google::protobuf::TextFormat;

Ledger::~Ledger() {
  for (auto* directive : directives) delete directive;
  for (auto* error : errors) delete error;
}

// _SORT_ORDER = {
//     extmodule.BodyCase.kOpen: -2,
//     extmodule.BodyCase.kBalance: -1,
//     extmodule.BodyCase.kDocument: 1,
//     extmodule.BodyCase.kClose: 2,
// }
//
// def entry_sortkey_v3(dir: pb.Directive):
//     type_order = _SORT_ORDER.get(extmodule.GetDirectiveType(dir), 0)
//     return (dir.date, type_order, dir.location.lineno)

struct CompareDirectives {

  bool operator() (const Directive* a, const Directive* b) const {
#if 0
    std::cerr << a->date().ShortDebugString() << " ; " << b->date().ShortDebugString() << std::endl;
#endif
    assert(a != nullptr);
    assert(b != nullptr);
    const Date& adate = a->date();
    const Date& bdate = b->date();

    assert(adate.has_year());
    assert(bdate.has_year());
    const int ayear = adate.year();
    const int byear = bdate.year();
    if (ayear < byear)
      return true;

    assert(adate.has_month());
    assert(bdate.has_month());
    const int amonth = adate.month();
    const int bmonth = bdate.month();
    if (amonth < bmonth)
      return true;

    assert(adate.has_day());
    assert(bdate.has_day());
    const int aday = adate.day();
    const int bday = bdate.day();
    if (aday < bday)
      return true;

    // TODO(blais): Add more
    return false;
  }
};

// TODO(blais): Move this to data.h/cc.
// TODO(blais): Convert directives to vector? Probably.
std::vector<Directive*> SortDirectives(
  const std::vector<Directive*>& directives) {

  std::vector<Directive*> vdirectives;
  std::cerr << "AAA " << directives.size() << std::endl;
  vdirectives.reserve(directives.size());
  std::cerr << "AAV " << vdirectives.size() << " " << vdirectives.capacity() << std::endl;
  std::copy(directives.begin(), directives.end(), std::back_inserter(vdirectives));
  std::cerr << "AAV " << vdirectives.size() << std::endl;
  for (auto dir : vdirectives) { assert(dir != nullptr); }
#if 1
  std::sort(vdirectives.begin(), vdirectives.end(), CompareDirectives());
#endif
  std::cerr << "AAS " << vdirectives.size() << std::endl;
  return vdirectives;
}

// TODO(blais): Pull error code.
int WriteToText(const Ledger& ledger, const string& filename) {
  // Open output file.
  int outfd = open(filename.c_str(), O_CREAT|O_WRONLY|O_TRUNC);
  if (outfd == -1) {
    std::cerr << "Error opening file '" << filename << "': " << strerror(errno) << std::endl;
    return -1;
  }
  ZeroCopyOutputStream* output = new FileOutputStream(outfd);

  // Output directives.
  for (const auto& dir : ledger.directives) {
    LedgerProto ledger_proto;
    ledger_proto.add_directives()->CopyFrom(*dir);
    if (!TextFormat::Print(ledger_proto, output)) {
      std::cerr << "Error writing out message to '" << filename << "'" << std::endl;
      return -1;
    }
  }

  // Output errors, options and processing info.
  LedgerProto ledger_proto;
  for (const auto& error : ledger.errors) {
    ledger_proto.add_errors()->CopyFrom(*error);
  }
  ledger_proto.mutable_options()->CopyFrom(*ledger.options);
  ledger_proto.mutable_info()->CopyFrom(*ledger.info);

  if (!TextFormat::Print(ledger_proto, output)) {
    std::cerr << "Error writing out message to '" << filename << "'" << std::endl;
    return -1;
  }

  // Close output.
  delete output;
  if (close(outfd) == -1) {
    std::cerr << "Error closing file '" << filename << "'" << std::endl;
    return -1;
  }
  return 0;
}

}  // namespace beancount
