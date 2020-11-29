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
  for (auto* directive : directives) {
    delete directive;
  }
  directives.clear();
  for (auto* error : errors) {
    delete error;
  }
  errors.clear();
}

// TODO(blais): Pull error code.
int WriteToText(const Ledger& ledger, const string& filename) {
  int outfd = open(filename.c_str(), O_CREAT|O_WRONLY|O_TRUNC);
  if (outfd == -1) {
    std::cerr << "Error opening file '" << filename << "': " << strerror(errno) << std::endl;
    return -1;
  }
  ZeroCopyOutputStream* output = new FileOutputStream(outfd);
  for (const auto& dir : ledger.directives) {
    inter::Ledger ledger_proto;
    ledger_proto.add_directive()->CopyFrom(*dir);
    if (!TextFormat::Print(ledger_proto, output)) {
      std::cerr << "Error writing out message to '" << filename << "'" << std::endl;
      return -1;
    }
  }
  delete output;
  if (close(outfd) == -1) {
    std::cerr << "Error closing file '" << filename << "'" << std::endl;
    return -1;
  }
  return 0;
}

}  // namespace beancount
