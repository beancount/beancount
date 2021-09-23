#include "beancount/cparser/ledger.h"

#include "beancount/ccore/std_utils.h"

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

// TODO(blais): Pull error code.
int WriteToText(const Ledger& ledger, const std::string& filename) {
  // Open output file.
  int outfd = open(filename.c_str(), O_CREAT|O_WRONLY|O_TRUNC, S_IRUSR|S_IWUSR);
  if (outfd == -1) {
    // TODO(blais): handle this properly, with Status<>.
    std::cerr << "Error opening file '" << filename << "': " << strerror(errno) << std::endl;
    return -1;
  }
  ZeroCopyOutputStream* output = new FileOutputStream(outfd);

  // Output directives.
  for (const auto& dir : ledger.directives) {
    LedgerProto ledger_proto;
    ledger_proto.add_directives()->CopyFrom(*dir);
    if (!TextFormat::Print(ledger_proto, output)) {
      // TODO(blais): handle this properly.
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
    // TODO(blais): handle this properly.
    std::cerr << "Error writing out message to '" << filename << "'" << std::endl;
    return -1;
  }

  // Close output.
  delete output;
  if (close(outfd) == -1) {
    // TODO(blais): handle this properly.
    std::cerr << "Error closing file '" << filename << "'" << std::endl;
    return -1;
  }
  return 0;
}

std::unique_ptr<inter::Ledger> LedgerToProto(const Ledger& ledger) {
  auto output = std::make_unique<inter::Ledger>();
  for (const auto* dir : ledger.directives) {
    output->add_directives()->CopyFrom(*dir);
  }
  for (const auto* error : ledger.errors) {
    output->add_errors()->CopyFrom(*error);
  }
  if (ledger.options->ByteSizeLong() > 0) {
    output->mutable_options()->CopyFrom(*ledger.options);
  }
  if (ledger.info->ByteSizeLong() > 0) {
    output->mutable_info()->CopyFrom(*ledger.info);
  }
  return output;
}

void AddError(Ledger* ledger, std::string_view message, const Location& location) {
  auto* error = new Error();
  ledger->errors.push_back(error);
  error->set_message(Capitalize(message));
  error->mutable_location()->CopyFrom(location);
}

}  // namespace beancount
