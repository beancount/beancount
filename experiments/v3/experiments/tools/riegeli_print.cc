// Print the contents of a serialized riegeli file.

// #include "riegeli/records/benchmarks/tfrecord_recognizer.h"

#include <iostream>
#include <string>

#include "riegeli/bytes/fd_reader.h"
#include "riegeli/records/record_reader.h"
#include "absl/base/internal/raw_logging.h"
#include "absl/base/log_severity.h"

#include "experiments/protos/beancount.pb.h"


int main(int argc, char** argv) {
  const char* filename = argv[1];
  ABSL_RAW_LOG(INFO, "Filename = %s", filename);

  riegeli::FdReader<> reader(filename, O_RDONLY);
  riegeli::RecordReader<> record_reader(&reader);

  std::string record;
  int num_messages = 0;
  while (record_reader.ReadRecord(&record)) {
    ++num_messages;

    /// ABSL_RAW_LOG(INFO, "Size -> %ld", record.size());
    beancount::Directive dir;
    if (!dir.ParseFromString(record)) {
      ABSL_RAW_LOG(ERROR, "Could not parse");
      continue;
    }
    std::cout << dir.DebugString() << std::endl;
    std::cout << std::endl;
  }
  ABSL_RAW_LOG(ERROR, "Num messages = %d", num_messages);

  if (!record_reader.Close()) {
    ABSL_RAW_LOG(ERROR, "Failed to close record reader: %s", record_reader.message().data());
  }

  return 0;
}
