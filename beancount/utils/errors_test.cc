#include "beancount/utils/errors.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#include "gtest/gtest.h"

namespace beancount {
namespace {

TEST(ErrorsTest, SystemError) {
  int outfd = open("/dev/non-existent", O_CREAT, S_IRUSR|S_IWUSR);
  assert(outfd == -1);
  auto error = SystemError("Error opening file");
  std::cout << error << std::endl;
}

}  // namespace
}  // namespace beancount
