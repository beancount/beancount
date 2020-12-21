#include "beancount/cparser/test_utils.h"
// #include "beancount/data.pb.h"

#include <string>

#include "google/protobuf/text_format.h"
#include "google/protobuf/util/message_differencer.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace beancount {
namespace {

using std::string;
using google::protobuf::TextFormat;
using google::protobuf::util::MessageDifferencer;

/// // Test just one item.
/// TEST(TestUtilsTest, ClearLineNumbers) {
///   string input = u8R"(
///     type {lineno: 42 type: "item"}
///     object {lineno: 142 id {type: "item" ident: "0"}}
///   )";
///   proto::Database input_db;
///   ASSERT_TRUE(TextFormat::ParseFromString(input, &input_db));
///   ClearLineNumbers(&input_db);
///
///   string expected = u8R"(
///     type {type: "item"}
///     object {id {type: "item" ident: "0"}}
///   )";
///   proto::Database expected_db;
///   ASSERT_TRUE(TextFormat::ParseFromString(expected, &expected_db));
///
///   EXPECT_TRUE(MessageDifferencer::Equals(expected_db, input_db));
/// }

}  // namespace
}  // namespace beancount
