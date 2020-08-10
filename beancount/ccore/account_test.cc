// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/account.h"

#include <string>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace beancount {
namespace {

TEST(TestAccount, AccountRegexp) {
  EXPECT_TRUE(beancount::kAccountRE.ok());
  EXPECT_TRUE(RE2::FullMatch("Assets:Something", kAccountRE));
  EXPECT_FALSE(RE2::FullMatch("assets:Something", kAccountRE));
}

TEST(TestAccount, IsAccountValid) {
  EXPECT_TRUE(IsAccountValid("Assets:US:RBS:Checking"));
  EXPECT_TRUE(IsAccountValid("Equity:Opening-Balances"));
  EXPECT_TRUE(IsAccountValid("Income:US:ETrade:Dividends-USD"));
  EXPECT_TRUE(IsAccountValid("Assets:US:RBS"));
  EXPECT_TRUE(IsAccountValid("Assets:US"));
  EXPECT_FALSE(IsAccountValid("Assets"));
  EXPECT_FALSE(IsAccountValid("Invalid"));
  EXPECT_FALSE(IsAccountValid("Other"));
  EXPECT_FALSE(IsAccountValid("Assets:US:RBS*Checking"));
  EXPECT_FALSE(IsAccountValid("Assets:US:RBS:Checking&"));
  EXPECT_FALSE(IsAccountValid("Assets:US:RBS:checking"));
  EXPECT_FALSE(IsAccountValid("Assets:us:RBS:checking"));
}

TEST(TestAccount, JoinAccount) {
  EXPECT_EQ("Expenses:Toys:Computer", JoinAccount({"Expenses", "Toys", "Computer"}));
  EXPECT_EQ("Expenses", JoinAccount({"Expenses"}));
  EXPECT_EQ("", JoinAccount({}));
}

TEST(TestAccount, SplitAccount) {
  EXPECT_EQ(vector<string>({"Expenses", "Toys", "Computer"}),
            SplitAccount("Expenses:Toys:Computer"));
  EXPECT_EQ(vector<string>({"Expenses"}),
            SplitAccount("Expenses"));
  EXPECT_EQ(vector<string>(),
            SplitAccount(""));
}

TEST(TestAccount, ParentAccount) {
  EXPECT_EQ("Expenses:Toys", ParentAccount("Expenses:Toys:Computer"));
  EXPECT_EQ("Expenses", ParentAccount("Expenses:Toys"));
  EXPECT_EQ("", ParentAccount("Expenses"));
  EXPECT_EQ("", ParentAccount(""));
}

TEST(TestAccount, LeafAccount) {
  EXPECT_EQ("Computer", LeafAccount("Expenses:Toys:Computer"));
  EXPECT_EQ("Toys", LeafAccount("Expenses:Toys"));
  EXPECT_EQ("Expenses", LeafAccount("Expenses"));
  EXPECT_EQ("", LeafAccount(""));
}

TEST(TestAccount, AccountSansRoot) {
  EXPECT_EQ("Toys:Computer", AccountSansRoot("Expenses:Toys:Computer"));
  EXPECT_EQ("US:BofA:Checking", AccountSansRoot("Assets:US:BofA:Checking"));
  EXPECT_EQ("", AccountSansRoot("Assets"));
}

TEST(TestAccount, AccountRoot) {
  string name = "Liabilities:US:Credit-Card:Blue";
  EXPECT_EQ("", AccountRoot(0, name));
  EXPECT_EQ("Liabilities", AccountRoot(1, name));
  EXPECT_EQ("Liabilities:US", AccountRoot(2, name));
  EXPECT_EQ("Liabilities:US:Credit-Card", AccountRoot(3, name));
  EXPECT_EQ("Liabilities:US:Credit-Card:Blue", AccountRoot(4, name));
  EXPECT_EQ("Liabilities:US:Credit-Card:Blue", AccountRoot(5, name));
}

//     def test_has_component(self):
//         self.assertTrue(account.has_component('Liabilities:US:Credit-Card', 'US'))
//         self.assertFalse(account.has_component('Liabilities:US:Credit-Card', 'CA'))
//         self.assertTrue(account.has_component('Liabilities:US:Credit-Card', 'Credit-Card'))
//         self.assertTrue(account.has_component('Liabilities:US:Credit-Card', 'Liabilities'))
//         self.assertFalse(account.has_component('Liabilities:US:Credit-Card', 'Credit'))
//         self.assertFalse(account.has_component('Liabilities:US:Credit-Card', 'Card'))
//
//     def test_commonprefix(self):
//         EXPECT_EQ('Assets:US:TD',
//                          account.commonprefix(['Assets:US:TD:Checking',
//                                                'Assets:US:TD:Savings']))
//         EXPECT_EQ('Assets:US',
//                          account.commonprefix(['Assets:US:TD:Checking',
//                                                'Assets:US:BofA:Checking']))
//         EXPECT_EQ('Assets',
//                          account.commonprefix(['Assets:US:TD:Checking',
//                                                'Assets:CA:RBC:Savings']))
//         EXPECT_EQ('',
//                          account.commonprefix(['Assets:US:TD:Checking',
//                                                'Liabilities:US:CreditCard']))
//         EXPECT_EQ('',
//                          account.commonprefix(['']))
//
//     def test_parent_matcher(self):
//         is_child = account.parent_matcher('Assets:Bank:Checking')
//         self.assertTrue(is_child('Assets:Bank:Checking'))
//         self.assertTrue(is_child('Assets:Bank:Checking:SubAccount'))
//         self.assertFalse(is_child('Assets:Bank:CheckingOld'))
//         self.assertFalse(is_child('Assets:Bank:Checking-Old'))
//
//     def test_parents(self):
//         iterator = account.parents('Assets:Bank:Checking')
//         self.assertIsInstance(iterator, types.GeneratorType)
//         EXPECT_EQ(['Assets:Bank:Checking', 'Assets:Bank', 'Assets'],
//                          list(iterator))
//
//
//
// class TestWalk(test_utils.TmpFilesTestBase):
//
//     TEST_DOCUMENTS = [
//         'root/Assets/US/Bank/Checking/other.txt',
//         'root/Assets/US/Bank/Checking/2014-06-08.bank-statement.pdf',
//         'root/Assets/US/Bank/Checking/otherdir/',
//         'root/Assets/US/Bank/Checking/otherdir/another.txt',
//         'root/Assets/US/Bank/Checking/otherdir/2014-06-08.bank-statement.pdf',
//         'root/Assets/US/Bank/Savings/2014-07-01.savings.pdf',
//         'root/Liabilities/US/Bank/',  # Empty directory.
//     ]
//
//     def test_walk(self):
//         actual_data = [
//             (root[len(self.root):], account_, dirs, files)
//             for root, account_, dirs, files in account.walk(self.root)]
//
//         EXPECT_EQ([
//             ('/Assets/US', 'Assets:US',
//              ['Bank'],
//              []),
//             ('/Assets/US/Bank', 'Assets:US:Bank',
//              ['Checking', 'Savings'],
//              []),
//             ('/Assets/US/Bank/Checking', 'Assets:US:Bank:Checking',
//              ['otherdir'],
//              ['2014-06-08.bank-statement.pdf', 'other.txt']),
//
//             ('/Assets/US/Bank/Savings', 'Assets:US:Bank:Savings',
//              [],
//              ['2014-07-01.savings.pdf']),
//
//             ('/Liabilities/US', 'Liabilities:US',
//              ['Bank'],
//              []),
//             ('/Liabilities/US/Bank', 'Liabilities:US:Bank',
//              [],
//              []),
//             ], actual_data)
//
//
// class TestAccountTransformer(unittest.TestCase):
//
//     def test_render(self):
//         xfr = account.AccountTransformer('__')
//         EXPECT_EQ('Assets__US__BofA__Checking',
//                          xfr.render('Assets:US:BofA:Checking'))
//
//     def test_parse(self):
//         xfr = account.AccountTransformer('__')
//         EXPECT_EQ('Assets:US:BofA:Checking',
//                          xfr.parse('Assets__US__BofA__Checking'))
//
//     def test_noop(self):
//         xfr = account.AccountTransformer()
//         acc = 'Assets:US:BofA:Checking'
//         EXPECT_EQ(acc, xfr.render(acc))
//         EXPECT_EQ(acc, xfr.parse(acc))
//
//
// if __name__ == '__main__':
//     unittest.main()

}  // namespace
}  // namespace beancount
