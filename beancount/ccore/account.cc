// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/account.h"

#include <string>
#include <initializer_list>
#include <iostream>

#include "absl/strings/str_format.h"
#include "absl/strings/str_join.h"
#include "absl/strings/str_split.h"
#include "re2/re2.h"

namespace beancount {

// Component separator for account names.
const char* kSep = ":";

// Regular expression string that matches valid account name components.
// Categories are:
//   Lu: Uppercase letters.
//   L: All letters.
//   Nd: Decimal numbers.
const char* kAccountCompTypeRe = "[\\p{Lu}][\\p{L}\\p{Nd}\\-]*";
const char* kAccountCompNameRe = "[\\p{Lu}\\p{Nd}][\\p{L}\\p{Nd}\\-]*";

// Regular expression string that matches a valid account.
RE2 kAccountRE(absl::StrFormat("(?:%s)(?:%s%s)+",
                               kAccountCompTypeRe, kSep, kAccountCompNameRe));

// TODO(blais): Remove this.
// A dummy object which stands for the account type. Values in custom directives
// use this to disambiguate between string objects and account names.
// TYPE = '<AccountDummy>'

bool IsAccountValid(string_view account) {
  return RE2::FullMatch(account, kAccountRE);
}

string JoinAccount(std::initializer_list<string_view> il) {
  return absl::StrJoin(il, kSep);
}

vector<string> SplitAccount(string_view account) {
  if (account.empty()) {
    return {};
  }
  return absl::StrSplit(account, kSep);
}

string ParentAccount(string_view account) {
  if (account.empty()) {
    return "";
  }
  auto components = SplitAccount(account);
  return absl::StrJoin(components.begin(), components.end()-1, kSep);
}

string LeafAccount(string_view account) {
  if (account.empty()) {
    return "";
  }
  auto startpos = account.rfind(kSep);
  if (startpos == string::npos){
    return string(account);
  }
  auto begin = account.data() + startpos + 1;
  return string(begin, account.end() - begin);
}

// Get the name of the account without the root. For example, an input of
// 'Assets:BofA:Checking' will produce 'BofA:Checking'.
string AccountSansRoot(string_view account) {
  if (account.empty()) {
    return "";
  }
  vector<string_view> components = absl::StrSplit(account, kSep);
  auto iter = components.begin() + 1;
  return absl::StrJoin(iter, components.end(), kSep);
}

// Return the first few components of an account's name.
string AccountRoot(int num_components, string_view account) {
  vector<string_view> components = absl::StrSplit(account, kSep);
  if (components.size() < num_components) {
    return string(account);
  }
  auto iter = components.begin() + num_components;
  return absl::StrJoin(components.begin(), iter, kSep);
}


// TODO(blais): Continue here.

// def has_component(account_name, component):
//     """Return true if one of the account contains a given component.
//
//     Args:
//       account_name: A string, an account name.
//       component: A string, a component of an account name. For instance,
//         ``Food`` in ``Expenses:Food:Restaurant``. All components are considered.
//     Returns:
//       Boolean: true if the component is in the account. Note that a component
//       name must be whole, that is ``NY`` is not in ``Expenses:Taxes:StateNY``.
//     """
//     return bool(re.search('(^|:){}(:|$)'.format(component), account_name))
//
//
// def commonprefix(accounts):
//     """Return the common prefix of a list of account names.
//
//     Args:
//       accounts: A sequence of account name strings.
//     Returns:
//       A string, the common parent account. If none, returns an empty string.
//     """
//     accounts_lists = [account_.split(sep)
//                       for account_ in accounts]
//     # Note: the os.path.commonprefix() function just happens to work here.
//     # Inspect its code, and even the special case of no common prefix
//     # works well with str.join() below.
//     common_list = path.commonprefix(accounts_lists)
//     return sep.join(common_list)
//
//
// def walk(root_directory):
//     """A version of os.walk() which yields directories that are valid account names.
//
//     This only yields directories that are accounts... it skips the other ones.
//     For convenience, it also yields you the account's name.
//
//     Args:
//       root_directory: A string, the name of the root of the hierarchy to be walked.
//     Yields:
//       Tuples of (root, account-name, dirs, files), similar to os.walk().
//     """
//     for root, dirs, files in os.walk(root_directory):
//         dirs.sort()
//         files.sort()
//         relroot = root[len(root_directory)+1:]
//         account_name = relroot.replace(os.sep, sep)
//         if is_valid(account_name):
//             yield (root, account_name, dirs, files)
//
//
// def parent_matcher(account_name):
//     """Build a predicate that returns whether an account is under the given one.
//
//     Args:
//       account_name: The name of the parent account we want to check for.
//     Returns:
//       A callable, which, when called, will return true if the given account is a
//       child of ``account_name``.
//     """
//     return re.compile(r'{}($|{})'.format(re.escape(account_name), sep)).match
//
//
// def parents(account_name):
//     """A generator of the names of the parents of this account, including this account.
//
//     Args:
//       account_name: The name of the account we want to start iterating from.
//     Returns:
//       A generator of account name strings.
//     """
//     while account_name:
//         yield account_name
//         account_name = parent(account_name)
//
//
// class AccountTransformer:
//     """Account name transformer.
//
//     This is used to support Win... huh, filesystems and platforms which do not
//     support colon characters.
//
//     Attributes:
//       rsep: A character string, the new separator to use in link names.
//     """
//     def __init__(self, rsep=None):
//         self.rsep = rsep
//
//     def render(self, account_name):
//         "Convert the account name to a transformed account name."
//         return (account_name
//                 if self.rsep is None
//                 else account_name.replace(sep, self.rsep))
//
//     def parse(self, transformed_name):
//         "Convert the transform account name to an account name."
//         return (transformed_name
//                 if self.rsep is None
//                 else transformed_name.replace(self.rsep, sep))
//

}  // namespace beancount
