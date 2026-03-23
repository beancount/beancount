package core

import (
	"regexp"
	"strings"
)

// Account separator for account names.
const AccountSeparator = ":"

// Regular expressions for account validation.
// Beancount account components must start with an uppercase letter, a digit, or any non-ASCII character.
// UTF-8-ONLY in Beancount is defined as any multi-byte UTF-8 character.
// In Go, we can use [^\x00-\x7f] to match any non-ASCII character.
const (
	utf8Only      = `[^\x00-\x7f]`
	accCompTypeRE = `(?:[A-Z]|` + utf8Only + `)(?:[A-Za-z0-9\-]|` + utf8Only + `)*`
	accCompNameRE = `(?:[A-Z0-9]|` + utf8Only + `)(?:[A-Za-z0-9\-]|` + utf8Only + `)*`
)

var (
	validRootRe = regexp.MustCompile("^" + accCompTypeRE + "$")
	validAccountRe = regexp.MustCompile(`^(?:` + accCompTypeRE + `)(?:` + AccountSeparator + accCompNameRE + `)+$`)
)

// IsValidRoot returns true if the string is a valid root account name.
func IsValidRoot(s string) bool {
	return validRootRe.MatchString(s)
}

// IsValid returns true if the string is a valid account name.
func IsValid(s string) bool {
	return validAccountRe.MatchString(s)
}

// Join joins the components with the account separator.
func Join(components ...string) string {
	return strings.Join(components, AccountSeparator)
}

// Split splits an account's name into its components.
func Split(accountName string) []string {
	if accountName == "" {
		return nil
	}
	return strings.Split(accountName, AccountSeparator)
}

// Parent returns the name of the parent account or an empty string if at the root.
func Parent(accountName string) string {
	if accountName == "" {
		return ""
	}
	idx := strings.LastIndex(accountName, AccountSeparator)
	if idx == -1 {
		return ""
	}
	return accountName[:idx]
}

// Leaf returns the name of the leaf component of the account.
func Leaf(accountName string) string {
	if accountName == "" {
		return ""
	}
	idx := strings.LastIndex(accountName, AccountSeparator)
	if idx == -1 {
		return accountName
	}
	return accountName[idx+len(AccountSeparator):]
}

// Parents returns all parent accounts including the account itself.
func Parents(accountName string) []string {
	if accountName == "" {
		return nil
	}
	components := Split(accountName)
	res := make([]string, len(components))
	for i := range components {
		res[i] = Join(components[:i+1]...)
	}
	// Reverse to match Python's parents (child to root)
	for i, j := 0, len(res)-1; i < j; i, j = i+1, j-1 {
		res[i], res[j] = res[j], res[i]
	}
	return res
}

// HasComponent returns true if the account contains a given component.
func HasComponent(accountName, component string) bool {
	components := Split(accountName)
	for _, c := range components {
		if c == component {
			return true
		}
	}
	return false
}

// SansRoot returns the account name without its root component.
func SansRoot(accountName string) string {
	components := Split(accountName)
	if len(components) <= 1 {
		return ""
	}
	return Join(components[1:]...)
}

// Root returns the root component of the account name up to a given depth.
func Root(depth int, accountName string) string {
	components := Split(accountName)
	if len(components) == 0 {
		return ""
	}
	if depth > len(components) {
		depth = len(components)
	}
	if depth <= 0 {
		return ""
	}
	return Join(components[:depth]...)
}

// CommonPrefix returns the common account prefix between multiple accounts.
func CommonPrefix(accounts []string) string {
	if len(accounts) == 0 {
		return ""
	}
	common := Split(accounts[0])
	for _, acc := range accounts[1:] {
		comp := Split(acc)
		n := len(common)
		if len(comp) < n {
			n = len(comp)
		}
		newCommon := []string{}
		for i := 0; i < n; i++ {
			if common[i] == comp[i] {
				newCommon = append(newCommon, common[i])
			} else {
				break
			}
		}
		common = newCommon
	}
	return Join(common...)
}

// ParentMatcher returns a function that matches an account and its subaccounts.
func ParentMatcher(parent string) func(string) bool {
	prefix := parent + AccountSeparator
	return func(account string) bool {
		return account == parent || strings.HasPrefix(account, prefix)
	}
}

// AccountTransformer handles renaming or transforming account names.
type AccountTransformer struct {
	rsep string
}

func NewAccountTransformer(rsep string) *AccountTransformer {
	return &AccountTransformer{rsep: rsep}
}

func (t *AccountTransformer) Render(accountName string) string {
	if t.rsep == "" {
		return accountName
	}
	return strings.ReplaceAll(accountName, AccountSeparator, t.rsep)
}

func (t *AccountTransformer) Parse(transformedName string) string {
	if t.rsep == "" {
		return transformedName
	}
	return strings.ReplaceAll(transformedName, t.rsep, AccountSeparator)
}
