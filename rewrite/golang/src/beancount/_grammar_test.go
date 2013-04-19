package beancount

import (
	"testing"
	// "regexp"
	// "fmt"
)

type parseTest struct {
	name  string
	input string
}

var parseTests = []parseTest{

	{"empty", `

`},
	{"transations", `

2013-04-18 * \"Blah-di-blah\"
  Assets:US:RBC:Checking        102.30 CAD
  Expenses:Restaurant\

`},

}

// 'collect' gathers the emitted items into a slice.
func collect(t *parseTest) (items []item) {
	l := MakeLexer(t.name, t.input)
	for {
		item := l.NextTok()
		items = append(items, item)
		if item.Type == EOF || item.Type == ERROR {
			break
		}
	}
	return
}

func equal(i1, i2 []item, checkPos bool) bool {
	if len(i1) != len(i2) {
		return false
	}
	for k := range i1 {
		if i1[k].Type != i2[k].Type {
			return false
		}
		if i1[k].val != i2[k].val {
			return false
		}
		if checkPos && i1[k].pos != i2[k].pos {
			return false
		}
	}
	return true
}

func TestParser(t *testing.T) {
	for _, test := range parseTests {
		items := collectGrammar(&test)
		// if !equal(items, test.parseTests, false) {
		// 	t.Errorf("%s: got\n\t%+v\nexpected\n\t%v", test.name, items, test.items)
		// }
	}
}
