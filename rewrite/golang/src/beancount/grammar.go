
//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:5

// This tag will end up in the generated y.go, so that forgetting
// 'make clean' does not fail the next build.

// +build ignore

// units.y
// example of a Go yacc program
// usage is
//	go tool yacc -p "units_" units.y (produces y.go)
//	go build -o units y.go
//	./units $GOROOT/src/cmd/yacc/units.txt
//	you have: c
//	you want: furlongs/fortnight
//		* 1.8026178e+12
//		/ 5.5474878e-13
//	you have:

package main

import (
	"bufio"
	"flag"
	"fmt"
	"math"
	"runtime"
	"os"
	"path/filepath"
	"strconv"
	"unicode/utf8"
)

const (
	Ndim = 15  // number of dimensions
	Maxe = 695 // log of largest number
)

type Node struct {
	vval float64
	dim  [Ndim]int8
}

type Var struct {
	name string
	node Node
}

var fi *bufio.Reader // input
var fund [Ndim]*Var  // names of fundamental units
var line string      // current input line
var lineno int       // current input line number
var linep int        // index to next rune in unput
var nerrors int      // error count
var one Node         // constant one
var peekrune rune    // backup runt from input
var retnode1 Node
var retnode2 Node
var retnode Node
var sym string
var vflag bool

//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:67
type bean_SymType struct {
	yys int
	node Node
	vvar *Var
	numb int
	vval float64
}

const VAR = 57346

var bean_Toknames = []string{
	"VAR",
}
var bean_Statenames = []string{}

const bean_EofCode = 1
const bean_ErrCode = 2
const bean_MaxDepth = 200

//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:208


type UnitsLex int

func (UnitsLex) Lex(yylval *units_SymType) int {
	var c rune
	var i int

	c = peekrune
	peekrune = ' '

loop:
	if (c >= '0' && c <= '9') || c == '.' {
		goto numb
	}
	if ralpha(c) {
		goto alpha
	}
	switch c {
	case ' ', '\t':
		c = getrune()
		goto loop
	case '×':
		return '*'
	case '÷':
		return '/'
	case '¹', 'ⁱ':
		yylval.numb = 1
		return _SUP
	case '²', '⁲':
		yylval.numb = 2
		return _SUP
	case '³', '⁳':
		yylval.numb = 3
		return _SUP
	}
	return int(c)

alpha:
	sym = ""
	for i = 0; ; i++ {
		sym += string(c)
		c = getrune()
		if !ralpha(c) {
			break
		}
	}
	peekrune = c
	yylval.vvar = lookup(0)
	return VAR

numb:
	sym = ""
	for i = 0; ; i++ {
		sym += string(c)
		c = getrune()
		if !rdigit(c) {
			break
		}
	}
	peekrune = c
	f, err := strconv.ParseFloat(sym, 64)
	if err != nil {
		fmt.Printf("error converting %v\n", sym)
		f = 0
	}
	yylval.vval = f
	return VÄL
}

func (UnitsLex) Error(s string) {
	Errorf("syntax error, last name: %v", sym)
}

func main() {
	var file string

	flag.BoolVar(&vflag, "v", false, "verbose")

	flag.Parse()

	file = filepath.Join(runtime.GOROOT(), "src/cmd/yacc/units.txt")
	if flag.NArg() > 0 {
		file = flag.Arg(0)
	} else if file == "" {
		fmt.Fprintf(os.Stderr, "cannot find data file units.txt; provide it as argument or set $GOROOT\n")
		os.Exit(1)
	}

	f, err := os.Open(file)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error opening %v: %v\n", file, err)
		os.Exit(1)
	}
	fi = bufio.NewReader(f)

	one.vval = 1

	/*
	 * read the 'units' file to
	 * develop a database
	 */
	lineno = 0
	for {
		lineno++
		if readline() {
			break
		}
		if len(line) == 0 || line[0] == '/' {
			continue
		}
		peekrune = ':'
		units_Parse(UnitsLex(0))
	}

	/*
	 * read the console to
	 * print ratio of pairs
	 */
	fi = bufio.NewReader(os.NewFile(0, "stdin"))

	lineno = 0
	for {
		if (lineno & 1) != 0 {
			fmt.Printf("you want: ")
		} else {
			fmt.Printf("you have: ")
		}
		if readline() {
			break
		}
		peekrune = '?'
		nerrors = 0
		units_Parse(UnitsLex(0))
		if nerrors != 0 {
			continue
		}
		if (lineno & 1) != 0 {
			if specialcase(&retnode, &retnode2, &retnode1) {
				fmt.Printf("\tis %v\n", &retnode)
			} else {
				div(&retnode, &retnode2, &retnode1)
				fmt.Printf("\t* %v\n", &retnode)
				div(&retnode, &retnode1, &retnode2)
				fmt.Printf("\t/ %v\n", &retnode)
			}
		} else {
			retnode2 = retnode1
		}
		lineno++
	}
	fmt.Printf("\n")
	os.Exit(0)
}

/*
 * all characters that have some
 * meaning. rest are usable as names
 */
func ralpha(c rune) bool {
	switch c {
	case 0, '+', '-', '*', '/', '[', ']', '(', ')',
		'^', ':', '?', ' ', '\t', '.', '|', '#',
		'×', '÷', '¹', 'ⁱ', '²', '⁲', '³', '⁳':
		return false
	}
	return true
}

/*
 * number forming character
 */
func rdigit(c rune) bool {
	switch c {
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
		'.', 'e', '+', '-':
		return true
	}
	return false
}

func Errorf(s string, v ...interface{}) {
	fmt.Printf("%v: %v\n\t", lineno, line)
	fmt.Printf(s, v...)
	fmt.Printf("\n")

	nerrors++
	if nerrors > 5 {
		fmt.Printf("too many errors\n")
		os.Exit(1)
	}
}

func Error(s string) {
	Errorf("%s", s)
}

func add(c, a, b *Node) {
	var i int
	var d int8

	for i = 0; i < Ndim; i++ {
		d = a.dim[i]
		c.dim[i] = d
		if d != b.dim[i] {
			Error("add must be like units")
		}
	}
	c.vval = fadd(a.vval, b.vval)
}

func sub(c, a, b *Node) {
	var i int
	var d int8

	for i = 0; i < Ndim; i++ {
		d = a.dim[i]
		c.dim[i] = d
		if d != b.dim[i] {
			Error("sub must be like units")
		}
	}
	c.vval = fadd(a.vval, -b.vval)
}

func mul(c, a, b *Node) {
	var i int

	for i = 0; i < Ndim; i++ {
		c.dim[i] = a.dim[i] + b.dim[i]
	}
	c.vval = fmul(a.vval, b.vval)
}

func div(c, a, b *Node) {
	var i int

	for i = 0; i < Ndim; i++ {
		c.dim[i] = a.dim[i] - b.dim[i]
	}
	c.vval = fdiv(a.vval, b.vval)
}

func xpn(c, a *Node, b int) {
	var i int

	*c = one
	if b < 0 {
		b = -b
		for i = 0; i < b; i++ {
			div(c, c, a)
		}
	} else {
		for i = 0; i < b; i++ {
			mul(c, c, a)
		}
	}
}

func specialcase(c, a, b *Node) bool {
	var i int
	var d, d1, d2 int8

	d1 = 0
	d2 = 0
	for i = 1; i < Ndim; i++ {
		d = a.dim[i]
		if d != 0 {
			if d != 1 || d1 != 0 {
				return false
			}
			d1 = int8(i)
		}
		d = b.dim[i]
		if d != 0 {
			if d != 1 || d2 != 0 {
				return false
			}
			d2 = int8(i)
		}
	}
	if d1 == 0 || d2 == 0 {
		return false
	}

	if fund[d1].name == "°C" && fund[d2].name == "°F" &&
		b.vval == 1 {
		for ll := 0; ll < len(c.dim); ll++ {
			c.dim[ll] = b.dim[ll]
		}
		c.vval = a.vval*9./5. + 32.
		return true
	}

	if fund[d1].name == "°F" && fund[d2].name == "°C" &&
		b.vval == 1 {
		for ll := 0; ll < len(c.dim); ll++ {
			c.dim[ll] = b.dim[ll]
		}
		c.vval = (a.vval - 32.) * 5. / 9.
		return true
	}
	return false
}

func printdim(str string, d, n int) string {
	var v *Var

	if n != 0 {
		v = fund[d]
		if v != nil {
			str += fmt.Sprintf("%v", v.name)
		} else {
			str += fmt.Sprintf("[%d]", d)
		}
		switch n {
		case 1:
			break
		case 2:
			str += "²"
		case 3:
			str += "³"
		default:
			str += fmt.Sprintf("^%d", n)
		}
	}
	return str
}

func (n Node) String() string {
	var str string
	var f, i, d int

	str = fmt.Sprintf("%.7e ", n.vval)

	f = 0
	for i = 1; i < Ndim; i++ {
		d = int(n.dim[i])
		if d > 0 {
			str = printdim(str, i, d)
		} else if d < 0 {
			f = 1
		}
	}

	if f != 0 {
		str += " /"
		for i = 1; i < Ndim; i++ {
			d = int(n.dim[i])
			if d < 0 {
				str = printdim(str, i, -d)
			}
		}
	}

	return str
}

func (v *Var) String() string {
	var str string
	str = fmt.Sprintf("%v %v", v.name, v.node)
	return str
}

func readline() bool {
	s, err := fi.ReadString('\n')
	if err != nil {
		return true
	}
	line = s
	linep = 0
	return false
}

func getrune() rune {
	var c rune
	var n int

	if linep >= len(line) {
		return 0
	}
	c, n = utf8.DecodeRuneInString(line[linep:len(line)])
	linep += n
	if c == '\n' {
		c = 0
	}
	return c
}

var symmap = make(map[string]*Var) // symbol table

func lookup(f int) *Var {
	var p float64
	var w *Var

	v, ok := symmap[sym]
	if ok {
		return v
	}
	if f != 0 {
		return nil
	}
	v = new(Var)
	v.name = sym
	symmap[sym] = v

	p = 1
	for {
		p = fmul(p, pname())
		if p == 0 {
			break
		}
		w = lookup(1)
		if w != nil {
			v.node = w.node
			v.node.vval = fmul(v.node.vval, p)
			break
		}
	}
	return v
}

type Prefix struct {
	vval float64
	name string
}

var prefix = []Prefix{ // prefix table
	{1e-24, "yocto"},
	{1e-21, "zepto"},
	{1e-18, "atto"},
	{1e-15, "femto"},
	{1e-12, "pico"},
	{1e-9, "nano"},
	{1e-6, "micro"},
	{1e-6, "μ"},
	{1e-3, "milli"},
	{1e-2, "centi"},
	{1e-1, "deci"},
	{1e1, "deka"},
	{1e2, "hecta"},
	{1e2, "hecto"},
	{1e3, "kilo"},
	{1e6, "mega"},
	{1e6, "meg"},
	{1e9, "giga"},
	{1e12, "tera"},
	{1e15, "peta"},
	{1e18, "exa"},
	{1e21, "zetta"},
	{1e24, "yotta"},
}

func pname() float64 {
	var i, j, n int
	var s string

	/*
	 * rip off normal prefixs
	 */
	n = len(sym)
	for i = 0; i < len(prefix); i++ {
		s = prefix[i].name
		j = len(s)
		if j < n && sym[0:j] == s {
			sym = sym[j:n]
			return prefix[i].vval
		}
	}

	/*
	 * rip off 's' suffixes
	 */
	if n > 2 && sym[n-1] == 's' {
		sym = sym[0 : n-1]
		return 1
	}

	return 0
}

// careful multiplication
// exponents (log) are checked before multiply
func fmul(a, b float64) float64 {
	var l float64

	if b <= 0 {
		if b == 0 {
			return 0
		}
		l = math.Log(-b)
	} else {
		l = math.Log(b)
	}

	if a <= 0 {
		if a == 0 {
			return 0
		}
		l += math.Log(-a)
	} else {
		l += math.Log(a)
	}

	if l > Maxe {
		Error("overflow in multiply")
		return 1
	}
	if l < -Maxe {
		Error("underflow in multiply")
		return 0
	}
	return a * b
}

// careful division
// exponents (log) are checked before divide
func fdiv(a, b float64) float64 {
	var l float64

	if b <= 0 {
		if b == 0 {
			Errorf("division by zero: %v %v", a, b)
			return 1
		}
		l = math.Log(-b)
	} else {
		l = math.Log(b)
	}

	if a <= 0 {
		if a == 0 {
			return 0
		}
		l -= math.Log(-a)
	} else {
		l -= math.Log(a)
	}

	if l < -Maxe {
		Error("overflow in divide")
		return 1
	}
	if l > Maxe {
		Error("underflow in divide")
		return 0
	}
	return a / b
}

func fadd(a, b float64) float64 {
	return a + b
}

//line yacctab:1
var bean_Exca = []int{
	-1, 1,
	1, -1,
	-2, 0,
}

const bean_Nprod = 20
const bean_Private = 57344

var bean_TokenNames []string
var bean_States []string

const bean_Last = 39

var bean_Act = []int{

	8, 10, 7, 9, 15, 16, 11, 11, 19, 14,
	21, 29, 6, 20, 17, 18, 12, 12, 15, 16,
	25, 26, 4, 28, 27, 5, 19, 19, 23, 24,
	13, 2, 1, 3, 0, 0, 0, 0, 22,
}
var bean_Pact = []int{

	26, -1000, 18, 2, 3, 10, 4, 2, 1, -3,
	-1000, -1000, 2, 10, -1000, 2, 2, 2, 2, 1,
	2, 2, -4, 4, 4, 2, 2, -3, -1000, -1000,
}
var bean_Pgo = []int{

	0, 32, 25, 1, 3, 0, 2, 12,
}
var bean_R1 = []int{

	0, 1, 1, 1, 1, 1, 2, 2, 2, 7,
	7, 7, 6, 6, 5, 5, 4, 4, 3, 3,
}
var bean_R2 = []int{

	0, 3, 3, 1, 2, 1, 1, 3, 3, 1,
	3, 3, 1, 2, 1, 3, 1, 3, 1, 3,
}
var bean_Chk = []int{

	-1000, -1, 5, 7, 4, -2, -7, -6, -5, -4,
	-3, 4, 14, -2, 6, 8, 9, 10, 11, -5,
	12, 13, -2, -7, -7, -6, -6, -4, -3, 15,
}
var bean_Def = []int{

	0, -2, 3, 5, 0, 4, 6, 9, 12, 14,
	16, 18, 0, 1, 2, 0, 0, 0, 0, 13,
	0, 0, 0, 7, 8, 10, 11, 15, 17, 19,
}
var bean_Tok1 = []int{

	1, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 6, 3, 3, 3, 3,
	14, 15, 10, 8, 3, 9, 3, 11, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 5, 3,
	3, 3, 3, 7, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 12, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 13,
}
var bean_Tok2 = []int{

	2, 3, 4,
}
var bean_Tok3 = []int{
	0,
}

//line yaccpar:1

/*	parser for yacc output	*/

var bean_Debug = 0

type bean_Lexer interface {
	Lex(lval *bean_SymType) int
	Error(s string)
}

const bean_Flag = -1000

func bean_Tokname(c int) string {
	if c > 0 && c <= len(bean_Toknames) {
		if bean_Toknames[c-1] != "" {
			return bean_Toknames[c-1]
		}
	}
	return fmt.Sprintf("tok-%v", c)
}

func bean_Statname(s int) string {
	if s >= 0 && s < len(bean_Statenames) {
		if bean_Statenames[s] != "" {
			return bean_Statenames[s]
		}
	}
	return fmt.Sprintf("state-%v", s)
}

func bean_lex1(lex bean_Lexer, lval *bean_SymType) int {
	c := 0
	char := lex.Lex(lval)
	if char <= 0 {
		c = bean_Tok1[0]
		goto out
	}
	if char < len(bean_Tok1) {
		c = bean_Tok1[char]
		goto out
	}
	if char >= bean_Private {
		if char < bean_Private+len(bean_Tok2) {
			c = bean_Tok2[char-bean_Private]
			goto out
		}
	}
	for i := 0; i < len(bean_Tok3); i += 2 {
		c = bean_Tok3[i+0]
		if c == char {
			c = bean_Tok3[i+1]
			goto out
		}
	}

out:
	if c == 0 {
		c = bean_Tok2[1] /* unknown char */
	}
	if bean_Debug >= 3 {
		fmt.Printf("lex %U %s\n", uint(char), bean_Tokname(c))
	}
	return c
}

func bean_Parse(bean_lex bean_Lexer) int {
	var bean_n int
	var bean_lval bean_SymType
	var bean_VAL bean_SymType
	bean_S := make([]bean_SymType, bean_MaxDepth)

	Nerrs := 0   /* number of errors */
	Errflag := 0 /* error recovery flag */
	bean_state := 0
	bean_char := -1
	bean_p := -1
	goto bean_stack

ret0:
	return 0

ret1:
	return 1

bean_stack:
	/* put a state and value onto the stack */
	if bean_Debug >= 4 {
		fmt.Printf("char %v in %v\n", bean_Tokname(bean_char), bean_Statname(bean_state))
	}

	bean_p++
	if bean_p >= len(bean_S) {
		nyys := make([]bean_SymType, len(bean_S)*2)
		copy(nyys, bean_S)
		bean_S = nyys
	}
	bean_S[bean_p] = bean_VAL
	bean_S[bean_p].yys = bean_state

bean_newstate:
	bean_n = bean_Pact[bean_state]
	if bean_n <= bean_Flag {
		goto bean_default /* simple state */
	}
	if bean_char < 0 {
		bean_char = bean_lex1(bean_lex, &bean_lval)
	}
	bean_n += bean_char
	if bean_n < 0 || bean_n >= bean_Last {
		goto bean_default
	}
	bean_n = bean_Act[bean_n]
	if bean_Chk[bean_n] == bean_char { /* valid shift */
		bean_char = -1
		bean_VAL = bean_lval
		bean_state = bean_n
		if Errflag > 0 {
			Errflag--
		}
		goto bean_stack
	}

bean_default:
	/* default state action */
	bean_n = bean_Def[bean_state]
	if bean_n == -2 {
		if bean_char < 0 {
			bean_char = bean_lex1(bean_lex, &bean_lval)
		}

		/* look through exception table */
		xi := 0
		for {
			if bean_Exca[xi+0] == -1 && bean_Exca[xi+1] == bean_state {
				break
			}
			xi += 2
		}
		for xi += 2; ; xi += 2 {
			bean_n = bean_Exca[xi+0]
			if bean_n < 0 || bean_n == bean_char {
				break
			}
		}
		bean_n = bean_Exca[xi+1]
		if bean_n < 0 {
			goto ret0
		}
	}
	if bean_n == 0 {
		/* error ... attempt to resume parsing */
		switch Errflag {
		case 0: /* brand new error */
			bean_lex.Error("syntax error")
			Nerrs++
			if bean_Debug >= 1 {
				fmt.Printf("%s", bean_Statname(bean_state))
				fmt.Printf("saw %s\n", bean_Tokname(bean_char))
			}
			fallthrough

		case 1, 2: /* incompletely recovered error ... try again */
			Errflag = 3

			/* find a state where "error" is a legal shift action */
			for bean_p >= 0 {
				bean_n = bean_Pact[bean_S[bean_p].yys] + bean_ErrCode
				if bean_n >= 0 && bean_n < bean_Last {
					bean_state = bean_Act[bean_n] /* simulate a shift of "error" */
					if bean_Chk[bean_state] == bean_ErrCode {
						goto bean_stack
					}
				}

				/* the current p has no shift on "error", pop stack */
				if bean_Debug >= 2 {
					fmt.Printf("error recovery pops state %d\n", bean_S[bean_p].yys)
				}
				bean_p--
			}
			/* there is no state on the stack with an error shift ... abort */
			goto ret1

		case 3: /* no shift yet; clobber input char */
			if bean_Debug >= 2 {
				fmt.Printf("error recovery discards %s\n", bean_Tokname(bean_char))
			}
			if bean_char == bean_EofCode {
				goto ret1
			}
			bean_char = -1
			goto bean_newstate /* try again in the same state */
		}
	}

	/* reduction by production bean_n */
	if bean_Debug >= 2 {
		fmt.Printf("reduce %v in:\n\t%v\n", bean_n, bean_Statname(bean_state))
	}

	bean_nt := bean_n
	bean_pt := bean_p
	_ = bean_pt // guard against "declared and not used"

	bean_p -= bean_R2[bean_n]
	bean_VAL = bean_S[bean_p+1]

	/* consult goto table to find next state */
	bean_n = bean_R1[bean_n]
	bean_g := bean_Pgo[bean_n]
	bean_j := bean_g + bean_S[bean_p].yys + 1

	if bean_j >= bean_Last {
		bean_state = bean_Act[bean_g]
	} else {
		bean_state = bean_Act[bean_j]
		if bean_Chk[bean_state] != -bean_n {
			bean_state = bean_Act[bean_g]
		}
	}
	// dummy call; replaced with literal code
	switch bean_nt {

	case 1:
		//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:82
		{
			var f int
			f = int(bean_S[bean_pt-1].vvar.node.dim[0])
			bean_S[bean_pt-1].vvar.node = bean_S[bean_pt-0].node
			bean_S[bean_pt-1].vvar.node.dim[0] = 1
			if f != 0 {
				Errorf("redefinition of %v", bean_S[bean_pt-1].vvar.name)
			} else if vflag {
				fmt.Printf("%v\t%v\n", bean_S[bean_pt-1].vvar.name, &bean_S[bean_pt-1].vvar.node)
			}
		}
	case 2:
		//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:94
		{
			var f, i int
			for i = 1; i < Ndim; i++ {
				if fund[i] == nil {
					break
				}
			}
			if i >= Ndim {
				Error("too many dimensions")
				i = Ndim - 1
			}
			fund[i] = bean_S[bean_pt-1].vvar
			f = int(bean_S[bean_pt-1].vvar.node.dim[0])
			bean_S[bean_pt-1].vvar.node = one
			bean_S[bean_pt-1].vvar.node.dim[0] = 1
			bean_S[bean_pt-1].vvar.node.dim[i] = 1
			if f != 0 {
				Errorf("redefinition of %v", bean_S[bean_pt-1].vvar.name)
			} else if vflag {
				fmt.Printf("%v\t#\n", bean_S[bean_pt-1].vvar.name)
			}
		}
	case 3:
		//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:117
		{
		}
	case 4:
		//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:120
		{
			retnode1 = bean_S[bean_pt-0].node
		}
	case 5:
		//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:124
		{
			retnode1 = one
		}
	case 6:
		bean_VAL.node = bean_S[bean_pt-0].node
	case 7:
		//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:131
		{
			add(&bean_VAL.node, &bean_S[bean_pt-2].node, &bean_S[bean_pt-0].node)
		}
	case 8:
		//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:135
		{
			sub(&bean_VAL.node, &bean_S[bean_pt-2].node, &bean_S[bean_pt-0].node)
		}
	case 9:
		bean_VAL.node = bean_S[bean_pt-0].node
	case 10:
		//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:142
		{
			mul(&bean_VAL.node, &bean_S[bean_pt-2].node, &bean_S[bean_pt-0].node)
		}
	case 11:
		//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:146
		{
			div(&bean_VAL.node, &bean_S[bean_pt-2].node, &bean_S[bean_pt-0].node)
		}
	case 12:
		bean_VAL.node = bean_S[bean_pt-0].node
	case 13:
		//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:153
		{
			mul(&bean_VAL.node, &bean_S[bean_pt-1].node, &bean_S[bean_pt-0].node)
		}
	case 14:
		//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:160
		{
	// xpn(&$$, &$1, $2)
	}
	case 15:
		//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:164
		{
			var i int
			for i = 1; i < Ndim; i++ {
				if bean_S[bean_pt-0].node.dim[i] != 0 {
					Error("exponent has units")
					bean_VAL.node = bean_S[bean_pt-2].node
					break
				}
			}
			if i >= Ndim {
				i = int(bean_S[bean_pt-0].node.vval)
				if float64(i) != bean_S[bean_pt-0].node.vval {
					Error("exponent not integral")
				}
				xpn(&bean_VAL.node, &bean_S[bean_pt-2].node, i)
			}
		}
	case 16:
		bean_VAL.node = bean_S[bean_pt-0].node
	case 17:
		//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:185
		{
			div(&bean_VAL.node, &bean_S[bean_pt-2].node, &bean_S[bean_pt-0].node)
		}
	case 18:
		//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:191
		{
			if bean_S[bean_pt-0].vvar.node.dim[0] == 0 {
				Errorf("undefined %v", bean_S[bean_pt-0].vvar.name)
				bean_VAL.node = one
			} else {
				bean_VAL.node = bean_S[bean_pt-0].vvar.node
			}
		}
	case 19:
		//line /home/blais/p/beancount/rewrite/golang/src/beancount/grammar.y:205
		{
			bean_VAL.node = bean_S[bean_pt-1].node
		}
	}
	goto bean_stack /* stack new state and value */
}
