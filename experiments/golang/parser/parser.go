package parser

import (
	"fmt"
	"time"

	"github.com/beancount/beancount/v3/core"
)

// Parser parses Beancount syntax into a list of directives.
type Parser struct {
	l         *Lexer
	cur       Token
	peek      Token
	options   map[string][]string
	errors    []error
	tagStack  map[string]int
	metaStack map[string]interface{}
}

// NewParser creates a new parser.
func NewParser(input, filename string) *Parser {
	p := &Parser{
		l:         NewLexer(input, filename),
		options:   make(map[string][]string),
		tagStack:  make(map[string]int),
		metaStack: make(map[string]interface{}),
	}
	p.nextToken()
	p.nextToken()
	return p
}

func (p *Parser) nextToken() {
	p.cur = p.peek
	p.peek = p.l.NextToken()
}

// Parse returns a list of directives and any errors encountered.
func (p *Parser) Parse() ([]core.Directive, []error) {
	var directives []core.Directive

	for p.cur.Type != EOF {
		if p.cur.Type == EOL || p.cur.Type == INDENT {
			p.nextToken()
			continue
		}

		if p.cur.Type == DATE {
			d := p.parseDirective()
			if d != nil {
				directives = append(directives, d)
			}
		} else {
			switch p.cur.Type {
			case OPTION:
				p.parseOption()
			case INCLUDE:
				p.parseInclude()
			case PLUGIN:
				p.parsePlugin()
			case PUSHTAG:
				p.parsePushTag()
			case POPTAG:
				p.parsePopTag()
			case PUSHMETA:
				p.parsePushMeta()
			case POPMETA:
				p.parsePopMeta()
			default:
				p.addError(fmt.Sprintf("Unexpected token at start of line: %v %q", p.cur.Type, p.cur.Literal))
				p.skipToEOL()
			}
		}
	}

	return directives, p.errors
}

func (p *Parser) addError(msg string) {
	p.errors = append(p.errors, fmt.Errorf("line %d: %s", p.cur.Line, msg))
}

func (p *Parser) skipToEOL() {
	for p.cur.Type != EOL && p.cur.Type != EOF {
		p.nextToken()
	}
}

func (p *Parser) parseDate(s string) time.Time {
	if len(s) < 10 {
		return time.Time{}
	}
	s = s[:4] + "-" + s[5:7] + "-" + s[8:10]
	t, _ := time.Parse("2006-01-02", s)
	return t
}

func (p *Parser) newMetadata(lineno int) core.Meta {
	meta := core.NewMetadata(p.l.Filename, lineno)
	for k, v := range p.metaStack {
		meta[k] = v
	}
	return meta
}

func (p *Parser) parseMetadata(meta core.Meta) {
	for p.cur.Type == INDENT {
		if p.peek.Type == KEY {
			p.nextToken() // consume INDENT
			key := p.cur.Literal
			p.nextToken() // consume KEY
			val := p.parseValue()
			meta[key] = val
			if p.cur.Type == EOL {
				p.nextToken()
			}
		} else {
			break
		}
	}
}

func (p *Parser) parseValue() interface{} {
	switch p.cur.Type {
	case STRING:
		res := p.cur.Literal
		p.nextToken()
		return res
	case NUMBER:
		numberStr := p.cur.Literal
		p.nextToken()
		if p.cur.Type == CURRENCY {
			currency := p.cur.Literal
			p.nextToken()
			return core.NewAmount(core.D(numberStr), currency)
		}
		return core.D(numberStr)
	case CURRENCY:
		res := p.cur.Literal
		p.nextToken()
		return res
	case DATE:
		res := p.parseDate(p.cur.Literal)
		p.nextToken()
		return res
	case ACCOUNT:
		res := p.cur.Literal
		p.nextToken()
		return res
	case BOOL:
		res := p.cur.Literal == "TRUE"
		p.nextToken()
		return res
	case NONE:
		p.nextToken()
		return nil
	default:
		return nil
	}
}

func (p *Parser) parseDirective() core.Directive {
	date := p.parseDate(p.cur.Literal)
	lineno := p.cur.Line
	p.nextToken()

	meta := p.newMetadata(lineno)

	var d core.Directive

	switch p.cur.Type {
	case OPEN:
		p.nextToken() // consume OPEN
		if p.cur.Type != ACCOUNT {
			p.addError("Expected account after open")
			p.skipToEOL()
			return nil
		}
		account := p.cur.Literal
		p.nextToken()

		var currencies []string
		if p.cur.Type == CURRENCY {
			currencies = append(currencies, p.cur.Literal)
			p.nextToken()
			for p.cur.Type == COMMA {
				p.nextToken()
				if p.cur.Type == CURRENCY {
					currencies = append(currencies, p.cur.Literal)
					p.nextToken()
				}
			}
		}

		if p.cur.Type == EOL {
			p.nextToken()
		}
		p.parseMetadata(meta)
		d = &core.Open{
			Date:       date,
			Meta:       meta,
			Account:    account,
			Currencies: currencies,
		}

	case CLOSE:
		p.nextToken()
		if p.cur.Type != ACCOUNT {
			p.addError("Expected account after close")
			p.skipToEOL()
			return nil
		}
		account := p.cur.Literal
		p.nextToken()

		if p.cur.Type == EOL {
			p.nextToken()
		}
		p.parseMetadata(meta)
		d = &core.Close{
			Date:    date,
			Meta:    meta,
			Account: account,
		}

	case COMMODITY:
		p.nextToken()
		if p.cur.Type != CURRENCY {
			p.addError("Expected currency after commodity")
			p.skipToEOL()
			return nil
		}
		currency := p.cur.Literal
		p.nextToken()
		if p.cur.Type == EOL {
			p.nextToken()
		}
		p.parseMetadata(meta)
		d = &core.Commodity{
			Date:     date,
			Meta:     meta,
			Currency: currency,
		}

	case PAD:
		p.nextToken()
		if p.cur.Type != ACCOUNT {
			p.addError("Expected account after pad")
			p.skipToEOL()
			return nil
		}
		account := p.cur.Literal
		p.nextToken()
		if p.cur.Type != ACCOUNT {
			p.addError("Expected source account after account in pad")
			p.skipToEOL()
			return nil
		}
		sourceAccount := p.cur.Literal
		p.nextToken()
		if p.cur.Type == EOL {
			p.nextToken()
		}
		p.parseMetadata(meta)
		d = &core.Pad{
			Date:          date,
			Meta:          meta,
			Account:       account,
			SourceAccount: sourceAccount,
		}

	case BALANCE:
		p.nextToken()
		if p.cur.Type != ACCOUNT {
			p.addError("Expected account after balance")
			p.skipToEOL()
			return nil
		}
		account := p.cur.Literal
		p.nextToken()

		amount, tolerance := p.parseAmountWithTolerance()
		if p.cur.Type == EOL {
			p.nextToken()
		}
		p.parseMetadata(meta)
		d = &core.Balance{
			Date:      date,
			Meta:      meta,
			Account:   account,
			Amount:    amount,
			Tolerance: tolerance,
		}

	case NOTE:
		p.nextToken()
		if p.cur.Type != ACCOUNT {
			p.addError("Expected account after note")
			p.skipToEOL()
			return nil
		}
		account := p.cur.Literal
		p.nextToken()
		if p.cur.Type != STRING {
			p.addError("Expected comment string after account in note")
			p.skipToEOL()
			return nil
		}
		comment := p.cur.Literal
		p.nextToken()
		if p.cur.Type == EOL {
			p.nextToken()
		}
		p.parseMetadata(meta)
		d = &core.Note{
			Date:    date,
			Meta:    meta,
			Account: account,
			Comment: comment,
		}

	case DOCUMENT:
		p.nextToken()
		if p.cur.Type != ACCOUNT {
			p.addError("Expected account after document")
			p.skipToEOL()
			return nil
		}
		account := p.cur.Literal
		p.nextToken()
		if p.cur.Type != STRING {
			p.addError("Expected filename string after account in document")
			p.skipToEOL()
			return nil
		}
		filename := p.cur.Literal
		p.nextToken()

		tags := make(map[string]struct{})
		links := make(map[string]struct{})
		for p.cur.Type == TAG || p.cur.Type == LINK {
			if p.cur.Type == TAG {
				tags[p.cur.Literal] = struct{}{}
			} else {
				links[p.cur.Literal] = struct{}{}
			}
			p.nextToken()
		}

		if p.cur.Type == EOL {
			p.nextToken()
		}
		p.parseMetadata(meta)
		d = &core.Document{
			Date:     date,
			Meta:     meta,
			Account:  account,
			Filename: filename,
			Tags:     tags,
			Links:    links,
		}

	case PRICE:
		p.nextToken()
		if p.cur.Type != CURRENCY {
			p.addError("Expected currency after price")
			p.skipToEOL()
			return nil
		}
		currency := p.cur.Literal
		p.nextToken()
		amount := p.parseAmount()
		if p.cur.Type == EOL {
			p.nextToken()
		}
		p.parseMetadata(meta)
		d = &core.Price{
			Date:     date,
			Meta:     meta,
			Currency: currency,
			Amount:   amount,
		}

	case EVENT:
		p.nextToken()
		if p.cur.Type != STRING {
			p.addError("Expected event type after event")
			p.skipToEOL()
			return nil
		}
		eventType := p.cur.Literal
		p.nextToken()
		if p.cur.Type != STRING {
			p.addError("Expected event description")
			p.skipToEOL()
			return nil
		}
		description := p.cur.Literal
		p.nextToken()
		if p.cur.Type == EOL {
			p.nextToken()
		}
		p.parseMetadata(meta)
		d = &core.Event{
			Date:        date,
			Meta:        meta,
			EventType:   eventType,
			Description: description,
		}

	case QUERY:
		p.nextToken()
		if p.cur.Type != STRING {
			p.addError("Expected query name after query")
			p.skipToEOL()
			return nil
		}
		name := p.cur.Literal
		p.nextToken()
		if p.cur.Type != STRING {
			p.addError("Expected query string")
			p.skipToEOL()
			return nil
		}
		queryString := p.cur.Literal
		p.nextToken()
		if p.cur.Type == EOL {
			p.nextToken()
		}
		p.parseMetadata(meta)
		d = &core.Query{
			Date:        date,
			Meta:        meta,
			Name:        name,
			QueryString: queryString,
		}

	case CUSTOM:
		p.nextToken()
		if p.cur.Type != STRING {
			p.addError("Expected custom type after custom")
			p.skipToEOL()
			return nil
		}
		typeStr := p.cur.Literal
		p.nextToken()
		var values []interface{}
		for p.cur.Type != EOL && p.cur.Type != EOF {
			val := p.parseValue()
			if val != nil {
				values = append(values, val)
			} else {
				if p.cur.Type != EOL && p.cur.Type != EOF {
					p.nextToken()
				}
			}
		}
		if p.cur.Type == EOL {
			p.nextToken()
		}
		p.parseMetadata(meta)
		d = &core.Custom{
			Date:    date,
			Meta:    meta,
			TypeStr: typeStr,
			Values:  values,
		}

	case FLAG:
		flag := p.cur.Literal
		p.nextToken()

		payee := ""
		narration := ""
		if p.cur.Type == STRING {
			payee = p.cur.Literal
			p.nextToken()
			if p.cur.Type == STRING {
				narration = p.cur.Literal
				p.nextToken()
			} else {
				narration = payee
				payee = ""
			}
		}

		tags := make(map[string]struct{})
		for t := range p.tagStack {
			tags[t] = struct{}{}
		}
		links := make(map[string]struct{})

		for p.cur.Type == TAG || p.cur.Type == LINK {
			if p.cur.Type == TAG {
				tags[p.cur.Literal] = struct{}{}
			} else {
				links[p.cur.Literal] = struct{}{}
			}
			p.nextToken()
		}

		if p.cur.Type == EOL {
			p.nextToken()
		}

		var postings []core.Posting
		for p.cur.Type == INDENT {
			if p.peek.Type == KEY {
				p.parseMetadata(meta)
				continue
			}
			p.nextToken() // consume INDENT
			if p.cur.Type == ACCOUNT {
				posting := p.parsePosting()
				if posting != nil {
					postings = append(postings, *posting)
				}
			} else if p.cur.Type == EOL {
				p.nextToken()
			} else {
				p.skipToEOL()
			}
		}

		d = &core.Transaction{
			Date:      date,
			Meta:      meta,
			Flag:      flag,
			Payee:     payee,
			Narration: narration,
			Tags:      tags,
			Links:     links,
			Postings:  postings,
		}

	default:
		p.addError(fmt.Sprintf("Unsupported directive type: %v", p.cur.Type))
		p.skipToEOL()
		return nil
	}

	return d
}

func (p *Parser) parseAmount() core.Amount {
	if p.cur.Type == NUMBER {
		numberStr := p.cur.Literal
		p.nextToken()
		if p.cur.Type == CURRENCY {
			currency := p.cur.Literal
			p.nextToken()
			return core.NewAmount(core.D(numberStr), currency)
		}
	}
	return core.Amount{}
}

func (p *Parser) parseAmountWithTolerance() (core.Amount, core.Decimal) {
	amount := p.parseAmount()
	var tolerance core.Decimal
	if p.cur.Type == TILDE {
		p.nextToken()
		if p.cur.Type == NUMBER {
			tolerance = core.D(p.cur.Literal)
			p.nextToken()
		}
	}
	return amount, tolerance
}

func (p *Parser) parsePosting() *core.Posting {
	account := p.cur.Literal
	p.nextToken()

	var units core.Amount
	var costSpec *core.CostSpec
	var price core.Amount

	if p.cur.Type == NUMBER {
		numberStr := p.cur.Literal
		p.nextToken()
		if p.cur.Type == CURRENCY {
			units = core.NewAmount(core.D(numberStr), p.cur.Literal)
			p.nextToken()
		} else {
			units.Number = core.D(numberStr)
		}
	} else if p.cur.Type == CURRENCY {
		units = core.NewAmount(core.MISSING, p.cur.Literal)
		p.nextToken()
	} else {
		units.Number = core.MISSING
	}

	if p.cur.Type == LCURL || p.cur.Type == LCURLCURL {
		merge := (p.cur.Type == LCURLCURL)
		p.nextToken()
		costSpec = &core.CostSpec{Merge: merge}
		for p.cur.Type != RCURL && p.cur.Type != RCURLCURL && p.cur.Type != EOL && p.cur.Type != EOF {
			switch p.cur.Type {
			case NUMBER:
				num := core.D(p.cur.Literal)
				p.nextToken()
				if p.cur.Type == HASH {
					p.nextToken()
					costSpec.NumberPer = num
					if p.cur.Type == NUMBER {
						costSpec.NumberTotal = core.D(p.cur.Literal)
						p.nextToken()
					}
				} else {
					costSpec.NumberPer = num
				}
				if p.cur.Type == CURRENCY {
					costSpec.Currency = p.cur.Literal
					p.nextToken()
				}
			case CURRENCY:
				costSpec.Currency = p.cur.Literal
				p.nextToken()
			case DATE:
				costSpec.Date = p.parseDate(p.cur.Literal)
				p.nextToken()
			case STRING:
				costSpec.Label = p.cur.Literal
				p.nextToken()
			case ASTERISK:
				costSpec.Merge = true
				p.nextToken()
			case COMMA:
				p.nextToken()
			default:
				p.nextToken()
			}
		}
		if p.cur.Type == RCURL || p.cur.Type == RCURLCURL {
			p.nextToken()
		}
	}

	if p.cur.Type == ATAT || p.cur.Type == AT {
		p.nextToken()
		if p.cur.Type == NUMBER {
			pNum := core.D(p.cur.Literal)
			p.nextToken()
			if p.cur.Type == CURRENCY {
				price = core.NewAmount(pNum, p.cur.Literal)
				p.nextToken()
			}
		} else if p.cur.Type == CURRENCY {
			price = core.NewAmount(core.MISSING, p.cur.Literal)
			p.nextToken()
		}
	}

	if p.cur.Type == EOL {
		p.nextToken()
	}

	lineno := p.cur.Line
	meta := core.NewMetadata(p.l.Filename, lineno)

	return &core.Posting{
		Account:  account,
		Units:    units,
		CostSpec: costSpec,
		Price:    price,
		Meta:     meta,
	}
}

func (p *Parser) parsePushTag() {
	p.nextToken() // consume PUSHTAG
	if p.cur.Type == TAG {
		p.tagStack[p.cur.Literal]++
		p.nextToken()
	}
	p.skipToEOL()
}

func (p *Parser) parsePopTag() {
	p.nextToken() // consume POPTAG
	if p.cur.Type == TAG {
		if count, ok := p.tagStack[p.cur.Literal]; ok {
			if count > 1 {
				p.tagStack[p.cur.Literal]--
			} else {
				delete(p.tagStack, p.cur.Literal)
			}
		}
		p.nextToken()
	}
	p.skipToEOL()
}

func (p *Parser) parsePushMeta() {
	p.nextToken() // consume PUSHMETA
	if p.cur.Type == KEY {
		key := p.cur.Literal
		p.nextToken()
		val := p.parseValue()
		p.metaStack[key] = val
	}
	p.skipToEOL()
}

func (p *Parser) parsePopMeta() {
	p.nextToken() // consume POPMETA
	if p.cur.Type == KEY {
		delete(p.metaStack, p.cur.Literal)
		p.nextToken()
	}
	p.skipToEOL()
}

func (p *Parser) parseInclude() {
	p.nextToken() // consume INCLUDE
	p.skipToEOL()
}

func (p *Parser) parsePlugin() {
	p.nextToken() // consume PLUGIN
	p.skipToEOL()
}

func (p *Parser) parseOption() {
	p.nextToken()
	if p.cur.Type == STRING {
		key := p.cur.Literal
		p.nextToken()
		if p.cur.Type == STRING {
			val := p.cur.Literal
			p.options[key] = append(p.options[key], val)
			p.nextToken()
		}
	}
	p.skipToEOL()
}

// GetOptions returns the options parsed from the input.
func (p *Parser) GetOptions() map[string][]string {
	return p.options
}

// ParseString parses a string into directives.
func ParseString(input string) ([]core.Directive, []error, map[string][]string) {
	p := NewParser(input, "<string>")
	d, errs := p.Parse()
	return d, errs, p.options
}
