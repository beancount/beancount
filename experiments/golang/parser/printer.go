package parser

import (
	"fmt"
	"sort"
	"strings"

	"github.com/beancount/beancount/v3/core"
)

// PrintDirectives returns a string representation of a list of directives.
func PrintDirectives(directives []core.Directive) string {
	var sb strings.Builder
	for i, d := range directives {
		if i > 0 {
			sb.WriteString("\n")
		}
		sb.WriteString(PrintDirective(d))
		sb.WriteString("\n")
	}
	return sb.String()
}

// PrintDirective returns a string representation of a single directive.
func PrintDirective(d core.Directive) string {
	switch e := d.(type) {
	case *core.Open:
		res := fmt.Sprintf("%s open %s", e.Date.Format("2006-01-02"), e.Account)
		if len(e.Currencies) > 0 {
			res += " " + strings.Join(e.Currencies, ", ")
		}
		return res
	case *core.Close:
		return fmt.Sprintf("%s close %s", e.Date.Format("2006-01-02"), e.Account)
	case *core.Commodity:
		res := fmt.Sprintf("%s commodity %s", e.Date.Format("2006-01-02"), e.Currency)
		return res + PrintMetadata(e.Meta)
	case *core.Pad:
		return fmt.Sprintf("%s pad %s %s", e.Date.Format("2006-01-02"), e.Account, e.SourceAccount)
	case *core.Balance:
		return fmt.Sprintf("%s balance %s %s", e.Date.Format("2006-01-02"), e.Account, e.Amount.String())
	case *core.Transaction:
		res := fmt.Sprintf("%s %s", e.Date.Format("2006-01-02"), e.Flag)
		if e.Payee != "" {
			res += fmt.Sprintf(" %q", e.Payee)
		}
		if e.Narration != "" {
			res += fmt.Sprintf(" %q", e.Narration)
		}
		for tag := range e.Tags {
			res += " #" + tag
		}
		for link := range e.Links {
			res += " ^" + link
		}
		res += "\n"
		for _, p := range e.Postings {
			res += "  " + PrintPosting(p) + "\n"
		}
		return strings.TrimRight(res, "\n")
	case *core.Note:
		return fmt.Sprintf("%s note %s %q", e.Date.Format("2006-01-02"), e.Account, e.Comment)
	case *core.Event:
		return fmt.Sprintf("%s event %q %q", e.Date.Format("2006-01-02"), e.EventType, e.Description)
	case *core.Query:
		return fmt.Sprintf("%s query %q %q", e.Date.Format("2006-01-02"), e.Name, e.QueryString)
	case *core.Price:
		return fmt.Sprintf("%s price %s %s", e.Date.Format("2006-01-02"), e.Currency, e.Amount.String())
	case *core.Document:
		return fmt.Sprintf("%s document %s %q", e.Date.Format("2006-01-02"), e.Account, e.Filename)
	case *core.Custom:
		res := fmt.Sprintf("%s custom %q", e.Date.Format("2006-01-02"), e.TypeStr)
		for _, v := range e.Values {
			res += fmt.Sprintf(" %v", v)
		}
		return res
	default:
		return ""
	}
}

// PrintPosting returns a string representation of a posting.
func PrintPosting(p core.Posting) string {
	res := fmt.Sprintf("%-20s %s", p.Account, p.Units.String())
	if p.Cost != nil {
		res += " " + p.Cost.String()
	}
	if p.Price.Number != nil {
		res += " @ " + p.Price.String()
	}
	return res + PrintMetadata(p.Meta)
}

// PrintMetadata returns a string representation of metadata.
func PrintMetadata(meta core.Meta) string {
	if len(meta) == 0 {
		return ""
	}
	var keys []string
	for k := range meta {
		if k == "filename" || k == "lineno" {
			continue
		}
		keys = append(keys, k)
	}
	sort.Strings(keys)

	var sb strings.Builder
	for _, k := range keys {
		sb.WriteString(fmt.Sprintf("\n    %s: %v", k, meta[k]))
	}
	return sb.String()
}
