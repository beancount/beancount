// Inventory class, used to accumulate and book currencies for each account.
//
// The concept is similar to that of a map, with the currency as the key and a
// decimal number as the value. An inventory may contain multiple kinds of
// things. The inventory object should check that values we attempt to remove
// from it are present in it.
//
// This is a simplified view, however, the key is actually a combination of
//
//    (currency, cost-basis, lot-date) = "lot"
//
// The cost bass and lot date meant to represent a specific acquisition of the
// given currency. This is typically used for shares of stock, where we want to
// be able to book capital gains automatically (in this case, the "currency" is
// a stock symbol, and the amount used in each transaction's balance is the
// cost-basis). In most cases, the cost basis and lot date are simply set to nil
// (as default) values. This allows us to handle the default case of non-stock
// units: a lot consisting of a currency with no cost nor lot date books with
// other similar lots and all is well.
//
// Note that for booking changes, we could technically be clever and attempt to
// do partial matches, e.g. +1 MSFT with cost = 17.93, -1 MSFT (no cost) would
// match each other. I think we might decide to just be strict and only match
// lots precisely and force the user to be consistent instead, or issue a
// warning instead.

// TODO(blais): Allow only subtracting without match (negative values) with a
// special flag.

package beancount

import (
	"bytes"
	"time"
	"container/list"
	"code.google.com/p/godec/dec"
	"fmt"
)

// FIXME: intern the currencies, there's no need to do string comparisons with
// such a small set, plus it then allows us to list them.
type Lot struct {
	currency string
	cost *dec.Dec
	lotdate *time.Time
	amount dec.Dec
}

func NewLot(currency string, amount string) *Lot {
	return &Lot{
		currency: currency,
		amount: *Amount(amount),
	}
}

func (lot *Lot) String() string {
	buf := new(bytes.Buffer)
	buf.WriteString(lot.currency)
	if lot.cost != nil {
		buf.WriteString(" @ ")
		buf.WriteString(lot.cost.String())
	}
	if lot.lotdate != nil {
		buf.WriteString(" / " )
		buf.WriteString(lot.lotdate.String())
	}
	buf.WriteRune(' ')
	buf.WriteString(lot.amount.String())
	return buf.String()
}



//------------------------------------------------------------------------------

// A shortcut to create an amount from a string.
func Amount(number string) *dec.Dec {
	n, _ := new(dec.Dec).SetString(number)
	return n
}



//------------------------------------------------------------------------------

// An inventory container.
// This is essentially an association list of (Lot -> Decimal).
type Inventory struct {
	lots list.List
}

func NewInv() *Inventory {
	return &Inventory{}
}

func NewInvWithLots(lots []Lot) *Inventory {
	inv := &Inventory{}
	for lot := range lots {
		inv.lots.PushBack(lot)
	}
	return inv
}


func (inv *Inventory) String() string {
	buf := new(bytes.Buffer)
	buf.WriteString("{")
	fmt.Printf("String_size = %#v\n", inv.lots.Len())
	last := inv.lots.Back()
	for lot := inv.lots.Front(); lot != nil; lot = lot.Next() {
		buf.WriteString(lot.Value.(*Lot).String())
		if lot != last {
			buf.WriteString(", ")
		}
	}
	buf.WriteString("}")
	return buf.String()
}

// Implement a linear search. In 99% of the cases, in practice this won't matter,
// because there will only be one or two lots in the inventory.
func (inv *Inventory) Find(currency string) *Lot {
	var lot *Lot
	for e := inv.lots.Front(); e != nil; e = e.Next() {
		clot := e.Value.(*Lot)
		if clot.currency == currency && clot.cost == nil && clot.lotdate == nil {
			lot = clot
			break
		}
	}
	return lot
}

// Add something to the inventory.
func (inv *Inventory) Add(currency string, amount *dec.Dec) {
	lot := inv.Find(currency)
	if lot == nil {
		lot = &Lot{currency: currency}
		inv.lots.PushBack(lot)
	}
	fmt.Printf("Add_size = %#v\n", inv.lots.Len())
	lot.amount.Add(&lot.amount, amount)
}

// Compute the number of lots in the inventory.
func (inv *Inventory) Len() int {
	return inv.lots.Len()
}
