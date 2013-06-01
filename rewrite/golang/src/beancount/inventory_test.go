package beancount

import (
	"testing"
	"fmt"
	// "regexp"
	// "math/big"
	// "code.google.com/p/godec/dec"
)


// FIXME: These methods need to be moved somewhere reusable; this is generic testing infra.
func Assert(t *testing.T, expr bool) {
	if expr {
		t.Errorf("Assert failed: %#v\n", expr)
	}
}

func AssertEqual(t *testing.T, a, b interface{}) {
	if a != b {
		t.Errorf("AssertEqual failed: a = %#v, b = %#v\n", a, b)
	}
}


func TestCreateLots(t *testing.T) {
	lot := NewLot("USD", "1.2032")
	fmt.Printf("lot = %#v\n", lot)
}

func TestCreateInventories(t *testing.T) {
	inv := NewInv()
	fmt.Printf("inv = %#v\n", inv)
}

func TestLotOperations(t *testing.T) {
	inv := &Inventory{}
	//fmt.Printf(">> %#v\n", amt)
	inv.Add("USD", Amount("100.17"))
	AssertEqual(t, inv.Len(), 1)

	inv.Add("EUR", Amount("57.34"))
	AssertEqual(t, inv.Len(), 2)
	inv.Add("EUR", Amount("1.01"))
	AssertEqual(t, inv.Len(), 2)
	inv.Add("JPY", Amount("100.232"))
	AssertEqual(t, inv.Len(), 3)
	fmt.Printf("Inventory: %v\n", inv)
}




// func TestBigInts(t *testing.T) {
// 	// i := big.NewRat(0, 1)
// 	// i.SetString("343.43")
//
// 	// i := big.Int{}
// 	// i.SetInt64(474)
//
// 	// i, _ := new(dec.Dec).SetString("3.43348394384397273827382382111")
// 	// j, _ := new(dec.Dec).SetString("7.232")
//
// 	i, _ := new(dec.Dec).SetString("5.00")
// 	j, _ := new(dec.Dec).SetString("2.50")
//
// 	k := new(dec.Dec).Quo(i, j, dec.Scale(2), dec.RoundDown)
// 	fmt.Printf("i = %v\n", k)
//
// //	fmt.Printf("i = %#v\n", i)
//
// }
