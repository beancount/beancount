package core

import (
	"fmt"
)

// RealAccount represents an account in a tree structure.
type RealAccount struct {
	AccountName string
	Children    map[string]*RealAccount
	Postings    []interface{} // Can be TxnPosting or other directives
	Balance     *Inventory
}

// NewRealAccount creates a new RealAccount node.
func NewRealAccount(name string) *RealAccount {
	return &RealAccount{
		AccountName: name,
		Children:    make(map[string]*RealAccount),
		Balance:     NewInventory(),
	}
}

// GetOrCreate fetches a subaccount or creates it if it doesn't exist.
func (ra *RealAccount) GetOrCreate(accountName string) *RealAccount {
	if accountName == "" {
		return ra
	}
	components := Split(accountName)
	curr := ra
	path := []string{}
	for _, comp := range components {
		path = append(path, comp)
		if _, ok := curr.Children[comp]; !ok {
			curr.Children[comp] = NewRealAccount(Join(path...))
		}
		curr = curr.Children[comp]
	}
	return curr
}

// Get fetches a subaccount or returns nil if it doesn't exist.
func (ra *RealAccount) Get(accountName string) *RealAccount {
	if accountName == "" {
		return ra
	}
	components := Split(accountName)
	curr := ra
	for _, comp := range components {
		if child, ok := curr.Children[comp]; ok {
			curr = child
		} else {
			return nil
		}
	}
	return curr
}

// Realize groups entries by account into a tree of RealAccount nodes.
func Realize(directives []Directive, minAccounts []string) *RealAccount {
	root := NewRealAccount("")

	// Ensure minimum accounts exist
	for _, acc := range minAccounts {
		root.GetOrCreate(acc)
	}

	for _, d := range directives {
		switch t := d.(type) {
		case *Transaction:
			for i := range t.Postings {
				p := &t.Postings[i]
				node := root.GetOrCreate(p.Account)
				node.Postings = append(node.Postings, TxnPosting{Txn: t, Posting: p})
				node.Balance.AddPosition(Position{Units: p.Units, Cost: p.Cost})
			}
		case *Open:
			node := root.GetOrCreate(t.Account)
			node.Postings = append(node.Postings, t)
		case *Close:
			node := root.GetOrCreate(t.Account)
			node.Postings = append(node.Postings, t)
		case *Balance:
			node := root.GetOrCreate(t.Account)
			node.Postings = append(node.Postings, t)
		case *Note:
			node := root.GetOrCreate(t.Account)
			node.Postings = append(node.Postings, t)
		case *Pad:
			node1 := root.GetOrCreate(t.Account)
			node1.Postings = append(node1.Postings, t)
			node2 := root.GetOrCreate(t.SourceAccount)
			node2.Postings = append(node2.Postings, t)
		case *Document:
			node := root.GetOrCreate(t.Account)
			node.Postings = append(node.Postings, t)
		}
	}

	return root
}

// ComputeBalance computes the total balance of an account and all its children.
func (ra *RealAccount) ComputeBalance() *Inventory {
	inv := ra.Balance.Clone()
	for _, child := range ra.Children {
		inv.AddInventory(child.ComputeBalance())
	}
	return inv
}

func (ra *RealAccount) String() string {
	return fmt.Sprintf("RealAccount(%q, balance=%v, children=%v)", ra.AccountName, ra.Balance, ra.Children)
}

// IterChildren yields all accounts in the tree, depth-first.
func (ra *RealAccount) IterChildren(leafOnly bool, yield func(*RealAccount)) {
	if !leafOnly || len(ra.Children) == 0 {
		yield(ra)
	}
	// Sort child names for deterministic iteration
	names := make([]string, 0, len(ra.Children))
	for name := range ra.Children {
		names = append(names, name)
	}
	// Note: using simple sort for now
	for i := 0; i < len(names); i++ {
		for j := i + 1; j < len(names); j++ {
			if names[i] > names[j] {
				names[i], names[j] = names[j], names[i]
			}
		}
	}

	for _, name := range names {
		ra.Children[name].IterChildren(leafOnly, yield)
	}
}
