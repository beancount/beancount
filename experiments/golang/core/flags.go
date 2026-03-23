package core

const (
	FLAG_OKAY        = "*" // Transactions that have been checked.
	FLAG_WARNING     = "!" // Mark by the user as something to be looked at later on.
	FLAG_PADDING     = "P" // Transactions created from padding directives.
	FLAG_SUMMARIZE   = "S" // Transactions created due to summarization.
	FLAG_TRANSFER    = "T" // Transactions created due to balance transfers.
	FLAG_CONVERSIONS = "C" // Transactions created to account for price conversions.
	FLAG_MERGING     = "M" // A flag to mark postings merging together legs for average cost.
)
