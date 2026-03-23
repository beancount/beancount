package main

import (
	"flag"
	"fmt"
	"os"

	beancount "github.com/beancount/beancount/v3"
)

func main() {
	flag.Parse()
	args := flag.Args()

	if len(args) == 0 {
		fmt.Println("usage: bean-check <filename>")
		os.Exit(1)
	}

	filename := args[0]
	_, errs, _ := beancount.LoadFile(filename)

	if len(errs) > 0 {
		for _, err := range errs {
			fmt.Fprintf(os.Stderr, "%v\n", err)
		}
		os.Exit(1)
	}

	fmt.Printf("No errors found in %q.\n", filename)
	os.Exit(0)
}
