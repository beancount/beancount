package main

import (
	"flag"
	"fmt"
	"log"
	"io/ioutil"
	"beancount"
)

func main() {

	// wordPtr := flag.String("word", "foo", "a string")
	// numbPtr := flag.Int("numb", 42, "an int")
	// boolPtr := flag.Bool("fork", false, "a bool")
  // var svar string
	// flag.StringVar(&svar, "svar", "bar", "a string var")
	flag.Parse()


	for _, filename := range flag.Args() {

		// file, err := os.Open(filename) // For read access.
    b, err := ioutil.ReadFile("input.txt")
		if err != nil {
			log.Fatal(err)
		}

		l := beancount.Lex(filename, b)
		for {
			item := l.nextItem()
			fmt.Println("item: %v", item)
			if item.typ == beancount.itemEOF || item.typ == beancount.itemError {
				break
			}
		}
		return
	}
}
