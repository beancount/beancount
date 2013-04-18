package main

import (
	"flag"
	"fmt"
	"log"
	"io/ioutil"
	"beancount"
)

func main() {

	// Parse arguments.
	debugLexer := flag.Bool("debug-lexer", false, "Print out tokens")
	// wordPtr := flag.String("word", "foo", "a string")
	// numbPtr := flag.Int("numb", 42, "an int")
  // var svar string
	// flag.StringVar(&svar, "svar", "bar", "a string var")
	flag.Parse()

	for _, filename := range flag.Args() {

    b, err := ioutil.ReadFile(filename)
		if err != nil {
			log.Fatal(err)
		}

		input := string(b)

		l := beancount.MakeLexer(filename, input)
		if *debugLexer {
			for {
				item := l.NextTok()
				fmt.Printf("item: %v\n", item)
				if item.Type == beancount.TokEOF || item.Type == beancount.TokERROR {
					break
				}
			}
		} else {
			beancount.Parse(l)
		}

		return
	}
}
