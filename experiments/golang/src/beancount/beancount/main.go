package main

import (
	"flag"
	"fmt"
	"log"
	"io/ioutil"
	"beancount"
	"time"
)

const (
	Day = time.Hour * 24
)

func main() {

// FIXME: remove
	// d0, error := time.Parse("2006-01-02", "2014-12-31")
	// d := d0.Add(Day)
	// fmt.Printf("%#v %v %#v\n", d, d.Format("2006-01-02"), error)

	// d, error := time.ParseDuration("2006-01-02", "2013-04-18")



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
				if item.Type == beancount.EOF || item.Type == beancount.ERROR {
					break
				}
			}
		} else {
			beancount.Parse(l)
		}

		return
	}
}
