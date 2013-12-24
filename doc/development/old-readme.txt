=============================
   Beancount Documentation
=============================
:Abstract:

  Documentation for the non-programmer population. Development docs
  and ideas are all under the "development" subdirectory.



Beancount works like this at the moment: you write a single text file,
and then you run 'bean-web' on it, which starts a little web server
program on your machine, which you access by pointer a web browser at
'http://localhost:8080'.

It works like this::

  bean-web [file.ledger]

Try it now::

  cd beancount/demo
  bean-web demo.ledger

Here's what it should say::

  tangerine:~/p/beancount/examples$ bean-web demo.ledger
  Ready. ( http://0.0.0.0:8080 )

Type ^C to get back to the terminal.



> So i must ask: is there some online place i can go to get the sort of
> handholding i obviously need, at least in the beginning? Or is it that
> this product is really not suitable for the likes of me, and i should
> just bite the bullet and use Quicken or some such?

There isn't any place with that specific kind of documentation. The
'doc' subdirectory contains working documents I was producing while I
designed it. The best documentation would be to read John Wigley's
Ledger documentation, which Beancount is modeled upon:

  http://github.com/downloads/jwiegley/ledger/ledger.pdf

  https://github.com/jwiegley/ledger/wiki

Alternatively, you could get started with the same file I provide in
beancount/demo/demo.ledger. Copy this file somewhere, and then run
'bean-web' on it (as above) and use your web browser to check out the
transactions. Then, use a text editor to modify the 'demo.ledger'
file, which contains EVERYTHING (that's the beauty of it). Follow the
syntax from the examples; comment out the transactions that are there,
and insert your own.

There is also another script called 'bean-convert-ofx' which allows
you to import OFX statements from your bank into beancount format.
Type 'bean-convert-ofx --help' for instructions. I use it like this::

  bean-convert-ofx [file.ledger] [file.ofx]



> I would really like to use Beancount, because it seems to be built
> on a set of principles that feels like a pretty close match with my
> own... But if that requires a level of programming savvy that it
> would take me years to develop, then i would have to accept that
> it's not for me. What do you think?

Beancount is a double-entry system, and as such is more appropriate
for bookkeeping than something like Quicken (single-entry). However
its feature set is pretty basic--it only deals with bookkeeping and
reconciliation, and there are some non-standard conventions (no credit
vs. debit account distinction, for example, it uses signs, so you just
have to "know" that an "income" is a negative number, for example). I
manage my personal finances, my company's, and the joint account I
have with my wife with it, so it's pretty powerful IMO, it's good
enough because AFAIK bookkeeping is just fancy manipulation of simple
accumulators of amounts with some conventions.

Do you know this wonderful website?

   http://www.dwmbeancounter.com/

I don't think you need to learn to program in order to use, you just
need to learn the basic format and the tools. On the other hand, it
_is_ built for tech-savvy folks, it has not been designed to be
user-friendly for the general population; it does assumes as a
prerequisite that you know how (and enjoy) to edit raw text files--not
Word documents.

I'd give it a shot if I were you; start from the sample file, run the
server, and play with the input format. The sample ledger file is well
documented, it should be enough to get you started. You can create as
many ledger files as you like for experimentation.
