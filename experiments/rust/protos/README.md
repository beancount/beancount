A copy with slight modifications of the proto definitions from branch 'cpp'.
I believe these are quite appropriate definitions, contemplating automatic
conversion to Rust and Python because

- automatic conversion can be done via prost
- protos provide a nice ascii syntax to write tests (prost should provide access
  to it)

On the downside, the compiled definitions aren't super nice. Maybe it would be
better to just write them in Rust and write a text parser (or literal syntax) to
create my tests.

(This is WIP.)
