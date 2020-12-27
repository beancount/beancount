/* Attempt at handling hanging transactions without newlines at the end of the file. */
/* {fab24459d79d} */

<<EOF>> {
  /* This is a bit of ugly logic inserted to properly grok transactions with no
   * final newline nor explicit dedents. We want to be able to accept these. */
  if (!final_eol_emitted && columno() != 0) {
    /* If not at end of line, generate one and reset the column. */
    /* Also save the number of dedents to synthesize further. */
    final_eol_emitted = true;
    std::cout << "A "  << std::endl;
    return Parser::make_EOL(location());
  } else {
    /* Synthesize dedents until we're back to beginning. */
    std::cout << "matcher().stops().size() = '" << matcher().stops().size() << "'" << std::endl;
    if (matcher().stops().size() > 0) {
      std::cout << "B1"  << std::endl;
      matcher().stops().pop_back();
      return Parser::make_DEDENT(location());
    }

    // if (num_shutdown_stops > 0) {
    //   std::cout << "B2"  << std::endl;
    //   --num_shutdown_stops;
    //   return Parser::make_DEDENT(location());
    // }

    /* Mark the end of file with an explicit token. */
    std::cout << "B2"  << std::endl;
    return Parser::make_YYEOF(location());
  }
}
