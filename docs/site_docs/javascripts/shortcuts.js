keyboard$.subscribe(function(key) {
  if (key.mode === "global" && key.type === "k" && (key.ctrl || key.meta)) {
    /* Focus the search input */
    document.querySelector(".md-search__input").focus()
    key.claim() // Prevent default browser behavior
  }
})
