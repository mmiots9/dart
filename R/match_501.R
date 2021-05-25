


match_501 <- function(player1, player2, nlegs = 1, nsets = 1, resume = "none"){

  # verifiche varie
  if (!is.character(player1) | !is.character(player2)) {stop("Players' names must be strings")}
  if (nlegs%%1!=0 | nsets%%1!=0) {stop("Number of sets and legs must be integers")}
  if (!(resume %in% c("none", "1", "2", "vs"))) {stop("resume must be 'none', '1', '2' or 'vs'")}

  # apro set
  p1sets <- p2sets <- 0

  while ((p1sets != nsets) | (p2sets != nsets)) {

    # funzione set

  }


  # riassunto




}
