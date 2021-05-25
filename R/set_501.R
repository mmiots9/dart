set_501 <- function(player1, player2, nlegs = 1){

  # set useful values
  p1.legs <- p2.legs <- 0
  which_leg <- 1

  while ((p1.legs != nlegs) & (p2.legs != nlegs)) {

    # leg
    cat(paste("Leg", which_leg), "Game on!")
    Sys.sleep(2)

    text1 <- paste("leg", which_leg, "<- leg_501(player1, player2)", sep = "")
    eval(parse(text = text1))

    text2 <- paste("leg", which_leg, sep = "")
    cat(paste("Congratulations", eval(parse(text = text2))$winner), "You've won this leg")

    # leg count
    if (eval(parse(text = text2))$winner == player1) {p1.legs <- p1.legs + 1} else {p2.legs <- p2.legs + 1}

  }

  # stats

}
