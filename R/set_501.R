set_501 <- function(player1, player2, nlegs = 1, which_set){

  # set useful values
  p1.legs <- p2.legs <- 0
  which_leg <- 1

  while ((p1.legs != nlegs) & (p2.legs != nlegs)) {

    # leg
    cat(paste("Leg", which_leg), "Game on!", "\n")
    Sys.sleep(2)

    # change players turn
    if (which_leg%%2 == 0) {
      play1 <- player2
      play2 <- player1} else {
        play1 <- player1
        play2 <- player2
      }

    text1 <- paste("s", which_set, "l", which_leg, "<- leg_501(play1, play2)", sep = "")
    eval(parse(text = text1))

    text2 <- paste("s", which_set, "l", which_leg, sep = "")

    # leg count
    if (eval(parse(text = text2))$winner == player1) {p1.legs <- p1.legs + 1} else {p2.legs <- p2.legs + 1}
    which_leg <- which_leg + 1

  }

  # stats
    # winner
    if (p1.legs == nlegs) {winner <- player1} else {winner <- player2}

    # unite stats
      # retrieve names of legs of this set
      set_legs <- str_subset(ls(), paste("s", which_set, sep = ""))

      # retrieve first leg stat
      text.stat <- paste("s", which_set, "l1", sep = "")
      t1 <- paste(player1, ".set.stats <- eval(parse(text = text.stat))$", player1, ".leg.stats" , sep = "")
      t2 <- paste(player2, ".set.stats <- eval(parse(text = text.stat))$", player2, ".leg.stats" , sep = "")
      eval(parse(text = t1))
      eval(parse(text = t2))

      # merge with other legs
      for (i in 2:length(set_legs)) {
        text.stat2 <- paste("s", which_set, "l", i, sep = "")
        t3 <- paste(player1, ".set.stats <- mapply(c,", player1, ".set.stats, eval(parse(text = text.stat2))$", player1, ".leg.stats)", sep = "")
        t4 <- paste(player2, ".set.stats <- mapply(c,", player2, ".set.stats, eval(parse(text = text.stat2))$", player2, ".leg.stats)", sep = "")
        eval(parse(text = t3))
        eval(parse(text = t4))
      }

  cat(paste("Congratulations", winner), "You've won this set")

  # return
  tres <- paste("setres <- list('winner' = winner,", player1, ".set.stats =", player1, ".set.stats,",
                player2, ".set.stats =", player2, ".set.stats)", sep = "")

  eval(parse(text = tres))

  return(setres)

}
