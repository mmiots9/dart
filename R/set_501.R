#' @name set_501
#' @title Single 501 set in a match
#' @description This function is used as calculator for a single 501 set
#' @usage set_501(player1, player2, nlegs, which_set, match.id, which_leg_match)
#' @param player1 player1 name
#' @param player2 player2 name
#' @returns A list containing:
#' \item{set.stat}{set statistics}
#' \item{player1.set.stat}{Set stats for player1}
#' \item{player2.set.stat}{Set stats for player2}
#' \item{set.legs.stat}{Legs statistics}
#' @author Matteo Miotto
#' @importFrom svDialogs dlg_form
#' @importFrom stringr str_split
#' @importFrom dplyr all_of
#' @export

set_501 <- function(player1, player2, nlegs = 1, which_set, match.id, which_leg_match){

  # set useful values
  p1.legs <- p2.legs <- 0
  which_leg_set <- 1
  sytime <- paste(unlist(str_split(
    paste(unlist(str_split(
      paste(unlist(str_split(Sys.time(), pattern = c(" "))), collapse = ""),
      pattern = c("-"))), collapse = ""),
    pattern = c(":"))), collapse = "")
  s.ID <- paste("s", sytime, sep = "")

  while ((p1.legs != nlegs) & (p2.legs != nlegs)) {

    # leg
    cat(paste("Leg", which_leg_set), "Game on!", "\n")
    Sys.sleep(2)

    # change players turn
    if (which_leg_set%%2 == 0) {
      play1 <- player2
      play2 <- player1} else {
        play1 <- player1
        play2 <- player2
      }

    text1 <- paste("s", which_set, "l", which_leg_set, "<- leg_501(play1, play2, n.leg.set = which_leg_set, set.id = s.ID, match.id = match.id, n.leg.match = which_leg_match)" ,sep = "")
    eval(parse(text = text1))

    text2 <- paste("s", which_set, "l", which_leg_set, sep = "")

    # leg count
    if (eval(parse(text = text2))$leg.stat$winner == player1) {p1.legs <- p1.legs + 1} else {p2.legs <- p2.legs + 1}
    which_leg_set <- which_leg_set + 1
    which_leg_match <- which_leg_match + 1

  }

  # stats
    # winner
    if (p1.legs == nlegs) {winner <- player1} else {winner <- player2}

    # unite stats
      # retrieve names of legs of this set
      set_legs <- str_subset(ls(), paste("s", which_set, sep = ""))

      # retrieve first leg stat
      t.leg1.stat <- paste("s", which_set, "legs.stat <- s",  which_set, "l1$leg.stat", sep = "")
      text.stat <- paste("s", which_set, "l1", sep = "")
      t.p1.leg1.stat <- paste(player1, ".set.stats <- eval(parse(text = text.stat))$", player1, ".leg.stats" , sep = "")
      t.p2.leg1.stat <- paste(player2, ".set.stats <- eval(parse(text = text.stat))$", player2, ".leg.stats" , sep = "")

      eval(parse(text = t.leg1.stat))
      eval(parse(text = t.p1.leg1.stat))
      eval(parse(text = t.p2.leg1.stat))

      # merge with other legs
      for (i in 2:length(set_legs))
        t.leg <- paste("s", which_set, "legs.stat <- rbind(s", which_set, "legs.stat, s", which_set, "l", i, "$leg.stat)", sep = "")
      eval(parse(text = t.leg))

      for (j in 1:length(eval(parse(text = paste("s", which_set, "l", i, "$", player1, ".leg.stats", sep = ""))))) {
        t.p1.l <- paste(player1, ".set.stats[[", j, "]] <- rbind(", player1, ".set.stats[[", j, "]],", "s", which_set, "l", i, "$", player1, ".leg.stats[[", j, "]])", sep ="")
        t.p2.l <- paste(player2, ".set.stats[[", j, "]] <- rbind(", player2, ".set.stats[[", j, "]],", "s", which_set, "l", i, "$", player2, ".leg.stats[[", j, "]])", sep ="")
        eval(parse(text = t.p1.l))
        eval(parse(text = t.p2.l))

      }

      # set df
      set.stat.df <- data.frame(
        "set.ID" = s.ID,
        "player.1" = player1,
        "player.2" = player2,
        "winner" = winner,
        "n.set.match" = which_set,
        "match.ID" = match.id,
        "n.leg.to.win" = nlegs,
        "n.played.legs" = which_leg_set - 1
      )

  cat(paste("Congratulations ", winner, "!", sep = ""), "You've won this set")

  # return
  tres <- paste("setres <- list('set.stat' = set.stat.df,", player1, ".set.stats =", player1, ".set.stats,",
                player2, ".set.stats =", player2, ".set.stats, s", which_set, ".legs.stat = s", which_set, "legs.stat)", sep = "")

  eval(parse(text = tres))

  return(setres)

}
