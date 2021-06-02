#' @name match_501
#' @title Single 501 match
#' @description This function is used as calculator for a single 501 match
#' @usage match_501(player1, player2, nsets = 1, nlegs = 1)
#' @param player1 player1 name
#' @param player2 player2 name
#' @returns A list containing:
#' \item{match.stat}{match statistics}
#' \item{sets.stat}{sets statistics}
#' \item{player1.set.stat}{Set stats for player1}
#' \item{player2.set.stat}{Set stats for player2}
#' \item{legs.stat}{Legs statistics}
#' @author Matteo Miotto
#' @importFrom svDialogs dlg_form
#' @importFrom stringr str_split
#' @importFrom dplyr all_of
#' @export

match_501 <- function(player1, player2, nsets = 1, nlegs = 1){

  # verifiche varie
  if (!is.character(player1) | !is.character(player2)) {stop("Players' names must be strings")}
  if (nlegs%%1!=0 | nsets%%1!=0) {stop("Number of sets and legs must be integers")}
  if(nlegs <= 0 | nsets <= 0) {stop("Number of legs or sets must be > 0")}

  # useful values
  which_leg_match <- 1
  which_set <- 1
  p1.sets <- p2.sets <- 0

  # df to print scores
  df.print <- data.frame(player = c(player1, player2),
                         sets = c(0,0),
                         legs = c(0,0),
                         scores = c(501, 501))

  sytime <- paste(unlist(str_split(
    paste(unlist(str_split(
      paste(unlist(str_split(Sys.time(), pattern = c(" "))), collapse = ""),
      pattern = c("-"))), collapse = ""),
    pattern = c(":"))), collapse = "")
  m.ID <- paste("m", sytime, sep = "")

  # show instructions in first leg
    dlg_form(form = list("Press Enter to start"=""), message = "To record the scores, for each hand insert the value for the thrown darts separated by a comma (,) in the following form:
                                          o#: for outer singles
                                          i#: for inner singles
                                          d#: for doubles
                                          t#: for trebles \n For example: t20, o19, i19",
             title = "Instructions")


  while ((p1.sets != nsets) & (p2.sets != nsets)) {

    # set intro
    cat(paste("Set", which_set), "Game on!", "\n")

    # change set values in df print
    df.print$sets[df.print$player == player1] <- p1.sets
    df.print$sets[df.print$player == player2] <- p2.sets


    # change players turn
    if (which_set%%2 == 0) {
      play1 <- player2
      play2 <- player1} else {
        play1 <- player1
        play2 <- player2
      }

    # launch set function
    text1 <- paste("s", which_set, "<- set_501(play1, play2, nlegs = nlegs, match.id = m.ID, which_set = which_set, which_leg_match = which_leg_match, df.print)" ,sep = "")
    eval(parse(text = text1))

    # set and leg count
    text2 <- paste("s", which_set, sep = "")
    if (eval(parse(text = text2))$set.stat$winner == player1) {p1.sets <- p1.sets + 1} else {p2.sets <- p2.sets + 1}
    which_set <- which_set + 1
    which_leg_match <- which_leg_match + 1

  }

  # stats
    # winner
    if (p1.sets == nsets) {winner <- player1} else {winner <- player2}

    # unite stats
    # retrieve names of sets of this match
    match_sets <- str_subset(ls(), "^s\\d")

    # retrieve first set stat
    t.set1.set.stat <- "m.sets.stat <- s1$set.stat"
    t.set1.legs.stat <- "m.legs.stat <- s1$legs.stat"

    t.p1.set1.stat <- paste(player1, ".sets.stat <- s1$", player1, ".set.stat", sep = "")
    t.p2.set1.stat <- paste(player2, ".sets.stat <- s1$", player2, ".set.stat", sep = "")

  eval(parse(text = t.set1.set.stat))
  eval(parse(text = t.set1.legs.stat))
  eval(parse(text = t.p1.set1.stat))
  eval(parse(text = t.p2.set1.stat))

  # merge with other sets
    if (nsets > 1) {
      for (i in 2:length(match_sets))
        t.leg <- paste("m.legs.stat <- rbind(m.legs.stat, s", i, "$legs.stat)", sep = "")
        t.set <- paste("m.sets.stat <- rbind(m.sets.stat, s", i, "$set.stat)", sep = "")
      eval(parse(text = t.leg))
      eval(parse(text = t.set))

      for (j in 1:length(eval(parse(text = paste("s1$", player1, ".set.stat", sep = ""))))) {
        t.p1.l <- paste(player1, ".sets.stat[[", j, "]] <- rbind(", player1, ".sets.stat[[", j, "]],", "s", i, "$", player1, ".set.stat[[", j, "]])", sep ="")
        t.p2.l <- paste(player2, ".sets.stat[[", j, "]] <- rbind(", player2, ".sets.stat[[", j, "]],", "s", i, "$", player2, ".set.stat[[", j, "]])", sep ="")
        eval(parse(text = t.p1.l))
        eval(parse(text = t.p2.l))
  }

  }

  # set df
  match.stat.df <- data.frame(
    "match.ID" = m.ID,
    "date" = Sys.Date(),
    "player.1" = player1,
    "player.2" = player2,
    "winner" = winner,
    "n.set.to.win" = nsets,
    "n.played.sets" = which_set - 1,
    "n.played.legs" = which_leg_match - 1
  )

  cat(paste("Congratulations ", winner, "!", sep = ""), "You've won the match")

  # return
  tres <- paste("matres <- list('match.stat' = match.stat.df,", player1, ".sets.stat =", player1, ".sets.stat,",
                player2, ".sets.stat =", player2, ".sets.stat, sets.stat = m.sets.stat, legs.stat = m.legs.stat)", sep = "")

  eval(parse(text = tres))

  return(matres)




}
