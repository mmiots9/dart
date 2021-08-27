#' @name match_501
#' @title Single 501 match
#' @description This function is used as calculator for a single 501 match
#' @usage match_501(player1, player2, sets2win = 1, nlegs = 1)
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

match_501 <- function(player1, player2, sets2win = 1, legs2win = 1){

  # verifiche varie
  if (!is.character(player1) | !is.character(player2)) {stop("Players' names must be strings")}
  if (legs2win%%1!=0 | sets2win%%1!=0) {stop("Number of sets and legs must be integers")}
  if (legs2win <= 0 | sets2win <= 0) {stop("Number of legs or sets must be > 0")}

  # useful values
  nset <- 1
  p1.sets <- p2.sets <- 0
  p1.sets.list <- p2.sets.list <- NULL

  # df to print scores
  df.print <- data.frame(player = c(player1, player2),
                         SETS = c(0,0),
                         LEGS = c(0,0),
                         scores = c(501, 501))

  # show instructions in first leg
    dlg_form(form = list("Press Enter to start"=""), message = "To record the scores, for each hand insert the value for the thrown darts separated by a comma (,) in the following form:
                                          #: for singles
                                          d#: for doubles
                                          t#: for trebles \nFor example: t20, 19, d25",
             title = "Instructions")


  while ((p1.sets != sets2win) & (p2.sets != sets2win)) {

    # set intro
    cat(paste("Set", nset), "Game on!", "\n")

    # change set values in df print
    df.print$SETS[df.print$player == player1] <- p1.sets
    df.print$LEGS[df.print$player == player2] <- p2.sets


    # change players turn
    if (nset%%2 == 0) {
      play1 <- player2
      play2 <- player1} else {
        play1 <- player1
        play2 <- player2
      }

    # launch set function
    set <- set_501(player1 = play1, player2 = play2, legs2win = legs2win, nset = nset, df.print = df.print)

    # get winner
    if (getWinner(set) == player1) {p1.sets <- p1.sets + 1} else {p2.sets <- p2.sets + 1}

    # add sets in list

    if (getPlayers(set@p1set) == player1) {
      p1.sets.list <- c(p1.sets.list, set@p1set)
      p2.sets.list <- c(p2.sets.list, set@p2set)
    } else {
      p1.sets.list <- c(p1.sets.list, set@p2set)
      p2.sets.list <- c(p2.sets.list, set@p1set)
    }

    # increase nlegs
    nset <- nset + 1

  }

  if (p1.sets == sets2win) {p1win <- 1; p2win <- 0} else {p1win <- 0; p2win <- 1}

  # put in classes and return
  p1.match <- match1p(
    win = p1win,
    sets = p1.sets.list
  )

  p2.match <- match1p(
    win = p2win,
    sets = p2.sets.list
  )

  matres <- match2p(
    p1match = p1.match,
    p2match = p2.match,
    set2win = sets2win)


  cat(paste("Congratulations ", getWinner(matres), "!", sep = ""), "You've won the match", "\n")
  Sys.sleep(0.5)
  return(matres)




}
