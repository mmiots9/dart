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

set_501 <- function(player1, player2, legs2win = 1, nset, df.print){

  # set useful values
  p1.legs <- p2.legs <- 0
  nleg <- 1
  p1.legs.list <- p2.legs.list <- NULL


  while ((p1.legs != legs2win) & (p2.legs != legs2win)) {

    # leg
    cat(paste("Leg", nleg), "Game on!", "\n", "\n")
    Sys.sleep(2)

    # change leg values in df print
    df.print$LEGS[df.print$player == player1] <- p1.legs
    df.print$LEGS[df.print$player == player2] <- p2.legs

    # change players turn
    if (nleg%%2 == 0) {
      play1 <- player2
      play2 <- player1} else {
        play1 <- player1
        play2 <- player2
      }

    # launch leg function
    leg <- leg_501(player1 = play1, player2 = play2, df.print, nset = nset, nleg = nleg)

    # get leg winner
    if (getWinner(leg) == player1) {p1.legs <- p1.legs + 1} else {p2.legs <- p2.legs + 1}

    # add legs in list
    if (getPlayers(leg@p1leg) == player1) {
      p1.legs.list <- c(p1.legs.list, leg@p1leg)
      p2.legs.list <- c(p2.legs.list, leg@p2leg)
    } else {
      p1.legs.list <- c(p1.legs.list, leg@p2leg)
      p2.legs.list <- c(p2.legs.list, leg@p1leg)
    }

    # increase nlegs
    nleg <- nleg + 1

  }

  if (p1.legs == legs2win) {p1win <- 1; p2win <- 0} else {p1win <- 0; p2win <- 1}



  # put in classes and return
  p1.set <- set1p(
    win = p1win,
    legs = p1.legs.list
    )

  p2.set <- set1p(
    win = p2win,
    legs = p2.legs.list
  )

  setres <- set2p(
    p1set = p1.set,
    p2set = p2.set,
    legs2win = legs2win)

  cat(paste("Congratulations ", getWinner(setres), "!", sep = ""), "You've won this set", "\n")
  Sys.sleep(0.5)

  return(setres)

}
