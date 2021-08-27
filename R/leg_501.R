#' @name leg_501
#' @title Single leg 501 in a match
#' @description This function is used as calculator for a single 501 leg
#' @usage leg_501(player1, player2)
#' @param player1 player1 name
#' @param player2 player2 name
#' @returns A list containing:
#' \item{winner}{character vector of the winner}
#' \item{player1.stat}{Leg stats for player1}
#' \item{player2.stat}{Leg stats for player2}
#' @author Matteo Miotto
#' @importFrom svDialogs dlg_form
#' @importFrom stringr str_split
#' @importFrom dplyr all_of mutate arrange group_by summarise full_join
#' @importFrom magrittr %>%
#' @importFrom huxtable hux set_position set_align print_screen everywhere
#' @export
leg_501 <- function(player1, player2, df.print, nset, nleg) {

  # leg id
  dateid <- gsub(
    x = gsub(
      x = gsub(
        x = as.character(Sys.time()),
        pattern = "-",
        replacement = ""),
      pattern = ":",
      replacement = ""
    ),
    pattern = " ",
    replacement = ":")

  id <- paste0("m", dateid, "s", nset, "l", nleg)


  # set useful values
  p1.score <- p2.score <- 501
  p1.dartsScoresCh <- p2.dartsScoresCh <- NULL
  doubles <- c((1:20)*2, 50)
  possible_scores <- c("0", as.character(1:20) ,paste("d", c(1:20), sep = ""), paste("t", c(1:20), sep = ""), "25", "d25")
  p1.win <- p2.win <- 0

  # print df
  hux(df.print) %>%
    set_position("left")%>%
    set_align(everywhere, everywhere, "center") %>%
    print_screen(colnames = F)

  cat("\n")

  while ((p1.score !=0) & (p2.score !=0)) {

    p1.handScoresCh <- p2.handScoresCh <- NA

    # player 1
      while (!all(all_of(p1.handScoresCh %in% possible_scores))){

        # ask for scored points for 3 darts
        form <- list("Darts score:TXT" = "1st dart, 2nd dart, 3rd dart")
        form_title <- paste0(player1, "'s turn")
        p1.handScore <- unlist(dlg_form(form, title = form_title)$res)

        # extrapolate dart score as chr and num vector
        p1.handScoresCh  <- as.character( trimws( unlist( strsplit(p1.handScore, split = ",")), which = "both"))
        p1.handScoresVal <- chr2val(p1.handScoresCh)

        if (!all(all_of(p1.handScoresCh %in% possible_scores))) {message("Error: you have inserted invalid values. Retry")}
      }

    # subtract darts score from score and evaluating bust
    p1.score_i <- p1.score

    for (i in seq_along(p1.handScoresVal)){
      p1.score_i <- p1.score_i - p1.handScoresVal[i]
      if (p1.score_i == 1 |
          p1.score_i < 0 |
          (p1.score_i == 0 & !(p1.handScoresCh[i] %in% c(paste0("d", c(1:20)), "d25")))){
        p1.handScoresVal <- c(0, 0, 0)

        # aggiungo NA
        if (i == 1){p1.handScoresCh[2:3] <- "NA"} else if (i == 2) {p1.handScoresCh[3] <- "NA"}
        message("No score\n")
        break

      }
    }

    p1.score <- p1.score - sum(p1.handScoresVal)

    # add chr darts to thrown dart score
    p1.dartsScoresCh <- c(p1.dartsScoresCh, p1.handScoresCh)

    # print df
    if (p1.score != 0){
      cat("\n")
      df.print$scores[df.print$player == player1] <- p1.score
      hux(df.print) %>%
        set_position("left")%>%
        set_align(everywhere, everywhere, "center") %>%
        print_screen(colnames = F)

      cat("\n")
    }



    # checkout
    if (p1.score == 0){
      df.print$scores[df.print$player == player1] <- 501
      df.print$scores[df.print$player == player2] <- 501
      p1.win <- 1
      next
    }


# -----------------  player 2
    while (!all(all_of(p2.handScoresCh %in% possible_scores))){

        # ask for scored points for 3 darts
        form <- list("Darts score:TXT" = "1st dart, 2nd dart, 3rd dart")
        form_title <- paste0(player2, "'s turn")
        p2.handScore <- unlist(dlg_form(form, title = form_title)$res)

        # extrapolate dart score as chr and num vector
        p2.handScoresCh  <- as.character( trimws( unlist( strsplit(p2.handScore, split = ",")), which = "both"))
        p2.handScoresVal <- chr2val(p2.handScoresCh)

        if (!all(all_of(p2.handScoresCh %in% possible_scores))) {message("Error: you have inserted invalid values. Retry")}
    }

    # subtract darts score from score and evaluating bust
    p2.score_i <- p2.score

    for (i in seq_along(p2.handScoresVal)){
      p2.score_i <- p2.score_i - p2.handScoresVal[i]
      if (p2.score_i == 1 |
          p2.score_i < 0 |
          (p2.score_i == 0 & !(p2.handScoresCh[i] %in% c(paste0("d", c(1:20)), "d25")))){
        p2.handScoresVal <- c(0, 0, 0)

        # aggiungo NA
        if (i == 1){p2.handScoresCh[2:3] <- "NA"} else if (i == 2) {p2.handScoresCh[3] <- "NA"}
        message("No score\n")
        break
      }
    }

    p2.score <- p2.score - sum(p2.handScoresVal)

    # add chr darts to thrown dart score
    p2.dartsScoresCh <- c(p2.dartsScoresCh, p2.handScoresCh)

    # print df
    if (p2.score != 0){
      cat("\n")
      df.print$scores[df.print$player == player2] <- p2.score
      hux(df.print) %>%
        set_position("left")%>%
        set_align(everywhere, everywhere, "center") %>%
        print_screen(colnames = F)

      cat("\n")
    }



    # checkout
    if (p2.score == 0){
      df.print$scores[df.print$player == player1] <- 501
      df.print$scores[df.print$player == player2] <- 501
      p2.win <- 1
      next
    }
}

# -------------------------------- return

  # mettere nelle due classi
  p1.leg <- leg1p(
    id = id,
    player = player1,
    start = 1,
    win = p1.win,
    dartsScoresCh = p1.dartsScoresCh
    )

  p2.leg <- leg1p(
    id = id,
    player = player2,
    start = 0,
    win = p2.win,
    dartsScoresCh = p2.dartsScoresCh
  )

  legres <- leg2p(
    p1leg = p1.leg,
    p2leg = p2.leg
  )

 Sys.sleep(0.5)

 return(legres)
}
