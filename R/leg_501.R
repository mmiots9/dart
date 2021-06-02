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
#' @export
leg_501 <- function(player1, player2, n.leg.set, set.id, match.id, n.leg.match) {

  # leg id
    sytime <- paste(unlist(str_split(
      paste(unlist(str_split(
      paste(unlist(str_split(Sys.time(), pattern = c(" "))), collapse = ""),
      pattern = c("-"))), collapse = ""),
      pattern = c(":"))), collapse = "")
    l.ID <- paste("l", sytime, sep = "")

  # set useful values
  p1.score <- p2.score <- 501
  p1.missed_doubles <- p1.closing_double <- p2.missed_doubles <- p2.closing_double <- NA
  p1.thrown_darts_score <- p1.checkout <- p2.thrown_darts_score <- p2.checkout <- NULL
  from_chr_to_score_vector <- c(0, 1:20, 1:20, (1:20)*2, (1:20)*3, 25, 50)
  from_chr_to_score_names  <- c("0", paste("o",(1:20), sep = ""), paste("i",(1:20), sep = ""), paste("d", c(1:20), sep = ""), paste("t", c(1:20), sep = ""), "25", "d25")
  names(from_chr_to_score_vector) <- from_chr_to_score_names
  p1.n_of_darts <- p1.busted <- p1.missed <- p1.100 <- p1.140 <- p1.180 <-
    p2.n_of_darts <- p2.busted <- p2.missed <- p2.100 <- p2.140 <- p2.180 <- 0
  doubles <- c((1:20)*2, 50)
  levels <- c("0", as.character(1:20) ,paste("d", c(1:20), sep = ""), paste("t", c(1:20), sep = ""), "25", "d25")

  while ((p1.score !=0) & (p2.score !=0)) {

    p1.hand_scores_chr <- p2.hand_scores_chr <- NA

    # player 1
      while (!all(all_of(p1.hand_scores_chr %in% from_chr_to_score_names))){
        # ask for scored points for 3 darts
        form <- list("Darts score:TXT" = "")
        form_title <- paste(player1, "score is:", as.character(p1.score))
        p1.hand_score <- unlist(dlg_form(form, message = "Insert the value for the thrown darts separated by a comma (,) in the following form:
                                          o#: for outer singles
                                          i#: for inner singles
                                          d#: for doubles
                                          t#: for triples",
                                      title = form_title)$res)

        # extrapolate dart score as chr and num vector
        p1.hand_scores_chr <- as.character( trimws( unlist( strsplit(p1.hand_score, split = ",")), which = "both"))
        p1.hand_scores_num <- as.numeric(from_chr_to_score_vector[c(p1.hand_scores_chr)])

        if (!all(all_of(p1.hand_scores_chr %in% from_chr_to_score_names))) {message("Error: you have inserted unvalid values. Retry")}
      }

    # subtract darts score from score and evaluating if miss double for checkout or bust
    p1.score_i <- p1.score
    for (i in seq_along(p1.hand_scores_num)){
      if(p1.score_i %in% doubles & (p1.score_i - p1.hand_scores_num[i]) !=0 ) {
        p1.missed <- p1.missed + 1
        p1.missed_doubles <- c(p1.missed_doubles, paste("d", p1.score_i/2, sep=""))
      }

      p1.score_i <- p1.score_i -  p1.hand_scores_num[i]
    }

    if (p1.score -  sum(p1.hand_scores_num) != 1 & (p1.score -  sum(p1.hand_scores_num)) >= 0){
      p1.score <- p1.score -  sum(p1.hand_scores_num)

      if (sum(p1.hand_scores_num) == 180){
        p1.180 <- p1.180 + 1
      } else if (sum(p1.hand_scores_num) >= 140){
        p1.140 <- p1.140 + 1
      } else if (sum(p1.hand_scores_num) >= 100){
        p1.100 <- p1.100 + 1
      }

    } else {
      p1.busted <- p1.busted + 1
      p1.hand_scores_chr <- c("0", "0", "0")

    }

    # add chr darts to thrown dart score
    p1.thrown_darts_score <- c(p1.thrown_darts_score, p1.hand_scores_chr)

    # count number of darts
    p1.n_of_darts <- p1.n_of_darts + length(p1.hand_scores_chr)


    # checkout
    if (p1.score == 0){
      p1.checkout <- sum(p1.hand_scores_num)
      p1.closing_double <- p1.hand_scores_chr[length(p1.hand_scores_chr)]
      p2.checkout <- NA
      next
    }



    # player 2
    while (!all(all_of(p2.hand_scores_chr %in% from_chr_to_score_names))){
      # ask for scored points for 3 darts
      form <- list("Darts score:TXT" = "")
      form_title <- paste(player2, "score is:", as.character(p2.score))
      p2.hand_score <- unlist(dlg_form(form, message = "Insert the value for the thrown darts separated by a comma (,) in the following form:
                                        o#: for outer singles
                                        i#: for inner singles
                                        d#: for doubles
                                        t#: for triples",
                                       title = form_title)$res)

      # extrapolate dart score as chr and num vector
      p2.hand_scores_chr <- as.character( trimws( unlist( strsplit(p2.hand_score, split = ",")), which = "both"))
      p2.hand_scores_num <- as.numeric(from_chr_to_score_vector[c(p2.hand_scores_chr)])

      if (!all(all_of(p2.hand_scores_chr %in% from_chr_to_score_names))) {message("Error: you have inserted unvalid values. Retry")}
    }

    # subtract darts score from score and evaluating if miss double for checkout or bust
    p2.score_i <- p2.score
    for (i in seq_along(p2.hand_scores_num)){
      if(p2.score_i %in% doubles & (p2.score_i - p2.hand_scores_num[i]) !=0 ) {
        p2.missed <- p2.missed + 1
        p2.missed_doubles <- c(p2.missed_doubles, paste("d", p2.score_i/2, sep=""))
      }

      p2.score_i <- p2.score_i -  p2.hand_scores_num[i]
    }

    if (p2.score -  sum(p2.hand_scores_num) != 1 & (p2.score -  sum(p2.hand_scores_num)) >= 0){
      p2.score <- p2.score -  sum(p2.hand_scores_num)

      if (sum(p2.hand_scores_num) == 180){
        p2.180 <- p2.180 + 1
      } else if (sum(p2.hand_scores_num) >= 140){
        p2.140 <- p2.140 + 1
      } else if (sum(p2.hand_scores_num) >= 100){
        p2.100 <- p2.100 + 1
      }

    } else {
      p2.busted <- p2.busted + 1
      p2.hand_scores_chr <- c("0", "0", "0")

    }

    # add chr darts to thrown dart score
    p2.thrown_darts_score <- c(p2.thrown_darts_score, p2.hand_scores_chr)

    # count number of darts
    p2.n_of_darts <- p2.n_of_darts + length(p2.hand_scores_chr)


    # checkout
    if (p2.score == 0){
      p2.checkout <- sum(p2.hand_scores_num)
      p2.closing_double <- p2.hand_scores_chr[length(p2.hand_scores_chr)]
      p1.checkout <- NA
    }
  }

  # Lists to return
    # Df
      p1.hand_dart_num <- rep(c(1, 2, 3), ceiling(length(p1.thrown_darts_score)/3))[1:length(p1.thrown_darts_score)]
      p1.leg_dart_num  <- 1:length(p1.thrown_darts_score)

      p1.darts.df <- data.frame("dart n in leg" = p1.leg_dart_num,
                             "dart n in hand" = p1.hand_dart_num,
                             "score" = p1.thrown_darts_score,
                             "leg ID" = l.ID)
      p1.power.df <- data.frame("leg ID" = l.ID,
                                "100+" = p1.100,
                                "140+" = p1.140,
                                "180" = p1.180)
      p1.checkout.df <- data.frame("leg ID" = l.ID,
                                   "checkout" = p1.checkout,
                                   "busted" = p1.busted)

        # doubles df
          if (!is.na(p1.missed_doubles) | !is.na(p1.closing_double)) {
            missed.df <- data.frame(scores = p1.missed_doubles) %>%
              group_by(scores) %>%
              summarise(missed = n())
            closing.df <- data.frame(scores = p1.closing_double) %>%
              group_by(scores) %>%
              summarise(hit = n())

            doubles.df <- suppressMessages(full_join(missed.df, closing.df))
            doubles.df <- doubles.df[-c(is.na(doubles.df$scores)),]
            doubles.df$hit[is.na(doubles.df$hit)] <- 0
            doubles.df$missed[is.na(doubles.df$missed)] <- 0

            p1.doubles.df <- doubles.df %>%
              mutate(scores = factor(scores, levels = levels)) %>%
              arrange(scores) %>%
              mutate("leg ID" = l.ID)
          } else {
            p1.doubles.df <- data.frame("scores" = NA, "hit" = NA, "missed" = NA, "leg ID" = l.ID)
          }


      p2.hand_dart_num <- rep(c(1, 2, 3), ceiling(length(p2.thrown_darts_score)/3))[1:length(p2.thrown_darts_score)]
      p2.leg_dart_num  <- 1:length(p2.thrown_darts_score)

      p2.darts.df <- data.frame("dart n in leg" = p2.leg_dart_num,
                                "dart n in hand" = p2.hand_dart_num,
                                "score" = p2.thrown_darts_score,
                                "leg ID" = l.ID)
      p2.power.df <- data.frame("leg ID" = l.ID,
                                `100+` = p2.100,
                                "140+" = p2.140,
                                "180" = p2.180)
      p2.checkout.df <- data.frame("leg ID" = l.ID,
                                   "checkout" = p2.checkout,
                                   "busted" = p2.busted)

      # doubles df
      if (!is.na(p2.missed_doubles) | !is.na(p2.closing_double)) {
        missed.df <- data.frame(scores = p2.missed_doubles) %>%
          group_by(scores) %>%
          summarise(missed = n())
        closing.df <- data.frame(scores = p2.closing_double) %>%
          group_by(scores) %>%
          summarise(hit = n())

        doubles.df <- suppressMessages(full_join(missed.df, closing.df))
        doubles.df <- doubles.df[-c(is.na(doubles.df$scores)),]
        doubles.df$hit[is.na(doubles.df$hit)] <- 0
        doubles.df$missed[is.na(doubles.df$missed)] <- 0

        p2.doubles.df <- doubles.df %>%
          mutate(scores = factor(scores, levels = levels)) %>%
          arrange(scores) %>%
          mutate("leg ID" = l.ID)
      } else {
        p2.doubles.df <- data.frame("scores" = NA, "hit" = NA, "missed" = NA, "leg ID" = l.ID)
      }


      p1.stats <- list(
        "darts" = p1.darts.df,
        "power scoring" = p1.power.df,
        "checkout" = p1.checkout.df,
        "doubles" = p1.doubles.df
      )

      p2.stats <- list(
        "darts" = p2.darts.df,
        "power scoring" = p2.power.df,
        "checkout" = p2.checkout.df,
        "doubles" = p2.doubles.df
      )
      if (p1.score == 0) {winner <- player1} else {winner <- player2}

      # leg df
      leg.stat.df <- data.frame("leg ID" = l.ID,
                                "player 1" = player1,
                                "player 2" = player2,
                                "winner" = winner,
                                "n leg in set" = n.leg.set,
                                "set ID" = set.id,
                                "n leg in match" = n.leg.match,
                                "match ID" = match.id)



 # return

 t <- paste("legres <- list('leg.stat' = leg.stat.df,", player1, ".leg.stats", "= p1.stats,", player2, ".leg.stats", "= p2.stats)", sep ="")

 eval(parse(text = t))

 cat(paste("Congratulations ", winner, "!", sep =""), "You've won this leg", "\n")
 Sys.sleep(0.5)

 return(legres)
}
