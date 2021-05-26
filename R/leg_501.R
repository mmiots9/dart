leg_501 <- function(player1, player2) {

  # set useful values
  p1.score <- p2.score <- 501
  p1.thrown_darts_score <- p1.first_dart <- p1.second_dart <- p1.third_dart <- p1.missed_doubles <- p1.checkout <- p1.closing_double <-
    p2.thrown_darts_score <- p2.first_dart <- p2.second_dart <- p2.third_dart <- p2.missed_doubles <- p2.checkout <- p2.closing_double <- NULL
  from_chr_to_score_vector <- c(0, 1:20, 1:20, (1:20)*2, (1:20)*3, 25, 50)
  from_chr_to_score_names  <- c("0", paste("o",(1:20), sep = ""), paste("i",(1:20), sep = ""), paste("d", c(1:20), sep = ""), paste("t", c(1:20), sep = ""), "25", "d25")
  names(from_chr_to_score_vector) <- from_chr_to_score_names
  p1.n_of_darts <- p1.busted <- p1.missed <- p1.100 <- p1.140 <- p1.180 <-
    p2.n_of_darts <- p2.busted <- p2.missed <- p2.100 <- p2.140 <- p2.180 <- 0
  doubles <- c((1:20)*2, 50)

  while ((p1.score !=0) & (p2.score !=0)) {

    # player 1
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

    # add chr darts to each dart vector
    for (i in seq_along(p1.hand_scores_chr)){

      sentence <- switch(i,
                         "1" = "p1.first_dart <- c(p1.first_dart, p1.hand_scores_chr[1])",
                         "2" = "p1.second_dart <- c(p1.second_dart, p1.hand_scores_chr[2])",
                         "3" = "p1.third_dart <- c(p1.third_dart, p1.hand_scores_chr[3])"
      )
      eval(parse(text = sentence))
    }

    # add chr darts to thrown dart score
    p1.thrown_darts_score <- c(p1.thrown_darts_score, p1.hand_scores_chr)

    # count number of darts
    p1.n_of_darts <- p1.n_of_darts + length(p1.hand_scores_chr)


    # checkout
    if (p1.score == 0){
      p1.checkout <- sum(p1.hand_scores_num)
      p1.closing_double <- p1.hand_scores_chr[length(p1.hand_scores_chr)]
      next
    }



    # player 2
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

    # add chr darts to each dart vector
    for (i in seq_along(p2.hand_scores_chr)){

      sentence <- switch(i,
                         "1" = "p2.first_dart <- c(p2.first_dart, p2.hand_scores_chr[1])",
                         "2" = "p2.second_dart <- c(p2.second_dart, p2.hand_scores_chr[2])",
                         "3" = "p2.third_dart <- c(p2.third_dart, p2.hand_scores_chr[3])"
      )
      eval(parse(text = sentence))
    }

    # add chr darts to thrown dart score
    p2.thrown_darts_score <- c(p2.thrown_darts_score, p2.hand_scores_chr)

    # count number of darts
    p2.n_of_darts <- p2.n_of_darts + length(p2.hand_scores_chr)


    # checkout
    if (p2.score == 0){
      p2.checkout <- sum(p2.hand_scores_num)
      p2.closing_double <- p2.hand_scores_chr[length(p2.hand_scores_chr)]
    }
  }

  p1.stats <- list("darts" = p1.thrown_darts_score,
                   "1st dart" = p1.first_dart,
                   "2nd dart" = p1.second_dart,
                   "3rd dart" = p1.third_dart,
                   "number of darts" = p1.n_of_darts,
                   "180" = p1.180,
                   "140+" = p1.140,
                   "100+" = p1.100,
                   "checkout" = p1.checkout,
                   "closing double" = p1.closing_double,
                   "missed doubles" = p1.missed_doubles,
                   "missed" = p1.missed,
                   "busted" = p1.busted
  )

 p2.stats <- list("darts" = p2.thrown_darts_score,
                  "1st dart" = p2.first_dart,
                  "2nd dart" = p2.second_dart,
                  "3rd dart" = p2.third_dart,
                  "number of darts" = p2.n_of_darts,
                  "180" = p2.180,
                  "140+" = p2.140,
                  "100+" = p2.100,
                  "checkout" = p2.checkout,
                  "closing double" = p2.closing_double,
                  "missed doubles" = p2.missed_doubles,
                  "missed" = p2.missed,
                  "busted" = p2.busted
  )

 if (p1.score == 0) {winner <- player1} else {winner <- player2}

 # return
 t <- paste("legres <- list('winner' = winner,", player1, ".leg.stats", "= p1.stats,", player2, ".leg.stats", "= p2.stats)", sep ="")

 eval(parse(text = t))

 cat(paste("Congratulations", winner), "You've won this leg", "\n")

 return(legres)
}
