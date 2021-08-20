#' @name tr_501
#' @title Single leg 501 training
#' @description This function is used as calculator for a single 501 leg
#' @usage tr_501()
#' @author Matteo Miotto
#' @importFrom svDialogs dlg_form dlg_dir
#' @importFrom stringr str_detect
#' @importFrom magrittr %>%
#' @importFrom lubridate month day year
#' @export

tr_501 <- function(){

  if(!("trleg.csv" %in% list.files())){
  cat("Select training result folder", "\n")
  Sys.sleep(1.0)
  save_dir <- dlg_dir()$res
  setwd(save_dir)
  }

  # set useful values
  score <- 501
  thrown_darts_score <- checkout <- NULL
  n_of_darts <- missed <- plus100 <- plus140 <- plus180 <- plus60 <- n20 <- n19 <- n18 <- n60 <- n57 <- n54 <-  0

  from_chr_to_score_vector <- c(0, 1:20, (1:20)*2, (1:20)*3, 25, 50)
  from_chr_to_score_names  <- c("0", as.character(1:20), paste("d", c(1:20), sep = ""), paste("t", c(1:20), sep = ""), "25", "d25")
  names(from_chr_to_score_vector) <- from_chr_to_score_names
  doubles <- c((1:20)*2, 50)
  levels <- c("0", as.character(1:20) ,paste("d", c(1:20), sep = ""), paste("t", c(1:20), sep = ""), "25", "d25")

  # show instructions in first leg
  dlg_form(form = list("Press Enter to start"=""), message = "To record the scores, for each hand insert the value for the thrown darts separated by a comma (,) in the following form:
                                          #: for singles
                                          d#: for doubles
                                          t#: for trebles \n For example: t20, 19, d19",
           title = "Instructions")

  cat("\n")

  while (score !=0) {
    hand_scores_chr <- NA

    while (!all(all_of(hand_scores_chr %in% from_chr_to_score_names))){

      # ask for scored points for 3 darts
      form <- list("Darts score:TXT" = "1st dart, 2nd dart, 3rd dart")
      form_title <- paste("Your score is", score)
      hand_score <- unlist(dlg_form(form, title = form_title)$res)

      # extrapolate dart score as chr and num vector
      hand_scores_chr <- as.character( trimws( unlist( strsplit(hand_score, split = ",")), which = "both"))
      hand_scores_num <- as.numeric(from_chr_to_score_vector[c(hand_scores_chr)])

      if (!all(all_of(hand_scores_chr %in% from_chr_to_score_names))) {message("Error: you have inserted unvalid values. Retry")}
    }

    # subtract darts score from score and evaluating if miss double for checkout or bust
    score_i <- score
    for (i in seq_along(hand_scores_num)){
      if(score_i %in% doubles & (score_i - hand_scores_num[i]) !=0 ) {
        missed <- missed + 1
      }

      score_i <- score_i -  hand_scores_num[i]
    }

    if (score -  sum(hand_scores_num) != 1 & (score -  sum(hand_scores_num)) >= 0){
      score <- score -  sum(hand_scores_num)

      if (sum(hand_scores_num) == 180){
        plus180 <- plus180 + 1
      } else if (sum(hand_scores_num) >= 140){
        plus140 <- plus140 + 1
      } else if (sum(hand_scores_num) >= 100){
        plus100 <- plus100 + 1
      } else if (sum(hand_scores_num >= 60)){
        plus60 <- plus60 + 1
      }

    } else {
      hand_scores_chr <- c("0", "0", "0")
    }

    # add chr darts to thrown dart score
    thrown_darts_score <- c(thrown_darts_score, hand_scores_chr)

    # count number of darts
    n_of_darts <- n_of_darts + length(hand_scores_chr)

    # checkout
    if (score == 0){
      checkout <- sum(hand_scores_num)
      next
    }



  }

  # count 18, 19, 20, 54, 57, 60
    n18 <- length(which(thrown_darts_score == "18"))
    n19 <- length(which(thrown_darts_score == "19"))
    n20 <- length(which(thrown_darts_score == "20"))
    n54 <- length(which(thrown_darts_score == "t18"))
    n57 <- length(which(thrown_darts_score == "t19"))
    n60 <- length(which(thrown_darts_score == "t20"))

  # darts means
    hand_dart_num <- rep(c(1, 2, 3), ceiling(length(thrown_darts_score)/3))[1:length(thrown_darts_score)]

    darts.df <- data.frame("dart.hand" = hand_dart_num,
                              "score" = as.numeric(from_chr_to_score_vector[c(thrown_darts_score)]))

    means <- darts.df %>%
      group_by(dart.hand) %>%
      summarize(mean = round(mean(score), 2))

    mean.1 <- means$mean[means$dart.hand == 1]
    mean.2 <- means$mean[means$dart.hand == 2]
    mean.3 <- means$mean[means$dart.hand == 3]

  # df
    res <- data.frame(
      year = year(Sys.Date()),
      month = month(Sys.Date()),
      day = day(Sys.Date()),
      n.darts = n_of_darts,
      mean.3darts = round(501/n_of_darts*3, 2),
      mean.first9 = round(mean( as.numeric(from_chr_to_score_vector[c(thrown_darts_score)])[1:9]), 2)*3,
      mean.1st = mean.1,
      mean.2nd = mean.2,
      mean.3d = mean.3,
      checkout = checkout,
      n.missed = missed,
      checkout.rate = round(1/(missed + 1)*100, 2),
      n.60plus = plus60,
      n.100plus = plus100,
      n.140plus = plus140,
      n.180 = plus180,
      n.18 = n18,
      n.19 = n19,
      n.20 = n20,
      n.54 = n54,
      n.57 = n57,
      n.60 = n60
    )

    # save
    filename <- "trleg.csv"
    opened_df <- read.csv(file = filename)


    names(opened_df) <- names(res)
    opened_df <- rbind(opened_df, res)
    write.csv(x = opened_df, file = filename, row.names = F)


  cat("\n","Training completed! Check out the results", "\n")
  return(res)
}
