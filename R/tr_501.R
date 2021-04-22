#' @name tr_501
#' @title Single leg 501 training
#' @description This function is used as calculator for a single 501 leg
#' @usage tr_501()
#' @returns A list containing:
#' \item{darts}{character vector of all the scores of the thrown darts}
#' \item{number of darts}{number of thrown darts}
#' \item{checkout}{checkout score}
#' \item{missed doubles}{number of missed doubles}
#' \item{busted}{number of busted}
#' @author Matteo Miotto
#' @importFrom svDialogs dlg_form
#' @export

tr_501 <- function(){

  # set useful vectors
    score <- 501
    thrown_darts_score <- NULL
    from_chr_to_score_vector <- c(0:20, (1:20)*2, (1:20)*3, 25, 50)
    from_chr_to_score_names  <- c(as.character(0:20), paste("d", c(1:20), sep = ""), paste("t", c(1:20), sep = ""), "25", "d25")
    names(from_chr_to_score_vector) <- from_chr_to_score_names
    n_of_darts <- 0
    doubles <- c((1:20)*2, 50)
    missed_doubles <- 0
    busted <- 0

  # open while cycle
    while(score !=0) {

      # ask for scored points for 3 darts
        form <- list("Darts score:TXT" = "")
        form_title <- paste("Your score is:", as.character(score))
        hand_score <- unlist(dlg_form(form, message = "Insert the value for the thrown darts separated by a comma (,). For double and triple, digit 'd' or 't' before the sector number",
                                      title = form_title)$res)

      # extrapolate dart score as chr and num vector
        hand_scores_chr <- as.character( trimws( unlist( strsplit(hand_score, split = ",")), which = "both"))
        hand_scores_num <- as.numeric(from_chr_to_score_vector[c(hand_scores_chr)])

      # add chr darts to thrown dart score
        thrown_darts_score <- c(thrown_darts_score, hand_scores_chr)

      # count number of darts
        n_of_darts <- n_of_darts + length(hand_scores_num)

      # subtract darts score from score and evaluating if miss double for checkout or bust
        score_i <- score
        for (i in seq_along(hand_scores_num)){
          if(score_i %in% doubles & (score_i - hand_scores_num[i]) !=0 ) {
            missed_doubles <- missed_doubles + 1
          }
          score_i <- score_i -  hand_scores_num[i]
        }

        if (score -  sum(hand_scores_num) != 1 & (score -  sum(hand_scores_num)) >= 0){
          score <- score -  sum(hand_scores_num)
        } else {
          busted <- busted + 1
        }



      # checkout
        if (score == 0){
        checkout <- sum(hand_scores_num)
        }

    }

    # cose importanti da return
      res <- list("darts" = thrown_darts_score,
                  "number of darts" = n_of_darts,
                  "checkout" = checkout,
                  "missed doubles" = missed_doubles,
                  "busted" = busted
                  )

      cat("Congratulations!", paste("You've completed the leg in", n_of_darts, "darts, with a", checkout, "checkout"), sep = "\n")
      return(res)

}
