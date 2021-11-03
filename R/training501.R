#' @name training501
#' @title Single leg 501 training
#' @description This function is used as calculator for a single 501 leg in training
#' @usage training501(player, save = T, file = "path/to/file")
#' @author Matteo Miotto
#' @importFrom svDialogs dlg_form
#' @importFrom magrittr %>%
#' @importFrom lubridate month day year
#' @export

training501 <- function(player, save = T, file = NA){

  # check if player name is chr
  if (!is.character(player)) {stop("Player name must be of type character")}

  # check if save is boolean
  if (!save %in% c(T, F, TRUE, FALSE)) {stop("Save value must be of type boolean")}

  # check if save or not
  if (save) {

    # check if file is present, otherwise select file
    if ((is.na(file)) & !(file %in% list.files())) {
      cat("Select training result file", "\n")
      Sys.sleep(0.5)

      file <- file.choose()
    }
  }

  # id leg
  id <- gsub(
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

  # set useful values
  p1.score <- 501
  p1.dartsScoresCh <- NULL
  doubles <- c((1:20)*2, 50)
  possible_scores <- c("0", as.character(1:20) ,paste("d", c(1:20), sep = ""), paste("t", c(1:20), sep = ""), "25", "d25")

  # game
  while (p1.score !=0) {

    p1.handScoresCh <- NA

    # player 1
    while (!all(all_of(p1.handScoresCh %in% possible_scores))){

      # ask for scored points for 3 darts
      cat("\n")
      form <- list("Darts score:TXT" = "1st dart, 2nd dart, 3rd dart")
      form_title <- paste0(player, ": ", p1.score, " points left")
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

      # if bust
      if (p1.score_i == 1 |
          p1.score_i < 0 |
          (p1.score_i == 0 & !(p1.handScoresCh[i] %in% c(paste0("d", c(1:20)), "d25")))){
        p1.handScoresVal <- c(0, 0, 0)

        # aggiungo NA
        if (i == 1){p1.handScoresCh[2:3] <- "NA"} else if (i == 2) {p1.handScoresCh[3] <- "NA"}
        message("No score\n")
        break
      }

      # if close
      if (p1.score_i == 0) {if (i == 1){p1.handScoresCh[2:3] <- "NA"} else if (i == 2) {p1.handScoresCh[3] <- "NA"}}
    }

    p1.score <- p1.score - sum(p1.handScoresVal)

    # add chr darts to thrown dart score
    p1.dartsScoresCh <- c(p1.dartsScoresCh, p1.handScoresCh)

  }

  p1.leg <- legtr(
    id = id,
    player = player,
    dartsScoresCh = p1.dartsScoresCh
  )

  if (save) {
    firstline <- paste(id, player)
    secondline <- paste(p1.dartsScoresCh, collapse = ",")
    tot <- rbind(firstline, secondline)

    write.table(tot, file = file, append = T, row.names = F, col.names = F, quote = F)
  }

  Sys.sleep(0.5)
  cat("\n","Training completed! Check out the results", "\n")
  return(p1.leg)
}
