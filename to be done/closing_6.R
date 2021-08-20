#' @name closing_6
#' @title closing exercise
#' @description This function is used to train in closures with 3 or 6 darts
#' @usage closing_6(level)
#' @param level "beginner" (default), "intermediate or "advanced"
#' @author Matteo Miotto
#' @importFrom magrittr %>%
#' @importFrom lubridate month day year
#' @importFrom svDialogs dlg_form
#' @importFrom dplyr mutate

#' @export



closing_6 <- function(level = "beginner"){

  exit <- 41:80
  if (level == "intermediate") {exit <- 81:120}
  if (level == "advanced") {exit <- c(121:158, 160, 161, 164, 167, 170)}

  exit_i <- sample(exit, 1)

  # create dataframe
  scores.df <- data.frame(
    year = year(Sys.Date()),
    month = month(Sys.Date()),
    day = day(Sys.Date())
  )

  tot_closed_3 <- 0
  tot_closed_6 <- 0

  for (i in seq_along(1:10)){

    if (exit_i > max(exit)) {exit_i <- sample(exit, 1)}
    if (exit_i < min(exit)) {exit_i <- sample(exit, 1)}
    if (!(exit_i %in% exit)) {exit_i <- sample(c(160, 161, 164, 167, 170), 1)}


    scored <- NA

    # ask 3 darts
    while(!(scored %in% c("n", "y"))){

      form <- list(":TXT" = "y/n")
      form_title <- paste("Did you close ", exit_i , " in 3 darts?", sep ="")
      scored <- unlist(dlg_form(form, title = form_title)$res)

      if (!(scored %in% c("y", "n"))) {message('Error: you inserted an invalid value. Value shold be y or n.')}

    }

    if (scored == "y") {scored <- 1; tot_closed_3 <- tot_closed_3 + 1; exit_i <- exit_i + 10} else {
      scored <- NA

      # ask 6 darts
      while(!(scored %in% c("n", "y"))){

      form <- list(":TXT" = "y/n")
      form_title <- paste("Did you close ", exit_i , " in 6 darts?", sep ="")
      scored <- unlist(dlg_form(form, title = form_title)$res)

      if (!(scored %in% c("y", "n"))) {message('Error: you inserted an invalid value. Value shold be y or n.')}

      }

      if (scored == "y") {scored <- 1; tot_closed_6 <- tot_closed_6 + 1; exit_i <- exit_i + 5} else {scored <- 0; exit_i <- exit_i - 3}

      }

    # insert in df
    scores.df[1, i+3] <- scored

  }

  scores.df <- scores.df %>%
    mutate("tot.closed.3" = tot_closed_3) %>%
    mutate("tot.closed.6" = tot_closed_6) %>%
    mutate("precision" = round( (tot_closed_3 + tot_closed_6)/(20 - tot_closed_3)*100, 2))

  # change df colnames
  colnames(scores.df) <- c("year", "month", "day", as.character(1:10), "tot.closed.3", "tot.closed.6", "precision")
  filename <- paste("closing_6_", level,".csv", sep = "")
  opened_df <- read.csv(file = filename)


  names(opened_df) <- names(scores.df)
  opened_df <- rbind(opened_df, scores.df)
  write.csv(x = opened_df, file = filename, row.names = F)

  return(scores.df)

}
