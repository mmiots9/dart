#' @name round_clock_3darts
#' @title round the clock 3 darts
#' @description This function is used to play a modified version of around the clock
#' @usage round_clock_3darts(section)
#' @param section which section to score in: "s" for singles (default), "d" for doubles or "t" for trebles
#' @author Matteo Miotto
#' @importFrom magrittr %>%
#' @importFrom lubridate month day year
#' @importFrom svDialogs dlg_form
#' @importFrom dplyr mutate

#' @export

round_clock_3darts <- function(section = "s"){

  # check input
  section <- tolower(section)
  if (length(section) != 1) {stop("Section must have lenght 1")}
  if (!(section %in% c("s", "d", "t"))) {stop('Section could be "s", "b", or "t".')}

  # set scores vector
  possible_scores <- as.character(c(1:20, 25, 50))
  if (section == "d") {possible_scores <- paste("d", as.character(c(1:20, 25)), sep = "")}
  if (section == "t") {possible_scores <- paste("t", as.character(1:20), sep = "")}

  # create dataframe
  scores.df <- data.frame(
    year = year(Sys.Date()),
    month = month(Sys.Date()),
    day = day(Sys.Date())
  )

  how_many_tot <- 0

  # for loop
  for (i in seq_along(possible_scores)){
    how_many <- NA


    # ask how many scores
    while (!(how_many %in% c(0:3))){

      form <- list(":TXT" = "")
      form_title <- paste("How many darts in", possible_scores[i])
      how_many <- as.numeric(unlist(dlg_form(form, title = form_title)$res))

      if (!(how_many %in% c(0:3))) {message("Error: you inserted an invalid value. Value shold be 0, 1, 2, or 3.")}
    }

    # insert in df
    scores.df[1, i+3] <- how_many

    how_many_tot <- how_many_tot + how_many
  }

  scores.df <- scores.df %>%
    mutate("tot.darts" = how_many_tot, "precision" = round(how_many_tot/(length(possible_scores)*3)*100, 2))

  # change df colnames
  colnames(scores.df) <- c("year", "month", "day", possible_scores, "tot.darts", "precision")
  filename <- paste("round_3_", section,".csv", sep = "")
  opened_df <- read.csv(file = filename)


  names(opened_df) <- names(scores.df)
  opened_df <- rbind(opened_df, scores.df)
  write.csv(x = opened_df, file = filename, row.names = F)

  return(scores.df)

}
