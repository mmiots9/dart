#' @name around_the_clock
#' @title around the clock training session
#' @description This function is used to play around the clock in training session
#' @usage around_the_clock(section)
#' @param section which section to score in: "s" for singles (default), "d" for doubles or "t" for trebles
#' @author Matteo Miotto
#' @importFrom magrittr %>%
#' @importFrom lubridate month day year
#' @importFrom svDialogs dlg_form
#' @importFrom dplyr mutate

#' @export

around_the_clock <- function(section = "s"){

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

  for (i in seq_along(possible_scores)){
    how_many <- 0
    scored <- "n"

    while(scored != "y"){
      form <- list(":TXT" = "y/n")
      form_title <- paste("Did you hit ", possible_scores[i], "?", sep ="")
      scored <- unlist(dlg_form(form, title = form_title)$res)

      how_many <- how_many + 1

      if (!(scored %in% c("y", "n"))) {message('Error: you inserted an invalid value. Value shold be y or n.'); how_many <- how_many - 1}
    }

  # insert in df
    scores.df[1, i+3] <- how_many

    how_many_tot <- how_many_tot + how_many
  }

    scores.df <- scores.df %>%
      mutate("tot.darts" = how_many_tot) %>%
      mutate("precision" = round(length(possible_scores)/how_many_tot*100, 2))

   # change df colnames
   colnames(scores.df) <- c("year", "month", "day", possible_scores, "tot.darts", "precision")
   filename <- paste("around_", section,".csv", sep = "")
   opened_df <- read.csv(file = filename)


   names(opened_df) <- names(scores.df)
   opened_df <- rbind(opened_df, scores.df)
   write.csv(x = opened_df, file = filename, row.names = F)

   return(scores.df)




}
