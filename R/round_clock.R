# vari giochi
  # Round the clock single
  # Round the clock double
  # Round the clock treble
  # Mix 20-19-18 (3x treble, 1x single)



library(lubridate) # month, day, year
library(svDialogs) # dlg_form

round_clock <- function(section = "s"){

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

  # for loop
  for (i in seq_along(possible_scores)){
    how_many <- NA

    # ask how many scores
    while (!(how_many %in% c(0:3))){

      form <- list(":TXT" = "")
      form_title <- paste("How many darts in", possible_scores[i])
      how_many <- unlist(dlg_form(form, title = form_title)$res)

      if (!(how_many %in% c(0:3))) {message("Error: you inserted an invalid value. Value shold be 0, 1, 2, or 3.")}
    }

    # insert in df
    scores.df[1, i+3] <- as.numeric(how_many)
  }

  # change df colnames
  colnames(scores.df) <- c("year", "month", "day", possible_scores)

  # ask to create file
  save_yn <- dlg_message("Do you want to save the results?", type = "yesno")$res
  if (save_yn == "no") {return(scores.df)}

  exist_file <- dlg_message("Do you already have a file where to store the results?", type = "yesno")$res
  if (exist_file == "no") {
    # ask name of file
    filename_asked <- dlg_input("Enter a name for the file (no extension)")$res
    filename_ok <- paste(filename_asked, ".csv", sep="")

    write.csv(x = scores.df, file = filename_ok, row.names = F)
    return(scores.df)
  }

  right_file <- 0
  while (right_file == 0){
    file_to_use <- file.choose()
    opened_df <- read.csv(file = file_to_use)

    if (ncol(opened_df) != ncol(scores.df)) {
      message("Error: selected file is not compatible with this function. You may have selected a file suitable for other section option")
      next
    }
    right_file <- 1
  }
  names(opened_df) <- names(scores.df)
  opened_df <- rbind(opened_df, scores.df)
  write.csv(x = opened_df, file = file_to_use, row.names = F)

  return(scores.df)

}
