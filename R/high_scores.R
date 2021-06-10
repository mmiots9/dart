high_scores <- function(sector = 20){
  # check input
  if (length(sector) != 1) {stop("sector must have lenght 1")}
  if (!(sector %in% 18:20)) {stop('sector could be 18, 19, or 20.')}

  # create dataframe
  scores.df <- data.frame(
    year = year(Sys.Date()),
    month = month(Sys.Date()),
    day = day(Sys.Date())
  )

  how_many_tot <- 0

  # for loop
  for (i in seq_along(1:10)){
    how_many <- NA

    # ask how many scores
    while (!(how_many %in% c(0:3))){

      form <- list(":TXT" = "")
      form_title <- paste("Visit ", i,": How many darts in ", sector, sep="")
      how_many <- as.numeric(unlist(dlg_form(form, title = form_title)$res))

      if (!(how_many %in% c(0:3))) {message("Error: you inserted an invalid value. Value shold be 0, 1, 2, or 3.")}
    }

    # insert in df
    scores.df[1, i+3] <- how_many

    how_many_tot <- how_many_tot + how_many
  }

  scores.df <- scores.df %>%
    mutate("tot.darts" = how_many_tot, "precision" = round(how_many_tot/30*100),2)

  # change df colnames
  colnames(scores.df) <- c("year", "month", "day", as.character(1:10), "tot.darts", "precision")
  filename <- paste("high_scores_", sector,".csv", sep = "")
  opened_df <- read.csv(file = filename)


  names(opened_df) <- names(scores.df)
  opened_df <- rbind(opened_df, scores.df)
  write.csv(x = opened_df, file = filename, row.names = F)

  return(scores.df)

}
