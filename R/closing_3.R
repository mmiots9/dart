closing_3 <- function(level = "beginner"){

  exit <- 41:80
  if (level == "intermediate") {exit <- 81:120}
  if (level == "advanced") {exit <- c(121:158, 160, 161, 164, 167, 170)}

  exit <- sample(exit, 20)

  # create dataframe
  scores.df <- data.frame(
    year = year(Sys.Date()),
    month = month(Sys.Date()),
    day = day(Sys.Date())
  )

  tot_closed <- 0

  for (i in seq_along(exit)){

    scored <- NA

    while(!(scored %in% c("n", "y"))){

      form <- list(":TXT" = "y/n")
      form_title <- paste("Did you close ", exit[i] , " in 3 darts?", sep ="")
      scored <- unlist(dlg_form(form, title = form_title)$res)

      if (!(scored %in% c("y", "n"))) {message('Error: you inserted an invalid value. Value shold be y or n.')}

    }

    if (scored == "y") {scored <- 1; tot_closed <- tot_closed + 1} else {scored <- 0}

    # insert in df
    scores.df[1, i+3] <- scored

  }

  scores.df <- scores.df %>%
    mutate("tot.darts" = tot_closed) %>%
    mutate("precision" = round(tot_closed/20*100),2)

  # change df colnames
  colnames(scores.df) <- c("year", "month", "day", as.character(1:n), "tot.darts", "precision")
  filename <- paste("closing_3_", level,".csv", sep = "")
  opened_df <- read.csv(file = filename)


  names(opened_df) <- names(scores.df)
  opened_df <- rbind(opened_df, scores.df)
  write.csv(x = opened_df, file = filename, row.names = F)

  return(scores.df)



}
