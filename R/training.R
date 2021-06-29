#' @name training
#' @title Training session
#' @description This function is used for a single training session
#' @usage training(save_dir)
#' @param save_dir in which directory are stored the csv files for savings
#' @author Matteo Miotto
#' @importFrom magrittr %>%
#' @importFrom lubridate month day year
#' @importFrom svDialogs dlg_dir
#' @importFrom readr parse_time
#'
#' @export
training <- function(save_dir = NA){
  if (is.na(save_dir)){
    cat("Select training result folder", "\n")
    Sys.sleep(1.0)
    save_dir <- dlg_dir()$res}
  setwd(save_dir)

  # useful values

  end_tr <- 0
  training_list_complete <- c("round the clock 3 darts", "around the clock", "high scores", "closing 3 darts", "closing 6 darts", "end the training")
  game_2_play <- 0
  levels_closing <- c("beginner", "intermediate", "advanced")
  tot.as <- tot.ad <- tot.at <- tot.cl6b <- tot.cl6i <- tot.cl6a <- 0

  # df
  tr.df <- data.frame(
    year = year(Sys.Date()),
    month = month(Sys.Date()),
    day = day(Sys.Date()),
    time = NA,
    rc3.s = NA,
    rc3.d = NA,
    rc3.t = NA,
    ac.s = NA,
    ac.d = NA,
    ac.t = NA,
    hs.18 = NA,
    hs.19 = NA,
    hs.20 = NA,
    cl3.b = NA,
    cl3.i = NA,
    cl3.a = NA,
    cl6.b = NA,
    cl6.i = NA,
    cl6.a = NA,
    tot.prec = NA,
    tot.clos = NA,
    tot = NA
  )


  begin_time <- Sys.time()

  while (end_tr == 0){

    # which training?
      title_dlg <- "Which game do you want to play?"
      game_2_play <- menu(training_list_complete, title = title_dlg)
      Sys.sleep(0.5)

      # vedere se è la fine
      if (training_list_complete[game_2_play] == "end the training") {end_tr <- 1; next}

      # training function launch
      if (training_list_complete[game_2_play] == "round the clock 3 darts") {
        cat("Round the clock 3 darts: singles", "\n", "Game on!", "\n")
        g <- round_clock_3darts("s")
        tr.df$rc3.s <- g$precision
        Sys.sleep(0.5)
        cat("Round the clock 3 darts: doubles", "\n", "Game on!", "\n")
        g <- round_clock_3darts("d")
        tr.df$rc3.d <- g$precision
        Sys.sleep(0.5)
        cat("Round the clock 3 darts: trebles", "\n", "Game on!", "\n")
        g <- round_clock_3darts("t")
        tr.df$rc3.t <- g$precision

        training_list_complete <- training_list_complete[-game_2_play]
        Sys.sleep(0.5)
        next}
      if (training_list_complete[game_2_play] == "around the clock"){
        cat("Around the clock: singles", "\n", "Game on!", "\n")
        g <- around_the_clock("s")
        tr.df$ac.s <- g$precision
        tot.as <- g$tot.darts
        Sys.sleep(0.5)
        cat("Around the clock: doubles", "\n", "Game on!", "\n")
        g <- around_the_clock("d")
        tr.df$ac.d <- g$precision
        tot.ad <- g$tot.darts
        Sys.sleep(0.5)
        cat("Around the clock: trebles", "\n", "Game on!", "\n")
        g <- around_the_clock("t")
        tr.df$ac.t <- g$precision
        tot.at <- g$tot.darts
        training_list_complete <- training_list_complete[-game_2_play]

        Sys.sleep(0.5)
        next}
      if (training_list_complete[game_2_play] == "high scores"){
        cat("High scores: 20", "\n", "Game on!", "\n")
        g <- high_scores(20)
        tr.df$hs.20 <- g$precision
        Sys.sleep(0.5)
        cat("High scores: 19", "\n", "Game on!", "\n")
        g <- high_scores(19)
        tr.df$hs.19 <- g$precision
        Sys.sleep(0.5)
        cat("High scores: 18", "\n", "Game on!", "\n")
        g <- high_scores(18)
        tr.df$hs.18 <- g$precision

        training_list_complete <- training_list_complete[-game_2_play]
        Sys.sleep(0.5)
        next}
      if (training_list_complete[game_2_play] == "closing 3 darts") {
        level_2_play <- menu(levels_closing, title = "Which level are you in?")

        if (levels_closing[level_2_play] == "beginner") {
          cat("Closing in 3 darts: beginner", "\n", "Game on!", "\n")
          g <- closing_3("beginner")
          tr.df$cl3.b <- g$precision
          training_list_complete <- training_list_complete[-game_2_play]
          Sys.sleep(0.5)
          next
        }
        if (levels_closing[level_2_play] == "intermediate") {
          cat("Closing in 3 darts: intermediate", "\n", "Game on!", "\n")
          g <- closing_3("intermediate")
          tr.df$cl3.i <- g$precision
          training_list_complete <- training_list_complete[-game_2_play]
          Sys.sleep(0.5)
          next
        }
        if (levels_closing[level_2_play] == "advanced") {
          cat("Closing in 3 darts: advanced", "\n", "Game on!", "\n")
          g <- closing_3("advanced")
          tr.df$cl3.a <- g$precision
          training_list_complete <- training_list_complete[-game_2_play]
          Sys.sleep(0.5)
          next
        }
        next
        }
      if (training_list_complete[game_2_play] == "closing 6 darts"){
        level_2_play <- menu(levels_closing, title = "Which level are you in?")

        if (levels_closing[level_2_play] == "beginner") {
          cat("Closing in 6 darts: beginner", "\n", "Game on!", "\n")
          g <- closing_6("beginner")
          tr.df$cl6.b <- g$precision
          tot.cl6b <- g$tot.closed.3
          training_list_complete <- training_list_complete[-game_2_play]
          Sys.sleep(0.5)
          next
        }
        if (levels_closing[level_2_play] == "intermediate") {
          cat("Closing in 6 darts: intermediate", "\n", "Game on!", "\n")
          g <- closing_6("intermediate")
          tr.df$cl6.i <- g$precision
          tot.cl6i <- g$tot.closed.3
          training_list_complete <- training_list_complete[-game_2_play]
          Sys.sleep(0.5)
          next
        }
        if (levels_closing[level_2_play] == "advanced") {
          cat("Closing in 6 darts: advanced", "\n", "Game on!", "\n")
          g <- closing_6("advanced")
          tr.df$cl6.a <- g$precision
          tot.cl6a <- g$tot.closed.3
          training_list_complete <- training_list_complete[-game_2_play]
          Sys.sleep(0.5)
          next
        }
        next

      }

  }

  end_time <- Sys.time()
  tr.df$time <- parse_time(as.character(difftime(end_time, begin_time, "sec")), "%S")

  cat("Training completed! Check out the results", "\n")

  # df.result
    # create vector of multiplied precisions
    var_prec <- as.numeric(subset(tr.df, select=rc3.s:hs.20))
    tot_darts_prec <- c(66, 63, 60, tot.as, tot.ad, tot.at, 30, 30, 30)
    tot_darts_prec <- tot_darts_prec[!is.na(var_prec)]
    var_prec <- as.numeric(na.omit(var_prec))

    tr.df$tot.prec <- round(sum(var_prec*tot_darts_prec)/sum(tot_darts_prec),2)

    # create vector of multiplied doubles
    var_doub <- as.numeric(subset(tr.df, select=cl3.b:cl6.a))
    tot_doub <- c(20, 20, 20, 20-tot.cl6b, 20-tot.cl6i, 20-tot.cl6a)
    tot_doub <- tot_doub[!is.na(var_doub)]
    var_doub <- as.numeric(na.omit(var_doub))

    tr.df$tot.clos <- round(sum(var_doub*tot_doub)/sum(tot_doub),2)

    tr.df$tot <- round((tr.df$tot.clos*sum(tot_doub) + tr.df$tot.prec*sum(tot_darts_prec))/(sum(tot_doub) + sum(tot_darts_prec)),2)

  # save
    opened_df <- read.csv(file = "trainings.csv")


    names(opened_df) <- names(tr.df)
    opened_df <- rbind(opened_df, tr.df)
    write.csv(x = opened_df, file = "trainings.csv", row.names = F)

    return(tr.df)
}
