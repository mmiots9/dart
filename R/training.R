
#' @importFrom svDialogs dlg_dir

training <- function(save_dir = NA){
  if (is.na(save_dir)){save_dir <- dlg_dir()$res}
  setwd(save_dir)

  begin_time <- Sys.time()
  end_tr <- 0
  while (end_tr == 0){
    # chiedere quale allenamento e lanciare la funzione corrispondente
      training_list_complete <- c("round the clock 3 darts singles", "around the clock", "high scores", "Closing 3")

    # possibili allenamenti:
        # - round_clock_3darts (s, d, t)
        # - around_the_clock (s, d, t)
        # - 18-19-20 training
        # - Closing 3 beginner
        # - Closing 3 intermediate
        # - Closing 3 advanced
        # - Closing 6 beginner
        # - Closing 6 intermediate
        # - Closing 6 advanced
        # - end


  }



}
