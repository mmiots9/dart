
#' @importFrom svDialogs glg_dir

training <- function(save_dir = NA){
  if (is.na(save_dir)){save_dir <- dlg_dir()$res}
  setwd(save_dir)

  end_tr <- 0
  while (end_tr == 0){
    # chiedere quale allenamento e lanciare la funzione corrispondente
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


  }



}
