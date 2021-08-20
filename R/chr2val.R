#' @export
chr2val <- function(x){
  scoresVal <- c(0, 1:20, (1:20)*2, (1:20)*3, 25, 50, 0)
  scoresCh  <- c("0", as.character(1:20), paste("d", c(1:20), sep = ""),
                 paste("t", c(1:20), sep = ""), "25", "d25", "NA")
  names(scoresVal) <- scoresCh
  scoresFinal <- as.numeric(scoresVal[c(x)])
  return(scoresFinal)
}
