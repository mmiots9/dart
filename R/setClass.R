# class
  #' @export set1p
  #' @exportClass set1p
  set1p <- setClass(
    "set1p",
    slots = list(
      legs = "list",
      dartsScoresCh = "character",
      dartsNumber = "numeric",
      means = "data.frame",
      powerScoring = "data.frame",
      checkout = "data.frame"
    )
  )























# Methods
  # show
  #' @export
  setMethod(
    "show",
    "set1p",
    function(object){
      cat("Date:", getID(object), "\n")
      cat("Player:", getPlayers(object), "\n")
      if (getWin(object) == 1) {
        cat("Winner: yes", "\n", "\n")
      } else {
        cat("Winner: no", "\n", "\n")
      }
      cat("Set closed with an avarage of", object@means[which(rownames(s1@means) == "3darts"), 2], " \n")
    }
  )

  # summary
  setMethod(
    "summary",
    "set1p",
    function(object){

      # creazione vettori chr
      meansCh <- c("3 darts ", "First 9 ", "1st dart", "2nd dart", "3rd dart")
      checkoutCh <- c("Missed", "Busted", "Rate  ")
      powerCh <- c("54  ", "57  ", "60  ", "100+", "140+", "180+")

      # creazione vettori valori
      meansVa <- object@means[, 2]
      meansVa <- as.character(format(round(meansVa, 2), nsmall = 2))
      checkoutVa <- object@checkout[, 2]
      checkoutVa <- as.character(format(round(checkoutVa, 2), nsmall = 2))
      powerVa <- object@powerScoring[, 2]
      powerVa <- as.character(format(round(powerVa, 2), nsmall = 0))

      # unione vettori
      meansTot <- c(paste(paste(meansCh, meansVa, sep = ": "), ""), NA)
      checkoutTot <- c(paste(paste(checkoutCh, checkoutVa, sep = ": "),""), NA, NA, NA)
      powerTot <- paste(powerCh, powerVa, sep = ": ")

      # creazione matrice
      matr <- matrix(c(meansTot, checkoutTot, powerTot), ncol = 3, byrow = F)

      # conversione in table
      attr(matr, "class") <- c("table")

      # set dimnames
      attr(matr, "dimnames") <- list(rep("", 6), c("     Means     ", "  Checkout  ", "Power Scoring"))

      # print out
      cat("Player:", object@player, "\n")
      if (object@win == 1) {
        cat("Winner: yes", "\n", "\n")
      } else {
        cat("Winner: no", "\n", "\n")
      }
      matr
    }
  )

