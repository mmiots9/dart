#' @importFrom knitr kable
# leg1p
# Class
  #' @export leg1p
  #' @exportClass leg1p
  leg1p <- setClass(
    "leg1p",
    slots = list(
      id = "character",
      player = "character",
      win = "numeric",
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
    "leg1p",
    function(object){
      cat("Date:", object@id[3], "\n")
      cat("Player:", object@player, "\n", "\n")
      if (object@win == 1) {
        cat("Leg closed in", object@dartsNumber, "darts \n")
      } else {
        cat("Leg lost")
      }

    }
  )

  # summary
  #' @export
  setMethod(
    "summary",
    "leg1p",
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

# leg2p
# Class
  #' @export leg2p
  #' @exportClass leg2p
  leg2p <- setClass(
    "leg2p",
    slots = list(
      id = "character",
      date = "character",
      players = "character",
      winner = "character",
      player1 = "leg1p",
      player2 = "leg1p"
    )
  )

# Methods
  # show
  #' @export
  setMethod(
    "show",
    "leg2p",
    function(object){

      if (object@winner == object@player1@player){
        ndarts <- object@player1@dartsNumber

      } else {
        ndarts <- object@player2@dartsNumber
      }

      cat("Date:", object@date, "\n")
      cat("Players: ", object@players[1], ", ", object@players[2], "\n", "\n", sep = "")
      cat(object@winner, "won this leg in", ndarts, "darts", "\n")
    }
  )

  # summary
  #' @export
  setMethod(
    "summary",
    "leg2p",
    function(object){
      # vettore di mezzo
      middle <- c("Means", "3 darts", "First 9", "1st dart", "2nd dart", "3rd dart",
                  "Checkout", "Missed", "Busted", "Rate",
                  "Power scoring", "54", "57", "60", "100+", "140+",
                  "180+")

      # vettore players
      p1 <- c("",
              as.character(format(round(object@player1@means[, 2], 2), nsmall = 2)),
              "",
              as.character(round(object@player1@checkout[, 2], 2)),
              "",
              as.character(format(round(object@player1@powerScoring[, 2], 2), nsmall = 0))
              )
      p2 <- c("",
              as.character(format(round(object@player2@means[, 2], 2), nsmall = 2)),
              "",
              as.character(round(object@player2@checkout[, 2], 2)),
              "",
              as.character(format(round(object@player2@powerScoring[, 2], 2), nsmall = 0))
              )

      # creazione matrice
      matr <- data.frame(p1, middle, p2)

      # set dimnames
      colnames(matr) <- c(object@player1@player, "", object@player2@player)

      # print out
      cat("Players: ", object@players[1], ", ", object@players[2], "\n", sep = "")
      cat("Winner:", object@winner)
      kable(matr, "simple", align = "ccc")
    }
  )






















