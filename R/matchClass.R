# class
#' @export match1p
#' @exportClass match1p
match1p <- setClass(
  "match1p",
  slots = list(
    win = "numeric",
    sets = "list"
  )
)

# Methods
# getPlayers
#' @export
setMethod(
  "getPlayers",
  "match1p",
  function(object){
    getPlayers(object@sets[[1]])
  }
)


# getID
#' @export
setMethod(
  "getID",
  "match1p",
  function(object){
    id <- getID(object@sets[[1]])
    strsplit(id, "s")[[1]][1]
  }
)

# getWin
#' @export
setMethod(
  "getWin",
  "match1p",
  function(object){
    wins <- 0
    for (i in 1:length(object@sets)) {
      wins <- wins + getWin(object@sets[[i]])
    }

    if (wins > (length(object@sets)/2)) {return(1)} else {return(0)}

  }
)


# getDate
#' @export
setMethod(
  "getDate",
  "match1p",
  function(object){
    getDate(object@sets[[1]])
  }
)


# getStats
#' @export
setMethod(
  "getStats",
  "match1p",
  function(object){

    # utilities
    spowerDfi <- data.frame(a = rep(0, 10))
    scheckoutDfi <- data.frame(a = rep(0, 3))
    doublesDfi <- data.frame(double = factor(), miss = numeric(), hit = numeric())
    ndartst <- mean3t <- 0
    n1dt <- n2dt <- n3dt <- mean1t <- mean2t <- mean3rdt <- 0
    mf9 <- NULL
    lost <- won <- 0

    # ciclo per ognuna
    for (i in 1:length(object@sets)) {

      # salvo stats
      stats <- getStats(object@sets[[i]])

      # aggiungo vinti o persi
      if (object@sets[[i]]@win == 1){won <- won + 1} else {lost <- lost + 1}

      # power
      # prendo la seconda colonna, unisco a quelle altre e poi rowSums
      spowerDfi <- cbind(spowerDfi, stats$power[,2])

      # checkout
      scheckoutDfi <- cbind(scheckoutDfi, stats$checkout[1:3, 2])

      # means
      # 3 darts
      mean3t  <- mean3t + stats$mean[1, 2] * stats$dartsNum
      ndartst <- ndartst + stats$dartsNum

      # first 9
      mf9 <- c(mf9, stats$mean[2, 2])

      # 1st, 2nd and 3rd
      # calcolo quantità frecce
      n3d <- floor(stats$dartsNum / 3)
      n1d <- ceiling(stats$dartsNum / 3)
      n2d <- stats$dartsNum - n3d - n1d

      n1dt <- n1dt + n1d
      n2dt <- n2dt + n2d
      n3dt <- n3dt + n3d

      mean1t   <- mean1t + stats$mean[3, 2] * n1d
      mean2t   <- mean2t + stats$mean[4, 2] * n2d
      mean3rdt <- mean3rdt + stats$mean[5, 2] * n3d

      # doubles
      doublesDfi <- rbind(doublesDfi, stats$doubles) %>%
        group_by(double) %>%
        summarise(miss = sum(miss), hit = sum(hit))
    }

    # ricreo dataset power
    spowerDf <- data.frame(what = c("18", "19", "20", "54", "57", "60", "60+", "100+", "140+", "180 "),
                           n = rowSums(spowerDfi))

    # ricreo dataset checkout
    scheckouDf <- data.frame(what = c("Missed", "Busted", "Hit", "Rate"),
                             value = c(rowSums(scheckoutDfi), 0))

    nHit  <- scheckouDf[3, 2]
    nMiss <- scheckouDf[1, 2]
    nBust <- scheckouDf[2, 2]

    if (nHit == 0) {scheckouDf[4, 2] <- 0} else if (nMiss + nBust == 0) {scheckouDf[4, 2] <- 1} else {scheckouDf[4, 2] <- round(nHit/(nMiss + nHit), 2)}

    # ricreo dataset means
    meanOv <- round(round(mean3t) / ndartst, 2)
    mean9  <- round(mean(mf9), 2)
    mean.1 <- round(mean1t / n1dt, 2)
    mean.2 <- round(mean2t / n2dt, 2)
    mean.3 <- round(mean3rdt / n3dt, 2)

    meanDf <- data.frame(
      mean  = c("3 darts", "Firts 9", "1st dart", "2nd dart", "3rd dart"),
      score = c(meanOv, mean9, mean.1, mean.2, mean.3)
    )

    # ricreo dataset double
    doublesDfi$double <- factor(doublesDfi$double, levels = c(paste("d", c(1:20), sep = "")), ordered = T)

    doublesDfi <- doublesDfi %>%
      arrange(double)

    attr(doublesDfi, "class") <- c("data.frame")

    # dataset sets
    setsDf <- data.frame(won, lost)

    # final list

    stats = list(
      mean     = meanDf,
      checkout = scheckouDf,
      doubles  = doublesDfi,
      power    = spowerDf,
      dartsNum = ndartst,
      sets     = setsDf
    )


  }
)

# Methods
# show
#' @export
setMethod(
  "show",
  "match1p",
  function(object){
    stats <- getStats(object)

    cat("Date:", getDate(object), "\n")
    cat("Player:", getPlayers(object), "\n")
    if (getWin(object) == 1) {
      win <- "won"
    } else {
      win <- "lost"
    }
    cat("Match ", win, " ", stats$sets$won, "-", stats$sets$lost," with an avarage of ",
        getStats(object)$mean[which(getStats(object)$mean[,1] == "3 darts"), 2], " \n", sep = "")
  }
)

# summary
setMethod(
  "summary",
  "match1p",
  function(object){

    # creazione vettori chr
    meansCh <- c("3 darts ", "First 9 ", "1st dart", "2nd dart", "3rd dart")
    checkoutCh <- c("Missed", "Busted", "Rate  ")
    powerCh <- c("54  ", "57  ", "60  ", "60+ ", "100+", "140+", "180 ")

    # creazione vettori valori
    stats <- getStats(object)
    meansVa <- stats$mean$score
    meansVa <- as.character(format(round(meansVa, 2), nsmall = 2))
    checkoutVa <- stats$checkout$value
    checkoutVa <- as.character(format(round(checkoutVa, 2), nsmall = 2))
    powerVa <- stats$power$n[4:10]
    powerVa <- as.character(format(round(powerVa, 2), nsmall = 0))

    # unione vettori
    meansTot <- c(paste(paste(meansCh, meansVa, sep = ": "), ""), NA, NA)
    checkoutTot <- c(paste(paste(checkoutCh, checkoutVa, sep = ": "),""), NA, NA, NA)
    powerTot <- paste(powerCh, powerVa, sep = ": ")

    # creazione matrice
    matr <- matrix(c(meansTot, checkoutTot, powerTot), ncol = 3, byrow = F)

    # conversione in table
    attr(matr, "class") <- c("table")

    # set dimnames
    attr(matr, "dimnames") <- list(rep("", 7), c("     Means     ", "  Checkout  ", "Power Scoring"))

    # print out
    cat("Player:", getPlayers(object), "\n")
    if (object@win == 1) {
      cat("Winner: yes", "\n")
    } else {
      cat("Winner: no", "\n")
    }
    cat("sets: ", stats$sets$won, "-", stats$sets$lost, "\n", "\n", sep = "")

    matr
  }
)

# ------------------------------------------------- match2p
# match2p
# class
  #' @export match2p
  #' @exportClass match2p
  match2p <- setClass(
    "match2p",
    slots = list(
      p1match = "match1p",
      p2match = "match1p",
      set2win = "numeric"
    )
  )

# Methods
  # getPlayers
  #' @export
  setMethod(
    "getPlayers",
    "match2p",
    function(object){
      players <- c(getPlayers(object@p1match), getPlayers(object@p2match))
    }
  )

  # getDate
  #' @export
  setMethod(
    "getDate",
    "match2p",
    function(object){
      getDate(object@p1match)
    }
  )

  # getID
  #' @export
  setMethod(
    "getID",
    "match2p",
    function(object){
      getID(object@p1match)
    }
  )

  # getWinner
  #' @export
  setMethod(
    "getWinner",
    "match2p",
    function(object){
      if (getWin(object@p1match) == 1) {winner <- getPlayers(object@p1match)} else {winner <- getPlayers(object@p2match)}
    }
  )

  # show
  #' @export
  setMethod(
    "show",
    "match2p",
    function(object){

      if (getWin(object@p1match) == 1) {
        winnersets <- 0
        losersets <- 0

        for (i in 1:length(object@p1match@sets)) {
          winnersets <- winnersets + getWin(object@p1match@sets[[i]])
          losersets <- losersets + getWin(object@p2match@sets[[i]])
        }

      } else {
        winnersets <- 0
        losersets <- 0

        for (i in 1:length(object@p1match@sets)) {
          winnersets <- winnersets + getWin(object@p2match@sets[[i]])
          losersets <- losersets + getWin(object@p1match@sets[[i]])
        }
      }

      cat("Date:", getDate(object), "\n")
      cat("Players:", paste(getPlayers(object), collapse = ", "), "\n", "\n")
      cat("Match won by ", getWinner(object), " ", winnersets, "-", losersets, sep = "")

    }
  )

  # summary
  #' @export
  setMethod(
    "summary",
    "match2p",
    function(object){
      p1stats <- getStats(object@p1match)
      p2stats <- getStats(object@p2match)

      # creo vettore centrale
      center <- c("Sets", "MEAN", p1stats$mean[,1], "CHECKOUT", p1stats$checkout[,1], "POWER SCORING", p1stats$power[,1])
      p1v    <- c(p1stats$sets$won, "", as.character(p1stats$mean[,2]),
                  "", as.character(p1stats$checkout[,2]),
                  "", as.character(p1stats$power[,2]))
      p2v    <- c(p2stats$sets$won, "", as.character(p2stats$mean[,2]),
                  "", as.character(p2stats$checkout[,2]),
                  "", as.character(p2stats$power[,2]))

      df <- data.frame(p1v, center, p2v)
      colnames(df) <- c(getPlayers(object@p1match), "", getPlayers(object@p2match))
      name.width <- max(sapply(names(df), nchar))
      names(df) <- format(names(df), width = name.width, justify = "centre")
      a <- format(df, justify = "centre")
      print(a, row.names = F)



    }
  )
