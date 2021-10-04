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
      start = "numeric",
      win = "numeric",
      dartsScoresCh = "character"
    )
  )

# Methods
  # getPlayers
  #' @export
  setMethod(
    "getPlayers",
    "leg1p",
    function(object){
      object@player
    }
  )

  # getID
  #' @export
  setMethod(
    "getID",
    "leg1p",
    function(object){
      object@id
    }
  )

  # getWin
  #' @export
  setMethod(
    "getWin",
    "leg1p",
    function(object){
      object@win
    }
  )

  # getDate
  #' @export
  setMethod(
    "getDate",
    "leg1p",
    function(object){
      id <- getID(object)
      datetime <- unlist(
        strsplit(
        unlist(strsplit(id, "s"))[1],
        "m"))[2]
      date <- paste(substr(datetime, 1, 4),
                    substr(datetime, 5, 6),
                    substr(datetime, 7, 8),
                    sep = "-")
    }
  )

  # getStats
  #' @export
  setMethod(
    "getStats",
    "leg1p",
    function(object){

      # default values
        scoreOv <- 501
        nMiss <- nBust <- nHit <- plus100 <- plus140 <- plus180 <- plus60 <-
          n20 <- n19 <- n18 <- n60 <- n57 <- n54 <- closingDouble <- 0
        missedDoubles <- dartsScoresVal <- NULL
        doubles <- c((1:20)*2, 50)

      # prendo 3 alla volta
        for (i in seq(1, length(object@dartsScoresCh)-3+1, 3)) {
          handScoresCh  <- object@dartsScoresCh[i:(i+2)]

          # converto
            handScoresVal <- chr2val(handScoresCh)

          # valuto se in chiusura e missed doubles
            scorePar <- scoreOv

            for (j in seq_along(handScoresVal)){
              if(scorePar %in% doubles & (scorePar - handScoresVal[j]) !=0 ) {
                nMiss <- nMiss + 1
                missedDoubles <- c(missedDoubles, paste("d", scorePar/2, sep = ""))
              }
              scorePar <- scorePar -  handScoresVal[j]
            }

          # valuto se bust
            if (scoreOv -  sum(handScoresVal) != 1 & (scoreOv -  sum(handScoresVal)) >= 0){

              if (sum(handScoresVal) == 180){
                plus180 <- plus180 + 1
              } else if (sum(handScoresVal) >= 140){
                plus140 <- plus140 + 1
              } else if (sum(handScoresVal) >= 100){
                plus100 <- plus100 + 1
              } else if (sum(handScoresVal) >= 60){
                plus60 <- plus60 + 1
              }
            } else {
              nBust <- nBust + 1
              handScoresVal <- c(0, 0, 0)
            }

          # sottraggo e aggiungo dartscoresval
            scoreOv <- scoreOv - sum(handScoresVal)
            dartsScoresVal <- c(dartsScoresVal, handScoresVal)

            }


      # tengo i valori ch buoni
        dartsScoresCh <- object@dartsScoresCh
        dartsScoresCh[which(dartsScoresVal == 0)] <- 0

      # calcolo power scoring
        n18 <- sum(dartsScoresCh == "18")
        n19 <- sum(dartsScoresCh == "19")
        n20 <- sum(dartsScoresCh == "20")
        n60 <- sum(dartsScoresCh == "t20")
        n57 <- sum(dartsScoresCh == "t19")
        n54 <- sum(dartsScoresCh == "t18")

      # tolgo eventuali 0 dopo chiusura e aggiungo valori chiusura
        if (sum(dartsScoresVal) == 501) {
          for (i in 1:2) {
            if (dartsScoresVal[length(dartsScoresVal)] == 0) {
              dartsScoresVal <- dartsScoresVal[-c(length(dartsScoresVal))]
            }

          nHit <- 1
          closingDouble <- paste0("d", dartsScoresVal[length(dartsScoresVal)]/2)
          }
        }

      # creo df
        # mean
        handDartsNum <- rep(c(1, 2, 3), ceiling(length(dartsScoresVal)/3))[1:length(dartsScoresVal)]

        darts.df <- data.frame("dart.hand" = handDartsNum,
                               "score" = dartsScoresVal)

        means <- darts.df %>%
          group_by(dart.hand) %>%
          summarize(mean = round(mean(score), 2))

        mean.1 <- means$mean[means$dart.hand == 1]
        mean.2 <- means$mean[means$dart.hand == 2]
        mean.3 <- means$mean[means$dart.hand == 3]
        meanOv <- round(sum(dartsScoresVal) / length(dartsScoresVal) * 3, 2)
        mean9  <- round(sum(dartsScoresVal[1:9]) / 3, 2)

        meanDf <- data.frame(
          mean  = c("3 darts", "Firts 9", "1st dart", "2nd dart", "3rd dart"),
          score = c(meanOv, mean9, mean.1, mean.2, mean.3)
        )

        # checkout
        if (nHit == 0) {rate <- 0} else if (nMiss + nBust == 0) {rate <- 1} else {rate <- round(nHit/(nMiss+1), 2)}
        checkoutDf <- data.frame(
          what  = c("Missed", "Busted", "Hit", "Rate"),
          value = c(nMiss, nBust, nHit, rate)
        )

        # doubles
        if (!is.null(missedDoubles)){

          doublesDf <- data.frame(double = missedDoubles) %>%
            group_by(double) %>%
            summarise(miss = n()) %>%
            mutate(hit = 0)

          if (closingDouble != 0) {
            if (closingDouble %in% missedDoubles) {
              doublesDf$hit[which(doublesDf$double == closingDouble)] <- doublesDf$hit[which(doublesDf$double == closingDouble)] + 1
            } else {
              tmpdf <- data.frame(double = closingDouble, miss = 0, hit = 1)
              doublesDf <- rbind(doublesDf, tmpdf)
            }

          }
        } else {
          if (closingDouble != 0) {
            doublesDf <- data.frame(double = closingDouble, miss = 0, hit = 1)
          } else {
            doublesDf <- data.frame(double = character(), miss = numeric(), hit = numeric())
          }
        }
        doublesDf$double <- factor(doublesDf$double, levels = c(paste("d", c(1:20), sep = "")), ordered = T)
        doublesDf <- doublesDf %>%
          arrange(double)

        attr(doublesDf, "class") <- c("data.frame")

        # power scoring
        powerDf <- data.frame(
          what = c("18", "19", "20", "54", "57", "60", "60+", "100+", "140+", "180 "),
          n    = c(n18, n19, n20, n54, n57, n60, plus60, plus100, plus140, plus180)
        )

        # final list

        stats = list(
          mean     = meanDf,
          checkout = checkoutDf,
          doubles  = doublesDf,
          power    = powerDf,
          dartsNum = length(dartsScoresVal)
        )
    }
  )

  # show
  #' @export
  setMethod(
    "show",
    "leg1p",
    function(object){
      cat("Date:", getDate(object), "\n")
      cat("Player:", getPlayers(object), "\n", "\n")
      if (object@win == 1) {
        cat("Leg closed in", getStats(object)$dartsNum, "darts \n")
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
      checkoutCh <- c("Missed", "Busted", "Hit   " , "Rate  ")
      powerCh <- c("54  ", "57  ", "60  ", "60+ " , "100+", "140+", "180 ")

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
        cat("Winner: yes", "\n", "\n")
      } else {
        cat("Winner: no", "\n", "\n")
      }
      matr
    }
  )

# ------------------------------------------------- leg2p

# leg2p
# class
  #' @export leg2p
  #' @exportClass leg2p
  leg2p <- setClass(
    "leg2p",
    slots = list(
      p1leg = "leg1p",
      p2leg = "leg1p"
    )
  )

# Methods

  # getPlayers
  #' @export
  setMethod(
    "getPlayers",
    "leg2p",
    function(object){
      players <- c(getPlayers(object@p1leg), getPlayers(object@p2leg))
    }
  )

  # getDate
  #' @export
  setMethod(
    "getDate",
    "leg2p",
    function(object){
      getDate(object@p1leg)
    }
  )

  # getID
  #' @export
  setMethod(
    "getID",
    "leg2p",
    function(object){
      getID(object@p1leg)
    }
  )

  # getWinner
  #' @export
  setMethod(
    "getWinner",
    "leg2p",
    function(object){
      if (getWin(object@p1leg) == 1) {winner <- getPlayers(object@p1leg)} else {winner <- getPlayers(object@p2leg)}
    }
  )

  # show
  #' @export
  setMethod(
    "show",
    "leg2p",
    function(object){

      if (getWin(object@p1leg) == 1) {ndarts <- getStats(object@p1leg)$dartsNum} else {ndarts <- getStats(object@p1leg)$dartsNum}

      cat("Date:", getDate(object), "\n")
      cat("Players:", paste(getPlayers(object), collapse = ", "), "\n", "\n")
      cat("Leg won by", getWinner(object), "in", ndarts, "darts")

    }
  )

  # summary
  #' @export
  setMethod(
    "summary",
    "leg2p",
    function(object){
      p1stats <- getStats(object@p1leg)
      p2stats <- getStats(object@p2leg)

      # creo vettore centrale
      center <- c("MEAN", p1stats$mean[,1], "CHECKOUT", p1stats$checkout[,1], "POWER SCORING", p1stats$power[,1])
      p1v    <- c("", as.character(p1stats$mean[,2]),
                  "", as.character(p1stats$checkout[,2]),
                  "", as.character(p1stats$power[,2]))
      p2v    <- c("", as.character(p2stats$mean[,2]),
                  "", as.character(p2stats$checkout[,2]),
                  "", as.character(p2stats$power[,2]))

      df <- data.frame(p1v, center, p2v)
      colnames(df) <- c(getPlayers(object@p1leg), "", getPlayers(object@p2leg))
      name.width <- max(sapply(names(df), nchar))
      names(df) <- format(names(df), width = name.width, justify = "centre")
      a <- format(df, justify = "centre")
      print(a, row.names = F)



    }
  )





















