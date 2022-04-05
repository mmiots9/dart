#' @name loadMatch
#' @title Load Match
#' @description Load a match file
#' @usage loadMatch(file)
#' @param file file path. Could be ommitted and file selection would be promped
#' @author Matteo Miotto
#' @export

loadMatch <- function(file = NULL){

  # check filename
  if (is.null(file)) {
    cat("Select match file", "\n")
    Sys.sleep(0.5)
    file <- file.choose()
    }

  # load file
  matchFile <- read.table(file, col.names = F)

  # useful values
  legs <- list()
  players <- character()
  sets <- numeric()
  matchlist <- list()


  # loop through all legs (2 lines each)
  for (i in seq(1, dim(matchFile)[1], 2)) {

    splitted1 <- unlist(strsplit(matchFile[i,], split = ",")) # split first line
    id        <- splitted1[1]
    player    <- splitted1[2]
    start     <- as.numeric(splitted1[3])
    win       <- as.numeric(splitted1[4])

    dartsScoresCh <- unlist(strsplit(matchFile[i+1,], split = ",")) # split second line

    # create a class Leg object
    l <- leg1p(id = id, player = player, start = start, win = win, dartsScoresCh = dartsScoresCh)

    # collect useful data to later sum up
    legs    <- c(legs, l)
    players <- c(players, getPlayers(l))
    sets    <- c(sets,
              unlist(strsplit(unlist(strsplit(id, split = "s"))[2], split = "l"))[1]
              )
  }

  # loop through players
  for (p in seq_along(unique(players))) {

    plegs <- legs[which(players == unique(players)[p])]  # get player legs
    psets <- sets[which(players == unique(players)[p])]  # get player sets numbers

    setsc <- list()

    # loop through sets
    for (s in seq_along(unique(psets))) {
      pslegs <- plegs[which(psets == unique(psets)[s])] # get legs of this set
      wins   <- sum(unlist(lapply(pslegs, getWin))) / length(pslegs) # % of winning legs
      win    <- ifelse(wins < 0.5, 0, 1) # win or lose the set

      sp <- set1p(win = win, legs = pslegs)

      setsc <- c(setsc, sp)
    }

    # create match for player
    wins <- sum(unlist(lapply(setsc, getWin))) / length(setsc) # % of winning sets
    win  <- ifelse(wins < 0.5, 0, 1) # win or lose the match
    set2win <- ifelse(wins < 0.5, length(setsc) - sum(unlist(lapply(setsc, getWin))),
                      sum(unlist(lapply(setsc, getWin))))

    match <- match1p(win = win, sets = setsc)
    matchlist <- c(matchlist, match)

  }

  # create 2p match
  set2win <- set2win
  match2 <- match2p(p1match = matchlist[[1]], p2match = matchlist[[2]], set2win = set2win)

  # return
  return(match2)

}
