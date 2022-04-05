#' @name loadTraining
#' @title Load 501 training legs
#' @description Load a 501 training file with different legs in it
#' @usage loadTraining(file)
#' @param file file path. Could be ommitted and file selection would be prompted
#' @author Matteo Miotto
#' @export

loadTraining501<- function(file = NULL){

  # check filename
  if (is.null(file)) {
    cat("Select 501 training result file", "\n")
    Sys.sleep(0.5)
    file <- file.choose()
    }

  # load file
  trFile <- read.table(file, col.names = F)

  # useful values
  legs <- list()
  players <- character()
  all <- list()


  # loop through all legs (2 lines each)
  for (i in seq(1, dim(trFile)[1], 2)) {

    splitted1 <- unlist(strsplit(trFile[i,], split = ",")) # split first line
    id        <- splitted1[1] # get leg ID
    player    <- splitted1[2] # get player name

    dartsScoresCh <- unlist(strsplit(trFile[i+1,], split = ",")) # split second line

    # create a class Legtr object
    l <- legtr(id = id, player = player, dartsScoresCh = dartsScoresCh)

    # collect useful data to later sum up
    legs    <- c(legs, l)
    players <- c(players, getPlayers(l))
  }

  # loop through players
  for (p in seq_along(unique(players))) {

    plegs <- legs[which(players == unique(players)[p])]  # get player legs
    all[[p]] <- plegs
    names(all)[p] <- unique(players)[p]

  }

  # return
  return(all)

}
