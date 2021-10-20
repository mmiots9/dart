# getPlayers
  #' @name getPlayers
  #' @title Get player/s name/s
  #' @usage getPlayers(input)
  #' @param input A leg1p, leg2p, set1p, set2p, match1p or match2p class object
  #' @returns Character vector containing player/s name/s
  #' @author Matteo Miotto
  #' @export
  setGeneric(
    "getPlayers",
    valueClass = "character",
    def = function(object){
      standardGeneric("getPlayers")
    }
  )


# getID
  #' @name getID
  #' @title Get leg/set/match ID
  #' @usage getID(input)
  #' @param input A leg1p, leg2p, set1p, set2p, match1p or match2p class object
  #' @returns Character vector containing leg, set or match ID
  #' @export
  setGeneric(
    "getID",
    valueClass = "character",
    def = function(object){
      standardGeneric("getID")
    }
  )


# getWin
  #' @name getWin
  #' @title Win or lost
  #' @usage getWin(input)
  #' @param input A leg1p, leg2p, set1p, set2p, match1p or match2p class object
  #' @returns 0 if lost, 1 if won
  #' @export
  setGeneric(
    "getWin",
    valueClass = "numeric",
    def = function(object){
      standardGeneric("getWin")
    }
  )


# getStats
  #' @name getStats
  #' @title Get leg/set/match stats
  #' @usage getStats(input)
  #' @param input A leg1p, set1p or match1p class object
  #' @returns A list containing leg, set or match stats (means, checkouts, doubles, power scoring, number of darts and number of legs/sets won and lost)
  #' @export
  setGeneric(
    "getStats",
    valueClass = "list",
    def = function(object){
      standardGeneric("getStats")
    }
  )


# getDate
  #' @name getDate
  #' @title Get leg/set/match date of play
  #' @usage getDate(input)
  #' @param input A leg1p, leg2p, set1p, set2p, match1p or match2p class object
  #' @returns Character vector containing the date of play (YYYY-MM-DD)
  #' @export
  setGeneric(
    "getDate",
    valueClass = "character",
    def = function(object){
      standardGeneric("getDate")
    }
  )

# getWinner
  #' @name getWinner
  #' @title Get leg/set/match winner
  #' @usage getWinner(input)
  #' @param input A leg2p, set2p or match2p class object
  #' @returns Character vector containing the name of the winner
  #' @export
  setGeneric(
    "getWinner",
    valueClass = "character",
    def = function(object){
      standardGeneric("getWinner")
    }
  )




