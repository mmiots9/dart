# getPlayers
  #' @export
  setGeneric(
    "getPlayers",
    valueClass = "character",
    def = function(object){
      standardGeneric("getPlayers")
    }
  )

  # methods
    #' @export
    setMethod(
      "getPlayers",
      "leg1p",
      function(object){
        object@player
      }
    )

  #' @export
  setMethod(
    "getPlayers",
    "set1p",
    function(object){
      getPlayers(object@legs[[1]])
    }
  )

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
  setGeneric(
    "getID",
    valueClass = "character",
    def = function(object){
      standardGeneric("getID")
    }
  )

  # Methods
    #' @export
      setMethod(
        "getID",
        "leg1p",
        function(object){
          object@id
        }
      )

    #' @export
      setMethod(
        "getID",
        "set1p",
        function(object){
          id <- getID(object@legs[[1]])
          strsplit(id, "l")[[1]][1]
        }
      )

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
  setGeneric(
    "getWin",
    valueClass = "numeric",
    def = function(object){
      standardGeneric("getWin")
    }
  )

  # Methods
    #' @export
      setMethod(
        "getWin",
        "leg1p",
        function(object){
          object@win
        }
      )

    #' @export
      setMethod(
        "getWin",
        "set1p",
        function(object){
          wins <- 0
          for (i in 1:length(object@legs)) {
            wins <- wins + getWin(object@legs[[i]])
          }

          if (wins > (length(object@legs)/2)) {return(1)} else {return(0)}

        }
      )

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






