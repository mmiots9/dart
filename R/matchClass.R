# class
  #' @export match1p
  #' @exportClass match1p
  match1p <- setClass(
    "match1p",
    slots = list(
      sets = "list",
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
    "match1p",
    function(object){
      cat("Date:", object@id[3], "\n")
      cat("Player:", object@player, "\n")
      if (object@win == 1) {
        cat("Winner: yes", "\n", "\n")
      } else {
        cat("Winner: no", "\n", "\n")
      }
      cat("Match closed with an avarage of", object@means[which(rownames(s1@means) == "3darts"), 2], " \n")
    }
  )


