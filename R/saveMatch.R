#' @name saveMatch
#' @title Save Match
#' @description Save the match as a file txt or csv
#' @usage saveMatch(data, file)
#' @param data match variable
#' @param file filename
#' @author Matteo Miotto
#' @export

saveMatch <- function(data, file){

  # check class
  if (class(data) != "match2p") {stop("Data class not valid! Only match2p is valid.")}
  if (!is.character(file)) {stop("filename must be a character vector")}
  datatxt <- data.frame(character())

  # player 1
    # loop thorugh sets
      for (s in seq_along(data@p1match@sets)) {

        # loop through legs
          for (l in seq_along(data@p1match@sets[[s]]@legs)) {

            firstLine  <- paste(getID(data@p1match@sets[[s]]@legs[[l]]),
                                getPlayers(data@p1match@sets[[s]]@legs[[l]]),
                                data@p1match@sets[[s]]@legs[[l]]@start,
                                data@p1match@sets[[s]]@legs[[l]]@win,
                                sep = ",")
            secondLine <- paste(data@p1match@sets[[s]]@legs[[l]]@dartsScoresCh, collapse = ",")
            thirdLine  <- ""

            datatxt <- rbind(datatxt, firstLine, secondLine, thirdLine)


          }

      }

  # player 2
    # loop thorugh sets
    for (s in seq_along(data@p2match@sets)) {

      # loop through legs
      for (l in seq_along(data@p2match@sets[[s]]@legs)) {

        firstLine  <- paste(getID(data@p2match@sets[[s]]@legs[[l]]),
                            getPlayers(data@p2match@sets[[s]]@legs[[l]]),
                            data@p2match@sets[[s]]@legs[[l]]@start,
                            data@p2match@sets[[s]]@legs[[l]]@win,
                            sep = ",")
        secondLine <- paste(data@p2match@sets[[s]]@legs[[l]]@dartsScoresCh, collapse = ",")
        thirdLine  <- ""

        datatxt <- rbind(datatxt, firstLine, secondLine, thirdLine)


      }

    }

  # save
  write.table(datatxt, file = file, quote = F, row.names = F, col.names = F)


}
