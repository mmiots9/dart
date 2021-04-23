library(tidyverse)
typeof(str_subset(ls(), "leg"))[1]
which(is.list(ls()))
eval(parse(text = paste("which(is.list(", ls(),"))")))

a <- eval(parse(text = str_subset(ls(), "leg"))[1])

day_resume_501_tr <- function(pattern){

  # set useful vectors
    darts <- first_darts <- second_darts <-third_darts <- number_of_darts <- checkouts <- missed_doubles <- busted <- NULL

  # find variables with the pattern
    list_to_use <- str_subset(ls(), pattern)

  # number of legs
    n_of_legs <- length(list_to_use)

  # loop to retrieve all infos
    for (i in seq_along(list_to_use)){

      # set useful variable
        leg_to_use <- eval(parse(text = list_to_use[i]))

      # add values to vectors
        darts <- c(darts, leg_to_use$darts)
        first_darts <- c(first_darts, leg_to_use$`1st dart`)
        second_darts <- c(second_darts, leg_to_use$`2nd dart`)
        third_darts <- c(third_darts, leg_to_use$`3rd dart`)
        number_of_darts <- c(number_of_darts, leg_to_use$`number of darts`)
        checkouts <- c(checkouts, leg_to_use$checkout)
        missed_doubles <- c(missed_doubles, leg_to_use$`missed doubles`)
        busted <- c(busted, leg_to_use$busted)
    }

  # return
    res <- list("darts" = darts,
                "1st dart" = first_darts,
                "2nd dart" = second_darts,
                "3rd dart" = third_darts,
                "number of darts" = number_of_darts,
                "checkout" = checkouts,
                "missed doubles" = missed_doubles,
                "busted" = busted
    )

}

