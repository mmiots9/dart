#' @name day_resume_501_tr
#' @title Daily resume of legs
#' @description This function is used to create a summary list of all the legs of the day, creating also a dash file
#' @usage day_resume_501_tr(pattern)
#' @param pattern Character string of the pattern to search in the variable names which ones to use
#' @returns A list containing:
#' \item{darts}{character vector of all the scores of the thrown darts}
#' \item{1st darts}{character vector of all the scores of the first thrown darts}
#' \item{2nd darts}{character vector of all the scores of the second thrown darts}
#' \item{3rd darts}{character vector of all the scores of the third thrown darts}
#' \item{number of darts}{number of thrown darts}
#' \item{checkouts}{checkouts score}
#' \item{180}{number of 180s}
#' \item{140+}{number of 140+}
#' \item{100+}{number of 100+}
#' \item{missed doubles}{number of missed doubles}
#' \item{busted}{number of busted}
#' \item{dataset count and \%}{dataset containing counts for each possible dart score}
#' \item{dataset checkouts}{dataset containing counts for checkouts}
#' \item{dataset mean and sd}{dataset containing mean and sd values for all 3 darts}
#' @author Matteo Miotto
#' @importFrom stringr str_subset
#' @importFrom dplyr arrange group_by summarise mutate full_join
#' @export

day_resume_501_tr <- function(pattern){

  # set useful vectors
    darts <- first_darts <- second_darts <-third_darts <- number_of_darts <- checkouts <- missed_doubles <- busted <- `100+` <- `140+` <- `180` <- NULL
    from_chr_to_score_vector <- c(0:20, (1:20)*2, (1:20)*3, 25, 50)
    from_chr_to_score_names  <- c(as.character(0:20), paste("d", c(1:20), sep = ""), paste("t", c(1:20), sep = ""), "25", "d25")
    names(from_chr_to_score_vector) <- from_chr_to_score_names

  # find variables with the pattern
    list_to_use <- str_subset(ls(envir = .GlobalEnv), pattern)

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
        `100+` <- c(`100+`, leg_to_use$`100+`)
        `140+` <- c(`140+`, leg_to_use$`140+`)
        `180` <- c(`180`, leg_to_use$`180`)
        number_of_darts <- c(number_of_darts, leg_to_use$`number of darts`)
        checkouts <- c(checkouts, leg_to_use$checkout)
        missed_doubles <- c(missed_doubles, leg_to_use$`missed doubles`)
        busted <- c(busted, leg_to_use$busted)
    }

  # create useful dataset
    levels <- c(as.character(0:20), paste("d", c(1:20), sep = ""), paste("t", c(1:20), sep = ""), "25", "d25")

    # dataset count and % all darts
      t1st <- as.data.frame(first_darts) %>%
        arrange(factor(x = first_darts, levels = levels, ordered = T)) %>%
        group_by(factor(x = first_darts, levels =  levels, ordered = T)) %>%
        summarise(`1st dart` = n()) %>%
        mutate(`1st dart` = paste(`1st dart`, " (", round(`1st dart`/sum(`1st dart`), 2), "%)", sep = ""))

      t2nd <- as.data.frame(second_darts) %>%
        arrange(factor(x = second_darts, levels = levels, ordered = T)) %>%
        group_by(factor(x = second_darts, levels =  levels, ordered = T)) %>%
        summarise(`2nd dart` = n()) %>%
        mutate(`2nd dart` = paste(`2nd dart`, " (", round(`2nd dart`/sum(`2nd dart`), 2), "%)", sep = ""))

      t3rd <- as.data.frame(third_darts) %>%
        arrange(factor(x = third_darts, levels = levels, ordered = T)) %>%
        group_by(factor(x = third_darts, levels =  levels, ordered = T)) %>%
        summarise(`3rd dart` = n()) %>%
        mutate(`3rd dart` = paste(`3rd dart`, " (", round(`3rd dart`/sum(`3rd dart`), 2), "%)", sep = ""))

      tall <- as.data.frame(darts) %>%
        arrange(factor(x = darts, levels = levels, ordered = T)) %>%
        group_by(factor(x = darts, levels =  levels, ordered = T)) %>%
        summarise(`All darts` = n()) %>%
        mutate(`All darts` = paste(`All darts`, " (", round(`All darts`/sum(`All darts`), 2), "%)", sep = ""))

      tlevels <- as.data.frame(levels) %>%
        arrange(factor(x = levels, levels = levels, ordered = T)) %>%
        group_by(factor(x = levels,levels =  levels, ordered = T))
      tlevels <- tlevels[1]

      colnames(t1st)[1] <- colnames(t2nd)[1] <- colnames(t3rd)[1] <- colnames(tall)[1] <- colnames(tlevels)[1] <- "score"

      df_count_perc <- full_join(t1st, t2nd) %>%
        full_join(t3rd) %>%
        full_join(tall) %>%
        full_join(tlevels) %>%
        mutate(score = factor(score, levels = levels)) %>%
        arrange(score)

    # checkouts dataset
      df_checkouts <- as.data.frame(checkouts) %>%
        arrange(checkouts) %>%
        group_by(checkouts) %>%
        summarise(`n(%)` = n()) %>%
        mutate(`n(%)` = paste(`n(%)`, " (", round(`n(%)`/sum(`n(%)`), 2), "%)", sep = ""))

    # dataset mean and sd
      first_darts_num <- as.numeric(from_chr_to_score_vector[c(first_darts)])
      second_darts_num <- as.numeric(from_chr_to_score_vector[c(second_darts)])
      third_darts_num <- as.numeric(from_chr_to_score_vector[c(third_darts)])
      darts_num <- as.numeric(from_chr_to_score_vector[c(darts)])

      first_darts_meands <- paste(round(mean(first_darts_num), 2), "±", round(sd(first_darts_num), 2))
      second_darts_meands <- paste(round(mean(second_darts_num), 2), "±", round(sd(second_darts_num), 2))
      third_darts_meands <- paste(round(mean(third_darts_num), 2), "±", round(sd(third_darts_num), 2))
      darts_meands <- paste(round(mean(darts_num), 2), "±", round(sd(darts_num), 2))

      df_mean_sd <- data.frame(first_darts_meands, second_darts_meands, third_darts_meands, darts_meands)
      colnames(df_mean_sd) <- colnames(df_count_perc)[-1]


  # return
    res <- list("darts" = darts,
                "1st darts" = first_darts,
                "2nd darts" = second_darts,
                "3rd darts" = third_darts,
                "number of darts" = number_of_darts,
                "checkouts" = checkouts,
                "180" = `180`,
                "140+" = `140+`,
                "100+" = `100+`,
                "missed doubles" = missed_doubles,
                "busted" = busted,
                "dataset count and %" = df_count_perc,
                "dataset checkouts" = df_checkouts,
                "dataset mean and sd" = df_mean_sd
    )

    return(res)

}

