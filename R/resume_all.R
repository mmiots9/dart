resume_all <- function(){

  # set useful vectors
  n_of_legs <- darts <- first_darts <- second_darts <-third_darts <- number_of_darts <- checkouts <- closing_doubles <- missed_doubles <- missed <- busted <- `100+` <- `140+` <- `180` <- NULL
  from_chr_to_score_vector <- c(0, 1:20, 1:20, (1:20)*2, (1:20)*3, 25, 50)
  from_chr_to_score_names  <- c("0", paste("o",(1:20), sep = ""), paste("i",(1:20), sep = ""), paste("d", c(1:20), sep = ""), paste("t", c(1:20), sep = ""), "25", "d25")
  names(from_chr_to_score_vector) <- from_chr_to_score_names


  # find variables with the pattern
  list_to_use <- list.files(pattern = "summa")

  # loop to retrieve all infos
  for (i in seq_along(list_to_use)){

    # load file
    load(list_to_use[i])

    # add values to vectors
    n_of_legs <- c(n_of_legs, res$number_of_legs)
    darts <- c(darts, res$darts)
    first_darts <- c(first_darts, res$`1st_darts`)
    second_darts <- c(second_darts, res$`2nd_darts`)
    third_darts <- c(third_darts, res$`3rd_darts`)
    `100+` <- c(`100+`, res$n100plus)
    `140+` <- c(`140+`, res$n140plus)
    `180` <- c(`180`, res$n180)
    number_of_darts <- c(number_of_darts, res$number_of_darts)
    checkouts <- c(checkouts, res$checkouts)
    closing_doubles <- c(closing_doubles, res$closing_doubles)
    missed_doubles <- c(missed_doubles, res$missed_doubles)
    missed <- c(missed, res$missed)
    busted <- c(busted, res$busted)
  }

  # create useful dataset
  levels <- c("0", as.character(1:20) ,paste("d", c(1:20), sep = ""), paste("t", c(1:20), sep = ""), "25", "d25")

  # dataset count and % all darts
  t1st <- first_darts %>%
    str_remove("[io]") %>%
    as.data.frame() %>%
    arrange(factor(x = ., levels = levels, ordered = T)) %>%
    group_by(factor(x = ., levels =  levels, ordered = T)) %>%
    summarise(`1st dart` = n()) %>%
    mutate(`1st dart` = paste(`1st dart`, " (", round(`1st dart`/sum(`1st dart`), 2), "%)", sep = ""))


  t2nd <- second_darts %>%
    str_remove("[io]") %>%
    as.data.frame() %>%
    arrange(factor(x = ., levels = levels, ordered = T)) %>%
    group_by(factor(x = ., levels =  levels, ordered = T)) %>%
    summarise(`2nd dart` = n()) %>%
    mutate(`2nd dart` = paste(`2nd dart`, " (", round(`2nd dart`/sum(`2nd dart`), 2), "%)", sep = ""))


  t3rd <- third_darts %>%
    str_remove("[io]") %>%
    as.data.frame() %>%
    arrange(factor(x = ., levels = levels, ordered = T)) %>%
    group_by(factor(x = ., levels =  levels, ordered = T)) %>%
    summarise(`3rd dart` = n()) %>%
    mutate(`3rd dart` = paste(`3rd dart`, " (", round(`3rd dart`/sum(`3rd dart`), 2), "%)", sep = ""))


  tall <- darts %>%
    str_remove("[io]") %>%
    as.data.frame() %>%
    arrange(factor(x = ., levels = levels, ordered = T)) %>%
    group_by(factor(x = ., levels =  levels, ordered = T)) %>%
    summarise(`all darts` = n()) %>%
    mutate(`all darts` = paste(`all darts`, " (", round(`all darts`/sum(`all darts`), 2), "%)", sep = ""))

  # tlevels <- as.data.frame(levels) %>%
  #   arrange(factor(x = levels, levels = levels, ordered = T)) %>%
  #   group_by(factor(x = levels,levels =  levels, ordered = T))
  # tlevels <- tlevels[1]

  colnames(t1st)[1] <- colnames(t2nd)[1] <- colnames(t3rd)[1] <- colnames(tall)[1] <- "score"
  # colnames(tlevels)[1]

  suppressMessages(df_count_perc <- full_join(t1st, t2nd) %>%
                     full_join(t3rd) %>%
                     full_join(tall) %>%
                     # full_join(tlevels) %>%
                     mutate(score = factor(score, levels = levels)) %>%
                     arrange(score))

  df_count_perc$`1st dart`[is.na(df_count_perc$`1st dart`)] <- df_count_perc$`2nd dart`[is.na(df_count_perc$`2nd dart`)] <- df_count_perc$`3rd dart`[is.na(df_count_perc$`3rd dart`)] <-"0 (0%)"

  # checkouts dataset
  df_checkouts <- as.data.frame(checkouts) %>%
    arrange(checkouts) %>%
    group_by(checkouts) %>%
    summarise(`n(%)` = n()) %>%
    mutate(`n(%)` = paste(`n(%)`, " (", round(`n(%)`/sum(`n(%)`), 2), "%)", sep = ""))

  # dataset missed doubles
  tclosing <- as.data.frame(closing_doubles) %>%
    arrange(factor(x = closing_doubles, levels = levels, ordered = T)) %>%
    group_by(factor(x = closing_doubles, levels =  levels, ordered = T)) %>%
    summarise(`hit` = n())

  colnames(tclosing)[1] <- "score"

  if(!is_empty(missed)){
    tmissed <- as.data.frame(missed) %>%
      arrange(missed) %>%
      group_by(missed) %>%
      summarise(`miss` = n())

    colnames(tmissed)[1] <- "score"

    suppressMessages(df_doubles <- full_join(tclosing, tmissed)  %>%
                       mutate(score = factor(score, levels = levels)) %>%
                       arrange(score))

    df_doubles$hit[is.na(df_doubles$hit)] <- 0
    df_doubles$miss[is.na(df_doubles$miss)] <- 0
    df_doubles <- df_doubles %>%
      mutate(`hit rate` = paste(round(`hit`/(`hit`+`miss`)*100, 2), "%", sep = ""))



  } else {
    df_doubles <- tclosing %>%
      mutate(`miss` = 0, `hit rate` = "100%")
  }

  overall_double <- data.frame( "overall", sum(df_doubles$hit), sum(df_doubles$miss), paste(round(sum(df_doubles$hit)/sum(df_doubles$miss)*100, 2), "%", sep=""))
  colnames(overall_double) <- colnames(df_doubles)

  df_doubles <- rbind(df_doubles, overall_double)

  # dataset mean and sd
  first_darts_num <- as.numeric(from_chr_to_score_vector[c(first_darts)])
  second_darts_num <- as.numeric(from_chr_to_score_vector[c(second_darts)])
  third_darts_num <- as.numeric(from_chr_to_score_vector[c(third_darts)])
  darts_num <- as.numeric(from_chr_to_score_vector[c(darts)])

  first_darts_meands <- paste(round(mean(first_darts_num), 2), "±", round(sd(first_darts_num), 2))
  second_darts_meands <- paste(round(mean(second_darts_num), 2), "±", round(sd(second_darts_num), 2))
  third_darts_meands <- paste(round(mean(third_darts_num), 2), "±", round(sd(third_darts_num), 2))
  darts_meands <- paste(round(mean(darts_num)*3, 2), "±", round(sd(darts_num), 2))

  df_mean_sd <- data.frame(first_darts_meands, second_darts_meands, third_darts_meands, darts_meands)
  colnames(df_mean_sd) <- colnames(df_count_perc)[-1]

  # dataset power scoring
  df_power <- data.frame("100+" = sum(`100+`), "140+" = sum(`140+`), "180" = sum(`180`))
  colnames(df_power) <- c("100+", "140+", "180")


  # create complete list
  res <- list("number_of_legs" = n_of_legs,
              "darts" = darts,
              "1st_darts" = first_darts,
              "2nd_darts" = second_darts,
              "3rd_darts" = third_darts,
              "number_of_darts" = number_of_darts,
              "checkouts" = checkouts,
              "n180" = `180`,
              "n140plus" = `140+`,
              "n100plus" = `100+`,
              "missed_doubles" = missed_doubles,
              "closing_doubles" = closing_doubles,
              "missed" = missed,
              "checkout_rate" = sum(n_of_legs)/(sum(missed_doubles) + sum(n_of_legs))*100,
              "busted" = busted,
              "dataset_count_perc" = df_count_perc,
              "dataset_checkouts" = df_checkouts,
              "dataset_doubles" = df_doubles,
              "dataset_mean_sd" = df_mean_sd,
              "dataset_power" = df_power
  )

  # # saving files
  # save_leg <- paste("save(", paste(list_to_use, collapse = ", "), ", file = '", Sys.Date(),"_legs.Rdata')", sep = "")
  # eval(parse(text = save_leg))
  #
  # save_summary <- paste("save(res, file = '", Sys.Date(),"_summary.Rdata')", sep = "")
  # eval(parse(text = save_summary))
  #
  # # create file html
  # output_name <- paste(getwd(), "/", Sys.Date(), "_dashboard.html", sep = "")
  # filenameparams <- paste(getwd(), "/", Sys.Date(),"_summary.Rdata", sep = "")
  #
  # render(input = "dash.Rmd",
  #        params = list(filename = filenameparams,
  #                      set_title = paste(Sys.Date(), "resume"),
  #                      wd = getwd()),
  #        output_file = output_name,
  # )
  #
  #

  # return
  return(res)

}
