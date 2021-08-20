#' @name frasi_dash_ita
#' @title Frasi per dashboard ita
#' @author Matteo Miotto
#' @importFrom purrr is_empty
#' @export

frasi_dash_ita <- function(daily, overall){

  consigli <- NULL

  # medie
  day_mean  <- as.numeric(strsplit(daily$dataset_mean_sd$`all darts`, " ")[[1]][1])
  all_mean  <- as.numeric(strsplit(overall$dataset_mean_sd$`all darts`, " ")[[1]][1])
  mean_diff <- abs(day_mean - all_mean)
  day_power <- sum(daily$dataset_power[1,])
  all_power <- sum(overall$dataset_power[1,])

# Come è stata la giornata?
  # confronto con la media dei giorni precedenti 3 freccette, power scoring e doppie
  # opzioni: terribile, pessima, buona, ottima
  if (all(day_mean < all_mean, day_power < all_power, daily$checkout_rate < overall$checkout_rate))({
    giornata <- "terribile"
  }) else if (sum (day_mean < all_mean, day_power < all_power, daily$checkout_rate < overall$checkout_rate) == 2) {
    giornata <- "pessima"
  }  else if (sum (day_mean < all_mean, day_power < all_power, daily$checkout_rate < overall$checkout_rate) == 1) {
    giornata <- "buona"
  } else {
    giornata <- "ottima"
  }

  # confronto con numero di leg:
  # opzioni: più del, meno del, come al
  if (daily$number_of_legs < (mean(overall$number_of_legs) - 1)) {
    n_leg <- "meno del"
  } else if (daily$number_of_legs > (mean(overall$number_of_legs) + 1)) {
    n_leg <- "più del"
  } else {
    n_leg <- "come al"
  }

  # confronto media giorni precedenti:
  # opzioni: tot punti più/meno della tua media solita, in linea col tuo storico
  if (day_mean < (all_mean - 3)){
    media_conf <- paste(mean_diff, "punti meno della tua media solita")
  } else if (day_mean > (all_mean + 3)){
    media_conf <- paste(mean_diff, "punti più della tua media solita")
  } else{
    media_conf <- "in linea con il tuo solita"
  }

  # confronto power scoring:
  # opzioni: hai lasicato un po' a desiderare, nulla da segnalare, è stata una giornata prolifica
  if (day_power < (all_power - 2)) {
    power_sc <- "hai lasciato un po' a desiderare"
  } else if (day_power > (all_power + 2)) {
    power_sc <- "è stata una giornata prolifica"
  } else {
    power_sc <- "nulla da segnalare"
  }

  # confronto doppie:
  # opzioni: migliore/peggiore del solito, uguale al solito
  if (daily$checkout_rate < (overall$checkout_rate - 3)) {
    confr_doppie <- "peggiore del solito"
  } else if (daily$checkout_rate > (overall$checkout_rate + 3)) {
    confr_doppie <- "migliore del solito"
  } else {
    confr_doppie <- "uguale al solito"
  }

  # criticità doppie
  # opzioni: Hai-alcune-: vs Non hai-alcuna-.
  # opzioni seconda parte: quali doppie eventualmente o ""

  if(all(daily$dataset_doubles$`hit rate` > "30%") | all(daily$dataset_doubles$`hit rate` == "100%")) {
    crit_doppie <- c("Non hai", "le", ".")
  } else {
    quali_doppie <- paste(daily$dataset_doubles$score[which(daily$dataset_doubles$`hit rate` < "35%")])
    if ("overall" %in% quali_doppie) {quali_doppie <- quali_doppie[-which(quali_doppie == "overall")]}

    quali_doppie <- paste(quali_doppie, collapse = ", ")

    crit_doppie  <- c("Hai", "alcune", paste(":", quali_doppie))
    consigli <- paste(consigli, "<li>Allenati sulle doppie in cui hai avuto criticità oggi</li>")
  }

# Punti di forza
  punti_forza <- NULL
  # Power scoring
  if (day_power > (all_power + 2)) {
    punti_forza <- paste(punti_forza, "<li>Power scoring</li>")
  }
  # Chiusure alte
  if (mean(daily$checkouts) > 70) {
    punti_forza <- paste(punti_forza, "<li>Chiusure alte</li>")
  }

  # Precisione in doppia
  if (daily$checkout_rate > 50) {
    punti_forza <- paste(punti_forza, "<li>Precisione in doppia</li>")
  }

  if (is_empty(punti_forza)){
    punti_forza <- "Nessuno, forse dovresti allenarti senza pensare ad altro..."
  } else {
    punti_forza <- paste("<ul>", punti_forza, "</ul>")
  }


# Punti deboli e consigli
  punti_deboli <- NULL


  # no power scoring
  if (day_power < (all_power - 2)) {
    punti_deboli <- paste(punti_deboli, "<li>Power scoring</li>")
    consigli <- paste(consigli, "<li>Allenati sulle triple dal 17 al 20</li>")
  }
  # Chiusure alte
  if (mean(daily$checkouts) < 70) {
    punti_deboli <- paste(punti_deboli, "<li>Chiusure alte</li>")
    consigli <- paste(consigli, "<li>Prova alcune chiusure alte, cercando di rimaner concentrato dalla prima all'ultima freccia</li>")
  }

  # Precisione in doppia
  if (daily$checkout_rate < 50) {
    punti_deboli <- paste(punti_deboli, "<li>Precisione in doppia</li>")
    consigli <- paste(consigli, "<li>Fai un bell'Around the clock in doppia</li>")
  }


  if (is_empty(punti_deboli)){
    punti_deboli <- "Incredibilmente nessuno, un miracolo?!"
  } else {
    punti_deboli <- paste("<ul>", punti_deboli, "</ul>")
  }

  # Consigli
  if (is_empty(consigli)){
    consigli <- "Bevi birra, che oggi sei andato bene"
  } else {
    consigli <- paste("<ul>", consigli, "</ul>")
  }


  frasi_lista <- list("giornata" = giornata,
                            "n_leg" = n_leg,
                            "media_conf" = media_conf,
                            "power_sc" = power_sc,
                            "confr_doppie" = confr_doppie,
                            "crit_doppie" = crit_doppie,
                            "punti_forza" = punti_forza,
                            "punti_deboli" = punti_deboli,
                      "consigli" = consigli
                            )

  return(frasi_lista)
}
