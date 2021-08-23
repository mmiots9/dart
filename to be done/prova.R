# leg primo player
l1 <- leg1p(id = "m20210612s1l1", win = 1, player = "me", dartsScoresCh = "t20", dartsNumber = 12,
            means = data.frame(c("3 darts ", "First 9 ", "1st dart", "2nd dart", "3rd dart"), c(1, 2, 1, 3, 3)),
            checkout = data.frame(c("Missed", "Busted", "Rate  "), c(1, 1, 0.33)),
            powerScoring = data.frame(c("54  ", "57  ", "60  ", "100+", "140+", "180+"), c(1, 1, 2, 2, 3, 3)))


l2 <- leg1p(id = "m12312s1l2", win = 1, player = "me", dartsScoresCh = "t20, d20",
                dartsNumber = 22,
                means = data.frame(c("3 darts ", "First 9 ", "1st dart", "2nd dart", "3rd dart"), c(1, 1, 4, 3, 4)),
                checkout = data.frame(c("Missed", "Busted", "Rate  "), c(1, 2, 0.00)),
                powerScoring = data.frame(c("54  ", "57  ", "60  ", "100+", "140+", "180+"), c(2, 1, 1, 2, 3, 3)))

l3 <- leg1p(id = "m12312s2l1", win = 1, player = "me", dartsScoresCh = "t20", dartsNumber = 2,
            means = data.frame(c("3 darts ", "First 9 ", "1st dart", "2nd dart", "3rd dart"), c(1, 2, 1, 3, 3)),
            checkout = data.frame(c("Missed", "Busted", "Rate  "), c(1, 1, 0.33)),
            powerScoring = data.frame(c("54  ", "57  ", "60  ", "100+", "140+", "180+"), c(1, 1, 2, 2, 3, 3)))


l4 <- leg1p(id = "m12312s2l2", win = 1, player = "me", dartsScoresCh = "t20, d20",
            dartsNumber = 1,
            means = data.frame(c("3 darts ", "First 9 ", "1st dart", "2nd dart", "3rd dart"), c(1, 1, 4, 3, 4)),
            checkout = data.frame(c("Missed", "Busted", "Rate  "), c(1, 2, 0.00)),
            powerScoring = data.frame(c("54  ", "57  ", "60  ", "100+", "140+", "180+"), c(2, 1, 1, 2, 3, 3)))

# set primo player
s1 <- set1p(
  legs = list(l1 = l1, l2 = l2),
  dartsScoresCh = "t20, t20, d20",
  dartsNumber = 13,
  means = data.frame(c("3 darts ", "First 9 ", "1st dart", "2nd dart", "3rd dart"), c(1, 1, 4, 3, 4)),
  checkout = data.frame(c("Missed", "Busted", "Rate  "), c(1, 2, 0.00)),
  powerScoring = data.frame(c("54  ", "57  ", "60  ", "100+", "140+", "180+"), c(2, 1, 1, 2, 3, 3))

)

s2 <- set1p(
  legs = list(l1 = l3, l2 = l4),
  dartsScoresCh = "t20, t20, d20",
  dartsNumber = 3,
  means = data.frame(c("3 darts ", "First 9 ", "1st dart", "2nd dart", "3rd dart"), c(1, 1, 4, 3, 4)),
  checkout = data.frame(c("Missed", "Busted", "Rate  "), c(1, 2, 0.00)),
  powerScoring = data.frame(c("54  ", "57  ", "60  ", "100+", "140+", "180+"), c(2, 1, 1, 2, 3, 3))

)

# match primo player
m1 <- match1p(
  sets = list(s1 = s1, s2 = s2),
  dartsScoresCh = "t20, 20, d20",
  dartsNumber = 15,
  means = data.frame(c("3 darts ", "First 9 ", "1st dart", "2nd dart", "3rd dart"), c(1, 1, 4, 3, 4)),
  checkout = data.frame(c("Missed", "Busted", "Rate  "), c(1, 2, 0.00)),
  powerScoring = data.frame(c("54  ", "57  ", "60  ", "100+", "140+", "180+"), c(2, 1, 1, 2, 3, 3))

)




object <- leg1p(id = "m12312s1l1", player = "me", win = 1,
            dartsScoresCh = c("t20", "t20", "t20", "t20", "t20", "t20", "t20", "t19", "d12"))














