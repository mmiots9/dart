# leg primo player
l1 <- leg1p(id = "m20210612s1l1",
            start = 1,
            win = 0,
            player = "me",
            dartsScoresCh = c("t20", "20",  "1",   "5",   "5",  "20",  "t19", "19",  "19", "t19", "t19", "t19", "t20", "20", "20", "12", "2", "5", "1", "2", "NA"))


l2 <- leg1p(id = "m20210612s1l2",
            start = 0,
            win = 1,
            player = "me",
            dartsScoresCh = c("t20", "t20", "20", "1", "1", "5", "t20", "t20", "20", "t19", "t19", "t20", "20", "10", "t20", "d5", "d5", "d10")
              )

l3 <- leg1p(id = "m20210612s1l1",
            start = 1,
            win = 1,
            player = "me",
            dartsScoresCh = c("t20", "t20",  "5",   "1",   "1",  "t20",  "t19", "19",  "7", "19", "t19", "t19", "t20", "20", "20"))

s1 <- set1p(
  win = 1,
  legs = list(l1, l2, l3)
)

l4 <- leg1p(id = "m20210612s1l1",
            win = 0,
            player = "me",
            dartsScoresCh = c("t20", "20",  "1",   "5",   "5",  "20",  "t19", "19",  "19", "t19", "t19", "t19", "t20", "20", "20", "12", "2", "5", "1", "2", "NA"))


l5 <- leg1p(id = "m20210612s1l2",
            win = 1,
            player = "me",
            dartsScoresCh = c("t20", "t20", "20", "1", "1", "5", "t20", "t20", "20", "t19", "t19", "t20", "20", "10", "t20", "d5", "d5", "d10")
)

l6 <- leg1p(id = "m20210612s1l1",
            win = 1,
            player = "me",
            dartsScoresCh = c("t20", "t20",  "5",   "1",   "1",  "t20",  "t19", "19",  "7", "19", "t19", "t19", "t20", "20", "d9"))

s2 <- set1p(
  win = 1,
  legs = list(l4, l5, l6)
)

l7 <- leg1p(id = "m20210612s1l1",
            win = 0,
            player = "me",
            dartsScoresCh = c("t20", "20",  "1",   "5",   "5",  "20",  "t19", "19",  "19", "t19", "t19", "t19", "t20", "20", "20", "12", "2", "5", "1", "2", "NA"))


l8 <- leg1p(id = "m20210612s1l2",
            win = 1,
            player = "me",
            dartsScoresCh = c("t20", "t20", "20", "1", "1", "5", "t20", "t20", "20", "t19", "t19", "t20", "20", "10", "t20", "d5", "d15", "NA")
)

l9 <- leg1p(id = "m20210612s1l1",
            win = 1,
            player = "me",
            dartsScoresCh = c("t20", "t20",  "5",   "1",   "1",  "t20",  "t19", "19",  "7", "19", "t19", "t19", "t20", "d19", "NA"))

s3 <- set1p(
  win = 1,
  legs = list(l7, l8, l9)
)

m1 <- match1p(
  win = 1,
  sets = list(s1, s2)
)

#--------------------------------------- secondo player

l10 <- leg1p(id = "m20210612s1l1",
            win = 0,
            start = 0,
            player = "parola",
            dartsScoresCh = c("t20", "20",  "1",   "5",   "5",  "20",  "t19", "19",  "19", "t19", "t19", "t19", "t20", "20", "20", "12", "2", "5", "1", "2", "d1"))


l20 <- leg1p(id = "m20210612s1l2",
            win = 1,
            start = 1,
            player = "parola",
            dartsScoresCh = c("t20", "t20", "20", "1", "1", "5", "t20", "t20", "20", "t19", "t19", "t20", "20", "10", "t20", "d5", "d5", "d10")
)

l30 <- leg1p(id = "m20210612s1l1",
            win = 0,
            start = 0,
            player = "parola",
            dartsScoresCh = c("t20", "t20",  "5",   "1",   "1",  "t20",  "t19", "19",  "7", "19", "t19", "t19", "t20", "20", "20"))

s10 <- set1p(
  win = 0,
  legs = list(l10, l20, l30)
)

l40 <- leg1p(id = "m20210612s1l1",
            win = 1,
            player = "parola",
            dartsScoresCh = c("t20", "20",  "1",   "5",   "5",  "20",  "t19", "19",  "19", "t19", "t19", "t19", "t20", "20", "20", "12", "2", "5", "1", "2", "NA"))


l50 <- leg1p(id = "m20210612s1l2",
            win = 0,
            player = "parola",
            dartsScoresCh = c("t20", "t20", "20", "1", "1", "5", "t20", "t20", "20", "t19", "t19", "t20", "20", "10", "t20", "d5", "d5", "d10")
)

l60 <- leg1p(id = "m20210612s1l1",
            win = 0,
            player = "parola",
            dartsScoresCh = c("t20", "t20",  "5",   "1",   "1",  "t20",  "t19", "19",  "7", "19", "t19", "t19", "t20", "20", "d9"))

s20 <- set1p(
  win = 0,
  legs = list(l40, l50, l60)
)

l70 <- leg1p(id = "m20210612s1l1",
            win = 0,
            player = "me",
            dartsScoresCh = c("t20", "20",  "1",   "5",   "5",  "20",  "t19", "19",  "19", "t19", "t19", "t19", "t20", "20", "20", "12", "2", "5", "1", "2", "NA"))


l80 <- leg1p(id = "m20210612s1l2",
            win = 1,
            player = "me",
            dartsScoresCh = c("t20", "t20", "20", "1", "1", "5", "t20", "t20", "20", "t19", "t19", "t20", "20", "10", "t20", "d5", "d15", "NA")
)

l90 <- leg1p(id = "m20210612s1l1",
            win = 1,
            player = "me",
            dartsScoresCh = c("t20", "t20",  "5",   "1",   "1",  "t20",  "t19", "19",  "7", "19", "t19", "t19", "t20", "d19", "NA"))

s30 <- set1p(
  win = 1,
  legs = list(l7, l8, l9)
)

m10 <- match1p(
  win = 0,
  sets = list(s10, s20)
)


# ---------------------------------------- 2 players
l110 <- leg2p(
  p1leg = l1,
  p2leg = l10
)

s110 <- set2p(
  p1set = s1,
  p2set = s10,
  leg2win = 2
)

s220 <- set2p(
  p1set = s2,
  p2set = s20,
  leg2win = 2
)

m110 <- match2p(
  p1match = m1,
  p2match = m10,
  set2win = 2
)
