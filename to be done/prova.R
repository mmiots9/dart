# leg primo player
l1 <- leg1p(id = "m20210612s1l1",
            win = 0,
            player = "me",
            dartsScoresCh = c("t20", "20",  "1",   "5",   "5",  "20",  "t19", "19",  "19", "t19", "t19", "t19", "t20", "20", "20", "12", "2", "5", "1", "2", "NA"))


l2 <- leg1p(id = "m20210612s1l2",
            win = 1,
            player = "me",
            dartsScoresCh = c("t20", "t20", "20", "1", "1", "5", "t20", "t20", "20", "t19", "t19", "t20", "20", "10", "t20", "d5", "d5", "d10")
              )

l3 <- leg1p(id = "m20210612s1l1",
            win = 0,
            player = "me",
            dartsScoresCh = c("t20", "t20",  "5",   "1",   "1",  "t20",  "t19", "19",  "7", "19", "t19", "t19", "t20", "20", "20"))

s1 <- set1p(
  win = 0,
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
  sets = list(s1, s2, s3)
)






