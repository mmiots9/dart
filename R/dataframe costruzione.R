# around the clock

a <- data.frame(
  year = numeric(),
  month = numeric(),
  day = numeric())


possible_scores <- as.character(c(1:20, 25, 50))

for (i in seq_along(possible_scores)) {
  a[,i+3] <- numeric()
}

a[,27] <- numeric()
colnames(a) <- c("year", "month", "day", possible_scores, "tot.darts", "precision")

# high scores

a <- data.frame(
  year = numeric(),
  month = numeric(),
  day = numeric())


for (i in 1:10) {
  a[,i+3] <- numeric()
}

a[,15] <- numeric()
colnames(a) <- c("year", "month", "day", as.character(1:10), "tot.darts", "precision")

write.csv(a, "high_scores_18.csv", row.names = F)

# closing 3
a <- data.frame(
  year = numeric(),
  month = numeric(),
  day = numeric())
for (i in 1:20) {
  a[,i+3] <- numeric()
}

a[,25] <- numeric()
colnames(a) <- c("year", "month", "day", as.character(1:20), "tot.darts", "precision")

write.csv(a, "closing_advanced.csv", row.names = F)


# closing 6
a <- data.frame(
  year = numeric(),
  month = numeric(),
  day = numeric())
for (i in 1:10) {
  a[,i+3] <- numeric()
}
a[,14] <- numeric()
a[,15] <- numeric()
a[,16] <- numeric()
colnames(a) <- c("year", "month", "day", as.character(1:10), "tot.closed.3", "tot.closed.6", "precision")
write.csv(a, "closing_6_intermediate.csv", row.names = F)
