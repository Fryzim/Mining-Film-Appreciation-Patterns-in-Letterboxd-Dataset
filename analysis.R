library(dplyr)
library(tidyr)
library(arules)
library(rpart)
library(rpart.plot)
library(class)
library(randomForest)

set.seed(2568)
t.start <- proc.time()


# 1. Load Data

movies <- read.csv("final_movies_dataset.csv", stringsAsFactors = FALSE)
movies$poster_filename <- NULL
n.raw <- nrow(movies)
print(paste("Raw dataset:", n.raw, "films"))



# 2. Data Cleaning

movies$year            <- as.integer(movies$date)
movies$country.primary <- trimws(gsub("\\|.*", "", movies$country))
movies$country.primary[movies$country.primary == ""] <- NA

split.len <- function(x) sapply(strsplit(ifelse(is.na(x), "", x), "\\|"), length)

movies.clean <- movies[
  !is.na(movies$rating) & !is.na(movies$minute) & !is.na(movies$year) &
    movies$minute > 40 & movies$minute < 300 &
    movies$year >= 1950 & movies$year <= 2024, ]

movies.clean$n.genres    <- split.len(movies.clean$genre)
movies.clean$n.studios   <- split.len(movies.clean$studio)
movies.clean$n.countries <- split.len(movies.clean$country)
movies.clean$n.actors    <- split.len(movies.clean$actors)
movies.clean$decade      <- floor(movies.clean$year / 10) * 10

n <- nrow(movies.clean)
print(paste("After cleaning:", n, "films"))
print(paste("Mean rating:", round(mean(movies.clean$rating), 3),
            "  SD:", round(sd(movies.clean$rating), 3),
            "  Q3:", round(quantile(movies.clean$rating, 0.75), 3)))



# 3. Rating Distribution

dev.new()
qplot(movies.clean$rating, bins = 35,
      fill = I("blue"), color = I("white"),
      xlab = "Rating", ylab = "Count",
      main = paste("Rating Distribution — n =", n,
                   "  mean =", round(mean(movies.clean$rating), 2)))


# 4. Temporal Analysis

decade.stats <- aggregate(
  cbind(rating, year) ~ decade,
  data = movies.clean,
  FUN  = mean)
decade.stats$n <- as.numeric(table(movies.clean$decade))
decade.stats   <- decade.stats[decade.stats$n >= 30, ]

# Linear trend over decades
lm.temp <- lm(rating ~ decade, data = decade.stats)
print(paste("Slope per decade:", round(coef(lm.temp)["decade"] * 10, 4)))

cor.surv <- cor(decade.stats$n, decade.stats$rating)
print(paste("cor(n_decade, avg_rating) =", round(cor.surv, 3)))

dev.new()
plot(decade.stats$decade, decade.stats$rating,
     type = "b", pch = 19, col = "blue",
     xlab = "Decade", ylab = "Average Rating",
     main = "Average Rating by Decade")
abline(lm.temp, col = "red", lty = 2)


dev.new()
plot(log10(decade.stats$n), decade.stats$rating,
     pch = 19, col = "blue",
     xlab = "log10(Films per Decade)", ylab = "Average Rating",
     main = paste("Survivorship Bias: r =", round(cor.surv, 3)))
abline(lm(rating ~ log10(n), data = decade.stats), col = "red", lty = 2)
text(log10(decade.stats$n), decade.stats$rating,
     labels = decade.stats$decade, pos = 3, cex = 0.8)



# 5. Geographic Analysis

country.df <- movies.clean[!is.na(movies.clean$country.primary), ]
country.stats <- aggregate(rating ~ country.primary, data = country.df, FUN = mean)
country.stats$n.films <- as.numeric(table(country.df$country.primary)[country.stats$country.primary])
country.stats <- country.stats[country.stats$n.films >= 30, ]
country.stats$vs.global <- country.stats$rating - mean(movies.clean$rating)

cor.vol <- cor(country.stats$n.films, country.stats$rating)
print(paste("cor(n_country, avg_rating) =", round(cor.vol, 3)))

top5 <- head(country.stats[order(-country.stats$rating),
                           c("country.primary", "n.films", "rating")], 5)
print(top5)


dev.new()
plot(log10(country.stats$n.films), country.stats$rating,
     pch = 19, col = "blue",
     xlab = "log10(Films per Country)", ylab = "Average Rating",
     main = paste("Volume Paradox: r =", round(cor.vol, 3)))
abline(lm(rating ~ log10(n.films), data = country.stats), col = "red", lty = 2)
notable <- country.stats[country.stats$n.films > 500 | country.stats$rating > 3.6, ]
text(log10(notable$n.films), notable$rating,
     labels = notable$country.primary, pos = 3, cex = 0.7)



# 6. Genre Diversity
cor.genre <- cor(movies.clean$n.genres, movies.clean$rating)
print(paste("cor(n_genres, rating) =", round(cor.genre, 3)))


# 7. Association Rules

# Define high rating class (top 25%)
threshold <- quantile(movies.clean$rating, 0.75)
movies.clean$high.rating <- factor(
  ifelse(movies.clean$rating >= threshold, "high", "not_high"))

print(paste("Threshold Q3 =", round(threshold, 2),
            "  High-rated films:", sum(movies.clean$high.rating == "high"),
            "(", round(100 * mean(movies.clean$high.rating == "high")), "%)"))

genre.list <- lapply(strsplit(movies.clean$genre, "\\|"), trimws)
genre.list <- genre.list[sapply(genre.list, length) > 0]
trans.all  <- as(genre.list, "transactions")

t.apr.all <- system.time(
  rules.all <- apriori(trans.all,
                       parameter = list(support = 0.02, confidence = 0.40, minlen = 2),
                       control   = list(verbose = FALSE))
)
print(paste("Apriori (all):", length(rules.all), "rules in",
            round(t.apr.all["elapsed"], 2), "s"))
inspect(head(sort(rules.all, by = "lift"), 10))

high.mask       <- movies.clean$high.rating == "high"
genre.list.high <- lapply(strsplit(movies.clean$genre[high.mask], "\\|"), trimws)
genre.list.high <- genre.list.high[sapply(genre.list.high, length) > 0]
trans.high <- as(genre.list.high, "transactions")

t.apr.high <- system.time(
  rules.high <- apriori(trans.high,
                        parameter = list(support = 0.03, confidence = 0.35, minlen = 2),
                        control   = list(verbose = FALSE))
)
print(paste("Apriori (high):", length(rules.high), "rules in",
            round(t.apr.high["elapsed"], 2), "s"))
inspect(head(sort(rules.high, by = "lift"), 10))



# 8. Growth Factor Analysis

# g(X) = supp(X|high) / supp(X |all)
# g > 1 means genre is over-represented in acclaimed films
item.freq.all  <- itemFrequency(trans.all)
item.freq.high <- itemFrequency(trans.high)
common <- intersect(names(item.freq.all), names(item.freq.high))

growth.df <- data.frame(
  genre    = common,
  sup.all  = item.freq.all[common],
  sup.high = item.freq.high[common]
)
growth.df$growth <- growth.df$sup.high / growth.df$sup.all
growth.df <- growth.df[order(growth.df$growth), ]
print(growth.df[, c("genre", "sup.all", "sup.high", "growth")])

dev.new()
par(mar = c(4, 11, 3, 2))
barplot(growth.df$growth, names.arg = growth.df$genre,
        horiz = TRUE, las = 1,
        col = ifelse(growth.df$growth > 1, "blue", "red"),
        xlab = "g(X) = supp(high) / supp(all)",
        main = "Genre Growth Factors in Acclaimed Films")
abline(v = 1, lty = 2)
abline(v = c(0.5, 1.5), lty = 3, col = "gray")



# 8b. Longitudinal Stability of g(X)

focal.genres <- c("Documentary", "Music", "Horror", "Drama", "Animation", "Thriller")

decade.growth.list <- lapply(seq(1960, 2010, 10), function(dec) {
  sub      <- movies.clean[movies.clean$decade == dec, ]
  sub.high <- sub[sub$high.rating == "high", ]
  if (nrow(sub) < 200 || nrow(sub.high) < 30) return(NULL)
  freq.all  <- table(trimws(unlist(strsplit(sub$genre,      "\\|")))) / nrow(sub)
  freq.high <- table(trimws(unlist(strsplit(sub.high$genre, "\\|")))) / nrow(sub.high)
  data.frame(
    decade = dec,
    genre  = focal.genres,
    growth = sapply(focal.genres, function(g) {
      fa <- ifelse(is.na(freq.all[g]),  0, as.numeric(freq.all[g]))
      fh <- ifelse(is.na(freq.high[g]), 0, as.numeric(freq.high[g]))
      if (fa == 0) NA_real_ else fh / fa
    })
  )
})

decade.growth.df <- do.call(rbind, Filter(Negate(is.null), decade.growth.list))
print(pivot_wider(decade.growth.df, names_from = genre, values_from = growth))


genre.colors <- c("blue", "darkgreen", "red", "purple", "orange", "brown")
ylim.g <- range(na.omit(decade.growth.df$growth))
dev.new()
plot(0, 0, type = "n", xlim = c(1960, 2010), ylim = ylim.g,
     xlab = "Decade", ylab = "g(X)", main = "Genre Growth Factors by Decade")
abline(h = 1,   lty = 2, col = "gray")
abline(h = 1.5, lty = 3, col = "gray")
for (i in seq_along(focal.genres)) {
  sub.g <- na.omit(decade.growth.df[decade.growth.df$genre == focal.genres[i], ])
  lines(sub.g$decade, sub.g$growth, col = genre.colors[i], type = "b", pch = 19)
}
legend("topleft", focal.genres, col = genre.colors, lty = 1, cex = 0.8)



# 9. Clustering
num.features <- c("year", "minute", "rating", "n.genres", "n.studios", "n.countries")
movies.scaled <- na.omit(as.data.frame(scale(movies.clean[, num.features])))

set.seed(2568)
movies.sample <- movies.scaled[sample(nrow(movies.scaled), 3000), ]

# WSS elbow to determine optimal k
wss <- numeric(7)
t.elbow <- system.time({
  for (k in 2:8) {
    km.tmp     <- kmeans(movies.sample, centers = k, nstart = 10)
    wss[k - 1] <- km.tmp$tot.withinss
  }
})
print(paste("Elbow computed in", round(t.elbow["elapsed"], 2), "s"))

dev.new()
plot(2:8, wss, type = "b", pch = 19, col = "blue",
     xlab = "k", ylab = "WSS", main = "Elbow Curve")
abline(v = 4, col = "red", lty = 2)

t.km <- system.time({
  set.seed(2568)
  km4 <- kmeans(movies.scaled, 4, nstart = 25)
})
movies.clean$cluster <- factor(km4$cluster)
print(paste("k-means (k=4) in", round(t.km["elapsed"], 2), "s"))
print(paste("Cluster sizes:", paste(table(km4$cluster), collapse = " / ")))

# Cluster profiles
cluster.profile <- aggregate(
  cbind(year, minute, rating, n.genres, n.studios) ~ cluster,
  data = movies.clean, FUN = mean)
cluster.profile$n <- as.numeric(table(movies.clean$cluster))
cluster.profile$pct.high <- round(
  100 * tapply(movies.clean$high.rating == "high", movies.clean$cluster, mean), 1)
print(cluster.profile)



# 10. Classification

set.seed(2568)
n <- nrow(movies.clean)
train <- sort(sample(1:n, floor(n * 0.7)))
movies.train <- droplevels(movies.clean[train, ])
movies.test  <- droplevels(movies.clean[-train, ])

predictors <- c("year", "minute", "n.genres", "n.studios", "n.countries")

# F1 score
f1.score <- function(actual, predicted) {
  mat <- table(actual, predicted)
  pre <- mat["high", "high"] / sum(mat[, "high"])
  rec <- mat["high", "high"] / sum(mat["high", ])
  round(2 * pre * rec / (pre + rec), 3)
}

# 10a. Decision Tree
# bias-variance tradeoff
depth.range <- c(1, 2, 3, 4, 5, 6, 8, 10, 15, 20)
dt.res <- data.frame(depth = depth.range, train = NA_real_, test = NA_real_)

for (i in seq_along(depth.range)) {
  movies.rp <- rpart(
    high.rating ~ year + minute + n.genres + n.studios + n.countries,
    data    = movies.train, method = "class",
    control = rpart.control(maxdepth = depth.range[i], minsplit = 50))
  
  pred.train <- predict(movies.rp, newdata = movies.train, type = "class")
  pred.test  <- predict(movies.rp, newdata = movies.test,  type = "class")
  
  dt.res[i, "train"] <- f1.score(movies.train$high.rating, pred.train)
  dt.res[i, "test"]  <- f1.score(movies.test$high.rating,  pred.test)
}
print(dt.res)

best.depth <- dt.res$depth[which.max(dt.res$test)]
print(paste("Best depth:", best.depth))

dev.new()
plot(dt.res$depth, dt.res$train, type = "b", pch = 19, col = "blue",
     xlab = "Max Depth", ylab = "F1 Score",
     main = "Decision Tree — Depth Sweep",
     ylim = range(dt.res[, 2:3], na.rm = TRUE))
lines(dt.res$depth, dt.res$test, type = "b", pch = 19, col = "red")
abline(v = best.depth, lty = 2)
legend("topright", c("train", "test"), col = c("blue", "red"), lty = 1)



# 10b. kNN

train.X <- scale(movies.train[, predictors])
test.X <- scale(movies.test[, predictors],
                 center = attr(train.X, "scaled:center"),
                 scale  = attr(train.X, "scaled:scale"))

k.range <- c(1, 3, 5, 7, 9, 11, 15, 21)
knn.res <- data.frame(k = k.range, f1 = NA_real_)

for (i in seq_along(k.range)) {
  pred.knn.i <- knn(train.X, test.X,
                    cl = movies.train$high.rating, k = k.range[i])
  knn.res[i, "f1"] <- f1.score(movies.test$high.rating, pred.knn.i)
}
print(knn.res)

dev.new()
plot(knn.res$k, knn.res$f1, type = "b", pch = 19, col = "blue",
     xlab = "k", ylab = "F1 Score", main = "kNN — k Sweep")
abline(v = 7, lty = 2, col = "red")



# 11. Final Models

# Decision Tree with best depth
t.dt <- system.time({
  movies.rp <- rpart(
    high.rating ~ year + minute + n.genres + n.studios + n.countries,
    data    = movies.train, method = "class",
    control = rpart.control(maxdepth = best.depth, minsplit = 50))
})
dev.new()
rpart.plot(movies.rp, type = 4, extra = 104, under = TRUE,
           main = "Decision Tree (best depth)")
print(rpart.rules(movies.rp, cover = TRUE))

pred.dt <- predict(movies.rp, newdata = movies.test, type = "class")
mat.dt  <- table(movies.test$high.rating, pred.dt)
mat.dt
ac.dt <- sum(diag(mat.dt)) / sum(mat.dt)
print(paste("DT accuracy on test set:", round(ac.dt * 100, 2), "%"))
print(paste("DT F1 on test set:", f1.score(movies.test$high.rating, pred.dt)))

# k-Nearest Neighbours
t.knn <- system.time(
  pred.knn <- knn(train.X, test.X, cl = movies.train$high.rating, k = 7)
)
mat.knn <- table(movies.test$high.rating, pred.knn)
mat.knn
ac.knn <- sum(diag(mat.knn)) / sum(mat.knn)
print(paste("kNN accuracy on test set:", round(ac.knn * 100, 2), "%"))
print(paste("kNN F1 on test set:", f1.score(movies.test$high.rating, pred.knn)))

# Random Forest
t.rf <- system.time({
  set.seed(999)
  movies.rf <- randomForest(
    high.rating ~ year + minute + n.genres + n.studios + n.countries,
    data = movies.train, ntree = 300, importance = TRUE)
})
pred.rf <- predict(movies.rf, newdata = movies.test)
mat.rf  <- table(movies.test$high.rating, pred.rf)
mat.rf
ac.rf <- sum(diag(mat.rf)) / sum(mat.rf)
print(paste("RF accuracy on test set:", round(ac.rf * 100, 2), "%"))
print(paste("RF F1 on test set:", f1.score(movies.test$high.rating, pred.rf)))

# Variable importance
dev.new()
varImpPlot(movies.rf, main = "Random Forest Variable Importance")
imp <- importance(movies.rf, type = 2)
print(imp[order(-imp[, 1]), , drop = FALSE])

# Naive baseline
ac.naive <- mean(movies.test$high.rating == "not_high")
print(paste("Naive baseline (majority class):", round(ac.naive, 3)))



# 12. 5-Fold Cross-Validation

k.folds  <- 5
fold.ids <- sample(rep(1:k.folds, length.out = nrow(movies.clean)))
cv.res   <- matrix(NA, nrow = k.folds, ncol = 3,
                   dimnames = list(paste0("Fold", 1:k.folds), c("DT", "kNN", "RF")))

t.cv <- system.time({
  for (i in 1:k.folds) {
    tr <- droplevels(movies.clean[fold.ids != i, ])
    te <- droplevels(movies.clean[fold.ids == i, ])
    
    # Decision Tree
    rp.cv <- rpart(
      high.rating ~ year + minute + n.genres + n.studios + n.countries,
      data = tr, method = "class",
      control = rpart.control(maxdepth = best.depth))
    cv.res[i, "DT"] <- f1.score(te$high.rating,
                                predict(rp.cv, te, type = "class"))
    
    # kNN
    trX <- scale(tr[, predictors])
    teX <- scale(te[, predictors],
                 center = attr(trX, "scaled:center"),
                 scale  = attr(trX, "scaled:scale"))
    cv.res[i, "kNN"] <- f1.score(te$high.rating,
                                 knn(trX, teX, cl = tr$high.rating, k = 7))
    
    # Random Forest
    rf.cv <- randomForest(
      high.rating ~ year + minute + n.genres + n.studios + n.countries,
      data = tr, ntree = 100)
    cv.res[i, "RF"] <- f1.score(te$high.rating, predict(rf.cv, te))
    
    print(paste("Fold", i,
                "DT =",  cv.res[i, "DT"],
                "kNN =", cv.res[i, "kNN"],
                "RF =",  cv.res[i, "RF"]))
  }
})

print(paste("Cross-validation completed in", round(t.cv["elapsed"], 1), "s"))
for (m in c("DT", "kNN", "RF"))
  print(paste(m, "mean F1:", round(mean(cv.res[, m], na.rm = TRUE), 3),
              "+/-", round(sd(cv.res[, m], na.rm = TRUE), 3)))



# 13. Timing
t.end <- proc.time()
print(paste("Total elapsed:", round((t.end - t.start)["elapsed"], 1), "s"))
print(paste("RF:", round(t.rf["elapsed"], 1),
            "  CV:", round(t.cv["elapsed"], 1),
            "  kNN:", round(t.knn["elapsed"], 1),
            "  kMeans:", round(t.km["elapsed"], 1),
            "  Elbow:", round(t.elbow["elapsed"], 1)))

