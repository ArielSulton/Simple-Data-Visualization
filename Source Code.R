#3323600054_Mochammmad Ariel Sulton

#1. menggunakan data pulse.csv

data1 <- read.csv("pulse.csv")

active <- data1 [, 1]
rest <- data1 [, 2]
smoke <- data1 [, 3]
gender <- data1 [, 4]
exercise <- data1 [, 5]
hgt <- data1 [, 6]
wgt <- data1 [, 7]


#A. summary

#1a
summary(data1)

height <- by(hgt, data1$Gender, summary)
print(height)

weight <- by(wgt, data1$Gender, summary)
print(weight)

#2a
activeS <- by(active, smoke, summary)
activeG <- by(active, gender, summary)
activeE <- by(active, exercise, summary)
print(activeS)
print(activeG)
print(activeE)

restS <- by(rest, smoke, summary)
restG <- by(rest, gender, summary)
restE <- by(rest, exercise, summary)
print(restS)
print(restG)
print(restE)


#B. boxplot

boxplot(hgt ~ gender, xlab = "Gender", ylab = "Height", main = "Boxplot - Height Based on Gender")
boxplot(wgt ~ gender, xlab = "Gender", ylab = "Weight", main = "Boxplot - Weight Based on Gender")

boxplot(active ~ smoke, xlab = "Smoke", ylab = "Activity", main = "Boxplot - Activity Based on Smoker")
boxplot(active ~ gender, xlab = "Gender", ylab = "Activity", main = "Boxplot - Activity Based on Gender")
boxplot(active ~ exercise, xlab = "Exercise", ylab = "Activity", main = "Boxplot - Activity Based on Exercises")

boxplot(rest ~ smoke, xlab = "Smoke", ylab = "Rest", main = "Boxplot - Smoker Rest Time")
boxplot(rest ~ gender, xlab = "Gender", ylab = "Rest", main = "Boxplot - Rest Time Based on Gender")
boxplot(rest ~ exercise, xlab = "Exercise", ylab = "Rest", main = "Boxplot - Exerciser Rest Time")


#C. statistik mean, median, variansi, QR, dan koefisien variasi

#mean
mean_hgt <- by(hgt, gender, mean)
mean_wgt <- by(wgt, gender, mean)
print(mean_hgt)
print(mean_wgt)

mean_activeS <- by(active, smoke, mean)
mean_activeG <- by(active, gender, mean)
mean_activeE <- by(active, exercise, mean)
print(mean_activeS)
print(mean_activeG)
print(mean_activeE)

mean_restS <- by(rest, smoke, mean)
mean_restG <- by(rest, gender, mean)
mean_restE <- by(rest, exercise, mean)
print(mean_restS)
print(mean_restG)
print(mean_restE)

#median
med_hgt <- by(hgt, gender, median)
med_wgt <- by(wgt, gender, median)
print(med_hgt)
print(med_wgt)

med_activeS <- by(active, smoke, median)
med_activeG <- by(active, gender, median)
med_activeE <- by(active, exercise, median)
print(med_activeS)
print(med_activeG)
print(med_activeE)

med_restS <- by(rest, smoke, median)
med_restG <- by(rest, gender, median)
med_restE <- by(rest, exercise, median)
print(med_restS)
print(med_restG)
print(med_restE)

#variansi
var_hgt <- by(hgt, gender, var)
var_wgt <- by(wgt, gender, var)
print(var_hgt)
print(var_wgt)

var_activeS <- by(active, smoke, var)
var_activeG <- by(active, gender, var)
var_activeE <- by(active, exercise, var)
print(var_activeS)
print(var_activeG)
print(var_activeE)

var_restS <- by(rest, smoke, var)
var_restG <- by(rest, gender, var)
var_restE <- by(rest, exercise, var)
print(var_restS)
print(var_restG)
print(var_restE)

#QR
Q1_hgt <- by(hgt, gender, function(x) quantile(x, 0.25))
Q3_hgt <- by(hgt, gender, function(x) quantile(x, 0.75))
QR_hgt <- Q3_hgt - Q1_hgt
print(QR_hgt)

Q1_wgt <- by(wgt, gender, function(x) quantile(x, 0.25))
Q3_wgt <- by(wgt, gender, function(x) quantile(x, 0.75))
QR_wgt <- Q3_wgt - Q1_wgt
print(QR_wgt)

Q1_activeS <- by(active, smoke, function(x) quantile(x, 0.25))
Q3_activeS <- by(active, smoke, function(x) quantile(x, 0.75))
QR_activeS <- Q3_activeS - Q1_activeS
print(QR_activeS)

Q1_activeG <- by(active, gender, function(x) quantile(x, 0.25))
Q3_activeG <- by(active, gender, function(x) quantile(x, 0.75))
QR_activeG <- Q3_activeG - Q1_activeG
print(QR_activeG)

Q1_activeE <- by(active, exercise, function(x) quantile(x, 0.25))
Q3_activeE <- by(active, exercise, function(x) quantile(x, 0.75))
QR_activeE <- Q3_activeE - Q1_activeE
print(QR_activeE)

Q1_restS <- by(rest, smoke, function(x) quantile(x, 0.25))
Q3_restS <- by(rest, smoke, function(x) quantile(x, 0.75))
QR_restS <- Q3_restS - Q1_restS
print(QR_restS)

Q1_restG <- by(rest, gender, function(x) quantile(x, 0.25))
Q3_restG <- by(rest, gender, function(x) quantile(x, 0.75))
QR_restG <- Q3_restG - Q1_restG
print(QR_restG)

Q1_restE <- by(rest, exercise, function(x) quantile(x, 0.25))
Q3_restE <- by(rest, exercise, function(x) quantile(x, 0.75))
QR_restE <- Q3_restE - Q1_restE
print(QR_restE)

#koefinsi variasi

#kv <- function(x) {
#  cv <- sd(x) / mean(x) * 100
#  return(cv)
#}

cv_hgt <- by(hgt, gender, function(x) {sd(x) / mean(x) * 100})
cv_wgt <- by(wgt, gender, function(x) {sd(x) / mean(x) * 100})
print(cv_hgt)
print(cv_wgt)

cv_activeS <- by(active, gender, function(x) {sd(x) / mean(x) * 100})
cv_activeG <- by(active, gender, function(x) {sd(x) / mean(x) * 100})
cv_activeE <- by(active, gender, function(x) {sd(x) / mean(x) * 100})
print(cv_activeS)
print(cv_activeG)
print(cv_activeE)

cv_restS <- by(rest, gender, function(x) {sd(x) / mean(x) * 100})
cv_restG <- by(rest, gender, function(x) {sd(x) / mean(x) * 100})
cv_restE <- by(rest, gender, function(x) {sd(x) / mean(x) * 100})
print(cv_restS)
print(cv_restG)
print(cv_restE)


#D. histogram

#res <- as.numeric(hgt)

hist(active[smoke == "1"], xlab = "Activity", col = "blue", border = "white", main = "Histogram - Smoker Activity")
hist(active[smoke == "0"], xlab = "Activity", col = "red", border = "white", main = "Histogram - Non-smoker Activity")
hist(active[gender == "1"], xlab = "Activity", col = "cyan", border = "black", main = "Histogram - Male Activity")
hist(active[gender == "0"], xlab = "Activity", col = "pink", border = "black", main = "Histogram - Female Activity")
hist(active[exercise == "1"], xlab = "Activity", col = "white", border = "black", main = "Histogram - Exerciser lvl 1 Activity")
hist(active[exercise == "2"], xlab = "Activity", col = "grey", border = "black", main = "Histogram - Exerciser lvl 2 Activity")
hist(active[exercise == "3"], xlab = "Activity", col = "black", border = "white", main = "Histogram - Exerciser lvl 3 Activity")

hist(rest[smoke == "1"], xlab = "Rest Time", col = "blue", border = "white", main = "Histogram - Smoker Rest Time")
hist(rest[smoke == "0"], xlab = "Rest Time", col = "red", border = "white", main = "Histogram - Non-smoker Rest Time")
hist(rest[gender == "1"], xlab = "Rest Time", col = "cyan", border = "black", main = "Histogram - Male Rest Time")
hist(rest[gender == "0"], xlab = "Rest Time", col = "pink", border = "black", main = "Histogram - Female Rest Time")
hist(rest[exercise == "1"], xlab = "Rest Time", col = "white", border = "black", main = "Histogram - Exerciser lvl 1 Rest Time")
hist(rest[exercise == "2"], xlab = "Rest Time", col = "grey", border = "black", main = "Histogram - Exerciser lvl 2 Rest Time")
hist(rest[exercise == "3"], xlab = "Rest Time", col = "black", border = "white", main = "Histogram - Exerciser lvl 3 Rest Time")

hist(hgt[smoke == "1"], xlab = "Height", col = "blue", border = "white", main = "Histogram - Smoker Heights")
hist(hgt[smoke == "0"], xlab = "Height", col = "red", border = "white", main = "Histogram - Non-smoker Heights")
hist(hgt[gender == "1"], xlab = "Height", col = "cyan", border = "black", main = "Histogram - Male Heights")
hist(hgt[gender == "0"], xlab = "Height", col = "pink", border = "black", main = "Histogram - Female Heights")
hist(hgt[exercise == "1"], xlab = "Height", col = "white", border = "black", main = "Histogram - Exerciser lvl 1 Heights")
hist(hgt[exercise == "2"], xlab = "Height", col = "grey", border = "black", main = "Histogram - Exerciser lvl 2 Heights")
hist(hgt[exercise == "3"], xlab = "Height", col = "black", border = "white", main = "Histogram - Exerciser lvl 3 Heights")

hist(wgt[smoke == "1"], xlab = "Weight", col = "blue", border = "white", main = "Histogram - Smoker Weights")
hist(wgt[smoke == "0"], xlab = "Weight", col = "red", border = "white", main = "Histogram - Non-smoker Weights")
hist(wgt[gender == "1"], xlab = "Weight", col = "cyan", border = "black", main = "Histogram - Male Weights")
hist(wgt[gender == "0"], xlab = "Weight", col = "pink", border = "black", main = "Histogram - Female Weights")
hist(wgt[exercise == "1"], xlab = "Weight", col = "white", border = "black", main = "Histogram - Exerciser lvl 1 Weights")
hist(wgt[exercise == "2"], xlab = "Weight", col = "grey", border = "black", main = "Histogram - Exerciser lvl 2 Weights")
hist(wgt[exercise == "3"], xlab = "Weight", col = "black", border = "white", main = "Histogram - Exerciser lvl 3 Weights")



#5. menggunakan data bears2.csv

data2 <- read.csv("bears.csv")

age <- data2 [, 1]
month <- data2 [, 2]
sex <- data2 [, 3]
headlen <- data2 [, 4]
headwth <- data2 [, 5]
neck <- data2 [, 6]
length <- data2 [, 7]
chest <- data2 [, 8]
weight <- data2 [, 9]


#A. summary variabel Head.L, Head.W, Neck.G, Chest.G, dan Weight.

summary(data2)

Head.L <- summary(headlen)
print(Head.L)

Head.W <- summary(headwth)
print(Head.W)

Neck.G <- summary(neck)
print(Neck.G)

Chest.G <- summary(chest)
print(Chest.G)

Weights <- summary(weight)
print(Weights)


#B. matrix plot variabel Head.L, Head.W, Neck.G, Chest.G, dan Weight.

plot(x = headlen, xlim = c(0,55), ylim = c(8,18), main = "Matrix Plot - Head.L Variable")
plot(x = headwth, xlim = c(0,55), ylim = c(4,10), main = "Matrix Plot - Head.W Variable")
plot(x = neck, xlim = c(0,55), ylim = c(10,32), main = "Matrix Plot - Neck.G Variable")
plot(x = chest, xlim = c(0,55),  ylim = c(19,55), main = "Matrix Plot - Chest.G Variable")
plot(x = weight, xlim = c(0,55), ylim = c(26,534), main = "Matrix Plot - Weight Variable")
