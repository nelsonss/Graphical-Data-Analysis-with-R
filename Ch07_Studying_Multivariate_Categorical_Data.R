# Ch07 Studying Multivariate Categorical Data
#  http://www.gradaanwr.net/content/ch07/
# ----------------------------------------------------------------------------------------------------------------
library(vcdExtra)
data(Alligator, package="vcdExtra")
Alg1a <- aggregate(count~food, data=Alligator, sum)
ggplot(Alg1a, aes(food, count)) + geom_bar(stat="identity")
# ----------------------------------------------------------------------------------------------------------------
data(Alligator, package="vcdExtra")
Alg1 <- Alligator
levels(Alg1$lake) <- abbreviate(levels(Alg1$lake), 3)
levels(Alg1$size) <- abbreviate(levels(Alg1$size), 3)
levels(Alg1$food) <- abbreviate(levels(Alg1$food), 2)
par(mfrow=c(2,2), mar=c(4 ,4, 0.1, 0.1))
mosaicplot(xtabs(count ~ lake, data=Alg1), main="")
mosaicplot(xtabs(count ~ lake + sex, data=Alg1), main="")
mosaicplot(xtabs(count ~ lake + sex + size, data=Alg1),
           main="")
mosaicplot(xtabs(count ~ lake + sex + size + food, data=Alg1),
           main="")
pairs(xtabs(count ~ ., Alligator))
# ----------------------------------------------------------------------------------------------------------------
doubledecker(xtabs(count ~ lake + sex, data = Alligator),
             gp = gpar(fill = c("grey90", "steelblue")))
doubledecker(xtabs(count ~ food + size, data = Alligator),
             gp = gpar(fill = c("grey90", "tomato")))
# ----------------------------------------------------------------------------------------------------------------
Alg2 <- aggregate(count~sex + food + size, data=Alligator, sum)
ggplot(Alg2, aes(food, count, fill=sex)) +
  geom_bar(stat = "identity") +
  facet_grid(size ~ sex) + theme(legend.position="none")
# ----------------------------------------------------------------------------------------------------------------
ggplot(Alg1, aes(food, count, fill=sex)) +
  geom_bar(stat = "identity") +
  facet_grid(lake ~ sex + size) +
  theme(legend.position="none")
# ----------------------------------------------------------------------------------------------------------------
# install.packages("extracat")
library(extracat)
data(housing, package="MASS")
rmb(formula = ~Type+Infl+Cont+Sat, data = housing,
    col.vars = c(FALSE,TRUE,TRUE,FALSE),
    label.opt = list(abbrev = 3, yaxis=FALSE))
# ----------------------------------------------------------------------------------------------------------------