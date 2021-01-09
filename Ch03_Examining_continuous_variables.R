# Ch03 Examining continuous variables
#  http://www.gradaanwr.net/content/ch03/
# install.packages("flexclust")
# -------------------------------------------------------------------------------------------------
data(btw2009, package = "flexclust")
btw2009 <- within(btw2009, Linke2 <- 100*LINKE2/valid2)
ggplot(btw2009, aes(Linke2))+
  geom_bar(binwidth = 1, fill = "mediumpurple") + ylab("") +
  xlab("Percentage voter support for Die Linke in 2009")
# -------------------------------------------------------------------------------------------------
# install.packages("UsingR")
data(galton, package="UsingR")
ht <- "height (in)"
par(mfrow=c(1,2), las=1, mar=c(3.1, 4.1, 1.1, 2.1))
with(galton, {
  hist(child, xlab=ht, main="Children", col="green")
  hist(parent, xlab=ht, main="Parents", col="blue")
})
# -------------------------------------------------------------------------------------------------
c1 <- ggplot(galton, aes(child)) + geom_bar( binwidth=1) +
  xlim(60, 75) + ylim(0, 225) + ylab("") + 
  geom_vline(xintercept = median(galton$child),col = "red")
p1 <- ggplot(galton, aes(parent)) + geom_bar( binwidth=1) +
  xlim(60, 75) + ylim(0, 225) + ylab("") +
  geom_vline(xintercept=median(galton$parent), col="red")
grid.arrange(c1, p1)
# -------------------------------------------------------------------------------------------------
data(father.son, package="UsingR")
c2 <- ggplot(father.son, aes(sheight)) + 
  geom_histogram(aes(y = ..density..), binwidth=1) +
  geom_density() + xlim(58, 80) + ylim(0, 0.16) +
  xlab("ht (inches)") + ylab("") + ggtitle("Sons")
p2 <- ggplot(father.son, aes(fheight)) + 
  geom_histogram(aes(y = ..density..), binwidth=1) +
  geom_density() + xlim(58, 80) + ylim(0, 0.16) +
  xlab("ht (inches)") + ylab("") +  ggtitle("Fathers")
grid.arrange(c2, p2, nrow = 1)
# -------------------------------------------------------------------------------------------------
with(father.son, {
  qqnorm(sheight, main="Sons", xlab="",
         ylab="", pch=16, ylim=c(55,80))
  qqline(sheight)
  qqnorm(fheight, main="Fathers", xlab="",
         ylab="", pch=16, ylim=c(55,80))
  qqline(fheight)})
# -------------------------------------------------------------------------------------------------
ggplot(MASS::Boston, aes(medv)) + geom_bar() + ylab("") +
  xlab("Median housing value (thousands of dollars)")
# -------------------------------------------------------------------------------------------------
library(tidyr)
B2 <- gather(MASS::Boston, BosVars, BosValues, crim:medv)
ggplot(B2, aes(BosValues)) + geom_histogram() + 
  xlab("") +  ylab("") + 
  facet_wrap(~ BosVars, scales = "free")
# -------------------------------------------------------------------------------------------------
btw2009 <- within(btw2009, Bundesland <- state)
btw2009 <- within(btw2009, levels(Bundesland) <- c("BW", "BY", "BE", "BB", "HB", "HH", "HE", "MV", "NI", "NW","RP", "SL", "SN", "ST", "SH", "TH"))
ggplot(btw2009, aes(Bundesland, Linke2)) + geom_boxplot(varwidth=TRUE) + ylab("")
# -------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------


