# Ch05 Looking for Structure
#  http://www.gradaanwr.net/content/ch05/
# install.packages("VGAMdata")
# -----------------------------------------------------------------------------------------------------------------------
data(oly12, package="VGAMdata")
ggplot(oly12, aes(Height, Weight)) + geom_point() +
  ggtitle("Athletes at the London Olympics 2012")
# -----------------------------------------------------------------------------------------------------------------------
data(geyser, package="MASS")
ggplot(geyser, aes(duration, waiting)) + geom_point()
ggplot(geyser, aes(duration, waiting)) + geom_point() +
  geom_density2d()

# -----------------------------------------------------------------------------------------------------------------------
# install.packages("hdrcde")
library(hdrcde)
par(mar=c(3.1, 4.1, 1.1, 2.1))
with(geyser, hdr.boxplot.2d(duration, waiting, show.points=TRUE, prob=c(0.01,0.05,0.5,0.75)))

# -----------------------------------------------------------------------------------------------------------------------
data(Cars93, package="MASS")
ggplot(Cars93, aes(Weight, MPG.city)) + geom_point() +
  geom_smooth(colour="green") + ylim(0,50)
                                     
# -----------------------------------------------------------------------------------------------------------------------
data(father.son, package="UsingR")
ggplot(father.son, aes(fheight, sheight)) + geom_point() +
  geom_smooth(method="lm", colour="red") +
  geom_abline(slope=1, intercept=0)
# -----------------------------------------------------------------------------------------------------------------------
oly12S <- within(oly12, Sport <- abbreviate(Sport, 12))
ggplot(oly12S, aes(Height, Weight)) +
  geom_point(size = 1) + facet_wrap(~Sport) +
  ggtitle("Weight and Height by Sport")

# -----------------------------------------------------------------------------------------------------------------------
library(GGally)
data(crime.us, package="VGAMdata")
crime.usR <- crime.us
names(crime.usR) <- gsub("*Rate", "", names(crime.usR))
names(crime.usR)[19:20] <- c("Larceny", "MotorVTheft")
ggpairs(crime.usR[, c(13:16, 18:20)], title="Crime rates in the USA", diag=list(continuous="density"), axisLabels="none")

# -----------------------------------------------------------------------------------------------------------------------
# install.packages("car")
# install.packages("gclus")
library(car)
data(bank, package="gclus")
par(mar=c(1.1, 1.1, 1.1, 1.1))
spm(select(bank, Length:Diagonal), pch=c(16, 16),
    diagonal="histogram", smoother=FALSE,
    reg.line=FALSE, groups=bank$Status)

# -----------------------------------------------------------------------------------------------------------------------



# -----------------------------------------------------------------------------------------------------------------------



# -----------------------------------------------------------------------------------------------------------------------
