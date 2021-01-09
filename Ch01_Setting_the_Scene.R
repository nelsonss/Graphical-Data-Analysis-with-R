# Ch01 Setting the Scene
#   http://www.gradaanwr.net/content/ch01/
# install.packages("GDAdata")
## Graphics in action
# -------------------------------------------------------------------------------------------------
library(ggplot2)
data(SpeedSki, package = "GDAdata")
ggplot(SpeedSki, aes(x=Speed, fill=Sex)) + xlim(160, 220) +
  geom_histogram(binwidth=2.5) + xlab("Speed (km/hr)") +
  facet_wrap(~Sex, ncol=1) + ylab("") +
  theme(legend.position="none")
# -------------------------------------------------------------------------------------------------
ggplot(SpeedSki, aes(Speed, fill=Sex)) +
  geom_histogram(binwidth=2.5) + xlab("Speed (km/hr)") +
  ylab("") + facet_grid(Sex~Event) +
  theme(legend.position="none")
# -------------------------------------------------------------------------------------------------
# install.packages("ggthemes")
library(ggthemes)
ggplot(iris, aes(Petal.Length, Petal.Width, color=Species)) +
  geom_point() + theme(legend.position="bottom") +
  scale_colour_colorblind()
# -------------------------------------------------------------------------------------------------
# install.packages("gridExtra")
library(gridExtra)
ucba <- as.data.frame(UCBAdmissions)
a <- ggplot(ucba, aes(Dept)) + geom_bar(aes(weight=Freq))
b <- ggplot(ucba, aes(Gender)) + geom_bar(aes(weight=Freq))
c <- ggplot(ucba, aes(Admit)) + geom_bar(aes(weight=Freq))
grid.arrange(a, b, c, nrow=1, widths=c(7,3,3))
# -------------------------------------------------------------------------------------------------
# install.packages("vcd")
library(vcd)
ucb <- data.frame(UCBAdmissions)
ucb <- within(ucb, Accept <- 
                factor(Admit, levels=c("Rejected", "Admitted")))
doubledecker(xtabs(Freq~ Dept + Gender + Accept, data = ucb),
             gp = gpar(fill = c("grey90", "steelblue")))
# -------------------------------------------------------------------------------------------------
library(dplyr)
PimaV <- select(Pima.tr2, glu:age)
par(mar=c(3.1, 4.1, 1.1, 2.1))
boxplot(scale(PimaV), pch=16, outcol="red")
# -------------------------------------------------------------------------------------------------
library(GGally)
ggpairs(PimaV, diag=list(continuous=’density’),
        axisLabels=’show’)



