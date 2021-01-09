# Ch06 Investigating Multivariate Continuous Data
# install.packages("agridat")
library(reshape2); data(nass.corn, package="agridat")
c1 <- melt(nass.corn, id=c("year", "state"))
c1 <- within(c1, StateV <- interaction(state, variable))
c2 <- dcast(c1, StateV~year)
ggparcoord(subset(c2[1:48,], c2[1:48,147]> 250000), columns=2:147, groupColumn="StateV",
           scale="globalminmax") + xlab("Year") + ylab("Acres") + 
           scale_x_discrete(breaks=seq(1865, 2015, 10)) +
           theme(legend.position = "none")
