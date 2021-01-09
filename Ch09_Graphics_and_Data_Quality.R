# Ch09 Graphics and Data Quality
#   http://www.gradaanwr.net/content/ch09/
# ----------------------------------------------------------------------------------------------------------------
# Visualising patterns of missing values

# ----------------------------------------------------------------------------------------------------------------
library(tidyr)
# install.packages("TeachingDemos")
data(USCrimes, package="TeachingDemos")
names(dimnames(USCrimes)) <- c("State", "Year", "Crime")
US10 <- USCrimes %>% as.table %>%
  as.data.frame(responseName = "Rate") %>%
  filter(Year==2010 & State != "United States-Total") %>%
  spread(Crime, Rate) %>% select(State, ends_with("Rate"))

# ----------------------------------------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
diam1 <- diamonds %>% select(carat, depth:z) %>%
  gather(dX, dV, carat:z)
ggplot(diam1, aes("dX", dV)) + geom_boxplot() +
  facet_wrap(~dX, scales = "free_y", nrow=1) +
  xlab("") + ylab("") + scale_x_discrete(breaks=NULL)

# ----------------------------------------------------------------------------------------------------------------
library(gridExtra)
a2 <- ggplot(diamonds, aes(y, z)) + geom_point() +
  xlab("width") + ylab("depth")
d2 <- filter(diamonds, y > 2 & y < 11 & z > 1 & z < 7)
b2 <- ggplot(d2, aes(y, z)) + geom_point() +
  xlab("width") + ylab("depth")
grid.arrange(a2, b2, ncol=2)
# ----------------------------------------------------------------------------------------------------------------
a <- ggplot(iris, aes("boxplot for all", Sepal.Width)) +
  xlab("") + geom_boxplot() +
  scale_x_discrete(breaks=NULL)
b <- ggplot(iris, aes(Species, Sepal.Width)) +
  geom_boxplot() + xlab("")
grid.arrange(a, b, nrow=1, widths=c(1,2))



# ----------------------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------------------

