# Ch08 Getting an Overview
#  http://www.gradaanwr.net/content/ch08/
# ----------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(reshape2)
# install.packages("Ecdat")
data(HI, package="Ecdat")
str(HI)
HIvs <- c("whrswk", "experience", "husby", "wght")
HIs <- melt(HI[, HIvs], value.name = "HIx", variable.name = "HIvars")
ggplot(HIs, aes(HIx)) + geom_histogram() +
  facet_wrap(~ HIvars, scales = "free") +
  xlab("") + ylab("")
# ----------------------------------------------------------------------------------------------------------------

uniqv <- function(x) length(unique(x)) < 20
vcs <- names(HI)[sapply(HI, uniqv)]
par(mfrow = n2mfrow(length(vcs)))
relativeWeight <- with(HI, wght/sum(as.numeric(wght))*100)
for(v in vcs) 
  barplot(tapply(relativeWeight, HI[[v]], sum), main = v)
# ----------------------------------------------------------------------------------------------------------------

data(Boston, package="MASS")
par(mfrow=c(1,2))
for (i in c("chas", "rad")) {
  barplot(table(Boston[, i]),
          main=(paste("Barchart of", i)))
}

# ----------------------------------------------------------------------------------------------------------------

vs1 <- !(names(Boston) %in% c("chas","rad"))
grs <- n2mfrow(sum(as.numeric(vs1)))
par(mfrow=grs)
for (i in names(Boston)[vs1]) {
  hist(Boston[,i], col="grey70", xlab="", ylab="",
       main=(paste("Histogram of", i)))
}

# ----------------------------------------------------------------------------------------------------------------

plot(Boston, pch=16)

# ----------------------------------------------------------------------------------------------------------------
par(mar=c(1.1, 1.1, 1.1, 1.1))
palette(rainbow(14, s = 0.6, v = 0.75))
stars(Boston[1:4,], labels=NULL, draw.segments = TRUE)

# ----------------------------------------------------------------------------------------------------------------
stars(Boston, labels=NULL, draw.segments = TRUE)
# ----------------------------------------------------------------------------------------------------------------
library(lattice)
data(barley, package="lattice")
dotplot(site ~ yield |variety , data = barley,
        groups = year, columns=2, pch=16, col=c("red","blue"),
        key = list(text=list(levels(barley$year)),
                   points = list(pch=16, col=c("red", "blue"))),
        xlab = "Barley Yield (bushels/acre) ", ylab=NULL,
        main="Barley Yields by Site for ten Varieties")
# ----------------------------------------------------------------------------------------------------------------

data(uniranks, package="GDAdata")
names(uniranks)[c(5, 6, 8, 9, 10, 11, 13)] <- c("AvTeach",
                                                "NSSTeach", "SpendperSt", "StudentStaffR",
                                                "Careers", "VAddScore", "NSSFeedb")
ur2 <- melt(uniranks[, c(3, 5:13)], id.vars="UniGroup",
            variable.name="uniV", value.name="uniX")
ggplot(ur2, aes(uniX)) + geom_histogram() + xlab("") +
  ylab("") + facet_grid(UniGroup~uniV, scales = "free_x")

# ----------------------------------------------------------------------------------------------------------------














