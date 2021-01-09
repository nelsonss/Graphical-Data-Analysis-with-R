# Ch10 Comparisons
#   http://www.gradaanwr.net/content/ch10/
# ----------------------------------------------------------------------------------------------------------------
#  install.packages("gclus")
library(ggplot2)
library(gridExtra)
data(bank, package="gclus")
bank <- within(bank, st <- ifelse(Status==0, "genuine", "forgery"))
c1 <- ggplot(bank, aes(x=Diagonal)) +
        geom_histogram(binwidth=0.2) + facet_grid(st~.)
c2 <- ggplot(bank, aes(x=Right)) +
        geom_histogram(binwidth=0.1) + facet_grid(st~.)
grid.arrange(c1, c2, ncol=2)

# ----------------------------------------------------------------------------------------------------------------
data(Michelson, package="HistData")
tc <- t.test(Michelson, mu=734.5)

# ----------------------------------------------------------------------------------------------------------------

ggplot(Michelson, aes(x=velocity)) + geom_bar(binwidth = 25) +
  geom_vline(xintercept = 734.5, colour="red", linetype = "longdash") +
  xlab("Speed of light in kms/sec (less 299,000)")

# ----------------------------------------------------------------------------------------------------------------

data(Cars93, package="MASS")
c1 <- ggplot(mtcars, aes(mpg)) + geom_bar(fill="blue") +
  xlim(10,50) + xlab("mpg for 32 cars from 1973-4")
c2 <- ggplot(Cars93, aes(MPG.city)) +
  geom_bar(fill="red") + xlim(10,50) +
  xlab("mpg in city driving for 93 cars from 1993")
grid.arrange(c1, c2, nrow=2)

# ----------------------------------------------------------------------------------------------------------------
tf <- t.test(Cars93$MPG.city, mtcars$mpg)
tf

# ----------------------------------------------------------------------------------------------------------------
require(ggplot2)
data(EastIndiesTrade,package="GDAdata")
c1 <- ggplot(EastIndiesTrade, aes(x=Year, y=Exports)) +
  ylim(0,2000) + geom_line(colour="red", size=2) +
  geom_line(aes(x=Year, y=Imports),
            colour="yellow", size=2) +
  geom_ribbon(aes(ymin=Exports, ymax=Imports),
              fill="pink",alpha=0.5) +
  ylab("Exports(red) and Imports(yellow)")
c2 <- ggplot(EastIndiesTrade, aes(x=Year,
                                  y=Exports-Imports)) + geom_line(colour="green")
c3 <- ggplot(EastIndiesTrade, aes(x=Year,
                                  y=(Exports-Imports)/((Exports + Imports)/2))) +
  geom_line(colour="blue")
grid.arrange(c1, c2, c3, nrow=3)
# ----------------------------------------------------------------------------------------------------------------
# Comparing group effects graphically

data(barley, package="lattice")
ggplot(barley, aes(yield)) + geom_histogram(binwidth=5) +
  ylab("") + facet_wrap(~year, ncol=1)

# ----------------------------------------------------------------------------------------------------------------

c1 <- ggplot(barley, aes(x=variety, y=yield)) +
      geom_point() + ylim(10,70)
barl1 <- barley %>% group_by(variety) %>%
          summarise(N = n(), mean = mean(yield), sd = sd(yield), se = sd/sqrt(N))
lims <- aes(ymax = mean + 2*se, ymin=mean - 2*se)
p1 <- ggplot(barl1, aes(x=variety, y=mean)) +
        geom_point() + ylim(10,70) +
        geom_errorbar(lims, width=0.2)
grid.arrange(c1, p1)

# ----------------------------------------------------------------------------------------------------------------

barl2 <- barley %>%
  mutate(Year = factor(year,levels = c("1931", "1932"))) %>%
  group_by(site, Year) %>%
  summarise(N = n(), mean = mean(yield), sd = sd(yield), se = sd/sqrt(N))
lims <- aes(ymax = mean + 2*se, ymin=mean - 2*se)
ggplot(barl2, aes(colour=Year, x=site, y=mean)) +
  geom_point() + geom_errorbar(lims, width=0.2) +
  ylim(10,70) + theme(legend.position = "bottom")

# ----------------------------------------------------------------------------------------------------------------

m1 <- lm(yield~site+year+variety, data=barley)
library(coefplot)
coefplot(m1, predictors="variety", lwdOuter=1) + ggtitle("") +
  ylab("") + xlab("yield difference from Svansota")

# ----------------------------------------------------------------------------------------------------------------





# install.packages("xlsx")
#   http://www.sthda.com/english/wiki/writing-data-from-r-to-excel-files-xls-xlsx
library("xlsx")
# Write the first data set in a new workbook
write.xlsx(barley, file = "barley.xlsx", sheetName = "barley", append = FALSE)
# Add a second data set in a new worksheet
write.xlsx(mtcars, file = "myworkbook.xlsx", sheetName="MTCARS", append=TRUE)
# Add a third data set
write.xlsx(iris, file = "myworkbook.xlsx", sheetName="IRIS", append=TRUE)

# ----------------------------------------------------------------------------------------------------------------
# Writing Data From R to txt|csv Files: R Base Functions
# http://www.sthda.com/english/wiki/writing-data-from-r-to-txt-csv-files-r-base-functions
# Loading mtcars data
data("mtcars")
# Writing mtcars data
write.table(mtcars, file = "mtcars.txt", sep = "\t",row.names = TRUE, col.names = NA)
# write.csv() uses “.” for the decimal point and a comma (“,”) for the separator.
# write.csv2() uses a comma (“,”) for the decimal point and a semicolon (“;”) for the separator.

# write.csv(my_data, file = "my_data.csv")
write.csv2(mtcars, file = "mtcars.csv")

# ----------------------------------------------------------------------------------------------------------------
# Installing
require("readr")
# Loading
library("readr")

# General function
write_delim(x, path, delim = " ")
# Write comma (",") separated value files
write_csv(file, path)
# Write tab ("\t") separated value files
write_tsv(file, path)

# x: a data frame to be written
# path: path to the result file
# delim: Delimiter used to separate values. Must be single character.

data("mtcars")
library("readr")
# Writing mtcars data to a tsv file
write_tsv(mtcars, path = "mtcars.txt")
# Writing mtcars data to a csv file
write_csv(mtcars, path = "mtcars.csv")

# ----------------------------------------------------------------------------------------------------------------



