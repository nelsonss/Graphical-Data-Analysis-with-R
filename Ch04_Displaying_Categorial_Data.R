# Ch04 Displaying Categorial Data
#  http://www.gradaanwr.net/content/ch04/
# -----------------------------------------------------------------------------------------------------------------------
data(btw2009, package = "flexclust")
btw2009 <- within (btw2009, stateA <- state)
btw2009 <- within (btw2009,
                   levels(stateA) <- c("BW", "BY", "BE",
                                       "BB", "HB", "HH", "HE", "MV", "NI", "NW",
                                       "RP", "SL", "SN", "ST", "SH", "TH"))
Voters <- with(btw2009, size <- tapply(eligible, stateA, sum))
Bundesland <- rownames(Voters)
btw9s <- data.frame(Bundesland, Voters)
btw9s$EW <- c("West")
btw9s[c("BB", "BE", "MV","SN","ST","TH"), "EW"] <- "East"
ls <- with(btw9s, Bundesland[order(EW, -Voters)])
btw9s <- within(btw9s, State1 <- factor(Bundesland, levels=ls))

b1 <- ggplot(btw9s, aes(Bundesland, Voters/1000000)) +
  geom_bar(stat="identity") +
  ylab("Voters (millions)")
b2 <- ggplot(btw9s, aes(reorder(Bundesland, -Voters),Voters/1000000)) + geom_bar(stat = "identity")  +
  xlab("Bundesland") + ylab("Voters (millions)")
b3 <- ggplot(btw9s, aes(State1, Voters/1000000)) +
  geom_bar(stat="identity") + xlab("Bundesland") + ylab("Voters (millions)")
grid.arrange(b1, b2, b3)

# -----------------------------------------------------------------------------------------------------------------------
Titanic1 <- data.frame(Titanic)
p <- ggplot(Titanic1, aes(weight=Freq)) +
  ylab("") + ylim(0,2250)
cs <- p + aes(Class) + geom_bar(fill="blue")
sx <- p + aes(Sex) + geom_bar(fill="green")
ag <- p + aes(Age) + geom_bar(fill="tan2")
su <- p + aes(Survived) + geom_bar(fill="red")
grid.arrange(cs, sx, ag, su, nrow=1, widths=c(3, 2, 2, 2))

# -----------------------------------------------------------------------------------------------------------------------
data(UKSoccer, package="vcd")
PL <- data.frame(UKSoccer)
lx <- c("0","1","2","3","4 or more")
b1 <- ggplot(PL, aes(x=factor(Home), weight=Freq)) +
  geom_bar(fill="firebrick1") +
  ylab("") + xlab("Home Goals")  +
  scale_x_discrete(labels=lx) +  ylim(0,150)
b2 <- ggplot(PL, aes(x=factor(Away), weight=Freq)) +
  geom_bar(fill="cyan1") +
  ylab("") + xlab("Away Goals")  +
  scale_x_discrete(labels=lx) + ylim(0,150)
grid.arrange(b1, b2, nrow=1)

# -----------------------------------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------------------------------------