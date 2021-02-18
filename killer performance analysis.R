rm(list=ls())

library(mgcv)
library(ggplot2)
library(chron)
library(dplyr)
library(plyr)

# read file
kpm <- read.csv("21-02-18 - kpm.csv", stringsAsFactors=FALSE)
kpm$notes <- NULL
head(kpm)

# convert the date data
kpm$date <- as.Date(kpm$date, "%Y-%m-%d")
kpm$numdate <- as.numeric(kpm$date)
kpm$dayofweek <- format(kpm$date, "%u")
kpm$weekend <- factor(kpm$dayofweek %in% c("6","7"))

# convert the time data
kpm$time <- paste(kpm$time, "00", sep=":")
kpm$time <- chron(times=kpm$time)
kpm$numtime <- as.numeric(kpm$time)

ggplot(kpm) + geom_histogram(aes(x=numtime))

# make sure the factors are factors
kpm$killer <- factor(kpm$killer)
kpm$crossplay <- factor(kpm$crossplay)

# cull killers with small counts
killercount <- dplyr::summarize(group_by(kpm, killer),
                                killercount=n())
kpm <- left_join(kpm, as.data.frame(killercount),
                 by="killer")
kpm <- kpm[kpm$killercount >= 5,]

# define covariates to analyze
kpm$loss <- 1*(kpm$kills <= 2)
kpm$win  <- 1*(kpm$kills >= 3)

# perks
perklist <- sort(unique(c(kpm$perk1, kpm$perk2, kpm$perk3, kpm$perk4)))
perklist

# define perk types
perktypes <- list("slowdown"=c("Oppression",
                               "Pop",
                               "Ruin",
                               "Sloppy Butcher",
                               "Surge",
                               "Thanatophobia"),
                  "chase"=c("Bamboozle",
                            "Brutal",
                            "Enduring",
                            "I'm All Ears",
                            "NOED",
                            "Predator",
                            "Save the Best for Last",
                            "Spirit Fury",
                            "Stridor",
                            "Unrelenting"),
                  "hex"=c("Ruin",
                          "Devour Hope",
                          "Undying",
                          "Thrill of the Hunt"),
                  "information"=c("A Nurse's Calling",
                                  "BBQ",
                                  "Discordance",
                                  "Spies from the Shadows",
                                  "Surveillance",
                                  "Thrilling Tremors",
                                  "Whispers"))
                     
for (curtype in names(perktypes)) {
  
  kpm[,paste("perks",
             curtype,
             sep="_")] <- 1*(kpm$perk1 %in% perktypes[[curtype]]) +
                          1*(kpm$perk2 %in% perktypes[[curtype]]) +
                          1*(kpm$perk3 %in% perktypes[[curtype]]) +
                          1*(kpm$perk4 %in% perktypes[[curtype]])

}

# calculate a princomp of all these, since they correlate
perkprincomp <- princomp(kpm[grep(pattern="perks_",
                                  x=colnames(kpm),
                                  fixed=TRUE)],
                         scores=TRUE)
kpm$perkprincomp <- perkprincomp$scores

# # some plots
# ggplot(kpm) + geom_histogram(aes(x=bloodpoints), bins=10) +
#   facet_wrap(~killer, ncol=1, scales="free_y") +
#   ggtitle("bloodpoints per killer")
# ggplot(kpm) + geom_histogram(aes(x=bloodpoints), bins=10) +
#   facet_wrap(~kills, ncol=1, scales="free_y") +
#   ggtitle("bloodpoints by kill count")

# run a model on bloodpoints
bpmodel <- gam(bloodpoints ~ killer + perkprincomp + crossplay + s(numtime, bs="cc"),
               data=kpm)
summary(bpmodel)
plot(bpmodel, se=FALSE, select=1)
# myplots <- plot.gam(bpmodel, se=FALSE, select=0)
# tempdf1 <- data.frame(x = myplots[[1]]$x,
#                       y = myplots[[1]]$fit,
#                       weekend=myplots[[1]]$ylab)
# tempdf2 <- data.frame(x = myplots[[2]]$x,
#                       y = myplots[[2]]$fit,
#                       weekend=myplots[[2]]$ylab)
# tempdf <- bind_rows(tempdf1, tempdf2)
# ggplot(tempdf) + geom_line(aes(x=x, y=y, group=weekend, color=weekend)) +
#   ggtitle("bloodpoint model, cyclical term")



# run a model on losses
winmodel <- gam(win ~ killer + perkprincomp + s(numtime, bs="cc"),
                data=kpm,
                family=binomial())
summary(winmodel)
anova(winmodel)
plot.gam(winmodel, se=FALSE, select=1)
# myplots <- plot.gam(winmodel, se=FALSE, select=0)
# tempdf1 <- data.frame(x = myplots[[1]]$x,
#                       y = myplots[[1]]$fit,
#                       weekend=myplots[[1]]$ylab)
# tempdf2 <- data.frame(x = myplots[[2]]$x,
#                       y = myplots[[2]]$fit,
#                       weekend=myplots[[2]]$ylab)
# tempdf <- bind_rows(tempdf1, tempdf2)
# ggplot(tempdf) + geom_line(aes(x=x, y=y, group=weekend, color=weekend)) +
#   ggtitle("win rate model, cyclical term")

# # do a quick plot on win rates per deci
# kpm$decnumtime <- round_any(kpm$numtime, 0.10)
# decsummary <- dplyr::summarize(group_by(kpm, decnumtime),
#                                win=mean(win, na.rm=TRUE))
# ggplot(decsummary) + geom_line(aes(x=decnumtime, win))
killsbykiller <- dplyr::summarize(group_by(kpm, killer, kills),
                                  killcount=n())
tempdf <- dplyr::summarise(group_by(kpm, killer),
                           totalkills=n())
killsbykiller <- left_join(killsbykiller, tempdf, by="killer")
killsbykiller$killfreq <- killsbykiller$killcount / killsbykiller$totalkills

ggplot(killsbykiller) + geom_bar(aes(x=kills, y=killfreq), stat="identity") +
  facet_wrap(~killer, ncol=1) +
  ylab("frequency")

kbk2 <- dplyr::summarize(group_by(kpm, killer),
                         meankills = mean(kills, na.rm=TRUE)/4,
                         n=n())
kbk2

ggplot(kpm) + geom_boxplot(aes(x=kills, y=bloodpoints, group=kills))
ggplot(kpm) + geom_boxplot(aes(x=killer, y=bloodpoints, group=killer))

# view princomp loadings for interpretation
princoefs <- as.data.frame(coef(winmodel))
princoefs$coef <- rownames(princoefs)
princoefs <- princoefs[grep(x=princoefs$coef,
                            pattern="perkprincomp",
                            fixed=TRUE),]
names(princoefs) <- c("est","coef")
perkprincomp$loadings %*% princoefs$est

# perform a salt model
kpm$anysalt <- factor(1*(kpm$salt > 0))
kpm$NOED <- 1*(kpm$perk1 == "NOED") + 1*(kpm$perk2 == "NOED") + 1*(kpm$perk3 == "NOED") + 1*(kpm$perk4 == "NOED")
saltmodel <- glm(anysalt ~ crossplay + NOED,
                 family=binomial(),
                 data=kpm)
summary(saltmodel)