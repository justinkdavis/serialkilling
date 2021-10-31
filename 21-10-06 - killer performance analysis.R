rm(list=ls())

require(mgcv)
require(ggplot2)
require(chron)
require(dplyr)
require(plyr)
require(googledrive)
require(zoo)

# install.packages(pkgs=c("mgcv",
#                         "ggplot2",
#                         "chron",
#                         "dplyr",
#                         "plyr"))




kpm <- drive_download(file=as_id("1a541yFoA2E1xGQEXwhC2Q1x8lZLxFMI1r5FDadc2Hmk"),
                      path="C:\\work\\serialkilling\\kpm data.csv",
                      type="csv",
                      overwrite=TRUE)
spm <- drive_download(file=as_id("15c4YdlPEaj7H8z1PbatOy30t6tGzv-wx7VBulue4hkw"),
                      path="C:\\work\\serialkilling\\spm data.csv",
                      type="csv",
                      overwrite=TRUE)

# read file
kpm <- read.csv("kpm data.csv", stringsAsFactors=FALSE)
kpm$notes <- NULL
head(kpm)
nrow(kpm)

# convert the date data
kpm$date <- as.Date(kpm$date, "%Y-%m-%d")
kpm$numdate <- as.numeric(kpm$date)
kpm$dayofweek <- format(kpm$date, "%u")
kpm$dayofmonth <- as.numeric(format(kpm$date, "%d"))
kpm$dayofweekfactor <- paste("_", kpm$dayofweek, sep="")
kpm$dayofweekfactor <- factor(kpm$dayofweek)
kpm$weekend <- factor(kpm$dayofweek %in% c("6","7"))

# convert the time data
kpm$hour <- floor(kpm$time / 100)
kpm$minute <- kpm$time - 100*kpm$hour
kpm$time <- paste(kpm$hour, kpm$minute, "00", sep=":")
kpm$time <- chron(times=kpm$time)
kpm$numtime <- as.numeric(kpm$time)*24
kpm$numweek <- as.numeric(as.character(kpm$dayofweek)) + kpm$numtime/24 - 1
ggplot(kpm) + geom_histogram(aes(x=numtime))

# calculate time since last match
kpm$numdatetime <- kpm$numdate + kpm$numtime/24
kpm <- kpm[order(kpm$numdatetime),]
kpm$lagdatetime <- 2
for (i in 2:nrow(kpm)) {
  
  kpm$lagdatetime[i] <- kpm$numdatetime[i] - kpm$numdatetime[i-1]
  
}
hist(log(kpm$lagdatetime))
min(kpm$lagdatetime, na.rm=TRUE)
kpm$lagdatetime[kpm$lagdatetime > 2] <- 2
#kpm$lagdatetime[is.infinite(kpm$lagdatetime)] <- NA

# game number
kpm$gamenum <- 1:nrow(kpm)

#kpm <- kpm[kpm$tampered == 0,]

# make sure the factors are factors
# cull killers with small counts
killercount <- dplyr::summarize(group_by(kpm, killer),
                                killercount=n())
print(killercount, n=nrow(killercount))
# retain top five killers
killercount <- killercount[order(killercount$killercount),]
kpm <- left_join(kpm, as.data.frame(killercount),
                 by="killer")
kpm$killer[kpm$killercount < 10] <- "_ok"
#kpm$killer[kpm$killer == "Freddy"] <- "_ok"

# kpm <- kpm[kpm$killer %in% c("Pig",
#                             "Ghostface",
#                             "Spirit"),]

# kpm$killer[!(kpm$killer %in% c("Pig",
#                               "Ghostface"))] <- "_otherkiller"

kpm$killer <- factor(kpm$killer)
kpm$crossplay <- factor(kpm$crossplay)

## temporary pig
#kpm <- kpm[kpm$killer == "Pig",]

# define covariates to analyze
kpm$loss <- 1*(kpm$kills <= 2)
kpm$win  <- 1*(kpm$kills >= 3)

# perks
perklist <- sort(unique(c(kpm$perk1, kpm$perk2, kpm$perk3, kpm$perk4)))
perklist

# get a factor for builds
kpm$build <- ""
for (currow in 1:nrow(kpm)) {
  
  kpm$build[currow] <- paste(sort(c(substr(kpm$perk1[currow], 1, 7),
                                    substr(kpm$perk2[currow], 1, 7),
                                    substr(kpm$perk3[currow], 1, 7),
                                    substr(kpm$perk4[currow], 1, 7))),
                             collapse=":")
}
buildtable <- as.data.frame(table(kpm$build))
names(buildtable) <- c("build", "buildfrequency")
kpm <- left_join(kpm, buildtable, by="build")
#kpm$build[kpm$killer != "Pig"] <- "otherkillerbuild"
kpm$build[(kpm$buildfrequency < 5)] <- "uncommon build"
kpm$build <- factor(kpm$build)
table(kpm$build)

# add one especially for NOED
kpm$NOED <- 1*(kpm$perk1 == "NOED") + 1*(kpm$perk2 == "NOED") + 1*(kpm$perk3 == "NOED") + 1*(kpm$perk4 == "NOED")
kpm$POP <- 1*(kpm$perk1 == "Pop") + 1*(kpm$perk2 == "Pop") + 1*(kpm$perk3 == "Pop") + 1*(kpm$perk4 == "Pop")
kpm$RUIN <- 1*(kpm$perk1 == "Ruin") + 1*(kpm$perk2 == "Ruin") + 1*(kpm$perk3 == "Ruin") + 1*(kpm$perk4 == "Ruin")
kpm$UNDYING <- 1*(kpm$perk1 == "Undying") + 1*(kpm$perk2 == "Undying") + 1*(kpm$perk3 == "Undying") + 1*(kpm$perk4 == "Undying")
kpm$SURGE <- 1*(kpm$perk1 == "Surge") + 1*(kpm$perk2 == "Surge") + 1*(kpm$perk3 == "Surge") + 1*(kpm$perk4 == "Surge")
kpm$slowdown <- kpm$POP + kpm$RUIN + kpm$UNDYING + kpm$SURGE
kpm$CORRUPT <- 1*(kpm$perk1 == "Corrupt") + 1*(kpm$perk2 == "Corrupt") + 1*(kpm$perk3 == "Corrupt") + 1*(kpm$perk4 == "Corrupt")
kpm$tampered <- 1*(kpm$addon1 == "Tampered Timer") + 1*(kpm$addon2 == "Tampered Timer")
kpm$chewed <- 1*(kpm$addon1 == "Chewed Pen") + 1*(kpm$addon2 == "Chewed Pen")


# # some plots
# ggplot(kpm) + geom_histogram(aes(x=bloodpoints), bins=10) +
#   facet_wrap(~killer, ncol=1, scales="free_y") +
#   ggtitle("bloodpoints per killer")
# ggplot(kpm) + geom_histogram(aes(x=bloodpoints), bins=10) +
#   facet_wrap(~kills, ncol=1, scales="free_y") +
#   ggtitle("bloodpoints by kill count")

# run a model on bloodpoints
bpmodel <- gam(bloodpoints ~ 0 + factor(kills) + build + killer + crossplay + s(numtime, bs="cc") + s(gamenum),
               data=kpm)
summary(bpmodel)
plot(bpmodel, se=FALSE, scale=0, select=1)
plot(bpmodel, se=FALSE, scale=0, select=2)
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

# run a model on kills
kpm$kills_plusone <- kpm$kills + 1
killmodel <- gam(kills_plusone ~ 0 + killer + tampered + slowdown + NOED + s(numtime, bs="cc") + s(dayofmonth, bs="cc") + s(gamenum),
                 data=kpm,
                 family=ocat(R = 5))
summary(killmodel)
plot.gam(killmodel, se=FALSE, scale=0, select=1)
plot.gam(killmodel, se=FALSE, scale=0, select=2)

# run a model on clear wins
winmodel <- gam(win ~ 0 + build + killer + platform + crossplay + dayofweek + slowdown + s(numtime, bs="cc") + s(dayofmonth, bs="cc") + s(gamenum),
                data=kpm,
                family=binomial())
summary(winmodel)
anova(winmodel)
plot.gam(winmodel, se=FALSE, scale=0, select=1)
plot.gam(winmodel, se=FALSE, scale=0, select=2)
plot.gam(winmodel, se=FALSE, scale=0, select=3)
plot.gam(winmodel, se=FALSE, scale=0, select=4)

wincoefs <- as.data.frame(coef(winmodel))
wincoefs$variable <- rownames(wincoefs)
names(wincoefs) <- c("est", "var")
wincoefs <- wincoefs[grep(x=wincoefs$var,
                          pattern="build",
                          fixed=TRUE),]
rownames(wincoefs) <- 1:nrow(wincoefs)
wincoefs[order(wincoefs$est),]
table(kpm$build, kpm$killer)
tempdf <- dplyr::summarise(group_by(kpm, build),
                           meanbp = mean(bloodpoints, na.rm=TRUE),
                           meankills = mean(kills, na.rm=TRUE),
                           n=n())

print(tempdf[order(tempdf$meankills),], n=nrow(tempdf))
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
  facet_wrap(~killer, ncol=3) +
  ylab("frequency")

kbk2 <- dplyr::summarize(group_by(kpm, killer),
                         meankills = mean(kills, na.rm=TRUE)/4,
                         meanwins  = mean(win, na.rm=TRUE),
                         zerokills = mean(1*(kills == 0), na.rm=TRUE),
                         n=n())
kbk2

ggplot(kpm) + geom_boxplot(aes(x=kills, y=bloodpoints, group=kills))
ggplot(kpm) + geom_boxplot(aes(x=killer, y=bloodpoints, group=killer)) +
  theme(text=element_text(angle=90))

# # view princomp loadings for interpretation
# princoefs <- as.data.frame(coef(winmodel))
# princoefs$coef <- rownames(princoefs)
# princoefs <- princoefs[grep(x=princoefs$coef,
#                             pattern="perkprincomp",
#                             fixed=TRUE),]
# names(princoefs) <- c("est","coef")
# perkprincomp$loadings %*% princoefs$est

# perform a salt model
kpm$anysalt <- 1*(kpm$salt > 0)
saltmodel <- gam(anysalt ~ kills + killer + NOED + crossplay + s(numtime, bs="cc") + s(gamenum),
                 family=binomial(),
                 data=kpm)
summary(saltmodel)
dplyr::summarise(group_by(kpm, crossplay),
                 meansalt = mean(salt, na.rm=TRUE))
plot.gam(saltmodel, se=FALSE, select=2)

winbydate <- as.data.frame(dplyr::summarize(group_by(kpm, date),
                                            winrate=mean(win, na.rm=TRUE)))
ggplot(winbydate) + geom_histogram(aes(x=winrate), bins=15)

# analyze runs
kpm$winrun <- 1
kpm$lossrun <- 1
for (currow in 2:nrow(kpm)) {
  
  if (is.na(kpm$win[currow])) {
    
    kpm$winrun[currow] <- kpm$winrun[currow-1] 
    kpm$lossrun[currow] <- kpm$lossrun[currow-1]
  
  } else {
  
    if (kpm$win[currow] == 1) {
       
      kpm$winrun[currow] <- kpm$winrun[currow-1] + 1
      kpm$lossrun[currow] <- 0
       
    } else {
       
      kpm$winrun[currow] <- 0
      kpm$lossrun[currow] <- kpm$lossrun[currow-1] + 1
       
    }
    
  }
  
}
max(kpm$winrun)
max(kpm$lossrun)

kpm$gamenum <- 1:nrow(kpm)
ggplot(kpm) + geom_line(aes(x=gamenum, y=winrun))

kpm$movingwin <- NA
kpm$movingsalt <- NA
for (currow in 31:nrow(kpm)) {
  
  kpm$movingwin[currow] <- mean(kpm$win[(currow-30):currow], na.rm=TRUE)
  kpm$movingsalt[currow] <- mean(kpm$anysalt[(currow-30):currow], na.rm=TRUE)
  
}
ggplot(kpm) + geom_line(aes(x=gamenum, y=movingwin)) +
  ggtitle("moving window win rate (>2K)") +
  xlab("game number") + ylab("win rate")
ggplot(kpm) + geom_line(aes(x=gamenum, y=movingsalt)) +
  ggtitle("moving window salt rate") +
  xlab("game number") + ylab("salt rate")

tempdf <- dplyr::summarise(group_by(kpm, map),
                 meankills=mean(kills,na.rm=TRUE),
                 meanwins =mean(1*(kills >= 3), na.rm=TRUE),
                 n=n())
print(tempdf[order(tempdf$meanwins),], n=nrow(tempdf))


#### survivor ####
spm <- read.csv("spm data.csv", stringsAsFactors=FALSE)
head(spm)
dplyr::summarise(group_by(spm, swfcount),
                 survivalrate = mean(self.survived, na.rm=TRUE),
                 survivals = sum(self.survived, na.rm=TRUE),
                 matchcount = n())


# convert the date data
spm$date <- as.Date(spm$date, "%Y-%m-%d")
spm$numdate <- as.numeric(spm$date)
spm$dayofweek <- format(spm$date, "%u")
spm$dayofmonth <- as.numeric(format(spm$date, "%d"))
spm$dayofweekfactor <- paste("_", spm$dayofweek, sep="")
spm$dayofweekfactor <- factor(spm$dayofweek)
spm$weekend <- factor(spm$dayofweek %in% c("6","7"))

# convert the time data
spm$hour <- floor(spm$time / 100)
spm$minute <- spm$time - 100*spm$hour
spm$time <- paste(spm$hour, spm$minute, "00", sep=":")
spm$time <- chron(times=spm$time)
spm$numtime <- as.numeric(spm$time)*24
spm$numweek <- as.numeric(as.character(spm$dayofweek)) + spm$numtime/24 - 1

survivalmodel <- gam(self.survived ~ 0 + killer + factor(swfcount) + s(numtime, bs="cc") + factor(dayofweek),
                     family=binomial(),
                     data=spm)
summary(survivalmodel)
plot.gam(survivalmodel, select=1)

