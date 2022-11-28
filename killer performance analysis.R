rm(list=ls())

require(mgcv)
require(ggplot2)
require(chron)
require(dplyr)
require(plyr)
require(googledrive)
require(zoo)
require(forcats)
require(binom)
library(cluster) 

# install.packages(pkgs=c("mgcv",
#                         "ggplot2",
#                         "chron",
#                         "dplyr",
#                         "plyr"))
# install.packages("binom")

#drive_deauth()
kpm <- drive_download(file=as_id("1a541yFoA2E1xGQEXwhC2Q1x8lZLxFMI1r5FDadc2Hmk"),
                      path="kpm data.csv",
                      type="csv",
                      overwrite=TRUE)
spm <- drive_download(file=as_id("15c4YdlPEaj7H8z1PbatOy30t6tGzv-wx7VBulue4hkw"),
                      path="spm data.csv",
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

# retain only serious data
kpm <- kpm[kpm$serious == 1,]

# recode Ghostface as primary
# kpm$killer[kpm$killer == "Ghostface"] <- "_Ghostface"

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

# # calculate grade scores
# kpm$iris <- 0
# for (i in 1:nrow(kpm)) {
# 
#   kpm$iris[i] <- (kpm$absgi[i] == "i") + (kpm$g2[i] == "i") + (kpm$g3[i] == "i") + (kpm$g4[i] == "i")
#   if (kpm$absgi[i] == "") { kpm$iris[i] <- 2 }
# 
# }

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
kpm$killer <- factor(kpm$killer)
kpm$crossplay <- factor(kpm$crossplay)


## temporary
#kpm <- kpm[kpm$killer %in% c("Pig", "Ghostface"),]

## temporary pig
#kpm <- kpm[kpm$killer == "Pig",]

# define covariates to analyze
kpm$loss <- 1*(kpm$kills < 2)
kpm$win  <- 1*(kpm$kills >= 3)

# perks
# get a factor for builds
kpm$build <- ""
kpm$perk1 <- gsub(x=kpm$perk1,
                  pattern=" ",
                  replacement="",
                  fixed=TRUE)
kpm$perk1 <- gsub(x=kpm$perk1,
                  pattern="'",
                  replacement="",
                  fixed=TRUE)
kpm$perk1 <- tolower(kpm$perk1)

kpm$perk2 <- gsub(x=kpm$perk2,
                  pattern=" ",
                  replacement="",
                  fixed=TRUE)
kpm$perk2 <- gsub(x=kpm$perk2,
                  pattern="'",
                  replacement="",
                  fixed=TRUE)
kpm$perk2 <- tolower(kpm$perk2)

kpm$perk3 <- gsub(x=kpm$perk3,
                  pattern=" ",
                  replacement="",
                  fixed=TRUE)
kpm$perk3 <- gsub(x=kpm$perk3,
                  pattern="'",
                  replacement="",
                  fixed=TRUE)
kpm$perk3 <- tolower(kpm$perk3)

kpm$perk4 <- gsub(x=kpm$perk4,
                  pattern=" ",
                  replacement="",
                  fixed=TRUE)
kpm$perk4 <- gsub(x=kpm$perk4,
                  pattern="'",
                  replacement="",
                  fixed=TRUE)
kpm$perk4 <- tolower(kpm$perk4)

perklist <- data.frame(table(c(kpm$perk1, kpm$perk2, kpm$perk3, kpm$perk4)))
perklist
names(perklist) <- c("perk", "freq")
perklist <- perklist[perklist$freq >= 30,]

# create perk columns and fill
perkcols <- paste("perk_", perklist$perk, sep="")
kpm[,perkcols] <- 0
for (curcol in perkcols) {
  
  shortname <- substr(x=curcol,
                      start=6,
                      nchar(curcol))
  
  for (currow in 1:nrow(kpm)) {
   
    if (kpm$perk1[currow] == shortname) { kpm[currow, curcol] <- 1 }
    if (kpm$perk2[currow] == shortname) { kpm[currow, curcol] <- 1 }
    if (kpm$perk3[currow] == shortname) { kpm[currow, curcol] <- 1 }
    if (kpm$perk4[currow] == shortname) { kpm[currow, curcol] <- 1 }
    
  }
  
}
names(kpm)
# perform kmeans clustering
# gower.dist <- daisy(kpm[perkcols],
#                     metric=c("gower"))
# div.clust <- diana(as.matrix(gower.dist),
#                    diss=TRUE,
#                    keep.diss=TRUE)
# #plot(div.clust, main="Divisive")
# kpm$perkcluster <- cutree(div.clust, k=10)
# 
# # create formula
perkform <- paste(perkcols, collapse = " + ")
head(kpm)

# run a model on bloodpoints
myform <- as.formula(paste("bloodpoints ~ 0 + factor(kills) + killer + crossplay + s(numtime, bs='cc') + s(gamenum)",
                           perkform,
                           sep=" + "))
bpmodel <- gam(formula=myform,
               data=kpm)
summary(bpmodel)
plot(bpmodel, se=FALSE, scale=0, select=1)
plot(bpmodel, se=FALSE, scale=0, select=2)

table(kpm$steam)
dplyr::summarise(group_by(kpm, steam),
                 avgkills=mean(kills,na.rm=TRUE))

# run a model on clear wins
# removed map
myform <- as.formula(paste("win ~ 0 + killer + map + platform + crossplay + dayofweek + s(numtime, bs='cc') + s(dayofmonth, bs='cc') + s(gamenum)",
                           perkform,
                           sep=" + "))
winmodel <- gam(formula=myform,
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
                          pattern="perk",
                          fixed=TRUE),]
rownames(wincoefs) <- 1:nrow(wincoefs)
wincoefs <- wincoefs[order(wincoefs$est),]
wincoefs


table(kpm$build, kpm$killer)



killsbykiller <- dplyr::summarize(group_by(kpm, killer, kills),
                                  killcount=n())
tempdf <- dplyr::summarise(group_by(kpm, killer),
                           totalkills=n())
killsbykiller <- left_join(killsbykiller, tempdf, by="killer")
killsbykiller$killfreq <- killsbykiller$killcount / killsbykiller$totalkills

ggplot(killsbykiller) + geom_bar(aes(x=kills, y=killfreq), stat="identity") +
  facet_wrap(~killer, ncol=3) +
  ylab("frequency")

kbk <- dplyr::summarize(kpm,
                        meankills = mean(kills, na.rm=TRUE)/4,
                        meanwins  = mean(win, na.rm=TRUE),
                        zerokills = mean(1*(kills == 0), na.rm=TRUE),
                        fourkills = mean(1*(kills == 4), na.rm=TRUE),
                        n=n())
kbk

kbk2 <- dplyr::summarize(group_by(kpm, killer),
                         meankills = mean(kills, na.rm=TRUE)/4,
                         meanwins  = mean(win, na.rm=TRUE),
                         zerokills = mean(1*(kills == 0), na.rm=TRUE),
                         fourkills = mean(1*(kills == 4), na.rm=TRUE),
                         n=n())
kbk2 <- kbk2[order(kbk2$meanwins),]
print(kbk2, n=nrow(kbk2))

#kpm <- kpm[kpm$bloodpoints > 10000,]
#kpm <- kpm[!is.na(kpm$bloodpoints),]

ggplot(kpm) + geom_boxplot(aes(x=kills, y=bloodpoints, group=kills))
ggplot(kpm) + geom_boxplot(aes(x=fct_reorder(killer, bloodpoints, na.rm=TRUE, .fun='median'),
                               y=bloodpoints, group=killer)) +
  theme(text=element_text(angle=90)) +
  xlab("") + ylab("bloodpoints")

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

kpm[which(kpm$winrun == max(kpm$winrun)),]
kpm[which(kpm$lossrun == max(kpm$lossrun)),]

kpm$gamenum <- 1:nrow(kpm)
ggplot(kpm) + geom_line(aes(x=gamenum, y=winrun))

kpm$movingwin <- mean(kpm$win, na.rm=TRUE)
alpha <- 0.04
for (currow in 2:nrow(kpm)) {
  
  #kpm$movingwin[currow] <- mean(kpm$kills[(currow-windowsize):currow], na.rm=TRUE)
  if (!is.na(kpm$kills[currow])) {
  
    kpm$movingwin[currow] <- (1-alpha) * kpm$movingwin[currow-1] + alpha*(kpm$win[currow])
      
  } else {
    
    kpm$movingwin[currow] <- kpm$movingwin[currow-1]
    
  }
  
}
# ggplot(kpm) + geom_line(aes(x=gamenum, y=movingwin)) +
#   ggtitle(paste("moving window kill rate, window: ", windowsize, sep="")) +
#   xlab("game number") + ylab("kill rate")

geoavg <- mgcv::bam(win ~ s(gamenum),
                    data=kpm,
                    family=binomial(),
                    gamma=0.1)
kpm$pred <- predict(geoavg, type="response", newdata=kpm)

ggplot(kpm[50:nrow(kpm),]) + geom_line(aes(x=gamenum, y=movingwin)) +
  geom_line(aes(x=gamenum, y=pred), color="red", linetype=2) +
  ggtitle("geometric average kill rate") +
  xlab("match number") + ylab("kill rate")
mean(kpm$win, na.rm=TRUE)

tempdf <- dplyr::summarise(group_by(kpm, hour),
                           killrate = mean(win, na.rm=TRUE),
                           upper    = mean(win, na.rm=TRUE) + 1.96 * sqrt(mean(win, na.rm=TRUE) * (1-mean(win, na.rm=TRUE))/n()),
                           lower    = mean(win, na.rm=TRUE) - 1.96 * sqrt(mean(win, na.rm=TRUE) * (1-mean(win, na.rm=TRUE))/n()))

ggplot(tempdf) + geom_line(aes(x=hour, y=killrate)) +
  geom_line(aes(x=hour, y=upper), linetype=2) +
  geom_line(aes(x=hour, y=lower), linetype=2)

# cutoffgamenum <- max(kpm$gamenum[kpm$date < as.Date("2021-10-19", "%Y-%m-%d")], na.rm=TRUE)-3
# ggplot(kpm) + geom_line(aes(x=gamenum, y=movingwin)) +
#   geom_vline(xintercept=cutoffgamenum, color="red", linetype=2) + 
#   ggtitle(paste("moving window kill rate, window: ", windowsize, sep="")) +
#   xlab("numdate") + ylab("kill rate")

dplyr::summarise(group_by(kpm, platform),
                 winrate = mean(win, na.rm=TRUE))

#ggplot(kpm) + geom_density2d(aes(x=movingwin, y=movingsalt)) + xlab("win rate") + ylab("salt rate")

# kpm$movingwin <- (kpm$movingwin - min(kpm$movingwin, na.rm=TRUE))/(max(kpm$movingwin, na.rm=TRUE)- min(kpm$movingwin, na.rm=TRUE))
# kpm$movingsalt <- (kpm$movingsalt - min(kpm$movingsalt, na.rm=TRUE))/(max(kpm$movingsalt, na.rm=TRUE)- min(kpm$movingsalt, na.rm=TRUE))
tempdf <- dplyr::summarise(group_by(kpm, map),
                 meankills=mean(kills,na.rm=TRUE),
                 meanwins =mean(1*(kills >= 3), na.rm=TRUE),
                 n=n())
print(tempdf[order(tempdf$meankills),], n=nrow(tempdf))

tempdf <- dplyr::summarise(group_by(kpm, map, killer),
                           meankills = mean(kills, na.rm=TRUE),
                           n=n())
tempdf <- tempdf[tempdf$map != "",]
tempdf <- tempdf[tempdf$n >= 10,]
print(tempdf[order(-tempdf$meankills),], n=nrow(tempdf))

# pigdf <- kpm[kpm$killer=="Pig",]
# nrow(pigdf)
# head(pigdf)
# 
# pigform <- as.formula(paste("win ~ 0 + map + s(numtime, bs='cc') + s(dayofmonth, bs='cc') + s(gamenum) + factor(steam)",
#                             perkform,
#                             sep=" + "))
# pigmodel <- gam(formula=pigform,
#                 data=pigdf,
#                 family=binomial())
# summary(pigmodel)

ggplot(kpm) + geom_jitter(aes(x=steam, y=kills))

table(kpm$steam)
table(kpm$steam, kpm$kills)
#table(kpm$iris, kpm$kills)















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
spm$gamenum <- 1:nrow(spm)






# set spm perk
spm$perk1 <- tolower(spm$perk1)
spm$perk2 <- tolower(spm$perk2)
spm$perk3 <- tolower(spm$perk3)
spm$perk4 <- tolower(spm$perk4)
spm$perk1 <- gsub(pattern=" ", replacement="", x=spm$perk1, fixed=TRUE)
spm$perk2 <- gsub(pattern=" ", replacement="", x=spm$perk2, fixed=TRUE)
spm$perk3 <- gsub(pattern=" ", replacement="", x=spm$perk3, fixed=TRUE)
spm$perk4 <- gsub(pattern=" ", replacement="", x=spm$perk4, fixed=TRUE)
spm$perk1 <- gsub(pattern="-", replacement="", x=spm$perk1, fixed=TRUE)
spm$perk2 <- gsub(pattern="-", replacement="", x=spm$perk2, fixed=TRUE)
spm$perk3 <- gsub(pattern="-", replacement="", x=spm$perk3, fixed=TRUE)
spm$perk4 <- gsub(pattern="-", replacement="", x=spm$perk4, fixed=TRUE)
spm$perk1 <- gsub(pattern="'", replacement="", x=spm$perk1, fixed=TRUE)
spm$perk2 <- gsub(pattern="'", replacement="", x=spm$perk2, fixed=TRUE)
spm$perk3 <- gsub(pattern="'", replacement="", x=spm$perk3, fixed=TRUE)
spm$perk4 <- gsub(pattern="'", replacement="", x=spm$perk4, fixed=TRUE)
perklist <- data.frame(table(c(spm$perk1, spm$perk2, spm$perk3, spm$perk4)))
perklist
names(perklist) <- c("perk", "freq")
perklist <- perklist[perklist$freq >= 30,]

# create perk columns and fill
perkcols <- paste("perk_", perklist$perk, sep="")
spm[,perkcols] <- 0
for (curcol in perkcols) {
  
  shortname <- substr(x=curcol,
                      start=6,
                      nchar(curcol))
  
  for (currow in 1:nrow(spm)) {
    
    if (spm$perk1[currow] == shortname) { spm[currow, curcol] <- 1 }
    if (spm$perk2[currow] == shortname) { spm[currow, curcol] <- 1 }
    if (spm$perk3[currow] == shortname) { spm[currow, curcol] <- 1 }
    if (spm$perk4[currow] == shortname) { spm[currow, curcol] <- 1 }
    
  }
  
}
names(spm)

perkform <- paste(perkcols, collapse = " + ")
perkform <- paste("self.survived ~ 0 + killer + factor(swfcount) + factor(dayofweek) + s(numtime, bs='cc') + s(gamenum)",
                  perkform,
                  sep=" + ")
perkform <- as.formula(perkform)




survivalmodel <- gam(formula=perkform,
                     family=binomial(),
                     data=spm)
summary(survivalmodel)
plot.gam(survivalmodel, select=1, se=FALSE)
plot.gam(survivalmodel, select=2, se=FALSE)


spm$selfsurvive <- mean(spm$self.survived, na.rm=TRUE)
for (currow in 2:nrow(spm)) {
  
  #kpm$movingwin[currow] <- mean(kpm$kills[(currow-windowsize):currow], na.rm=TRUE)
  if (!is.na(spm$self.survived[currow])) {
    
    spm$selfsurvive[currow] <- (1-alpha) * spm$selfsurvive[currow-1] + alpha*(spm$self.survived[currow])
    
  } else {
    
    spm$selfsurvive[currow] <- spm$selfsurvive[currow-1]
    
  }
  
}

ggplot(spm) +
  geom_line(aes(x=1:nrow(spm), y=selfsurvive)) +
  #geom_vline(xintercept=as.numeric(as.Date("2021-10-19",  "%Y-%m-%d")), color="red", linetype=2) + 
  ggtitle("geometric average kill rate") +
  xlab("gamenum") + ylab("survival rate")

spm[which(spm$selfsurvive == max(spm$selfsurvive)),]