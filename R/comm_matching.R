# this script is used to combine the independent coding done by Lisa, Marvin and Matt
# and resolves any disagreement over how comms should be coded. We use the final agreed coding 
# as an instructive guide for the interns

# interrater reliability --------------------------------------------------
library(tidyverse)
library(irr)

d <- read_csv("data/comm_coding_files/comm_matching/interrater_data.csv") %>% 
  mutate(EventB = ifelse(EventB == "Frustration [F]", 
                         "Frustration/Anxiety/Surprise [F]", EventB)) %>% 
  select(-contains("Time"))

agree(d)

kappam.fleiss(d, detail = T)



# original code -----------------------------------------------------------
library(tidyverse)
library(stringr)

data <- read_csv("data/comm_coding_files/comm_matching/190218_CommCoding_LZMLMB_preR.csv")

data2 <- data %>% 
  mutate(Match = (LZ == ML & ML == MB),
         MatchLZML = (LZ == ML & abs(LZTime-MLTime)<5),
         MatchLZMB = (LZ == MB & abs(LZTime-MBTime)<5),
         MatchMLMB = (ML == MB & abs(MBTime-MLTime)<5))



data3 <- data2 %>% 
  filter(!(is.na(LZ) & is.na(MB) & ML == "Ambiguous [ ` or = ]"))

data3 %>% 
  summary()

sum(!is.na(data3$LZ))
sum(!is.na(data3$MB))
sum(!is.na(data3$ML))

datamis <- data3 %>% 
  filter(MatchLZMB == FALSE) %>% 
  select(LZ, MB, MatchLZMB)

#Total Match Results Between All 3
245/556

#Match between Coders
#LZ and ML
312/556
#LZ and MB
297/556
#MB and ML
283/556

(312+297+283)/3/556



data3

# dataprocess <- data2 %>% 
#   mutate(Notes = NA,
#          Correct = NA,
#          CorrectTime = NA)
# 
# ##Notes
# 
# processdata <- function(num, notes, correctp, correcttimep) {
#   dataprocess$Notes[num] = paste0(notes)
#   dataprocess$Correct[num] = dataprocess[paste0(correctp)][num,]
#   #dataprocess$CorrectTime[num] = dataprocess[paste0(correcttimep)][num,]
# }
# 
# dataprocess$Notes[1] = "Missed"
# dataprocess$Correct[1] = dataprocess$ML[1]
# dataprocess$CorrectTime[1] = dataprocess$MLTime[1]
# 
# dataprocess$Notes[3] = "TooHardtoHear"
# dataprocess$Notes[7] = "StartswithUselessWords"
# 
# dataprocess$Notes[8] = "Joinwithprev"
# dataprocess$Correct[8] = dataprocess$ML[8]
# dataprocess$CorrectTime[8] = dataprocess$MLTime[8]
# 
# processdata(12, "Confirmation", "LZ", "LZTime")
# 
# num = 12
# correctp <- "LZ"
# 
# dataprocess[12,]

###Fenella Check

fdata <- read_csv("data/comm_coding_files/comm_matching/CommCodingTest_Fenella.csv")

fdata$EventMML <- fdata$EventMML %>% 
  str_replace("Frustration", "Frustration/Anxiety/Surprise")

colnames(fdata) <- c("EventMML", "TimeMML", "EventF", "TimeF")

fdata <- fdata %>% 
  mutate(Match = EventMML == EventF,
         TimeDiff = (TimeMML - TimeF))

fdatatime <- fdata %>% 
  filter(TimeDiff >= 5 & Match == TRUE)

summary(fdata)

332/520

fdata %>% 
  ggplot(aes(x = TimeDiff, fill = EventMML)) +
  geom_histogram()+
  facet_wrap(~ EventMML)

#Fenella had but we missed

FUnique <- fdata %>% 
  filter(is.na(EventMML))

#We had but Fenella missed
FMissed <- fdata %>% 
  filter(is.na(EventF))

#Mismatches
FMismatch <- fdata %>% 
  filter(Match == F)

table(FUnique$EventF)

table(FMissed$EventMML)


###Ben Check

bdata <- read_csv("data/comm_coding_files/comm_matching/CommCodingTest_Ben.csv")

#bdata$EventMML <- bdata$EventMML %>% 
#  str_replace("Frustration", "Frustration/Anxiety/Surprise")

colnames(bdata) <- c("EventMML", "TimeMML", "EventB", "TimeB")

bdata <- bdata %>% 
  mutate(Match = EventMML == EventB,
         TimeDiff = (TimeMML - TimeB))

bdatatime <- bdata %>% 
  filter(TimeDiff >= 5 & Match == TRUE)


summary(bdata)

348 / 538

bdata %>% 
  ggplot(aes(x = TimeDiff, fill = EventMML)) +
  geom_histogram()+
  facet_wrap(~ EventMML)

#Fenella had but we missed

BUnique <- bdata %>% 
  filter(is.na(EventMML))

table(BUnique$EventB)

#We had but Fenella missed
BMissed <- bdata %>% 
  filter(is.na(EventB))

table(BMissed$EventMML)

#Mismatches
BMismatch <- bdata %>% 
  filter(Match == F)

table(bdata$EventMML)
table(bdata$EventF)
table(bMissed$EventMML)

520-115

### Vivian Check

vdata <- read_csv("data/comm_coding_files/comm_matching/CommCodingTest_Vivian.csv")

#bdata$EventMML <- bdata$EventMML %>% 
#  str_replace("Frustration", "Frustration/Anxiety/Surprise")

colnames(vdata) <- c("EventMML", "TimeMML", "EventV", "TimeV")

vdata <- vdata %>% 
  mutate(Match = EventMML == EventV,
         TimeDiff = (TimeMML - TimeV))

vdatatime <- vdata %>% 
  filter(TimeDiff >= 5 & Match == TRUE)

#Vivian had one match with a massive time difference, and thus this row was counted as a mismatch

summary(vdata)

table(vdata$EventV)

317 / 526

vdata %>% 
  ggplot(aes(x = TimeDiff, fill = EventMML)) +
  geom_histogram()+
  facet_wrap(~ EventMML)

#Fenella had but we missed

VUnique <- vdata %>% 
  filter(is.na(EventMML))

table(VUnique$EventV)

#We had but Fenella missed
VMissed <- vdata %>% 
  filter(is.na(EventV))

table(VMissed$EventMML)

#Mismatches
VMismatch <- vdata %>% 
  filter(Match == F)

table(bdata$EventMML)
table(bdata$EventF)
table(bMissed$EventMML)


(63.85+64.68+60.27)/3

# ###Fenella Check

#Can't Use Andrew's Data since it is not matched up to the right file
# 
# adata <- read_csv("../CommCodingTest_Andrew.csv")
# 
# #bdata$EventMML <- bdata$EventMML %>% 
# #  str_replace("Frustration", "Frustration/Anxiety/Surprise")
# 
# colnames(adata) <- c("EventMML", "TimeMML", "EventA", "TimeA")
# 
# adata <- adata %>% 
#   mutate(Match = EventMML == EventA,
#          TimeDiff = (TimeMML - TimeA))
# 
# adatatime <- adata %>% 
#   filter(TimeDiff >= 5 & Match == TRUE)
# 
# 
# summary(adata)
# 
# 336 / 557

# adata %>% 
#   ggplot(aes(x = TimeDiff, fill = EventMML)) +
#   geom_histogram()+
#   facet_wrap(~ EventMML)
# 
# #Fenella had but we missed
# 
# AUnique <- adata %>% 
#   filter(is.na(EventMML))
# 
# table(AUnique$EventV)
# 
# #We had but Fenella missed
# AMissed <- adata %>% 
#   filter(is.na(EventV))
# 
# table(AMissed$EventMML)
# 
# #Mismatches
# AMismatch <- adata %>% 
#   filter(Match == F)
# 
# table(adata$EventMML)
# table(adata$EventF)
# table(AMissed$EventMML)

#Dan


ddata <- read_csv("data/comm_coding_files/comm_matching/CommCodingTest_Dan.csv")

colnames(ddata) <- c("EventMML", "TimeMML", "EventD", "TimeD")

ddata <- ddata %>% 
  mutate(Match = EventMML == EventD,
         TimeDiff = (TimeMML - TimeD))

ddatatime <- ddata %>% 
  filter(TimeDiff >= 5 & Match == TRUE)

summary(ddata)
360/560

#Collate Data



adata$Coder <- "Andrew"
fdata$Coder <- "Fenella"
vdata$Coder <- "Vivian"
bdata$Coder <- "Ben"

asum <- adata %>% 
  group_by(EventA, Coder) %>% 
  filter(!is.na(EventA)) %>% 
  summarise(count = n()/nrow(adata))

fsum <- fdata %>% 
  group_by(EventF, Coder) %>% 
  filter(!is.na(EventF)) %>% 
  summarise(count = n()/nrow(adata))

vsum <- vdata %>% 
  group_by(EventV, Coder) %>% 
  filter(!is.na(EventV)) %>% 
  summarise(count = n()/nrow(adata))

bsum <- bdata %>% 
  group_by(EventB, Coder) %>% 
  filter(!is.na(EventB)) %>% 
    summarise(count = n()/nrow(adata))

colnames(asum) <- c("Event", "Coder", "Andrew")
colnames(bsum) <- c("Event", "Coder", "Ben")
colnames(fsum) <- c("Event", "Coder", "Fenella")
colnames(vsum) <- c("Event", "Coder", "Vivian")

asum <- asum[-2]
fsum <- fsum[-2]
vsum <- vsum[-2]
bsum <- bsum[-2]

bsum[10,1] <- "Frustration/Anxiety/Surprise [F]"

collateddata <- left_join(asum, fsum)
collateddata <- left_join(collateddata, bsum)
collateddata <- left_join(collateddata, vsum)

mmlsum <- fdata %>% 
  group_by(EventMML) %>% 
  filter(!is.na(EventMML)) %>% 
  summarise(count = n()/nrow(fdata))

colnames(mmlsum) <- c("Event", "MML")

collateddata <- left_join(collateddata, mmlsum)

collateddata %>% 
  gather(key = Coder, value = Value, Andrew:MML) %>% 
  ggplot(aes(x = Coder, y = Value, fill = Coder)) +
  geom_bar(stat = "identity")+
  facet_wrap(~ Event)
  







