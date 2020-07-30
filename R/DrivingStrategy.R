#Driving Strategy Coding#
#Marvin law#
#Script Version: 0.5#
#Start Date 190403#

#Packages
library(tidyverse)

#Read Driving Strategy Coding Spreadsheet

spreadsheet <- read_csv("TaskSpreadsheet_Edited.csv")
colnames(spreadsheet) <- str_replace_all(colnames(spreadsheet), "\\s", "")

#Read In FileName to Teams

filestoteams <- read_csv("DrivingFilesToTeams.csv")
colnames(filestoteams)[1] <- "VideoName"

spreadsheetcomplete <- left_join(spreadsheet, filestoteams, by = "VideoName")

#Read In Driving Videos to Be Removed

removalvideos <- read_csv("DrivingVideosToBeRemoved.csv")
removalvideos <- removalvideos %>% 
  mutate(Team = str_sub(DrivingVideosToBeRemoved, 1, 10))

spreadsheetcomplete2 <- left_join(spreadsheetcomplete, removalvideos, by = "Team")

#Filter Spreadsheet to Videos which Are Included

goodvideos <- spreadsheetcomplete2 %>%
  filter(Reviewed == "Yes" | Reviewed == "No Need to Review") %>% 
  filter(Code == "Yes")
  
#Extract CSV files
files <- list.files("./AllCSV")

reviewedfiles <- list.files("./AllCSV", pattern = "reviewed", ignore.case = TRUE)
# reviewedfiles <- reviewedfiles %>% 
#   str_sub(1, 35)

codedfiles <- list.files("./AllCSV", pattern = "coded", ignore.case = TRUE)
# codedfiles <- codedfiles %>% 
#   str_sub(1, 35)

goodvideosreview <- goodvideos %>% 
  filter(Reviewed == "Yes") %>% 
  select(VideoName)

goodvideosnoreview <- goodvideos %>% 
  filter(Reviewed == "No Need to Review") %>% 
  select(VideoName)

#Extract Reviewed Data

revieweddata <- lapply(paste0("./AllCSV/", reviewedfiles), read_csv) %>% 
  tibble()

revieweddata$filename <- reviewedfiles

revieweddata <- revieweddata %>% 
  unnest() %>% 
  select(filename, everything())

colnames(revieweddata) <- c("filename", "Check", "Event", "Time", "AA", "BB")

revieweddata <- revieweddata %>% 
  select(filename:Time)

revieweddata <- revieweddata %>% 
  mutate(VideoName = ifelse(str_sub(filename, 35, 35)=="v", 
                           str_sub(filename, 1, 35), 
                           str_sub(filename, 1, 36)))

reviewedcomplete <- left_join(revieweddata, spreadsheetcomplete2)

#Extract Other Data

codeddata <- lapply(paste0("./AllCSV/", codedfiles), read_csv) %>% 
  tibble()

codeddata$filename <- codedfiles

codeddata <- codeddata %>% 
  filter(!(filename %in% reviewedfiles))

codeddata <- codeddata %>% 
  unnest() %>% 
  select(filename, everything())

colnames(codeddata) <- c("filename", "Check", "Event", "Time", "AA", "BB")

codeddata <- codeddata %>% 
  select(filename:Time)

codeddata <- codeddata %>% 
  mutate(VideoName = ifelse(str_sub(filename, 35, 35)=="v", 
                            str_sub(filename, 1, 35), 
                            str_sub(filename, 1, 36)))

codedcomplete <- left_join(codeddata, spreadsheetcomplete2)

codedcomplete <- codedcomplete %>% 
  filter(!Team %in% reviewedcomplete$Team)

data <- rbind(codedcomplete, reviewedcomplete)

data2 <- data %>% 
  filter(Team %in% goodvideos$Team)

colnames(data2)

data2 <- data2 %>% 
  nest(Check:Time)

#Removed Replicates of csv file coding for the following Teams
#17031509_2
#17031609_1
#17033111_2
#17040609_2


data2 <- data2 %>% 
  filter(filename != "2017-03-15 09-26-59-6_processed.m4v_coded_reviewed (2).csv") %>% 
  filter(filename != "2017-03-16 09-20-53-9_processed.m4v_coded_reviewedwip.csv") %>% 
  filter(filename != "2017-04-06 09-29-15-33_processed.m4v_coded_reviewed WIP.csv") %>% 
  filter(filename != "2017-04-06 09-29-15-33_processed.m4v_coded_reviewed_first - Copy - Copy.csv") %>% 
  filter(filename != "2017-04-06 09-29-15-33_processed.m4v_coded_reviewed_first CSV.csv") %>% 
  filter(filename != "2017-04-06 09-29-15-33_processed.m4v_coded_reviewed_first.csv") %>% 
  filter(filename != "2017-04-06 09-29-15-33_processed.m4v_coded_reviewed_second.csv") %>% 
  filter(filename != "2017-04-06 09-29-15-33_processed.m4v_coded_reviewed_SECONDREDO_TBC.csv")

#Remove Technical Malfunction Data (Just one team missed from before)
#17031515_1-g2

#Remove Team which has issue mid drive and had to restart sim (17032209_2)

data2 <- data2 %>% 
  filter(is.na(DrivingVideosToBeRemoved)) %>% 
  filter(Team != "17032209_2")

goodfiles <- lapply(paste0("./AllCSV/", data2$filename %>% unique()), read_csv) %>% 
  tibble()

goodfiles$name <- data2$filename %>% unique()
colnames(goodfiles) <- c("data", "name")

lapply(goodfiles$data, write.csv(paste0("./FinalFiles/", goodfiles$name)))



allcleandata <- data2 %>% 
  unnest() %>% 
  select(-Comments)

write.csv(allcleandata, "drivingstrategycleandata2.csv")

###Data Analysis
# 
# dataan <- read_csv("drivingstrategycleandata2.csv") %>% 
#   select(Team, Check:Time)
# 
# 
# dataan <- dataan %>% 
#   group_by(Team) %>% 
#   nest() %>% 
#   select(Team, data) %>% 
#   mutate(data = map(data, ~select(., -Check)),
#          data = map(data, ~arrange(., Time)),
#          data = map(data, ~mutate(., Duration = lead(Time) - (Time))),
#          data = map(data, ~mutate(., FollowingEvent = (lead(Event)))),
#          data = map(data, ~mutate(., EventCategory = ifelse(str_detect(Event, "Non-Instructed"), "Driver", 
#                                                             ifelse(str_detect(Event, "Instructed"), "Co-Driver", "Crash")))),
#          data = map(data, ~mutate(., Outcome = ifelse(str_detect(FollowingEvent, "Crash"), 
#                                                       "Failure", "NoIssue"))),
#          performance = map(data, ~group_by(., Event) %>% 
#                              summarise(Freq = length(Event),
#                                        Duration = sum(Duration, na.rm = TRUE),
#                                        Failure = sum(Outcome == "Failure", na.rm = TRUE))))
# 
# dataan$data[[1]] %>% View()
# 
# summary <- dataan %>% 
#   select(Team, performance) %>% 
#   unnest()
# 
# write.csv(summary, "drivingoutcomedatasummary.csv")
# 
# summary <- summary %>% 
#   mutate(Event = ifelse(Event == "Driving in the Middle Non-Instructed [K]", "MidNon",
#                         ifelse(Event == "Driving in the Opposite Lane Instructed [O]", "OppIns",
#                                ifelse(Event == "Driving in the Opposite Lane Non-Instructed [L]", "OppNon",
#                                       ifelse(Event == "Normal Driving Non-Instructed [J]", "NormNon",
#                                              ifelse(Event == "Normal Driving Instructed [U]", "NormIns",
#                                                     ifelse(Event == "End of Strategy from Car Crash [W]", "Crash",
#                                                            ifelse(Event == "Driving in the Middle Instructed [I]", "MidIns", "Issue")
#                                                     )
#                                              )
#                                       )
#                                )
#                         )))
# 
# teamdata <- summary %>% 
#   gather(Variable, Value, Freq:Failure) %>% 
#   unite(EventType, Event, Variable) %>% 
#   spread(EventType, Value)
# 
# teamsummary <- teamdata %>% 
#   group_by(Team) %>% 
#   summarise(TotalTime = sum(Crash_Duration,
#                           MidIns_Duration,
#                           MidNon_Duration,
#                           OppIns_Duration,
#                           OppNon_Duration,
#                           NormIns_Duration,
#                           NormNon_Duration, na.rm = TRUE),
#          CrashRecovery = Crash_Duration/Crash_Freq,
#          InsDuration = sum(MidIns_Duration,
#                            OppIns_Duration,
#                            NormIns_Duration,
#                            na.rm = TRUE),
#          NonDuration = sum(MidNon_Duration,
#                            OppNon_Duration,
#                            NormNon_Duration,
#                            na.rm = TRUE),
#          InstoNonTimeRatio = InsDuration/(InsDuration + NonDuration),
#          InsFail = sum(MidIns_Failure,
#                            OppIns_Failure,
#                            NormIns_Failure,
#                            na.rm = TRUE),
#          NonFail = sum(MidNon_Failure,
#                            OppNon_Failure,
#                            NormNon_Failure,
#                            na.rm = TRUE),
#          InstoNonFailRatio = InsFail/sum(InsFail, NonFail, na.rm = TRUE),
#          MidInsPref = MidIns_Freq/sum(MidIns_Freq, OppIns_Freq, NormIns_Freq, na.rm = TRUE),
#          MidNonPref = MidNon_Freq/sum(MidNon_Freq, OppNon_Freq, NormNon_Freq, na.rm = TRUE),
#          OppInsPref = OppIns_Freq/sum(MidIns_Freq, OppIns_Freq, NormIns_Freq, na.rm = TRUE),
#          OppNonPref = OppNon_Freq/sum(MidNon_Freq, OppNon_Freq, NormNon_Freq, na.rm = TRUE),
#          NormInsPref = NormIns_Freq/sum(MidIns_Freq, OppIns_Freq, NormIns_Freq, na.rm = TRUE),
#          NormNonPref = NormNon_Freq/sum(MidNon_Freq, OppNon_Freq, NormNon_Freq, na.rm = TRUE))
# 
# complete <- left_join(teamsummary, teamdata)
# 
# library(corrr)
# 
# write.csv(complete, "completeddrivingstrategydatadraft1.csv")

#### Driving Strategy Analyses ####
library(tidyverse)
library(psych)
library(corrr)


dataan <- read_csv("drivingstrategycleandata2.csv") %>% 
  select(Team, Check:Time)


dataan <- dataan %>% 
  group_by(Team) %>% 
  nest() %>% 
  select(Team, data) %>% 
  mutate(data = map(data, ~select(., -Check)),
         data = map(data, ~arrange(., Time)),
         data = map(data, ~mutate(., Duration = lead(Time) - (Time))),
         data = map(data, ~mutate(., FollowingEvent = (lead(Event)))),
         data = map(data, ~mutate(., EventCategory = ifelse(str_detect(Event, "Non-Instructed"), "Driver", 
                                                            ifelse(str_detect(Event, "Instructed"), "Co-Driver", "Crash")))),
         data = map(data, ~mutate(., Outcome = ifelse(str_detect(FollowingEvent, "Crash"), 
                                                      "Failure", "NoIssue"))),
         performance = map(data, ~group_by(., Event) %>% 
                             summarise(Freq = length(Event),
                                       Duration = sum(Duration, na.rm = TRUE),
                                       Failure = sum(Outcome == "Failure", na.rm = TRUE))))

dataan$data[[1]] %>% View()

summary <- dataan %>% 
  select(Team, performance) %>% 
  unnest()

write.csv(summary, "drivingoutcomedatasummary.csv")

summary <- summary %>% 
  mutate(Event = ifelse(Event == "Driving in the Middle Non-Instructed [K]", "MidNon",
                        ifelse(Event == "Driving in the Opposite Lane Instructed [O]", "OppIns",
                               ifelse(Event == "Driving in the Opposite Lane Non-Instructed [L]", "OppNon",
                                      ifelse(Event == "Normal Driving Non-Instructed [J]", "NormNon",
                                             ifelse(Event == "Normal Driving Instructed [U]", "NormIns",
                                                    ifelse(Event == "End of Strategy from Car Crash [W]", "Crash",
                                                           ifelse(Event == "Driving in the Middle Instructed [I]", "MidIns", "Issue")
                                                    )
                                             )
                                      )
                               )
                        )))


summary <- summary %>% 
  filter(Event != "Issue") # Remove communication files which are in wrong folder

teamdata <- summary %>% 
  gather(Variable, Value, Freq:Failure) %>% 
  unite(EventType, Event, Variable) %>% 
  spread(EventType, Value)

teamdata <- teamdata %>% 
  gather(key = key, val = val, matches("Duration|Freq")) %>% 
  mutate(val = ifelse(is.na(val), 0, val)) %>%
  spread(key, val)

teamsummary <- teamdata %>% 
  group_by(Team) %>% 
  summarise(CrashRecovery = Crash_Duration/Crash_Freq,
            InsDuration = sum(MidIns_Duration,
                              OppIns_Duration,
                              NormIns_Duration,
                              na.rm = TRUE),
            NonDuration = sum(MidNon_Duration,
                              OppNon_Duration,
                              NormNon_Duration,
                              na.rm = TRUE),
            InstoNonTimeRatio = InsDuration/(InsDuration + NonDuration))

complete <- left_join(teamsummary, teamdata)


write.csv(summary, "drivingoutcomedatasummary.csv")
