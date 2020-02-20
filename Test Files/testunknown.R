library(readxl)
library(janitor)
library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)


setwd("DEPENd/")
getwd()

#WEEKLY - all participants in flow for the week of X (change X)
#SCHEDULED - all participants scheduled for sessions since the start of the study
#FLOW - all participants currently in the study

# --------------- begin code below


#read in data for master file spreadsheet and R01 information
MASTER <- read_xlsx("NeuroMAP/Recruitment/Participant Management/NEUROMAP Master V2.xlsm", sheet = "MasterData")
as_tibble(MASTER)
TIMELINE <- read_xlsx("NeuroMAP/Recruitment/Participant Management/RO1 Timeline.xlsx", sheet = "R01 Timeline")
AGGREGATE <- read_xlsx("NeuroMAP/Recruitment/Participant Management/RO1 Timeline.xlsx", sheet = "Aggregate")

#tidy up column names for MASTER
MASTER<-dplyr::rename(MASTER, PAI= "PAI (M)", AI = "AI (M)", SH = "SH (M)", SPIN = "SPIN (M)", SCREEN_SENTDATE = "SCREEN: SENT DATE (M)", SCREEN_COMPLETEDATE = "SCREEN: COMPLETE DATE (M)", SCREEN_COMPLETEYN =  "SCREEN: COMPLETE Y/N (M)")
MASTER<-dplyr::rename(MASTER, INEL_CODE = "INELEGIBILITY CODE (M)", INEL_PHASE ="PHASE DEEMED INELIGIBLE (M)", INEL_YN = "ELIGIBLE YN (M)",  GROUP_SL = "GROUPED: SL (M)" )
MASTER<-dplyr::rename(MASTER, S1_DATE = "S1 DATE (M)", S2_DATE ="S2 DATE (M)", S3_DATE = "S3 DATE (M)", S4_DATE = "S4 DATE (M)", S5_DATE = "S5 DATE (M)", INVITES1_YN = "INVITED TO S1 (M)")

AGGREGATE <- dplyr::rename (AGGREGATE, STUDY_YEAR = YEAR) %>%
  dplyr::group_by(AGGREGATE, STUDY_YEAR)

colnames(AGGREGATE)

#convert dates to proper format
MASTER$SCREEN_COMPLETEDATE<-excel_numeric_to_date(as.numeric(as.character(MASTER$SCREEN_COMPLETEDATE)), date_system = "modern")    
MASTER$S1_DATE<-excel_numeric_to_date(as.numeric(as.character(MASTER$S1_DATE)), date_system = "modern")    
MASTER$SCREEN_SENTDATE<-excel_numeric_to_date(as.numeric(as.character(MASTER$SCREEN_SENTDATE)), date_system = "modern")    
as.factor(MASTER$SOURCE)


#group participants

#WEEKLY - all participants in flow for the week of X (change X)
#SCHEDULED - all participants scheduled for sessions since the start of the study
#FLOW - all participants currently in the study
#PROJECTED - all participants scheduled, but not yet signed consent (i.e. S1 not completed)
#EXPECTED_CURRENTMONTH - Expected enrollment for month according to R01 Timetable for Recruitment
#EXPECTED_CURRENTYEAR - calls table of expected enrollment for the year according to R01 Timetable for Recruitment
#ENROLLED_A - all participants enrolled in study to date
#ENROLLED_L - all participants enrolled (i.e S1 completed, consent) under R01 critiera
#ENROLLED_S - all participants enrolled (i.e S1 completed, consent) under expanded enrollment criteria (i.e. will not complete session 4-5)


MONTHLY <- MASTER %>% 
  dplyr::select(NAME, STATUS, GROUP, S1_DATE, S2_DATE, S3_DATE, S4_DATE, S5_DATE, SCREEN_SENTDATE) %>%
  ##group_by(STATUS, GROUP) %>%
  ##arrange()
  filter(SCREEN_SENTDATE >= as.Date("2019-07-24") & SCREEN_SENTDATE <= as.Date("2019-07-30"), STATUS == "Invited S1")

WEEKLY <- MASTER %>% 
  dplyr::select(NAME, STATUS, SCREEN_SENTDATE, SCREEN_COMPLETEDATE, SCREEN_COMPLETEYN, GROUP, SOURCE) %>%
  filter(SCREEN_SENTDATE >= as.Date("2019-08-01") & SCREEN_SENTDATE <= as.Date(" 2019-08-30"))

#<-rename(MASTER, N_WEEKLY = "n") 


WEEKLY$STATUS<- factor(WEEKLY$STATUS,levels = c("Screen sent", "Completed screen", "Invited S1"))

WEEKLY$SOURCE <- factor(WEEKLY$SOURCE, levels = c("Facebook/Contact Form", "StudyFinder", "Craigslist", "Unknown"))
WEEKLY$SOURCE[is.na(WEEKLY$SOURCE)] <- "Unknown"

WEEKLY_TEST<- WEEKLY %>%
  add_tally()


SCHEDULED <- MASTER %>% 
  select(NAME, STATUS, GROUP, GROUP_SL, S1_DATE, S2_DATE, S3_DATE, S4_DATE, S5_DATE) %>%
  filter(!is.na(S1_DATE)) %>%
  
  
  PROJECTED <- MASTER %>% 
  select(NAME, STATUS, GROUP, S1_DATE, CONSENT) %>%
  #filter(!is.na(S1_DATE) & CONSENT == 0) %>%
  filter(S1_DATE >= as.Date("2019-01-24") & S1_DATE <= as.Date(" 2019-09-30"))

SCHEDULED$TALLY<-tally(SCHEDULED)

add_tally()



ENROLLED_A <- MASTER %>% 
  select(NAME, STATUS, GROUP, GROUP_SL, S1_DATE, CONSENT) %>%
  group_by(GROUP_SL) %>%
  ##mutate(cumsum = CONSENT) %>%
  filter(!is.na(S1_DATE) & CONSENT == 1) %>%
  add_tally()

ENROLLED_L <- MASTER %>% 
  select(NAME, ID, STATUS, GROUP, GROUP_SL) %>%
  group_by(GROUP_SL) %>%
  filter(ID <=1 & GROUP_SL == "L")

ENROLLED_S <- MASTER %>% 
  select(NAME, ID, STATUS, GROUP, GROUP_SL) %>%
  group_by(GROUP_SL) %>%
  filter(ID <=1 & GROUP_SL == "S")

EXPECTED_CURRENTMONTH <- AGGREGATE %>%
  select(YEAR, MONTH, GROUP, EXPECTED) %>%
  filter(YEAR == "1" & MONTH == "Aug")

EXPECTED_CURRENTYEAR <- AGGREGATE %>%
  select(YEAR, MONTH, GROUP, EXPECTED) %>%
  filter(YEAR == "1")

FLOW <- MASTER %>% 
  select(ID, S1_DATE, GROUP, GROUP_SL, CONSENT) %>%
  filter(!is.na(S1_DATE))

STUDY_AGG <- SCHEDULED %>%
  filter(GROUP_SL == "L")

#Produce Counts of Weekly Variables of Interest

plyr::count(WEEKLY, "SOURCE")
plyr::count(WEEKLY, "SCREEN_COMPLETEYN")
plyr::count(WEEKLY, "STATUS")
plyr::count(ENROLLED, "GROUP_SL")
plyr::count(MASTER)

#Greate distribution of sources for the week

graph_WEEKLY <-ggplot(WEEKLY, aes(STATUS)) +
  geom_bar(aes(STATUS, fill = STATUS,)) +
  scale_fill_manual(values = c("red", "pink", "blue"),
                    labels = c("Completed Screen", "Invited S1", "Screen Sent")) +
  labs(title ="Distribution of Enrollment Activity (1 Week)", y = "Number of Contacts", x = "Status") +
  theme(legend.position = "none")

graph_source<-ggplot(WEEKLY, aes(SOURCE)) + 
  geom_bar(aes(SOURCE, fill = SOURCE,)) + 
  scale_fill_manual(values = c("red", "pink", "blue", "yellow"),
                    labels = c("Completed Screen", "Invited S1", "Screen Sent", "None")) +
  labs(title ="Source of Past Week Recruitment", y = "Number of Contacts", x = "Source") +
  theme(legend.position = "none")

