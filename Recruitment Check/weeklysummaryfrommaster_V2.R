library(readxl)
library(janitor)
library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(lubridate)


setwd("Box Sync/")

#WEEKLY - all participants in flow for the week of X (change X)
#SCHEDULED - all participants scheduled for sessions since the start of the study
#FLOW - all participants currently in the study

# --------------- begin code below


#read in data for master file spreadsheet and R01 information
MASTER <- read_xlsx("DEPENd/NeuroMAP/Recruitment/Participant Management/Test Files/testmaster.xlsm", sheet = "MasterData")
as_tibble(MASTER)
TIMELINE <- read_xlsx("DEPENd/NeuroMAP/Recruitment/Participant Management/RO1 Timeline.xlsx", sheet = "R01 Timeline")
AGGREGATE <- read_xlsx("DEPENd/NeuroMAP/Recruitment/Participant Management/RO1 Timeline.xlsx", sheet = "Aggregate")

#tidy up column names
MASTER<-dplyr::rename(MASTER, PAI= "PAI (M)", AI = "AI (M)", SH = "SH (M)", SPIN = "SPIN (M)", SCREEN_SENTDATE = "SCREEN: SENT DATE (M)", SCREEN_COMPLETEDATE = "SCREEN: COMPLETE DATE (M)", SCREEN_COMPLETEYN =  "SCREEN: COMPLETE Y/N (M)")
MASTER<-dplyr::rename(MASTER, INEL_CODE = "INELEGIBILITY CODE (M)", INEL_PHASE ="PHASE DEEMED INELIGIBLE (M)", INEL_YN = "ELIGIBLE YN (M)",  GROUP_SL = "GROUPED: SL (M)" )
MASTER<-dplyr::rename(MASTER, S1_DATE = "S1 DATE (M)", S2_DATE ="S2 DATE (M)", S3_DATE = "S3 DATE (M)", S4_DATE = "S4 DATE (M)", S5_DATE = "S5 DATE (M)", INVITES1_YN = "INVITED TO S1 (M)")

colnames(AGGREGATE)
#add year and month for each study timepoint

MASTER$SCREEN_SENTDATE <- as.POSIXct(strptime(MASTER$SCREEN_SENTDATE, format = '%Y-%m-%d'))


MASTER$SCREEN_COMPLETEDATE<-janitor::excel_numeric_to_date(as.numeric(as.character(MASTER$SCREEN_COMPLETEDATE)), date_system = "modern", include_time = FALSE)  


#MASTER$SCREEN_COMPLETEDATE<- as.POSIXct(strptime(MASTER$SCREEN_COMPLETEDATE, format = '%Y-%m-%d'))
MASTER$S1_DATE <- as.POSIXct(strptime(MASTER$S1_DATE, format = '%Y-%m-%d'))

MASTER$SS_MONTH <- format(MASTER$SCREEN_SENTDATE, '%m')
MASTER$SS_YEAR <- format(MASTER$SCREEN_SENTDATE, '%Y')

MASTER$SC_MONTH <- format(MASTER$SCREEN_COMPLETEDATE, '%m')
MASTER$SC_YEAR <- format(MASTER$SCREEN_COMPLETEDATE, '%Y')

MASTER$S1_MONTH <- format(MASTER$S1_DATE, '%m')
MASTER$S1_YEAR <- format(MASTER$SCREEN_COMPLETEDATE, '%Y')

# 
# MASTER <-mutate(MASTER, SS_MONTH = month(SCREEN_SENTDATE), SS_YEAR = year(SCREEN_SENTDATE))
# MASTER <-mutate(MASTER, SC_MONTH = month(SCREEN_COMPLETEDATE), SC_YEAR = year(SCREEN_COMPLETEDATE))
# MASTER <-mutate(MASTER, S1_MONTH = month(S1_DATE), S1_YEAR = year(S1_DATE))
# MASTER <-mutate(MASTER, S2_MONTH = month(S2_DATE), S2_YEAR = year(S2_DATE))
# MASTER <-mutate(MASTER, S3_MONTH = month(S3_DATE), S3_YEAR = year(S3_DATE))
# MASTER <-mutate(MASTER, S4_MONTH = month(S4_DATE), S4_YEAR = year(S4_DATE))
# MASTER <-mutate(MASTER, S5_MONTH = month(S5_DATE), S5_YEAR = year(S5_DATE))
# 
# colnames(AGGREGATE)

#convert dates to proper format

#BROKEN ____
#MASTER$SCREEN_COMPLETEDATE<-janitor::excel_numeric_to_date(as.numeric(as.character(MASTER$SCREEN_COMPLETEDATE)), date_system = "modern", include_time = FALSE)    
#MASTER$SCREEN_COMPLETEDATE<-excel_numeric_to_date(MASTER$SCREEN_COMPLETEDATE, date_system = "modern", include_time = FALSE)

#MASTER$S1_DATE<-excel_numeric_to_date(as.numeric(as.character(MASTER$S1_DATE)), date_system = "modern")
#MASTER$SCREEN_SENTDATE<-excel_numeric_to_date(as.numeric(as.character(MASTER$SCREEN_SENTDATE)), date_system = "modern")  


#group participants

#WEEKLY - all participants in flow for the week of X (change X) 
#MONTHLY -?
#SCHEDULED - all participants scheduled for sessions since the start of the study
#FLOW - all participants currently in the study
#PROJECTED - all participants scheduled, but not yet signed consent (i.e. S1 not completed)
#EXPECTED_CURRENTMONTH - Expected enrollment for month according to R01 Timetable for Recruitment
#EXPECTED_CURRENTYEAR - calls table of expected enrollment for the year according to R01 Timetable for Recruitment
#ENROLLED_A - all participants enrolled in study to date, i.e. gave consent at S1
#ENROLLED_L - all participants enrolled (i.e S1 completed, consent) under R01 critiera
#ENROLLED_S - all participants enrolled (i.e S1 completed, consent) under expanded enrollment criteria (i.e. will not complete session 4-5)


WEEKLY <- MASTER %>% 
  dplyr::select(NAME, STATUS, SCREEN_SENTDATE, SCREEN_COMPLETEDATE, SCREEN_COMPLETEYN, GROUP, SOURCE, SS_MONTH, SC_MONTH, SS_YEAR, SC_YEAR) %>%
  filter(SCREEN_SENTDATE >= as.Date("2019-07-01") & SCREEN_SENTDATE <= as.Date(" 2019-08-30")) %>%
  group_by(STATUS) 
  #transmute(cumall=STATUS)
n_WEEKLY<-tally(WEEKLY)
n_WEEKLY$sum<-sum(n_WEEKLY$n)
WEEKLY$SUM<-sum(n_WEEKLY$n)


as.factor(x)
n_WEEKLY$x <-x
n_WEEKLY<-tally(WEEKLY)
gather(n_WEEKLY, "STAUS", "x")


WEEKLY$STATUS<- factor(WEEKLY$STATUS,levels = c("Screen sent", "Completed screen", "Invited S1"))
WEEKLY$SOURCE <- factor(WEEKLY$SOURCE, levels = c("Facebook/Contact Form", "StudyFinder", "Craigslist", "Unknown"))
WEEKLY$SOURCE[is.na(WEEKLY$SOURCE)] <- "Unknown"


INVTEDS1_MONTHLY <- MASTER %>% 
  dplyr::select(NAME, STATUS, GROUP, S1_DATE, S2_DATE, S3_DATE, S4_DATE, S5_DATE, SCREEN_SENTDATE, GENDER, AGE) %>%
  ##group_by(STATUS, GROUP) %>%
  ##arrange()
  filter(SCREEN_SENTDATE >= as.Date("2019-07-01") & SCREEN_SENTDATE <= as.Date("2019-08-01"), STATUS == "Invited S1", )

#WEEKLY_TEST<- WEEKLY %>%
  #add_tally()

SCHEDULED <- MASTER %>% 
  select(NAME, STATUS, GROUP, GROUP_SL, S1_DATE) %>%
  filter(!is.na(S1_DATE)) %>%
  group_by(GROUP)

n_SCHEDULED<-tally(SCHEDULED)
  

PROJECTED <- MASTER %>%
  select(NAME, STATUS, GROUP, S1_DATE, CONSENT, S1_MONTH, GROUP_SL) %>%
  filter(S1_DATE >= as.Date("2019-01-24") & S1_DATE <= as.Date(" 2019-09-30"), STATUS == "Invited S1", CONSENT == 0) %>%
  group_by(GROUP, GROUP_SL, S1_MONTH) 
n_PROJECTED<-tally(PROJECTED) 
names(n_PROJECTED) <- c("GROUP", "GROUP_SL", "MONTH", "N_PROJECTED")
colnames(n_PROJECTED)

PROJECTED_BPD <- MASTER %>%
  select(NAME, STATUS, GROUP, S1_DATE, CONSENT, S1_MONTH, GROUP_SL) %>%
  filter(S1_DATE >= as.Date("2019-01-24") & S1_DATE <= as.Date(" 2019-09-30"), STATUS == "Invited S1", CONSENT == 1, GROUP =="BPD") %>%
  group_by(GROUP, GROUP_SL, S1_MONTH) 
n_PROJECTED_BPD<-tally(PROJECTED_BPD) 
names(n_PROJECTED_BPD) <- c("GROUP", "GROUP_SL", "MONTH", "N_PROJECTED")
colnames(n_PROJECTED_BPD)


df<-dplyr::full_join(n_PROJECTED, n_ENROLLED_A, by = "GROUP")

###MAKE THIS RETURN ERROR, OR NOTE IF 0
#ENROLLED_A <- MASTER %>% 
  #select(NAME, STATUS, GROUP, GROUP_SL, S1_DATE, CONSENT, S1_MONTH) %>%
  #filter(S1_DATE >= as.Date("2019-01-24") & S1_DATE <= as.Date(" 2019-09-30"), !is.na(S1_DATE) & CONSENT == 1) %>%
  #group_by(GROUP, S1_MONTH, GROUP_SL) 
#n_ENROLLED_A<-tally(ENROLLED_A)
  
  
ENROLLED <- MASTER %>% 
  select(NAME, STATUS, GROUP, GROUP_SL, S1_DATE, CONSENT, S1_MONTH) %>%
  filter(S1_DATE >= as.Date("2019-01-24") & S1_DATE <= as.Date(" 2019-09-30"), !is.na(S1_DATE), CONSENT == 1) %>%
  group_by(GROUP, GROUP_SL, S1_MONTH) 
n_ENROLLED<-tally(ENROLLED)
  names(n_ENROLLED) <- c("GROUP", "GROUP_SL", "MONTH", "N_ENROLLED")
  
##ENROLLED_S <- MASTER %>% 
  #select(NAME, STATUS, GROUP, GROUP_SL, S1_DATE, CONSENT) %>%
  #filter(S1_DATE >= as.Date("2019-01-24") & S1_DATE <= as.Date(" 2019-09-30"), !is.na(S1_DATE), CONSENT == 1, GROUP_SL == "S")
  #group_by(GROUP) %>%
#n_ENROLLED_S<-tally(ENROLLED_S)

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
  



####CREATE/AMMEND DF WITH NEW VALUES FOR PROJECTED, EXPECTED, ENROLLED

  
  
AGGREGATE_MOD<-semi_join(AGGREGATE, n_PROJECTED, by = ) %>%
arrange((YEAR))

AGGREGATE_RJ<-right_join(n_ENROLLED, n_PROJECTED, by = "N_ENROLLED")%>%
  arrange(GROUP)

AGGREGATE_FJ<-full_join(AGGREGATE, n_PROJECTED, by = "MONTH")%>%
  arrange((YEAR)) %>%
  group_by(YEAR_GRANT, MONTH, GROUP.x)

AGGREGATE_U<-bind_cols(n_PROJECTED, AGGREGATE)%>%
  arrange((YEAR)) %>%
  group_by(YEAR_GRANT, MONTH, GROUP.x)
  
AGGREGATE_FJ<-full_join(AGGREGATE, n_PROJECTED, by ="GROUP")%>%
  arrange((YEAR)) %>%
  group_by(YEAR_GRANT, MONTH, GROUP.x)


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






