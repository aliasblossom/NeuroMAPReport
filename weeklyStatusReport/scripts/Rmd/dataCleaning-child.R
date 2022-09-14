#directory for worksheet with participant data
ws_path <- "~/Documents/DepenD Lab/NeuroMAP/NMAPReports/weeklyStatusReport/data/rawData/NEUROMAP Master V2_Copy.xlsx"
s3_path <- "~/Documents/DepenD Lab/NeuroMAP/NMAPReports/weeklyStatusReport/data/rawData/s3_quality_data.xlsx"


#read in all worksheets for the s3 progress report
s3_report_data <- s3_path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = s3_path)

#return the names of each sheet
neuroMAP_master<- ws_path %>% excel_sheets()

s3_notes <- s3_path %>% excel_sheets()
EnrollOverview <- read_excel(ws_path, sheet = "RO1 Aggregate")

#load master participant data worksheet
Master<- read_excel(ws_path, sheet = "MasterData")


s3_shrooms <-read_excel(s3_path, sheet = "Sorting Mushroom Notes")

#tidy up column names for Master

Master<-Master %>% 
  rename(
    SCREEN_SENTDATE = "SCREEN: SENT DATE",
    SCREEN_COMPLETEDATE = "SCREEN: COMPLETE DATE", 
    SCREEN_COMPLETEYN = "SCREEN: COMPLETE Y/N", INEL_CODE = "INELEGIBILITY CODE", 
    INEL_PHASE ="PHASE DEEMED INELIGIBLE", 
    INEL_YN = "ELIGIBLE YN",
    NAME = "Name")

#create record of start month and year for participants
Master$SCREEN_SENTDATE <- as.POSIXct(strptime(Master$SCREEN_SENTDATE, format = '%Y-%m-%d'))
Master$SCREEN_COMPLETEDATE <- as.POSIXct(strptime(Master$SCREEN_COMPLETEDATE, format = '%Y-%m-%d'))
#Master$S1_DATE <- as.POSIXct(strptime(Master$S1_DATE, format = '%Y-%m-%d'))

Master$SCREEN_SENT_MONTH <- format(Master$SCREEN_SENTDATE, '%m')
Master$SCREEN_SENT_YEAR <- format(Master$SCREEN_SENTDATE, '%Y')

Master$SCREEN_COMPLETE_MONTH <- format(Master$SCREEN_COMPLETEDATE, '%m')
Master$SCREEN_COMPLETE_YEAR <- format(Master$SCREEN_COMPLETEDATE, '%Y')

#Master$S1_MONTH <- format(Master$S1_DATE, '%m')
#Master$S1_YEAR <- format(Master$SCREEN_COMPLETEDATE, '%Y')

#factorize source
Master$SOURCE <- as.factor(Master$SOURCE)


#Master$STATUS <-factor(Master$STATUS, levels = c("Completed S2", "Completed screen", "Inquiry", "Invited S1", "No longer interested", "Scheduled S1", "Screen sent"))
levels(Master$STATUS)[levels(Master$STATUS)=="Screen sent"] <- "Screen Sent, Not Completed"
levels(Master$STATUS)[levels(Master$STATUS)=="Completed screen"] <- "Ineligible After Screen"

Master$SOURCE[is.na(Master$SOURCE)] <- "Unknown"

#establish path for Tracker file, load enrolled participant data
Tracker <-read_excel(ws_path, sheet = "Tracker")

Tracker <- Tracker %>%
  #rename column names
  rename(
    R01_ENROLLMENT = "FMRI GROUP (Y/N)", 
    STATUS = "NOTES ABOUT STATUS") %>%
  #create variable with participant status 
  mutate(`Status - Recode` = case_when(
    .$STATUS == "Scheduled S1" ~ "S1",
    .$STATUS == "Canceled S1" ~ "Not Enrolled",
    .$STATUS == "Scheduled S1 FU" ~ "S1",
    .$STATUS == "Completed S1" ~ "Stuck",
    .$STATUS == "Scheduled S2" ~ "S2",
    .$STATUS == "Completed S2" ~ "Stuck",
    .$STATUS == "Scheduled S3, Scheduled S4" ~ "S3/S4 Scheduled",
    .$STATUS == "Scheduled S3" ~ "S3",
    .$STATUS == "S3 Not Completed" ~ "Stuck",
    .$STATUS == "Completed S3" & .$R01_ENROLLMENT == "Y" ~ "Scan Needed",
    .$STATUS == "Completed S3" & .$R01_ENROLLMENT == "N" ~ "No fMRI - Completed T1",
    .$STATUS == "Scheduling S4" ~ "Scheduling",
    .$STATUS == "Invited S3" ~ "Scheduling",
    .$STATUS == "Ineligible" ~ "Ineligible",
    .$STATUS == "Non Responsive" ~ "MIA",
    .$STATUS == "Completed S4" ~ "fMRI - Completed T1",
    .$STATUS == "Non Responsive/Missed Session/Stuck" ~ "Stuck",
    .$STATUS == "Scheduled S4" ~ "S4",
    .$STATUS == "Scheduled S3, Completed S4" ~ "S3 Scheduled, S4 Completed",
    .$STATUS == "Excluded" ~ "Ineligible",
    .$STATUS == "Completed S3, Scheduled S4" ~ "S4",
    .$`STUDY PROGRESS` == "Mia" ~ "MIA"))
# ) %>%
# select(NEW_VAR) %>%
# drop_na()%>%
# dplyr::filter(!NEW_VAR %in% c("NR", "INEL")) %>%
# ggplot(aes(x=NEW_VAR)) +
# geom_bar(stat="count")

#TRACKER$`STUDY PROGRESS`<-TRACKER$`STUDY PROGRESS` %>% str_to_title() 

# ) %>%
# select(NEW_VAR) %>%
# drop_na()%>%
# dplyr::filter(!NEW_VAR %in% c("NR", "INEL")) %>%
# ggplot(aes(x=NEW_VAR)) +
# geom_bar(stat="count")

#Master$NAME<-gsub("(?<=[A-Z])[^A-Z]+", "", Master$NAME, perl = TRUE) # remove names and make initials
#TRACKER$NAME<-gsub("(?<=[A-Z])[^A-Z]+", "", TRACKER$NAME, perl = TRUE) # remove names and make initials TRACKER

#TRACKER$`S1 FU 1`<- as_date(TRACKER$`S1 FU 1`)
