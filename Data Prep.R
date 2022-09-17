library(RODBC)
library(hillmakeR)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(ipsR)

set.seed(50)

# Define helper function
remove_outliers <- function(x, na.rm = TRUE, ...) 
{
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y[!is.na(y)]
}

conn <- odbcConnect("sql-2008R2.improvementpath.com")

LDData <- sqlQuery(conn, "SELECT A.[PATID]
    ,A.[ACTION]
    ,A.[UNIT]
    ,A.[BED]
    ,CASE WHEN A.[BED] LIKE '%ALDR%' THEN 'C-Section Pre/Post-Op'
    WHEN A.[BED] LIKE '%3E%' THEN 'Complicated Post-Partum'
    WHEN A.[BED] LIKE '%4E%' THEN 'Uncomplicated Post-Partum'
    WHEN A.[BED] LIKE '%LDR%' THEN 'Delivery Room'
    WHEN A.[BED] LIKE '%OR%' THEN 'C-Section Room'
    WHEN A.[BED] LIKE '%TRIAGE%' THEN 'Triage'
    END AS [BED_TYPE]
    ,A.[ADT_START_TIMESTAMP]
    ,A.[ADT_STOP_TIMESTAMP]
    ,B.[NAME]
    --,B.[DOB]
    --,B.[FMP_SSN]
    --,B.[ADMIT_TYPE]
    --,C.[PATIENT_ID]
	  ,E.[RoomName]
  	,E.[estStartTime]
    ,E.[EntryDateTime]
    FROM [NMCSD_CSS_2016_11_29].[PROCESSED].[CSS_ADT] A
    INNER JOIN [NMCSD_CSS_2016_11_29].[PROCESSED].[CSS_PATIENT] B ON A.[PATID] = B.[PATID]
    LEFT JOIN (
      SELECT [PATIENT_ID]
      ,[DOD_ID]
      ,[PATIENT_FULL_NAME]
      ,[PATIENT_LAST_NAME]
      ,[PATIENT_FIRST_NAME]
      ,[PATIENT_MIDDLE_NAME]
      ,REPLACE(FMP_SSN, '/', '-') AS [FMP_SSN]
      FROM [IDIQ_WOFT_Analysis_Sandbox].[DATA].[tbl_Patient]
    ) C ON B.[FMP_SSN] = C.[FMP_SSN]
    LEFT JOIN (
        SELECT [LASTNAME] + ', ' + [FIRSTNAME] + ' ' + [MIDDLENAME] as [NAME]
              ,[DOB]s
              ,[RoomName]
              ,[estStartTime]
              ,[EntryDateTime]
        FROM [IDIQ_WOFT_Analysis_Sandbox].[S3].[Scheduled_OB_GYN_Procedures_2017_01_24]
) E ON B.[NAME] = E.[NAME]
    WHERE A.[ADT_START_TIMESTAMP] >= '6/1/2016' AND A.[ADT_STOP_TIMESTAMP] < '12/31/2016'
                   AND (
                   A.[UNIT] = '3E'
                   OR A.[UNIT] = '3W-LDR'
                   OR A.[UNIT] = '4E-MAT'
)
    AND (A.[BED] NOT LIKE '%L-D-H%') AND (A.[BED] NOT LIKE '%LD-PEND%') AND (A.[BED] NOT LIKE '%TRIAGE%')
    AND B.[NAME] NOT LIKE '%BABY%'
    ORDER BY B.[NAME], [ADT_START_TIMESTAMP]
    "
)

# Define the increase in birth volume
birth_volume <- 1.00

# Define LOS timestamp and remove ADT timestamps that are less than five minutes.
LOS <- difftime(LDData$ADT_STOP_TIMESTAMP, LDData$ADT_START_TIMESTAMP, units = "mins")
LDData <- cbind(LDData, LOS)
LDData <- LDData %>% filter(LOS > 5)

# We need a vector that gives the length of time from a patient's timestamp, to their next timestamp.
length <- nrow(LDData)
EndToBeginDifferences <- vector(length=length)
PatientEpisode <- vector(length=length)
EndToBeginDifferences[1] <- NA
PatientEpisode[1] <- 1
temp <- 1

for(i in 2:length){
  if(LDData$NAME[i]==LDData$NAME[i-1]){
    EndToBeginDifferences[i] <- as.double(difftime(LDData$ADT_START_TIMESTAMP[i], LDData$ADT_STOP_TIMESTAMP[i-1],units=c("mins")))
  }
  else
    EndToBeginDifferences[i] <- NA
}

#If the above difference was greater than 3 hours, we assume they went home.  We say this is their "next episode".
for(i in 2:length){
  if(EndToBeginDifferences[i] >= 180 & !is.na(EndToBeginDifferences[i])) {
    temp <- temp + 1
  }
  if(LDData$NAME[i]!=LDData$NAME[i-1]) {
    temp <- 1
  }
  PatientEpisode[i] <- temp
}
LDData <- cbind(LDData, PatientEpisode)

#At this point we don't care about extra ADT rows caused by patients moving from, say, one post-partum bed to another.
#Let's get rid of those.
length <- nrow(LDData)
RedundancyFlag <- vector(length=length)
RedundancyFlag[1] <- 0

for(i in 1:(length-1)){
  if((LDData$NAME[i+1]==LDData$NAME[i]) & (LDData$BED_TYPE[i+1]==LDData$BED_TYPE[i]) & (LDData$PatientEpisode[i+1]==LDData$PatientEpisode[i])){
    LDData$ADT_STOP_TIMESTAMP[i] <- LDData$ADT_STOP_TIMESTAMP[i+1]
    RedundancyFlag[i+1] <- 1
  }
  else{
    RedundancyFlag[i+1] <- 0
  }
}

LDData <- LDData[RedundancyFlag==0,]

#Now we will use a logic to change some c-sections/inductions to unscheduled.  If they arrive 72 hours before appointment, this is called unscheduled.
#Additionally, if they are entered in the system less than 12 hours before their first timestamp, they are a "walk-in" and unscheduled.
for(i in 1:length){
  if(!is.na(LDData$estStartTime[i]) & as.double(difftime(LDData$estStartTime[i],LDData$ADT_START_TIMESTAMP[i], units=c("hours"))) > 72) {
    LDData$RoomName[i] <- NA
  }
  if(abs(as.double(difftime(LDData$EntryDateTime[i],LDData$estStartTime[i], units=c("hours")))) < 12 & !is.na(LDData$EntryDateTime[i]) & !is.na(LDData$estStartTime[i])){
    LDData$RoomName[i] <- NA
  }
}

#Update LOS
LDData$LOS <- difftime(LDData$ADT_STOP_TIMESTAMP, LDData$ADT_START_TIMESTAMP, units = "mins")
LDData <- LDData[!((LDData$BED_TYPE == 'Uncomplicated Post-Partum' | LDData$BED_TYPE == 'Complicated Post-Partum')
                   & LDData$LOS < 120),]
length <- nrow(LDData)

#Create a "step" vector which represents which "step" number a patient is on, in their L&D journey.
Step <- vector(length=length)
Step[1] <- 1

for(i in 2:length){
  if((LDData$PatientEpisode[i]==LDData$PatientEpisode[i-1]) & LDData$NAME[i]==LDData$NAME[i-1]){
    Step[i]<-(Step[i-1] + 1)
  }
  else
    Step[i]<-1
}

LDData <- cbind(LDData,Step)

#Assign cases
Case <-vector(length=length)
Case[LDData$RoomName=='Induction'] <- 'Scheduled Induction'
Case[LDData$RoomName=='C-S Room 1'] <- 'Scheduled C-Section'
Case[LDData$RoomName=='C-S Room 2'] <- 'Scheduled C-Section'
Case[(LDData$RoomName=='ENDO1' | LDData$RoomName=='LOR1' | LDData$RoomName=='OR 1' | LDData$RoomName=='OR 7' | is.na(LDData$RoomName))] <- 'Unscheduled'
LDData <- cbind(LDData, Case)
LDData <- select(LDData, -RoomName)

# Create a vector of unique name/patient episode combos
LDData$NameEpisodeCombo <- paste(LDData$NAME, LDData$PatientEpisode)

#BUILD OUT THE L&D PATHS
#Now we build out the individual L&D journeys, by patient by episode!
uniques <- unique(LDData$NameEpisodeCombo)
PatientPaths <- data.frame(NameEpisodeCombo = uniques, Path = NA)
LDData$BED_TYPE <- as.character(LDData$BED_TYPE)

BuildPatientPaths <- function(uniques){
  temp_df <- LDData %>% dplyr::filter(NameEpisodeCombo == uniques)
  path <- paste(temp_df$BED_TYPE, collapse=" -> ")
  PatientPaths[PatientPaths$NameEpisodeCombo==uniques,]$Path <<- path
}

PatientPaths <- sapply(uniques, BuildPatientPaths)
PatientPaths <- data.frame(NameEpisodeCombo = names(PatientPaths), LDPath = PatientPaths, row.names = NULL)
LDData <- LDData %>%
  left_join(PatientPaths, by = c("NameEpisodeCombo" = "NameEpisodeCombo"))


######################################################################################################
######################################################################################################
# It was determined that there are 9 main paths that people typically use when they flow
# through L&D.  They could be summarized as:

# 1. Unscheduled inductions or SVDs: LDR -> PP
# 2. Scheduled inductions: LDR -> PP
# 3. Unscheduled induction turning into c-section: LDR -> OR -> RR -> PP
# 4. Scheduled induction turning into c-section: LDR -> OR -> RR -> PP
# 5. Scheduled c-sections: RR -> OR -> RR -> PP
# 6. Unscheduled c-section starting in RR: RR -> OR -> RR -> PP
# 7. Unscheduled c-section bypassing RR: OR -> RR -> PP
# 8. Scheduled c-section bypassing pre-op: OR -> RR -> PP
# 9: Patients who only enter delivery room or recovery room
# 10. Patients who only enter post-partum

# There is no path 9.  Long story.

# We simulate patients flowing through the system, based on these paths.  Meaning we looked at ADT
# timestamps, and based on complete paths and scheduled vs. unscheduled, gave everyone an ID based on these
# paths.  
# There are some assumptions that had to be made.  i.e. a LDR -> post-partum -> recovery room -> post-partum
# would probably just be assumed to be ID 1: unscheduled induction/SVD.

# Pull the lookup IDs with full paths in to the ADT data
SimulationData <- sqlQuery(conn, "SELECT A.[NAME]
	                          ,A.[PatientEpisode]
                           ,A.[BED_TYPE]
                           ,A.[LOS]
                           ,A.[ADT_START_TIMESTAMP]
                           ,A.[ADT_STOP_TIMESTAMP]
                           ,A.[Step]
                           ,A.[Case]
                           ,A.[LDPath]
                           ,B.[Lookup]
                           FROM [IDIQ_WOFT_Analysis_Sandbox].[dbo].[PathsByPatient] A
                           LEFT JOIN [IDIQ_WOFT_Analysis_Sandbox].[dbo].[Patient_Path_Lookup_Table] B
                           ON A.[LDPath] = B.[LDPath] AND A.[Case] = B.[Case]
                           ORDER BY A.NAME, ADT_START_TIMESTAMP")

###########################################################################################################################

# Find what proportion of timestamps are in UPP vs in CPP for each path
UPP_Prob_Calc <- function(id){
  temp <- dplyr::filter(SimulationData, Lookup == id)
  nrow(temp[temp$BED_TYPE=='Uncomplicated Post-Partum',]) /
    nrow(temp[temp$BED_TYPE %in% c('Uncomplicated Post-Partum', 'Complicated Post-Partum'),])
}

# Store these probabilities for input into the simulation branches
p1 <- UPP_Prob_Calc(id=1)     #68.43%
p2 <- UPP_Prob_Calc(id=2)     #73.70%
p3 <- UPP_Prob_Calc(id=3)     #48.22%
p4 <- UPP_Prob_Calc(id=4)     #52.94%
p5 <- UPP_Prob_Calc(id=5)     #73.73%
p6 <- UPP_Prob_Calc(id=6)     #57.02%
p7 <- UPP_Prob_Calc(id=7)     #25.84%
p8 <- UPP_Prob_Calc(id=8)     #60.14%
p10 <- UPP_Prob_Calc(id=10)   #11.24%

# Check: what is the aggregate?
nrow(SimulationData[SimulationData$BED_TYPE=='Uncomplicated Post-Partum',])  #1091
nrow(SimulationData[SimulationData$BED_TYPE=='Complicated Post-Partum',])  #821
#1021/1716  = 60.74% UPP,   695/1716 = 39.26% CPP

SimulationData$ArrivalHour <- hour(SimulationData$ADT_START_TIMESTAMP)
SimulationData <- SimulationData %>%
  rename(id = Lookup)

# Write a function to gather length of stays
Gather_LOS <- function(data, name, begin, finish, Lookup){
  data <- data %>%
    dplyr::filter(BED_TYPE==name) %>%
    dplyr::filter(id==Lookup) %>%
    dplyr::filter(ArrivalHour >= begin & ArrivalHour < finish) %>%
    dplyr::select(LOS)
    data$LOS <- as.numeric(data$LOS)
    return(data$LOS)
}

######################################################################################################
######################################################################################################
# Besides what "type" of patient we have, the other key inputs are LOS and interarrival times.
# I wanted to sample from the actual data (i.e. use the empirical distribution) as much as possible. The
# level of granularity to this is based off some bins of start/finish time, patient type, and room.  So if
# we want the length of stay for a scheduled induction patient in uncomplicated post-partum Monday at 2 PM,
# we would look at the LOS for all scheduled inductions in uncomplicated post-partum, who entered between 12
# and 3 PM.

# Lengths of stay for unscheduled patients who are SVDs or inductions (Delivery Room to Post-Partum)
LOS_Delivery_Room_1 <- Gather_LOS(data=SimulationData, name="Delivery Room", begin = 0, finish = 24, Lookup=1)
LOS_UPP_1_0_3 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 0, finish = 3, Lookup=1)
LOS_UPP_1_3_6 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 3, finish = 6, Lookup=1)
LOS_UPP_1_6_9 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 6, finish = 9, Lookup=1)
LOS_UPP_1_9_12 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 9, finish = 12, Lookup=1)
LOS_UPP_1_12_15 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 12, finish = 15, Lookup=1)
LOS_UPP_1_15_18 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 15, finish = 18, Lookup=1)
LOS_UPP_1_18_21 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 18, finish = 21, Lookup=1)
LOS_UPP_1_21_24 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 21, finish = 24, Lookup=1)
LOS_CPP_1_0_3 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 0, finish = 3, Lookup=1)
LOS_CPP_1_3_6 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 3, finish = 6, Lookup=1)
LOS_CPP_1_6_9 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 6, finish = 9, Lookup=1)
LOS_CPP_1_9_12 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 9, finish = 12, Lookup=1)
LOS_CPP_1_12_15 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 12, finish = 15, Lookup=1)
LOS_CPP_1_15_18 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 15, finish = 18, Lookup=1)
LOS_CPP_1_18_21 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 18, finish = 21, Lookup=1)
LOS_CPP_1_21_24 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 21, finish = 24, Lookup=1)

# Lengths of stay for scheduled inductions (Delivery Room to Post-Partum)
LOS_Delivery_Room_2 <- Gather_LOS(data=SimulationData, name="Delivery Room", begin = 0, finish = 24, Lookup=2)
LOS_UPP_2_0_3 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 0, finish = 3, Lookup=2)
LOS_UPP_2_3_6 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 3, finish = 6, Lookup=2)
LOS_UPP_2_6_9 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 6, finish = 9, Lookup=2)
LOS_UPP_2_9_12 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 9, finish = 12, Lookup=2)
LOS_UPP_2_12_15 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 12, finish = 15, Lookup=2)
LOS_UPP_2_15_18 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 15, finish = 18, Lookup=2)
LOS_UPP_2_18_21 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 18, finish = 21, Lookup=2)
LOS_UPP_2_21_24 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 21, finish = 24, Lookup=2)
LOS_CPP_2_0_3 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 0, finish = 3, Lookup=2)
LOS_CPP_2_3_6 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 3, finish = 6, Lookup=2)
LOS_CPP_2_6_9 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 6, finish = 9, Lookup=2)
LOS_CPP_2_9_12 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 9, finish = 12, Lookup=2)
LOS_CPP_2_12_15 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 12, finish = 15, Lookup=2)
LOS_CPP_2_15_18 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 15, finish = 18, Lookup=2)
LOS_CPP_2_18_21 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 18, finish = 21, Lookup=2)
LOS_CPP_2_21_24 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 21, finish = 24, Lookup=2)

# Lengths of stay for unscheduled inductions that turn into c-sections (Delivery Room to C-Section Room to Recovery Room to Post-Partum)
LOS_Delivery_Room_3 <- Gather_LOS(data=SimulationData, name="Delivery Room", begin = 0, finish = 24, Lookup=3)
LOS_UPP_3_0_3 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 0, finish = 3, Lookup=3)
LOS_UPP_3_3_6 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 3, finish = 6, Lookup=3)
LOS_UPP_3_6_9 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 6, finish = 9, Lookup=3)
LOS_UPP_3_9_12 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 9, finish = 12, Lookup=3)
LOS_UPP_3_12_15 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 12, finish = 15, Lookup=3)
LOS_UPP_3_15_18 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 15, finish = 18, Lookup=3)
LOS_UPP_3_18_21 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 18, finish = 21, Lookup=3)
LOS_UPP_3_21_24 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 21, finish = 24, Lookup=3)
LOS_CPP_3_0_3 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 0, finish = 3, Lookup=3)
LOS_CPP_3_3_6 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 3, finish = 6, Lookup=3)
LOS_CPP_3_6_9 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 6, finish = 9, Lookup=3)
LOS_CPP_3_9_12 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 9, finish = 12, Lookup=3)
LOS_CPP_3_12_15 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 12, finish = 15, Lookup=3)
LOS_CPP_3_15_18 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 15, finish = 18, Lookup=3)
LOS_CPP_3_18_21 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 18, finish = 21, Lookup=3)
LOS_CPP_3_21_24 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 21, finish = 24, Lookup=3)
LOS_OR_3 <- Gather_LOS(data=SimulationData, name="C-Section Room", begin = 0, finish = 24, Lookup=3)
LOS_RR_3 <- Gather_LOS(data=SimulationData, name="C-Section Pre/Post-Op", begin = 0, finish = 24, Lookup=3)

# Lengths of stay for scheduled inductions that turn into c-sections (Delivery Room to C-Section Room to Recovery Room to Post-Partum)
LOS_Delivery_Room_4 <- Gather_LOS(data=SimulationData, name="Delivery Room", begin = 0, finish = 24, Lookup=4)
LOS_UPP_4_0_3 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 0, finish = 3, Lookup=4)
LOS_UPP_4_3_6 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 3, finish = 6, Lookup=4)
LOS_UPP_4_6_9 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 6, finish = 9, Lookup=4)
LOS_UPP_4_9_12 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 9, finish = 12, Lookup=4)
LOS_UPP_4_12_15 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 12, finish = 15, Lookup=4)
LOS_UPP_4_15_18 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 15, finish = 18, Lookup=4)
LOS_UPP_4_18_21 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 18, finish = 21, Lookup=4)
LOS_UPP_4_21_24 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 21, finish = 24, Lookup=4)
LOS_CPP_4_0_3 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 0, finish = 3, Lookup=4)
LOS_CPP_4_3_6 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 3, finish = 6, Lookup=4)
LOS_CPP_4_6_9 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 6, finish = 9, Lookup=4)
LOS_CPP_4_9_12 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 9, finish = 12, Lookup=4)
LOS_CPP_4_12_15 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 12, finish = 15, Lookup=4)
LOS_CPP_4_15_18 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 15, finish = 18, Lookup=4)
LOS_CPP_4_18_21 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 18, finish = 21, Lookup=4)
LOS_CPP_4_21_24 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 21, finish = 24, Lookup=4)
LOS_OR_4 <- Gather_LOS(data=SimulationData, name="C-Section Room", begin = 0, finish = 24, Lookup=4)
LOS_RR_4 <- Gather_LOS(data=SimulationData, name="C-Section Pre/Post-Op", begin = 0, finish = 24, Lookup=4)

# Lengths of stay for scheduled c-sections (Recovery room to operating room to recovery room to post-partum)
LOS_UPP_5_0_3 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 0, finish = 3, Lookup=5)
LOS_UPP_5_3_6 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 3, finish = 6, Lookup=5)
LOS_UPP_5_6_9 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 6, finish = 9, Lookup=5)
LOS_UPP_5_9_12 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 9, finish = 12, Lookup=5)
LOS_UPP_5_12_15 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 12, finish = 15, Lookup=5)
LOS_UPP_5_15_18 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 15, finish = 18, Lookup=5)
LOS_UPP_5_18_21 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 18, finish = 21, Lookup=5)
LOS_UPP_5_21_24 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 21, finish = 24, Lookup=5)
LOS_CPP_5_0_3 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 0, finish = 3, Lookup=5)
LOS_CPP_5_3_6 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 3, finish = 6, Lookup=5)
LOS_CPP_5_6_9 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 6, finish = 9, Lookup=5)
LOS_CPP_5_9_12 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 9, finish = 12, Lookup=5)
LOS_CPP_5_12_15 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 12, finish = 15, Lookup=5)
LOS_CPP_5_15_18 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 15, finish = 18, Lookup=5)
LOS_CPP_5_18_21 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 18, finish = 21, Lookup=5)
LOS_CPP_5_21_24 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 21, finish = 24, Lookup=5)
LOS_OR_5 <- Gather_LOS(data=SimulationData, name="C-Section Room", begin = 0, finish = 24, Lookup=5)
LOS_RR_5 <- Gather_LOS(data=SimulationData, name="C-Section Pre/Post-Op", begin = 0, finish = 24, Lookup=5)

# Lengths of stay for unscheduled c-sections who start in pre-op
LOS_UPP_6_0_3 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 0, finish = 3, Lookup=6)
LOS_UPP_6_3_6 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 3, finish = 6, Lookup=6)
LOS_UPP_6_6_9 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 6, finish = 9, Lookup=6)
LOS_UPP_6_9_12 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 9, finish = 12, Lookup=6)
LOS_UPP_6_12_15 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 12, finish = 15, Lookup=6)
LOS_UPP_6_15_18 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 15, finish = 18, Lookup=6)
LOS_UPP_6_18_21 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 18, finish = 21, Lookup=6)
LOS_UPP_6_21_24 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 21, finish = 24, Lookup=6)
LOS_CPP_6_0_3 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 0, finish = 3, Lookup=6)
LOS_CPP_6_3_6 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 3, finish = 6, Lookup=6)
LOS_CPP_6_6_9 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 6, finish = 9, Lookup=6)
LOS_CPP_6_9_12 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 9, finish = 12, Lookup=6)
LOS_CPP_6_12_15 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 12, finish = 15, Lookup=6)
LOS_CPP_6_15_18 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 15, finish = 18, Lookup=6)
LOS_CPP_6_18_21 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 18, finish = 21, Lookup=6)
LOS_CPP_6_21_24 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 21, finish = 24, Lookup=6)
LOS_OR_6 <- Gather_LOS(data=SimulationData, name="C-Section Room", begin = 0, finish = 24, Lookup=6)
LOS_RR_6 <- Gather_LOS(data=SimulationData, name="C-Section Pre/Post-Op", begin = 0, finish = 24, Lookup=6)

# Lengths of stay for unscheduled c-sections who start in the OR
LOS_UPP_7_0_3 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 0, finish = 3, Lookup=7)
LOS_UPP_7_3_6 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 3, finish = 6, Lookup=7)
LOS_UPP_7_6_9 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 6, finish = 9, Lookup=7)
LOS_UPP_7_9_12 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 9, finish = 12, Lookup=7)
LOS_UPP_7_12_15 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 12, finish = 15, Lookup=7)
LOS_UPP_7_15_18 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 15, finish = 18, Lookup=7)
LOS_UPP_7_18_21 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 18, finish = 21, Lookup=7)
LOS_UPP_7_21_24 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 21, finish = 24, Lookup=7)
LOS_CPP_7_0_3 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 0, finish = 3, Lookup=7)
LOS_CPP_7_3_6 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 3, finish = 6, Lookup=7)
LOS_CPP_7_6_9 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 6, finish = 9, Lookup=7)
LOS_CPP_7_9_12 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 9, finish = 12, Lookup=7)
LOS_CPP_7_12_15 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 12, finish = 15, Lookup=7)
LOS_CPP_7_15_18 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 15, finish = 18, Lookup=7)
LOS_CPP_7_18_21 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 18, finish = 21, Lookup=7)
LOS_CPP_7_21_24 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 21, finish = 24, Lookup=7)
LOS_OR_7 <- Gather_LOS(data=SimulationData, name="C-Section Room", begin = 0, finish = 24, Lookup=7)
LOS_RR_7 <- Gather_LOS(data=SimulationData, name="C-Section Pre/Post-Op", begin = 0, finish = 24, Lookup=7)

# Lengths of stay for scheduled c-sections who start in the OR
LOS_UPP_8_0_3 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 0, finish = 3, Lookup=8)
LOS_UPP_8_3_6 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 3, finish = 6, Lookup=8)
LOS_UPP_8_6_9 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 6, finish = 9, Lookup=8)
LOS_UPP_8_9_12 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 9, finish = 12, Lookup=8)
LOS_UPP_8_12_15 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 12, finish = 15, Lookup=8)
LOS_UPP_8_15_18 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 15, finish = 18, Lookup=8)
LOS_UPP_8_18_21 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 18, finish = 21, Lookup=8)
LOS_UPP_8_21_24 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 21, finish = 24, Lookup=8)
LOS_CPP_8_0_3 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 0, finish = 3, Lookup=8)
LOS_CPP_8_3_6 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 3, finish = 6, Lookup=8)
LOS_CPP_8_6_9 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 6, finish = 9, Lookup=8)
LOS_CPP_8_9_12 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 9, finish = 12, Lookup=8)
LOS_CPP_8_12_15 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 12, finish = 15, Lookup=8)
LOS_CPP_8_15_18 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 15, finish = 18, Lookup=8)
LOS_CPP_8_18_21 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 18, finish = 21, Lookup=8)
LOS_CPP_8_21_24 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 21, finish = 24, Lookup=8)
LOS_OR_8 <- Gather_LOS(data=SimulationData, name="C-Section Room", begin = 0, finish = 24, Lookup=8)
LOS_RR_8 <- Gather_LOS(data=SimulationData, name="C-Section Pre/Post-Op", begin = 0, finish = 24, Lookup=8)

# Lengths of stay for patients who only appear to drop in to the delivery room or recovery room
LOS_Delivery_Room_9 <- Gather_LOS(data=SimulationData, name="Delivery Room", begin = 0, finish = 24, Lookup=9)
LOS_RR_9 <- Gather_LOS(data=SimulationData, name="C-Section Pre/Post-Op", begin = 0, finish = 24, Lookup=9)

# Antepartum or complications in post-partum
LOS_UPP_10_0_3 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 0, finish = 3, Lookup=10)
LOS_UPP_10_3_6 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 3, finish = 6, Lookup=10)
LOS_UPP_10_6_9 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 6, finish = 9, Lookup=10)
LOS_UPP_10_9_12 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 9, finish = 12, Lookup=10)
LOS_UPP_10_12_15 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 12, finish = 15, Lookup=10)
LOS_UPP_10_15_18 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 15, finish = 18, Lookup=10)
LOS_UPP_10_18_21 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 18, finish = 21, Lookup=10)
LOS_UPP_10_21_24 <- Gather_LOS(data=SimulationData, name="Uncomplicated Post-Partum", begin = 21, finish = 24, Lookup=10)
LOS_CPP_10_0_3 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 0, finish = 3, Lookup=10)
LOS_CPP_10_3_6 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 3, finish = 6, Lookup=10)
LOS_CPP_10_6_9 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 6, finish = 9, Lookup=10)
LOS_CPP_10_9_12 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 9, finish = 12, Lookup=10)
LOS_CPP_10_12_15 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 12, finish = 15, Lookup=10)
LOS_CPP_10_15_18 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 15, finish = 18, Lookup=10)
LOS_CPP_10_18_21 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 18, finish = 21, Lookup=10)
LOS_CPP_10_21_24 <- Gather_LOS(data=SimulationData, name="Complicated Post-Partum", begin = 21, finish = 24, Lookup=10)

#Paths 2 and 4 appear to be scheduled as inductions, Paths 5 and 8 appear to be scheduled as c-sections.

## Next, we need to gather the interarrival times.  This is the time between a patient at step 1 of their process, and another patient at step 1 of their
## process, of the same lookup ID.
Step1 <- SimulationData %>%
  dplyr::filter(Step == 1) %>%
  dplyr::select(NAME, PatientEpisode, ADT_START_TIMESTAMP, ADT_STOP_TIMESTAMP, LDPath, id) %>%
  dplyr::arrange(id, ADT_START_TIMESTAMP)
Step1$hour <- hour(Step1$ADT_START_TIMESTAMP)
Step1$wday <- wday(Step1$ADT_START_TIMESTAMP)

length <- nrow(Step1)
Step1$IAT <- rep(NA, length)

for(i in 1:(length-1)){
  if(Step1$id[i+1]==Step1$id[i]){
    Step1$IAT[i] <- as.double(difftime(Step1$ADT_START_TIMESTAMP[i+1], Step1$ADT_START_TIMESTAMP[i],units=c("mins")))
  }
}

# Use this for looking at arrival patterns
ArrivalTable <- Step1 %>%
  dplyr::filter(id %in% c(1, 3, 6, 7, 9, 10)) %>%
  group_by(wday, hour) %>%
  summarize(count = n()) %>%
  arrange(wday, hour)

# We need the IATs for the unscheduled patients: Lookup IDs 1, 3, 6, 7, 9, and 10.
Gather_IAT <- function(data, days, hours){
  data <- data %>%
    dplyr::filter(id %in% c(1, 3, 6, 7, 9, 10)) %>%
    dplyr::filter(wday %in% days) %>%
    dplyr::filter(hour %in% hours) %>%
    dplyr::select(IAT)
  data$IAT <- as.numeric(data$IAT)
  data$IAT <- data$IAT + 1
  return(data$IAT)
}

# Removing the outliers will mostly remove outliers on the high side.

###
# You'll see this birth volume parameter.  That's how we set things to change if we want to see 3%,
# 6%, etc. increases in birth volume.
IAT_Sun_0_2 <- remove_outliers(Gather_IAT(data = Step1, days = 1, hours = 0:2)) / birth_volume
IAT_Sun_3_5 <- remove_outliers(Gather_IAT(data = Step1, days = 1, hours = 3:5)) / birth_volume
IAT_Sun_6_8 <- remove_outliers(Gather_IAT(data = Step1, days = 1, hours = 6:8)) / birth_volume
IAT_Sun_9_11 <- remove_outliers(Gather_IAT(data = Step1, days = 1, hours = 9:11)) / birth_volume
IAT_Sun_12_14 <- remove_outliers(Gather_IAT(data = Step1, days = 1, hours = 12:14)) / birth_volume
IAT_Sun_15_17 <- remove_outliers(Gather_IAT(data = Step1, days = 1, hours = 15:17)) / birth_volume
IAT_Sun_18_20 <- remove_outliers(Gather_IAT(data = Step1, days = 1, hours = 18:20)) / birth_volume
IAT_Sun_21_23 <- remove_outliers(Gather_IAT(data = Step1, days = 1, hours = 21:23)) / birth_volume

IAT_Mon_0_2 <- remove_outliers(Gather_IAT(data = Step1, days = 2, hours = 0:2)) / birth_volume
IAT_Mon_3_5 <- remove_outliers(Gather_IAT(data = Step1, days = 2, hours = 3:5)) / birth_volume
IAT_Mon_6_8 <- remove_outliers(Gather_IAT(data = Step1, days = 2, hours = 6:8)) / birth_volume
IAT_Mon_9_11 <- remove_outliers(Gather_IAT(data = Step1, days = 2, hours = 9:11)) / birth_volume
IAT_Mon_12_14 <- remove_outliers(Gather_IAT(data = Step1, days = 2, hours = 12:14)) / birth_volume
IAT_Mon_15_17 <- remove_outliers(Gather_IAT(data = Step1, days = 2, hours = 15:17)) / birth_volume
IAT_Mon_18_20 <- remove_outliers(Gather_IAT(data = Step1, days = 2, hours = 18:20)) / birth_volume
IAT_Mon_21_23 <- remove_outliers(Gather_IAT(data = Step1, days = 2, hours = 21:23)) / birth_volume

IAT_Tues_0_2 <- remove_outliers(Gather_IAT(data = Step1, days = 3, hours = 0:2)) / birth_volume
IAT_Tues_3_5 <- remove_outliers(Gather_IAT(data = Step1, days = 3, hours = 3:5)) / birth_volume
IAT_Tues_6_8 <- remove_outliers(Gather_IAT(data = Step1, days = 3, hours = 6:8)) / birth_volume
IAT_Tues_9_11 <- remove_outliers(Gather_IAT(data = Step1, days = 3, hours = 9:11)) / birth_volume
IAT_Tues_12_14 <- remove_outliers(Gather_IAT(data = Step1, days = 3, hours = 12:14)) / birth_volume
IAT_Tues_15_17 <- remove_outliers(Gather_IAT(data = Step1, days = 3, hours = 15:17)) / birth_volume
IAT_Tues_18_20 <- remove_outliers(Gather_IAT(data = Step1, days = 3, hours = 18:20)) / birth_volume
IAT_Tues_21_23 <- remove_outliers(Gather_IAT(data = Step1, days = 3, hours = 21:23)) / birth_volume

IAT_Wed_0_2 <- remove_outliers(Gather_IAT(data = Step1, days = 4, hours = 0:2)) / birth_volume
IAT_Wed_3_5 <- remove_outliers(Gather_IAT(data = Step1, days = 4, hours = 3:5)) / birth_volume
IAT_Wed_6_8 <- remove_outliers(Gather_IAT(data = Step1, days = 4, hours = 6:8)) / birth_volume
IAT_Wed_9_11 <- remove_outliers(Gather_IAT(data = Step1, days = 4, hours = 9:11)) / birth_volume
IAT_Wed_12_14 <- remove_outliers(Gather_IAT(data = Step1, days = 4, hours = 12:14)) / birth_volume
IAT_Wed_15_17 <- remove_outliers(Gather_IAT(data = Step1, days = 4, hours = 15:17)) / birth_volume
IAT_Wed_18_20 <- remove_outliers(Gather_IAT(data = Step1, days = 4, hours = 18:20)) / birth_volume
IAT_Wed_21_23 <- remove_outliers(Gather_IAT(data = Step1, days = 4, hours = 21:23)) / birth_volume

IAT_Thurs_0_2 <- remove_outliers(Gather_IAT(data = Step1, days = 5, hours = 0:2)) / birth_volume
IAT_Thurs_3_5 <- remove_outliers(Gather_IAT(data = Step1, days = 5, hours = 3:5)) / birth_volume
IAT_Thurs_6_8 <- remove_outliers(Gather_IAT(data = Step1, days = 5, hours = 6:8)) / birth_volume
IAT_Thurs_9_11 <- remove_outliers(Gather_IAT(data = Step1, days = 5, hours = 9:11)) / birth_volume
IAT_Thurs_12_14 <- remove_outliers(Gather_IAT(data = Step1, days = 5, hours = 12:14)) / birth_volume
IAT_Thurs_15_17 <- remove_outliers(Gather_IAT(data = Step1, days = 5, hours = 15:17)) / birth_volume
IAT_Thurs_18_20 <- remove_outliers(Gather_IAT(data = Step1, days = 5, hours = 18:20)) / birth_volume
IAT_Thurs_21_23 <- remove_outliers(Gather_IAT(data = Step1, days = 5, hours = 21:23)) / birth_volume

IAT_Fri_0_2 <- remove_outliers(Gather_IAT(data = Step1, days = 6, hours = 0:2)) / birth_volume
IAT_Fri_3_5 <- remove_outliers(Gather_IAT(data = Step1, days = 6, hours = 3:5)) / birth_volume
IAT_Fri_6_8 <- remove_outliers(Gather_IAT(data = Step1, days = 6, hours = 6:8)) / birth_volume
IAT_Fri_9_11 <- remove_outliers(Gather_IAT(data = Step1, days = 6, hours = 9:11)) / birth_volume
IAT_Fri_12_14 <- remove_outliers(Gather_IAT(data = Step1, days = 6, hours = 12:14)) / birth_volume
IAT_Fri_15_17 <- remove_outliers(Gather_IAT(data = Step1, days = 6, hours = 15:17)) / birth_volume
IAT_Fri_18_20 <- remove_outliers(Gather_IAT(data = Step1, days = 6, hours = 18:20)) / birth_volume
IAT_Fri_21_23 <- remove_outliers(Gather_IAT(data = Step1, days = 6, hours = 21:23)) / birth_volume

IAT_Sat_0_2 <- remove_outliers(Gather_IAT(data = Step1, days = 7, hours = 0:2)) / birth_volume
IAT_Sat_3_5 <- remove_outliers(Gather_IAT(data = Step1, days = 7, hours = 3:5)) / birth_volume
IAT_Sat_6_8 <- remove_outliers(Gather_IAT(data = Step1, days = 7, hours = 6:8)) / birth_volume
IAT_Sat_9_11 <- remove_outliers(Gather_IAT(data = Step1, days = 7, hours = 9:11)) / birth_volume
IAT_Sat_12_14 <- remove_outliers(Gather_IAT(data = Step1, days = 7, hours = 12:14)) / birth_volume
IAT_Sat_15_17 <- remove_outliers(Gather_IAT(data = Step1, days = 7, hours = 15:17)) / birth_volume
IAT_Sat_18_20 <- remove_outliers(Gather_IAT(data = Step1, days = 7, hours = 18:20)) / birth_volume
IAT_Sat_21_23 <- remove_outliers(Gather_IAT(data = Step1, days = 7, hours = 21:23)) / birth_volume


p_induction <- nrow(Step1[Step1$id==2,])/nrow(Step1[Step1$id %in% c(2,4),])
p_csection <- nrow(Step1[Step1$id==5,])/nrow(Step1[Step1$id %in% c(5,8),])

p_path1 <- nrow(Step1[Step1$id==1,])/nrow(Step1[Step1$id %in% c(1,3,6,7,9,10),])
p_path3 <- nrow(Step1[Step1$id==3,])/nrow(Step1[Step1$id %in% c(1,3,6,7,9,10),])
p_path6 <- nrow(Step1[Step1$id==6,])/nrow(Step1[Step1$id %in% c(1,3,6,7,9,10),])
p_path7 <- nrow(Step1[Step1$id==7,])/nrow(Step1[Step1$id %in% c(1,3,6,7,9,10),])
p_path9 <- nrow(Step1[Step1$id==9,])/nrow(Step1[Step1$id %in% c(1,3,6,7,9,10),])
p_path10 <- nrow(Step1[Step1$id==10,])/nrow(Step1[Step1$id %in% c(1,3,6,7,9,10),])
