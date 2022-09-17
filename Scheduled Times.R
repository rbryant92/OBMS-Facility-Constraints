library(dplyr)
library(RODBC)
library(lubridate)

######################################################################################################
# For scheduled inductions and scheduled c-sections, we use the actual times they are scheduled in the
# data.  
# Let's say in 26 out of 33 Tuesdays at 7:30am, there is a scheduled c-section.  Then whenever we hit this
# corresponding time, in the simulation, we will have a 26/33 = 0.788 probability of a scheduled c-section
# entering the system.
# We need static times to load into the simulation.
######################################################################################################

conn <- odbcConnect("sql-2008R2.improvementpath.com")
S3 <- sqlQuery(conn, "SELECT * FROM [IDIQ_WOFT_Analysis_Sandbox].[S3].[Scheduled_OB_GYN_Procedures_2017_01_24]")
difftime(max(S3$estStartTime), min(S3$estStartTime), units="weeks")   #33.2 weeks, a Wednesday through Thursday

# If we do a count(*) on room name, it looks like 383/33.2 inductions or 230/33.2 c-sections per week is standard. (11.54, 6.93)
# One pass at the InductionTimes and CSectionTimes vectors yields 601/52 inductions or 317/52 c-sections per week. (11.56, 6.10)

S3 <- S3 %>%
  dplyr::select(PatientID, LastName, FirstName, RoomName, estStartTime) %>%
  dplyr::mutate(ScheduledDay = wday(estStartTime),
         ScheduledHour = hour(estStartTime),
         ScheduledMinute = minute(estStartTime))

S3_Inductions <- S3 %>% dplyr::filter(RoomName == 'Induction')
S3_Csections <- S3 %>% dplyr::filter(RoomName != 'Induction')

# Collect counts of inductions and c-sections performed at given day/hour/minute combinations.
InductionCount <- S3_Inductions %>%
  group_by(ScheduledDay, ScheduledHour, ScheduledMinute) %>%
  summarize(count_procedures = n()) %>%
  arrange(ScheduledDay, ScheduledHour)
CSectionCount <- S3_Csections %>%
  group_by(ScheduledDay, ScheduledHour, ScheduledMinute) %>%
  summarize(count_procedures = n()) %>%
  arrange(ScheduledDay, ScheduledHour)

# Join this to a calendar which has 7 days, 24 hours, and 4 minute blocks (every 15 minutes).
# 7*24*4 = 672
Calendar <- data.frame(
  ScheduledDay = rep(1:7, each=96),
  ScheduledHour = rep(0:23, each=4),
  ScheduledMinute = rep(c(0, 15, 30, 45), times=168)
)

## Create a data frame with each time block, and the probability of a procedure occurring that week at that time frame.
# Inductions
InductionProbs <- InductionCount %>%
  right_join(Calendar, by=c("ScheduledDay" = "ScheduledDay", "ScheduledHour" = "ScheduledHour",
                            "ScheduledMinute" = "ScheduledMinute")) %>%
  mutate(count_procedures = replace(count_procedures, which(is.na(count_procedures)), 0)) %>%
  mutate(probability = ifelse(ScheduledDay %in% c(4,5), count_procedures/34, count_procedures/33))
InductionProbs <- as.data.frame(InductionProbs)

ScheduledDay <- rep(InductionProbs$ScheduledDay, 104)
ScheduledHour <- rep(InductionProbs$ScheduledHour, 104)
ScheduledMinute <- rep(InductionProbs$ScheduledMinute, 104)
probability <- rep(InductionProbs$probability, 104)

# Construct induction data frame
InductionProbs <- data.frame(
  ScheduledDay = ScheduledDay,
  ScheduledHour = ScheduledHour,
  ScheduledMinute = ScheduledMinute,
  probability = probability
)

InductionProbs <- InductionProbs %>%
  mutate(sim_mins = (row(InductionProbs)[,1] * 15)-15)

# C-sections
CSectionProbs <- CSectionCount %>%
  right_join(Calendar, by=c("ScheduledDay" = "ScheduledDay", "ScheduledHour" = "ScheduledHour",
                            "ScheduledMinute" = "ScheduledMinute")) %>%
  mutate(count_procedures = replace(count_procedures, which(is.na(count_procedures)), 0)) %>%
  mutate(probability = ifelse(ScheduledDay %in% c(4,5), count_procedures/34, count_procedures/33))
CSectionProbs <- as.data.frame(CSectionProbs)

# Construct c-section data frame
ScheduledDay <- rep(CSectionProbs$ScheduledDay, 104)
ScheduledHour <- rep(CSectionProbs$ScheduledHour, 104)
ScheduledMinute <- rep(CSectionProbs$ScheduledMinute, 104)
probability <- rep(CSectionProbs$probability, 104)
CSectionProbs <- data.frame(
  ScheduledDay = ScheduledDay,
  ScheduledHour = ScheduledHour,
  ScheduledMinute = ScheduledMinute,
  probability = probability
)

CSectionProbs <- CSectionProbs %>%
  mutate(sim_mins = (row(CSectionProbs)[,1] * 15)-15)

InductionProbs$probability <- InductionProbs$probability * birth_volume
CSectionProbs$probability <- CSectionProbs$probability * birth_volume
