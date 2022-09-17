##############################################################################################
# This simulation was created in July 2017, using data from a hospital currently supporting
# about 250 births per months.   The motivation is to discover how many more births the
# hospital may be able to support before encountering capacity issues.    For example, one
# of the various rooms having more patients than beds available >5% of the time was considered
# an unacceptable level of risk.   
##############################################################################################

library(simmer)
library(simmer.plot)
library(ggplot2)
library(dplyr)
library(parallel)
library(Rcpp)
library(gridExtra)

##########################################################

source("Data Prep.R")
source("Scheduled Times.R")

SelectTimes <- function(data, row){
  return(sample(c(1,0), size = 1, prob=c(data$probability[row], 1-data$probability[row])))
}

# Define environment
env <- simmer("LaborAndDelivery")

#### We have ten distinct patient paths:

# Path 1: Unscheduled SVD or induction (LDR -> PP)
Path1 <- trajectory("random SVD/induction") %>%
  seize("delivery room", 1) %>%
  timeout(function() sample(LOS_Delivery_Room_1, 1, replace=TRUE)) %>%
  branch(function() sample(c(1,2), size=1, replace=TRUE, prob=c(p1, (1-p1))), continue=c(T, T),
        trajectory("path 1 uncomplicated post-partum") %>%
          seize("uncomplicated post-partum", 1, continue=TRUE,
                post.seize = trajectory() %>% release("delivery room")) %>%
          timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_UPP_1_0_3, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_UPP_1_3_6, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_UPP_1_6_9, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_UPP_1_9_12, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_UPP_1_12_15, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_UPP_1_15_18, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_UPP_1_18_21, 1, replace=TRUE),
                sample(LOS_UPP_1_21_24))))))))) %>%
          release("uncomplicated post-partum", 1),
        trajectory("path 1 complicated post-partum") %>%
          seize("complicated post-partum", 1, continue=TRUE,
                post.seize = trajectory() %>% release("delivery room")) %>%
          timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_CPP_1_0_3, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_CPP_1_3_6, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_CPP_1_6_9, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_CPP_1_9_12, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_CPP_1_12_15, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_CPP_1_15_18, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_CPP_1_18_21, 1, replace=TRUE),
                sample(LOS_CPP_1_21_24))))))))) %>%
          release("complicated post-partum")
  )

# Path 2: Scheduled induction (LDR -> PP)
Path2 <- trajectory("scheduled induction") %>%
  seize("delivery room", 1) %>%
  timeout(function() sample(LOS_Delivery_Room_2, 1, replace=TRUE)) %>%
  branch(function() sample(c(1,2), size=1, replace=TRUE, prob=c(p2, (1-p2))), continue=c(T, T),
         trajectory("path 2 uncomplicated post-partum") %>%
           seize("uncomplicated post-partum", 1, continue=TRUE,
                 post.seize = trajectory() %>% release("delivery room")) %>%
           timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_UPP_2_0_3, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_UPP_2_3_6, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_UPP_2_6_9, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_UPP_2_9_12, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_UPP_2_12_15, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_UPP_2_15_18, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_UPP_2_18_21, 1, replace=TRUE),
                sample(LOS_UPP_2_21_24))))))))) %>%
           release("uncomplicated post-partum", 1),
         trajectory("path 2 complicated post-partum") %>%
           seize("complicated post-partum", 1, continue=TRUE,
                 post.seize = trajectory() %>% release("delivery room")) %>%
           timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_CPP_1_0_3, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_CPP_1_3_6, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_CPP_1_6_9, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_CPP_1_9_12, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_CPP_2_12_15, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_CPP_1_15_18, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_CPP_2_18_21, 1, replace=TRUE),
                sample(LOS_CPP_2_21_24))))))))) %>%
           release("complicated post-partum")
  )

# Path 3: Unscheduled induction turned to c-section (LDR -> OR -> RR -> PP)
Path3 <- trajectory("unscheduled induction/c-section") %>%
  seize("delivery room", 1) %>%
  timeout(function() sample(LOS_Delivery_Room_3, 1, replace=TRUE)) %>%
  release("delivery room", 1) %>%
  seize("operating room", 1) %>%
  timeout(function() sample(LOS_OR_3, 1, replace=TRUE)) %>%
  release("operating room", 1) %>%
  seize("recovery room", 1) %>%
  timeout(function() sample(LOS_RR_3, 1, replace=TRUE)) %>%
  branch(function() sample(c(1,2), size=1, replace=TRUE, prob=c(p3, (1-p3))), continue=c(T, T),
         trajectory("path 3 uncomplicated post-partum") %>%
           seize("uncomplicated post-partum", 1, continue=TRUE,
                 post.seize = trajectory() %>% release("recovery room")) %>%
           timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_UPP_3_0_3, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_UPP_3_3_6, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_UPP_3_6_9, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_UPP_3_9_12, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_UPP_3_12_15, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_UPP_3_15_18, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_UPP_3_18_21, 1, replace=TRUE),
                  sample(LOS_UPP_3_21_24))))))))) %>%
           release("uncomplicated post-partum", 1),
         trajectory("path 3 complicated post-partum") %>%
           seize("complicated post-partum", 1, continue=TRUE,
                 post.seize = trajectory() %>% release("recovery room")) %>%
           timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_CPP_3_0_3, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_CPP_1_3_6, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_CPP_3_6_9, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_CPP_3_9_12, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_CPP_3_12_15, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_CPP_3_15_18, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_CPP_3_18_21, 1, replace=TRUE),
                  sample(LOS_CPP_3_21_24))))))))) %>%
           release("complicated post-partum")
  )

# Path 4: Scheduled induction turned to c-section (LDR -> OR -> RR -> PP)
Path4 <- trajectory("scheduled induction/c-section") %>%
  seize("delivery room", 1) %>%
  timeout(function() sample(LOS_Delivery_Room_4, 1, replace=TRUE)) %>%
  release("delivery room", 1) %>%
  seize("operating room", 1) %>%
  timeout(function() sample(LOS_OR_4, 1, replace=TRUE)) %>%
  release("operating room", 1) %>%
  seize("recovery room", 1) %>%
  timeout(function() sample(LOS_RR_4, 1, replace=TRUE)) %>%
  branch(function() sample(c(1,2), size=1, replace=TRUE, prob=c(p4, (1-p4))), continue=c(T, T),
         trajectory("path 4 uncomplicated post-partum") %>%
           seize("uncomplicated post-partum", 1, continue=TRUE,
                 post.seize = trajectory() %>% release("recovery room")) %>%
           timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_UPP_4_0_3, 1, replace=TRUE),
                    ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_UPP_1_0_3, 1, replace=TRUE),
                    ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_UPP_4_6_9, 1, replace=TRUE),
                    ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_UPP_1_9_12, 1, replace=TRUE),
                    ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_UPP_4_12_15, 1, replace=TRUE),
                    ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_UPP_4_15_18, 1, replace=TRUE),
                    ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_UPP_4_18_21, 1, replace=TRUE),
                    sample(LOS_UPP_1_21_24))))))))) %>%
           release("uncomplicated post-partum", 1),
         trajectory("path 4 complicated post-partum") %>%
           seize("complicated post-partum", 1, continue=TRUE,
                 post.seize = trajectory() %>% release("recovery room")) %>%
           timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_CPP_1_0_3, 1, replace=TRUE),
                    ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_CPP_1_3_6, 1, replace=TRUE),
                    ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_CPP_1_6_9, 1, replace=TRUE),
                    ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_CPP_1_9_12, 1, replace=TRUE),
                    ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_CPP_1_12_15, 1, replace=TRUE),
                    ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_CPP_1_15_18, 1, replace=TRUE),
                    ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_CPP_1_18_21, 1, replace=TRUE),
                    sample(LOS_CPP_1_21_24))))))))) %>%
           release("complicated post-partum")
  )

# Path 5: Scheduled c-section (RR -> OR -> RR -> PP)
Path5 <- trajectory("scheduled c-section") %>%
  seize("recovery room", 1) %>%
  timeout(function() sample(LOS_RR_5, 1, replace=TRUE)) %>%
  release("recovery room", 1) %>%
  seize("operating room", 1) %>%
  timeout(function() sample(LOS_OR_5, 1, replace=TRUE)) %>%
  release("operating room", 1) %>%
  seize("recovery room", 1) %>%
  timeout(function() sample(LOS_RR_5, 1, replace=TRUE)) %>%
  branch(function() sample(c(1,2), size=1, replace=TRUE, prob=c(p5, (1-p5))), continue=c(T, T),
         trajectory("path 5 uncomplicated post-partum") %>%
           seize("uncomplicated post-partum", 1, continue=TRUE,
                 post.seize = trajectory() %>% release("recovery room")) %>%
           timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_UPP_1_0_3, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_UPP_1_3_6, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_UPP_1_6_9, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_UPP_1_9_12, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_UPP_5_12_15, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_UPP_5_15_18, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_UPP_5_18_21, 1, replace=TRUE),
                  sample(LOS_UPP_1_21_24))))))))) %>%
           release("uncomplicated post-partum", 1),
         trajectory("path 5 complicated post-partum") %>%
           seize("complicated post-partum", 1, continue=TRUE,
                 post.seize = trajectory() %>% release("recovery room")) %>%
           timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_CPP_1_0_3, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_CPP_1_3_6, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_CPP_1_6_9, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_CPP_5_9_12, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_CPP_5_12_15, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_CPP_5_15_18, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_CPP_1_18_21, 1, replace=TRUE),
                  sample(LOS_CPP_1_21_24))))))))) %>%
           release("complicated post-partum")
  )

# Path 6: Unscheduled c-section starting in RR (RR -> OR -> RR -> PP)
Path6 <- trajectory("unscheduled c-section") %>%
  seize("recovery room", 1) %>%
  timeout(function() sample(LOS_RR_6, 1, replace=TRUE)) %>%
  release("recovery room", 1) %>%
  seize("operating room", 1) %>%
  timeout(function() sample(LOS_OR_6, 1, replace=TRUE)) %>%
  release("operating room", 1) %>%
  seize("recovery room", 1) %>%
  timeout(function() sample(LOS_RR_6, 1, replace=TRUE)) %>%
  branch(function() sample(c(1,2), size=1, replace=TRUE, prob=c(p6, (1-p6))), continue=c(T, T),
         trajectory("path 6 uncomplicated post-partum") %>%
           seize("uncomplicated post-partum", 1, continue=TRUE,
                 post.seize = trajectory() %>% release("recovery room")) %>%
           timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_UPP_6_0_3, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_UPP_1_3_6, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_UPP_1_6_9, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_UPP_1_9_12, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_UPP_6_12_15, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_UPP_6_15_18, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_UPP_1_18_21, 1, replace=TRUE),
                sample(LOS_UPP_1_21_24))))))))) %>%
           release("uncomplicated post-partum", 1),
         trajectory("path 6 complicated post-partum") %>%
           seize("complicated post-partum", 1, continue=TRUE,
                 post.seize = trajectory() %>% release("recovery room")) %>%
           timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_CPP_1_0_3, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_CPP_1_3_6, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_CPP_1_6_9, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_CPP_1_9_12, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_CPP_1_12_15, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_CPP_1_15_18, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_CPP_1_18_21, 1, replace=TRUE),
                sample(LOS_CPP_1_21_24))))))))) %>%
           release("complicated post-partum")
  )

# Path 7: Unscheduled c-sections who bypass pre-op (OR -> RR -> PP)
Path7 <- trajectory("unscheduled c-section no pre-op") %>%
  seize("operating room", 1) %>%
  timeout(function() sample(LOS_OR_7, 1, replace=TRUE)) %>%
  release("operating room", 1) %>%
  seize("recovery room", 1) %>%
  timeout(function() sample(LOS_RR_7, 1, replace=TRUE)) %>%
  branch(function() sample(c(1,2), size=1, replace=TRUE, prob=c(p7, (1-p7))), continue=c(T, T),
         trajectory("path 7 uncomplicated post-partum") %>%
           seize("uncomplicated post-partum", 1, continue=TRUE,
                 post.seize = trajectory() %>% release("recovery room")) %>%
           timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_UPP_1_0_3, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_UPP_1_3_6, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_UPP_1_6_9, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_UPP_1_9_12, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_UPP_1_12_15, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_UPP_1_15_18, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_UPP_1_18_21, 1, replace=TRUE),
                  sample(LOS_UPP_1_21_24))))))))) %>%
           release("uncomplicated post-partum", 1),
         trajectory("path 7 complicated post-partum") %>%
           seize("complicated post-partum", 1, continue=TRUE,
                 post.seize = trajectory() %>% release("recovery room")) %>%
           timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_CPP_1_0_3, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_CPP_1_3_6, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_CPP_1_6_9, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_CPP_1_9_12, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_CPP_1_12_15, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_CPP_1_15_18, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_CPP_1_18_21, 1, replace=TRUE),
                  sample(LOS_CPP_1_21_24))))))))) %>%
           release("complicated post-partum")
  )

# Path 8: Scheduled c-sections who bypass pre-op (OR -> RR -> PP)
Path8 <- trajectory("scheduled c-section no pre-op") %>%
  seize("operating room", 1) %>%
  timeout(function() sample(LOS_OR_8, 1, replace=TRUE)) %>%
  release("operating room", 1) %>%
  seize("recovery room", 1) %>%
  timeout(function() sample(LOS_RR_8, 1, replace=TRUE)) %>%
  branch(function() sample(c(1,2), size=1, replace=TRUE, prob=c(p8, (1-p8))), continue=c(T, T),
         trajectory("path 8 uncomplicated post-partum") %>%
           seize("uncomplicated post-partum", 1, continue=TRUE,
                 post.seize = trajectory() %>% release("recovery room")) %>%
           timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_UPP_1_0_3, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_UPP_1_3_6, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_UPP_1_6_9, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_UPP_1_9_12, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_UPP_8_12_15, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_UPP_1_15_18, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_UPP_1_18_21, 1, replace=TRUE),
                  sample(LOS_UPP_1_21_24))))))))) %>%
           release("uncomplicated post-partum", 1),
         trajectory("path 8 complicated post-partum") %>%
           seize("complicated post-partum", 1, continue=TRUE,
                 post.seize = trajectory() %>% release("recovery room")) %>%
           timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_CPP_1_0_3, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_CPP_1_3_6, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_CPP_1_6_9, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_CPP_1_9_12, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_CPP_1_12_15, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_CPP_1_15_18, 1, replace=TRUE),
                  ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_CPP_1_18_21, 1, replace=TRUE),
                  sample(LOS_CPP_1_21_24))))))))) %>%
           release("complicated post-partum")
  )

# Path 9: Patients who only enter delivery room or recovery room
Path9 <- trajectory("DR or RR only") %>%
  branch(function() sample(c(1,2), size=1, replace=TRUE, prob=c(p9, (1-p9))), continue=c(T, T),
         trajectory("path 9 delivery room") %>%
           seize("delivery room", 1) %>%
           timeout(function() sample(LOS_Delivery_Room_9, 1, replace=TRUE)) %>%
           release("delivery room", 1),
         trajectory("path 9 recovery room") %>%
           seize("recovery room", 1) %>%
           timeout(function() sample(LOS_RR_9, 1, replace=TRUE)) %>%
           release("recovery room", 1)
         )

# Path 10: Patients who only enter post-partum
Path10 <- trajectory("antepartum") %>%
  branch(function() sample(c(1,2), size=1, replace=TRUE, prob=c(p10, (1-p10))), continue=c(T,T),
         trajectory("path 10 uncomplicated post-partum") %>%
           #log_(function() as.character(simmer::now(env))) %>%
           seize("uncomplicated post-partum", 1) %>%
           timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_UPP_10_0_3, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_UPP_10_3_6, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_UPP_10_6_9, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_UPP_10_9_12, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_UPP_10_12_15, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_UPP_10_15_18, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_UPP_10_18_21, 1, replace=TRUE),
                sample(LOS_UPP_10_21_24))))))))) %>%
           release("uncomplicated post-partum", 1),
         trajectory("path 10 complicated post-partum") %>%
           seize("complicated post-partum", 1) %>%
           timeout(function() ifelse(simmer::now(env) %% 1440 < 180, sample(LOS_CPP_10_0_3, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 180, 360), sample(LOS_CPP_10_3_6, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 360, 540), sample(LOS_CPP_10_6_9, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 540, 720), sample(LOS_CPP_10_9_12, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 720, 900), sample(LOS_CPP_10_12_15, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 900, 1080), sample(LOS_CPP_10_15_18, 1, replace=TRUE),
                ifelse(between(simmer::now(env) %% 1440, 1080, 1260), sample(LOS_CPP_10_18_21, 1, replace=TRUE),
                sample(LOS_CPP_10_21_24))))))))) %>%
           release("complicated post-partum", 1)
  )

# Create a trajectory for patients who go through the scheduled induction paths.
ScheduledInductions <- trajectory() %>%
  branch(function() sample(c(1,2), size=1, replace=TRUE, prob=c(p_induction, 1-p_induction)), continue=c(T, T),
         Path2,
         Path4
  )

# Create a trajectory for patients who go through the scheduled c-section paths.
ScheduledCSections <- trajectory() %>%
  branch(function() sample(c(1,2), size=1, replace=TRUE, prob=c(p_csection, 1-p_csection)), continue=c(T, T),
         Path5,
         Path8
  )

# Create a trajectory for patients who go through the unscheduled paths.
Unscheduled <- trajectory() %>%
  branch(function() sample(1:6, size=1, replace=TRUE, prob=c(p_path1, p_path3, p_path6, p_path7, p_path9, p_path10)),
         continue = c(T,T,T,T,T,T),
         Path1,
         Path3,
         Path6,
         Path7,
         Path9,
         Path10
         )

########################################################################################################

#sim_time <- env %>% now()

InductionProbs$Selection <- sapply(1:nrow(InductionProbs), SelectTimes, data=InductionProbs)
InductionTimes <- InductionProbs[InductionProbs$Selection==1,]$sim_mins   #path 2 and 4
CSectionProbs$Selection <- sapply(1:nrow(CSectionProbs), SelectTimes, data=CSectionProbs)
CSectionTimes <- CSectionProbs[CSectionProbs$Selection==1,]$sim_mins  #path 5 and 8

# Update environment to generate arrivals by time of day
env <-
  simmer("LaborAndDelivery") %>%
      add_resource("delivery room", 11) %>%
      add_resource("operating room", 4) %>%
      add_resource("recovery room", 4) %>%
      add_resource("uncomplicated post-partum", 19) %>%
      add_resource("complicated post-partum", 20) %>%
      add_generator("unscheduled Sun midnight to three", Unscheduled,
                    from_to(0, 180, function() sample(IAT_Sun_0_2, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Sun three to six am", Unscheduled,
                    from_to(181, 360, function() sample(IAT_Sun_3_5, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Sun six to nine am", Unscheduled,
                    from_to(361, 540, function() sample(IAT_Sun_6_8, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Sun nine to noon", Unscheduled,
                    from_to(541, 720, function() sample(IAT_Sun_9_11, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Sun noon to three", Unscheduled,
                    from_to(721, 900, function() sample(IAT_Sun_12_14, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Sun three to six pm", Unscheduled,
                    from_to(901, 1080, function() sample(IAT_Sun_15_17, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Sun six to nine pm", Unscheduled,
                    from_to(1081, 1260, function() sample(IAT_Sun_18_20, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Sun nine to midnight", Unscheduled,
                    from_to(1261, 1440, function() sample(IAT_Sun_21_23, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Mon midnight to three", Unscheduled,
                    from_to(1441, 1620, function() sample(IAT_Mon_0_2, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Mon three to six am", Unscheduled,
                    from_to(1621, 1800, function() sample(IAT_Mon_3_5, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Mon six to nine am", Unscheduled,
                    from_to(1801, 1980, function() sample(IAT_Mon_6_8, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Mon nine to noon", Unscheduled,
                    from_to(1981, 2160, function() sample(IAT_Mon_9_11, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Mon noon to three", Unscheduled,
                    from_to(2161, 2340, function() sample(IAT_Mon_12_14, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Mon three to six pm", Unscheduled,
                    from_to(2341, 2520, function() sample(IAT_Mon_15_17, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Mon six to nine pm", Unscheduled,
                    from_to(2521, 2700, function() sample(IAT_Mon_18_20, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Mon nine to midnight", Unscheduled,
                    from_to(2701, 2880, function() sample(IAT_Mon_21_23, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Tues midnight to three", Unscheduled,
                    from_to(2881, 3060, function() sample(IAT_Tues_0_2, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Tues three to six am", Unscheduled,
                    from_to(3061, 3240, function() sample(IAT_Tues_3_5, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Tues six to nine am", Unscheduled,
                    from_to(3241, 3420, function() sample(IAT_Tues_6_8, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Tues nine to noon", Unscheduled,
                    from_to(3421, 3600, function() sample(IAT_Tues_9_11, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Tues noon to three", Unscheduled,
                    from_to(3601, 3780, function() sample(IAT_Tues_12_14, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Tues three to six pm", Unscheduled,
                    from_to(3781, 3960, function() sample(IAT_Tues_15_17, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Tues six to nine pm", Unscheduled,
                    from_to(3961, 4140, function() sample(IAT_Tues_18_20, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Tues nine to midnight", Unscheduled,
                    from_to(4141, 4320, function() sample(IAT_Tues_21_23, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Wed midnight to three", Unscheduled,
                    from_to(4321, 4500, function() sample(IAT_Wed_0_2, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Wed three to six am", Unscheduled,
                    from_to(4501, 4680, function() sample(IAT_Wed_3_5, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Wed six to nine am", Unscheduled,
                    from_to(4681, 4860, function() sample(IAT_Wed_6_8, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Wed nine to noon", Unscheduled,
                    from_to(4861, 5040, function() sample(IAT_Wed_9_11, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Wed noon to three", Unscheduled,
                    from_to(5041, 5220, function() sample(IAT_Wed_12_14, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Wed three to six pm", Unscheduled,
                    from_to(5221, 5400, function() sample(IAT_Wed_15_17, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Wed six to nine pm", Unscheduled,
                    from_to(5401, 5580, function() sample(IAT_Wed_18_20, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Wed nine to midnight", Unscheduled,
                    from_to(5581, 5760, function() sample(IAT_Wed_21_23, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Thurs midnight to three", Unscheduled,
                    from_to(5761, 5940, function() sample(IAT_Thurs_0_2, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Thurs three to six am", Unscheduled,
                    from_to(5941, 6120, function() sample(IAT_Thurs_3_5, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Thurs six to nine am", Unscheduled,
                    from_to(6121, 6300, function() sample(IAT_Thurs_6_8, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Thurs nine to noon", Unscheduled,
                    from_to(6301, 6480, function() sample(IAT_Thurs_9_11, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Thurs noon to three", Unscheduled,
                    from_to(6481, 6660, function() sample(IAT_Thurs_12_14, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Thurs three to six pm", Unscheduled,
                    from_to(6661, 6840, function() sample(IAT_Thurs_15_17, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Thurs six to nine pm", Unscheduled,
                    from_to(6841, 7020, function() sample(IAT_Thurs_18_20, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Thurs nine to midnight", Unscheduled,
                    from_to(7021, 7200, function() sample(IAT_Thurs_21_23, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Fri midnight to three", Unscheduled,
                    from_to(7201, 7380, function() sample(IAT_Fri_0_2, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Fri three to six am", Unscheduled,
                    from_to(7381, 7560, function() sample(IAT_Fri_3_5, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Fri six to nine am", Unscheduled,
                    from_to(7561, 7740, function() sample(IAT_Fri_6_8, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Fri nine to noon", Unscheduled,
                    from_to(7741, 7920, function() sample(IAT_Fri_9_11, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Fri noon to three", Unscheduled,
                    from_to(7921, 8100, function() sample(IAT_Fri_12_14, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Fri three to six pm", Unscheduled,
                    from_to(8101, 8280, function() sample(IAT_Fri_15_17, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Fri six to nine pm", Unscheduled,
                    from_to(8281, 8460, function() sample(IAT_Fri_18_20, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Fri nine to midnight", Unscheduled,
                    from_to(8461, 8640, function() sample(IAT_Fri_21_23, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Sat midnight to three", Unscheduled,
                    from_to(8641, 8820, function() sample(IAT_Sat_0_2, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Sat three to six am", Unscheduled,
                    from_to(8821, 9000, function() sample(IAT_Sat_3_5, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Sat six to nine am", Unscheduled,
                    from_to(9001, 9180, function() sample(IAT_Sat_6_8, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Sat nine to noon", Unscheduled,
                    from_to(9181, 9360, function() sample(IAT_Sat_9_11, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Sat noon to three", Unscheduled,
                    from_to(9361, 9540, function() sample(IAT_Sat_12_14, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Sat three to six pm", Unscheduled,
                    from_to(9541, 9720, function() sample(IAT_Sat_15_17, 1, replace=TRUE), arrive=T, every=10080)) %>%
      add_generator("unscheduled Sat six to nine pm", Unscheduled,
                    from_to(9721, 9900, function() sample(IAT_Sat_18_20, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("unscheduled Sat nine to midnight", Unscheduled,
                    from_to(9900, 10080, function() sample(IAT_Sat_21_23, 1, replace=TRUE), arrive=F, every=10080)) %>%
      add_generator("scheduled inductions", ScheduledInductions, at(InductionTimes)) %>%
      add_generator("scheduled c-sections", ScheduledCSections, at(CSectionTimes))

# Run simulation for three days
env %>% run(until=1572480)
