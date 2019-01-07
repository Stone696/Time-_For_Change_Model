
# LOAD LIBRARIES ####

library(ggplot2)
library(tidyverse)
library(dplyr)

# CREATE AN OBJECT THAT IS A DATA FRAME WITH SOME COLUMNS ####

Time_for_Change <- data.frame(col1 = 'Years', col2 = 'IncumbentVoteShare', col3 = 'NetAPP', col4 = 'G2GDP', col5 = 'TermInc')
  

# MAKE A VECTOR OF NAMES FOR THE COLUMNS OF THE DATA FRAME ###

Columns <- c('Years', 'IncumbentVoteShare', 'NetAPP', 'G2GDP', 'TermInc')


# ADD THE NAMED VECTOR TO THE DATA FRAME AS THE COLUMN NAMES ####

names(Time_for_Change) <- c(Columns)

# #### CREATE EMPTY ROWS FOR THE DATAFRAME, SINCE THIS IS FOR EVERY ELECTION SINCE 1948 THERE WILL BE 18 OBSERVATIONS ####

Time_for_Change <- data.frame(Years = character(18), IncumbentVoteShare = numeric(18),  NetAPP = numeric(18), G2GDP = numeric(18), TermInc = numeric(18))

# ADD A LIST OF YEARS TO THE YEARS COLUMN OF THE DATAFRAME DIRECTLY ####

Time_for_Change$Years <- (c('1948', '1952', '1956', '1960', '1964', '1968', '1972', '1976', '1980', '1984', '1988', '1992', '1996', '2000', '2004', '2008', '2012', '2016'))

# CLEAN/REMOVE THE VECTOR/LIST FROM YOUR WORKSPACE TO KEEP IT TIDY ####

rm(Columns)

# ADD A LIST OF The DEPENDENT VARIABL (i.e. THE SHARE OF VOTES THE CANDIDATE FROM THE INCUMBENT PARTY RECEIVED) TO THE INCUMBENTVOTESHARE COLUMN OF THE DATAFRAME DIRECTLY ####

Time_for_Change$IncumbentVoteShare <- (c(49.6, 44.4,57.4,49.5,61.1,42.4,60.2,48.0,41.0,58.8,53.4,37.5,49.2,48.4
,50.7,45.7,51.1,48.2))

# ##### CREATE A LIST OF Q2 ELECTION YEAR GDP AND A LIST OF Q2 GDP FROM THE YEAR PRIOR #####
GDPq2ElecYr <- c(1856.90, 2218.60, 2545.90, 2834.40, 3376.60, 4132.00, 4633.10, 5128.90, 5787.40, 6559.60, 7592.90, 8244.30, 9407.10, 11258.50,12213.80, 13359.00, 16152.26,18640.73)

GDPq2YrPrior <- c(1769.50, 2147.60, 2490.30, 2778.80, 3180.40, 3919.60, 4398.80, 4831.90, 5831.40, 6077.60, 7269.50, 8003.80, 9044.70, 10684.00, 11738.70, 13194.10, 15496.19, 18221.30)


# CALCULATE ANNUAL GROWTH RATE FOR EACH YEAR AND YEAR -1 PAIR TO GET ANNUAL REAL GROWTH RATE OF GDP IN Q2 ####

AnnualQ2GrowthRate <- ((GDPq2ElecYr-GDPq2YrPrior)/GDPq2YrPrior)

Percent <- AnnualQ2GrowthRate*100

# ADD CALCULATED VALUE TO THE DATAFRAME IN THE CORRECT COLUMN  #####

Time_for_Change$G2GDP <- Percent




# CREATE A LIST OF THE NET APPRoval RATING OF INCUMBENT PARTY CURRENT PRESIDENT IN JUNE OF ELECTION YEAR #####

NetApprovalJune <- c(-7, -26, 55, 16, 60, -2, 24, 5, -27, 22, 8, -18, 10, 37, -1, -27, 0, 8)

# ### ADD IT TO THE DATA FRAME IN THE CORRECT COLUMN ####

Time_for_Change$NetAPP <- NetApprovalJune

# ##### CREATE A LIST OF VALUES THAT REPRESENT IF 1 INCUMBENT'S PARTY HAS BEEN IN OFFICE 2 of MORE TERMs, OR 0 IF THEY ARE A FIRST TERM INCUMBENT ####

Two_Term_Incumbent <- c(1, 1, 0,1,0,1,0,1,0,0,1,1,0,1,0,1,0,1)

# ADD LIST TO THE CORRECT DUMMY VARIABLE COLUMN IN THE DATAFRAME ####

Time_for_Change$TermInc <- Two_Term_Incumbent

# DATA SET IS COMPLETE NOW VISUALIZE/RUN MODELS #####

Model <- lm(IncumbentVoteShare ~ NetAPP + G2GDP + TermInc, data = Time_for_Change)


summary(Model)


plot(Model)
