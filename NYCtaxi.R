setwd('C:/Users/VisualBI/Desktop/R Stuff/Analyzing Big Data with Microsoft R')
options(max.print = 1000, scipen = 999, width = 90)
library(RevoScaleR)
rxOptions(reportProgress = 1) # reduces the amount of output RevoScaleR produces
library(dplyr)
options(dplyr.print_max = 2000)
options(dplyr.width = Inf) # shows all columns of a tbl_df object
library(stringr)
library(lubridate)
library(rgeos) # spatial package
library(sp) # spatial package
library(maptools) # spatial package
library(ggmap)
library(ggplot2)
library(gridExtra) # for putting plots side by side
library(ggrepel) # avoid text overlap in plots
library(tidyr)
library(seriation) # package for reordering a distance matrix

input <- 'mht_lab2.xdf'
mht_lab2 <-RxXdfData(input)
rxGetInfo(mht_lab2, getVarInfo = TRUE, numRows = 10) 
rxCrossTabs(~payment_type_desc:Ratecode_type_desc, mht_lab2)
rxSummary(~ payment_type_desc, mht_lab2, rowSelection = (trip_distance >5 & trip_duration <= 10*60))


rxHistogram(~ tip_percent, mht_lab2, numBreaks = 20)
rxHistogram(~ tip_percent, mht_lab2, numBreaks = 20, rowSelection = (payment_type_desc == 'card'))
rxHistogram(~ tip_percent, mht_lab2, numBreaks = 20, rowSelection = (payment_type_desc == 'cash'))
rxHistogram(~ tip_percent|Ratecode_type_desc, mht_lab2, rowSelection = (payment_type_desc == 'card'))

form_lab1 <- as.formula(tip_percent ~ trip_duration + pickup_dow:pickup_hour)
rxlm_lab1 <- rxLinMod(form_lab1, data = mht_lab2, dropFirst = TRUE, covCoef = TRUE)
form_lab2 <- as.formula(tip_percent ~ trip_duration + payment_type_desc + pickup_dow:pickup_hour)
rxlm_lab2 <- rxLinMod(form_lab2, data = mht_lab2, dropFirst = TRUE, covCoef = TRUE)

summary(rxlm_lab2)
summary(rxlm_lab1)

rxPredict(rxlm_lab1, data = mht_lab2, computeStdErrors = TRUE,
                       writeModelVars = TRUE, predVarNames = 'tip_percent_pred1', residVarNames = 'tip_percent_StdErr1', overwrite =  TRUE )
head(pred_df_1)
rxPredict(rxlm_lab2, data = mht_lab2, computeStdErrors = TRUE, 
                       writeModelVars = TRUE, predVarNames = 'tip_percent_pred2', residVarNames = 'tip_percent_StdErr1', overwrite = TRUE)

ggplot(data = rxmht_lab2) +
  geom_histogram(aes(x = tip_percent_pred1, col = "tippred1")) +
  geom_histogram(aes(x = tip_percent_pred2, col = "tippred2")) +
  xlim(-1, 30) + 
  xlab("tip percent")
