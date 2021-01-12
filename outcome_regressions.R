## migration outcome anaylsis

rm(list = ls())

library("openxlsx")
library("caret")

setwd("C:/R/migration")

## data cleaning

# load data
input <- read.xlsx("ITT_cumulative.xlsx")

summary(input)


# random data for now

set.seed(5)
input$HS_bt <- runif(length(input$HS_bt))
input$HS_at <- runif(length(input$HS_at))
input$tox_bt <- runif(length(input$tox_bt))
input$tox_at <- runif(length(input$tox_at))

# substitute NA with 0 
input$deleted[is.na(input$deleted)] <- 0
input$out_sample[is.na(input$out_sample)] <- 0
input$uncertain[is.na(input$uncertain)] <- 0
input$treatment[is.na(input$treatment)] <- 0

# group treatment into 3 main categories

input$treatment_main <- substr(input$treatment, 1, nchar(input$treatment)-1)

# treatment dummy

input$treatment_dummy <- 0
input$treatment_dummy[input$treatment != 0] <- 1

# treatment vars as factors

input$treatment <- as.factor(input$treatment)
input$treatment_main <- as.factor(input$treatment_main)
input$treatment_dummy <- as.factor(input$treatment_dummy)

## regressions

# hypothesis 1 

H1 <- lm(HS_at ~ treatment_dummy + HS_bt, input)
summary(H1)

# hypothesis 3

H3 <- lm(tox_at ~ treatment_dummy + tox_bt, input)

# hypothesis 4

H4 <- lm(HS_at ~ treatment_main + HS_bt, input)

# hypothesis 6

H6 <- lm(tox_bt ~ treatment_main + HS_at, input)

## robustness check

input_robust <- subset(input, ITT == 0)

# hypothesis 1 

H1_robust <- lm(HS_at ~ treatment_dummy + HS_bt, input_robust)

summary(H1)$r.squared/summary(H1_robust)$r.squared # calculates R2 ratio of two models -> good estimate for roubstness? 

# hypothesis 4

H4_robust <- lm(tox_at ~ treatment_dummy + tox_bt, input_robust)

summary(H4)$r.squared/summary(H4_robust)$r.squared

# check if treatment effects different in sub categories of treatment

H1_subgroup_treatment <- lm(HS_at ~ treatment + HS_bt, input)

# excluding HS_bt control var

H1_wo_HS_bt <- lm(HS_at ~ treatment_dummy, input)
