##### Replicating Meer and West (2016) #####

library(tidyverse)
library(rio)
library(fixest)
library(estimatr) 
# load estimatr to use lm_robust https://declaredesign.org/r/estimatr/articles/getting-started.html#lm_robust
library(usethis)

# Read in data
data <- import("bds.dta") # this is the dataset used for the main tables

options(scipen=999)

# Replicating table 1: standard fixed effects #
tbl1 <- list()
# Col 1 #
# tbl1_col1 <- felm(lemp ~ lnmin | statefips + year | 0 | statefips, data = data)
# felm note: response variable (Y) followed by a four part formula:
# 1. ordinary covariates, 
# 2. factors to be projected out (the fixed effects). 
# 3. an IV-specification. 
# 4. a cluster specification for the standard errors. 
# I.e. something like y ~ x1 + x2 | f1 + f2 | (Q|W ~ x3+x4) | clu1 + clu2 where
# y is the response, x1,x2 are ordinary covariates, f1,f2 are factors to be
# projected out, Q and W are covariates which are instrumented by x3 and x4, and
# clu1,clu2 are factors to be used for computing cluster robust standard errors.
# Parts that are not used should be specified as 0. The parentheses are needed
# in the third part since | has higher precedence than ~. Multiple left hand
# sides like y|w|x ~ x1 + x2 |f1+f2|... are allowed.

# feols syntax:
# https://www.rdocumentation.org/packages/fixest/versions/0.8.4/topics/feols

tbl1_col1 <- feols(lemp ~ lnmin | statefips + year, cluster = "statefips",
                         data = data)
# note that feols produces the same results as the paper

# Col 2 #
tbl1_col2 <- feols(lemp ~ lnmin+lpopall+share1559+lrgspcap | statefips + year,
                   cluster = "statefips", data = data)

# Col 3 #
tbl1_col3 <- feols(lemp ~ lnmin+lpopall+share1559+lrgspcap | statefips + year^censusreg, 
                   cluster = "statefips", data = data)

# Col 4 #
# prepare data
data_lead_lag <- data %>% 
  group_by(statefips) %>% 
  mutate(lnmin_firstlead = lead(lnmin, n = 1L), # create leads of the min. wage
         lnmin_secondlead = lead(lnmin, n = 2L)) %>% 
  ungroup()

tbl1_col4 <- feols(lemp ~ lnmin + lnmin_firstlead+lpopall+share1559+lrgspcap| statefips + year^censusreg, 
                   cluster = "statefips", data = data_lead_lag)

# Col 5 #  
tbl1_col5 <- feols(lemp ~ lnmin + lnmin_firstlead+lnmin_secondlead+lpopall+share1559+lrgspcap| statefips + year^censusreg, 
                   cluster = "statefips", data = data_lead_lag)

# Col 6: include factor(statefips):year for jurisdiction time trend #  
tbl1_col6 <- feols(lemp ~ lnmin +lpopall+share1559+lrgspcap +factor(statefips):year| statefips + year^censusreg, cluster = "statefips", data = data_lead_lag)

tbl1[[1]] <- tbl1_col1
tbl1[[2]] <- tbl1_col2
tbl1[[3]] <- tbl1_col3
tbl1[[4]] <- tbl1_col4
tbl1[[5]] <- tbl1_col5
tbl1[[6]] <- tbl1_col6

rm(tbl1_col1, tbl1_col2, tbl1_col3, tbl1_col4, tbl1_col5, tbl1_col6, data_lead_lag)

# Show individual regression results
# summary(tbl1[[1]])
# summary(tbl1[[2]])
# summary(tbl1[[3]])
# summary(tbl1[[4]])
# summary(tbl1[[5]])
# summary(tbl1[[6]])
################################################################################

# Replicating table 2: long-differences specification #

tbl2_no_trend <- list()
tbl2_trend <- list()

for (index in 1:8) {
  # prepare data  
  data_temp <- data %>% 
    group_by(statefips) %>%
    mutate(lemp = lemp - lag(lemp, n = index),
           lnmin = lnmin - lag(lnmin, n = index),
           lpopall = lpopall - lag(lpopall, n = index),
           share1559 = share1559 - lag(share1559, n = index),
           lrgspcap = lrgspcap - lag(lrgspcap, n = index)
          )
  # run regression using lm_robust from {estimatr}
  reg_temp_no_trend <- lm_robust(lemp ~ lnmin +lpopall+share1559+lrgspcap + factor(year):factor(censusreg), data = data_temp, clusters = statefips, se_type = "stata")
  tbl2_no_trend[[index]] <- reg_temp_no_trend
  
  # include factor(statefips):year for jurisdiction time trend #  
  reg_temp_trend <- lm_robust(lemp ~ lnmin +lpopall+share1559+lrgspcap + factor(year):factor(censusreg) + factor(statefips):year, data = data_temp, clusters = statefips, se_type = "stata")
  tbl2_trend[[index]] <- reg_temp_trend
  
  # remove temporary objects  
  rm(data_temp, reg_temp_no_trend, reg_temp_trend)
}


# summary(tbl2_no_trend[[1]])
# summary(tbl2_no_trend[[2]])
# summary(tbl2_no_trend[[3]])
# summary(tbl2_no_trend[[4]])
# summary(tbl2_no_trend[[5]])
# summary(tbl2_no_trend[[6]])
# summary(tbl2_no_trend[[7]])
# summary(tbl2_no_trend[[8]])
# 
# summary(tbl2_trend[[1]])
# summary(tbl2_trend[[2]])
# summary(tbl2_trend[[3]])
# summary(tbl2_trend[[4]])
# summary(tbl2_trend[[5]])
# summary(tbl2_trend[[6]])
# summary(tbl2_trend[[7]])
# summary(tbl2_trend[[8]])


################################################################################

# Replicating table 3: distributed lag model #

# prepare data
wmin2012 <- import("wmin2012.xlsx") # add in 2012 state minimum wages

tbl3_data <- bind_rows(wmin2012, data) %>% # merge in 2012 state minimum wages
  arrange(statefips, year) %>% # arrange data by state and by year 
  group_by(statefips) %>% 
  mutate(diff_lemp = lemp - lag(lemp, n = 1L),
         diff_lpopall = lpopall - lag(lpopall, n = 1L),
         diff_share1559 = share1559 - lag(share1559, n = 1L),
         diff_lrgspcap = lrgspcap - lag(lrgspcap, n = 1L)) %>% 
  mutate(lnmin_dlag0 = lnmin - lag(lnmin, n = 1L),
         lnmin_dlag1 = lag(lnmin, n = 1L) - lag(lnmin, n = 2L),
         lnmin_dlag2 = lag(lnmin, n = 2L) - lag(lnmin, n = 3L),
         lnmin_dlag3 = lag(lnmin, n = 3L) - lag(lnmin, n = 4L)) %>% 
  mutate(lnmin_dlead0 = lead(lnmin, n = 1L) - lnmin,
         lnmin_dlead1 = lead(lnmin, n = 2L) - lead(lnmin, n = 1L)) %>% 
  ungroup()

tbl3 <- list()

# Col 1
tbl3_col1 <- lm_robust(diff_lemp ~ lnmin_dlag0 + lnmin_dlag1 + lnmin_dlag2 + lnmin_dlag3 + factor(year):factor(censusreg) + 
                         diff_lpopall + diff_share1559 + diff_lrgspcap, data = tbl3_data, cluster = statefips, se_type = "stata")

# Col 2
tbl3_col2 <- lm_robust(diff_lemp ~ lnmin_dlag0 + lnmin_dlag1 + lnmin_dlag2 + lnmin_dlag3 + factor(year):factor(censusreg) + lnmin_dlead0 +
                         diff_lpopall + diff_share1559 + diff_lrgspcap, data = tbl3_data, cluster = statefips, se_type = "stata")

# Col 3
tbl3_col3 <- lm_robust(diff_lemp ~ lnmin_dlag0 + lnmin_dlag1 + lnmin_dlag2 + lnmin_dlag3 + factor(year):factor(censusreg) + lnmin_dlead0 + lnmin_dlead1 +
                         diff_lpopall + diff_share1559 + diff_lrgspcap, data = tbl3_data, cluster = statefips, se_type = "stata")

# Col 4
tbl3_col4 <- lm_robust(diff_lemp ~ lnmin_dlag0 + lnmin_dlag1 + lnmin_dlag2 + lnmin_dlag3 + factor(year):factor(censusdiv) + 
                         diff_lpopall + diff_share1559 + diff_lrgspcap, data = tbl3_data, cluster = statefips, se_type = "stata")

tbl3[[1]] <- tbl3_col1
tbl3[[2]] <- tbl3_col2
tbl3[[3]] <- tbl3_col3
tbl3[[4]] <- tbl3_col4

summary(tbl3[[1]])
summary(tbl3[[2]])
summary(tbl3[[3]]) 
summary(tbl3[[4]]) 

rm(tbl3_col1, tbl3_col2, tbl3_col3, tbl3_col4, wmin2012)