
# Set Desired Working Directory
#setwd('Documents/PGCBA/Placement/Sagitec/SSN_SACE_2017_Jan-master/CSV')

require(data.table)

# reading data
file_names <- c('prescriber.detailed.csv', 'prescriber.summary.csv',
                'PUF_pres_conso.csv', 'PUF.detailed.csv', 'PUF.summary.csv')
list_1 <- lapply(file_names, fread) # easiser to search for attribute
str(list_1)

# variables in each data table
dt_names <- lapply(list_1, names)

# get all unique variable names
all_names <- vector()
for (i in 1:length(list_1)) {
  a <- names(list_1[[i]])
  all_names <- unique(c(a, all_names))
}
length(all_names)

# split into tables
dt_1 <- list_1[[1]] # 'prescriber.detailed.csv'
dt_2 <- list_1[[2]] # 'prescriber.summary.csv'
dt_3 <- list_1[[3]] # 'PUF_pres_conso.csv'
dt_4 <- list_1[[4]] # 'PUF.detailed.csv'
dt_5 <- list_1[[5]] # 'PUF.summary.csv

dt_names[[1]]

# is doc_id similar to npi ? npi must have 10, while doc_id has 9.
require(stringr)
which(dt_names %like% '.*doc_id*')
str_count(dt_1$doc_id[1])
doc_id_string_counter <- function(x) {
  doc_id_length <- lapply(x$doc_id, str_count)
  less_than <- 0
  greater_than <- 0
  actual <- 0
  for (i in 1:length(x$doc_id)) {
    if (doc_id_length[[i]] < 9) {
      less_than <- less_than + 1
    } else if (doc_id_length[[i]] > 9) {
      greater_than <- greater_than + 1
    } else if (doc_id_length[[i]] == 9) {
      actual <- actual + 1
    }
  }
  if (actual == length(x$doc_id)) {
    paste('Charater length is equal 9 for all doc_id')
  } else {
    c(less_than, greater_than)
  }
}

doc_id_string_counter(dt_1)
doc_id_string_counter(dt_2)
doc_id_string_counter(dt_3)
doc_id_string_counter(dt_4)
doc_id_string_counter(dt_5)

dt_names[[1]]

# state & city in dt_3 same as provider_* in others ? # need to look into it !
all_names[all_names %like% '.*provider*']
which(dt_names %like% '.*provider*')
head(names(dt_3))

# how to handle state = 'ZZ' ? # foreign vs domestic ?
sum(dt_1$nppes_provider_state == 'ZZ')
state_zz <- dt_1[dt_1$nppes_provider_state == 'ZZ']

unique(state_zz$doc_id)

require(dplyr)

select(dt_1, doc_id, nppes_provider_state) %>% filter(doc_id == 'SR7876440') # No other state
select(dt_1, doc_id, nppes_provider_state) %>% filter(doc_id == 'ZH4066278') # No other state
select(dt_1, doc_id, nppes_provider_state) %>% filter(doc_id == 'PI6937446') # No other state
select(dt_1, doc_id, nppes_provider_state) %>% filter(doc_id == 'FP6297812') # No other state

select(dt_1, doc_id, nppes_provider_city) %>% filter(doc_id == 'SR7876440') # No other city
select(dt_1, doc_id, nppes_provider_city) %>% filter(doc_id == 'ZH4066278') # No other city
select(dt_1, doc_id, nppes_provider_city) %>% filter(doc_id == 'PI6937446') # No other city
select(dt_1, doc_id, nppes_provider_city) %>% filter(doc_id == 'FP6297812') # No other city

select(dt_1, doc_id, nppes_provider_city, nppes_provider_state) %>%
  filter(nppes_provider_city == "JAIPUR") # No other state
select(dt_1, doc_id, nppes_provider_city, nppes_provider_state) %>%
  filter(nppes_provider_city == "TORONTO") # No other state
select(dt_1, doc_id, nppes_provider_city, nppes_provider_state) %>%
  filter(nppes_provider_city == "ST. JOHN'S") # No other state
select(dt_1, doc_id, nppes_provider_city, nppes_provider_state) %>%
  filter(nppes_provider_city == "DARTMOUTH") # No other state

# New Feature
dt_1$cost_per_day_per_claim <- round(dt_1$total_drug_cost/dt_1$total_day_supply/dt_1$total_claim_count, digits = 3)

# Plotting by drug_name for bene_NA
drug_generic_bene_NA <- select(dt_1, doc_id:cost_per_day_per_claim) %>% filter(is.na(bene_count))
drug_generic_bene <- select(dt_1, doc_id:cost_per_day_per_claim) %>% filter(!is.na(bene_count))

split_drug_generic_bene_NA <- split(drug_generic_bene_NA, as.factor(drug_generic_bene_NA$drug_name))
length(split_drug_generic_bene_NA)

boxplot_drug_bene_NA <- function(drug_name) {
  i <- which(drug_name %in% names(split_drug_generic_bene_NA))
  boxplot(split_drug_generic_bene_NA[[i]][[10]], xlab = 'bene_NA', ylab = 'cost/day/claim')
}


split_drug_generic_bene <- split(drug_generic_bene, as.factor(drug_generic_bene$drug_name))
length(split_drug_generic_bene)

boxplot_drug_bene <- function(drug_name) {
  i <- which(drug_name %in% names(split_drug_generic_bene))
  boxplot(split_drug_generic_bene[[i]][[10]], xlab = 'bene', ylab = 'cost/day/claim')
}

# The range and outliers for those in bene_NA seem to be greater than the other
# Can we claim that these records could be further scrutinized ? Drill Down to Location ?
par(mfrow=c(1,2))
boxplot_drug_bene_NA(drug_name = 'ABILIFY')
boxplot_drug_bene(drug_name = 'ABILIFY')
