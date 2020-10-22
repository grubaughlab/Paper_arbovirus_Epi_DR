# File: Age- and Sex-Adjusted Attack Rates by Province and Outbreak using the Direct Standardization Method

# This script compares the observed attack rate for each province to what we would expect if each province had the same age and sex structure of the reference population, and will use this to calculate the age- and sex-adjusted attack rates. 

# Study population = province-level data
# Reference (or standard) population = national-level data

# Data needed:
## Need 1: cases by age group and sex for each province and outbreak (our study population)
## Need 2: number of people by age group and sex for each province (our study population)
## Need 3: number of people by age group and sex for the country (our reference population)

# STEP 1: LOAD DATA ########################################################

# 1st - load libraries
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)

# 2nd - load cleaned diagnosis data into list with all outbreaks of interest
# file_root <- # SET FILE PATH
file_root <- paste0(file_root, "clean_dx_data_")
file_names <- c("DENV", "CHKV", "ZIKV")
all_outbreaks <- list()

for(file_name in file_names) {
  file_path <- paste0(file_root, file_name, ".csv")
  print(file_path)
  dat <- read.csv(file_path, stringsAsFactors = FALSE)
  dat$outbreak <- file_name
  dat$Date.of.Onset.of.Symptoms <- as.Date(dat$Date.of.Onset.of.Symptoms, format = "%m/%d/%Y")
  names(dat)[names(dat) == "Age.Group"] <- "Age_Group"
  dat$Age_Group <- as.character(dat$Age_Group)
  
  dat$outbreak <- file_name
  
  if (file_name == "DENV") {
    dat$outbreak <- "DENV_not_ABC"
    dat[(dat$Date.of.Onset.of.Symptoms >= "2012-04-01") & (dat$Date.of.Onset.of.Symptoms <= "2013-03-31"), "outbreak"] <- "DENV_A"
    dat[(dat$Date.of.Onset.of.Symptoms >= "2013-04-01") & (dat$Date.of.Onset.of.Symptoms <= "2014-03-31"), "outbreak"] <- "DENV_B"
    dat[(dat$Date.of.Onset.of.Symptoms >= "2015-04-01") & (dat$Date.of.Onset.of.Symptoms <= "2016-03-31"), "outbreak"] <- "DENV_C"
  }
  
  all_outbreaks[[file_name]] <- dat
}

# 3rd - load population data

# Note: data from DR govt site has province population data in one file, and proportion of province population by age and sex group in another, so we have to load two files and combine them later to get the number of people by province in each age and sex category
# Note: prop_data = proportion of province population in each age and sex category
# Note: count_data = number of people in each province

# file_root <- SET
prop_data <- read.csv(paste0(file_root, "age_sex_structure_prov.csv"), stringsAsFactors = FALSE)
# file_root <- SET
count_data <- read.csv(paste0(file_rooth, "age_sex_counts_prov.csv"), stringsAsFactors = FALSE)

# STEP 2: ADDRESS DATA NEEDS ########################################################

# Data Need 1: Study Population: Count Cases for Each Age and Sex Group by Province and Outbreak **************************

# 1st- fix age groups in diagnosis date to match population data so can compare later

# Note: age groups are the same, except pop_data uses 70_74, 75_79, and >80, whereas diagnostic data uses > 70

outbreaks <- names(all_outbreaks)
for (outbreak in outbreaks) {
  dat <- all_outbreaks[[outbreak]]
  dat[which((dat$Age >= 70) & (dat$Age <= 74)), "Age_Group"] <- "70_74"
  dat[which((dat$Age >= 75) & (dat$Age <= 79)), "Age_Group"] <- "75_79"
  dat[which(dat$Age >= 80), "Age_Group"] <- ">80"
  all_outbreaks[[outbreak]] <- dat
}

# 2nd - move province number to separate column in diagnosis data so can work off of that (province names often get messed up due to accents, so this makes for easier matching later)
outbreaks <- names(all_outbreaks)
for (outbreak in outbreaks) {
  dat <- all_outbreaks[[outbreak]]
  dat <- separate(dat, "Province",  c("Prov_Number", "Prov_Name"), 2)
  dat$Prov_Number <- as.numeric(dat$Prov_Number)
  dat <- dat[which(dat$Prov_Number != 99), ]
  all_outbreaks[[outbreak]] <- dat
}

# 3rd - count diagnosed cases for each age and sex group by province
outbreaks <- names(all_outbreaks)
cases_subgroups <- list()

for (outbreak in outbreaks) {
  dat <- all_outbreaks[[outbreak]]
  
  if (outbreak == "DENV") {

    denv_outbreaks <- c("DENV_A", "DENV_B", "DENV_C")
    for (denv_outbreak in denv_outbreaks) {
      dat_subset <- dat[which(dat$outbreak == denv_outbreak), ]
      cases <- ddply(dat_subset, .(Prov_Number, Age_Group, Sex), nrow)
      names(cases)[names(cases) == "V1"] <- "Cases"
      cases_subgroups[[denv_outbreak]] <- cases
    }
  } else {
    cases <- ddply(dat, .(Prov_Number, Age_Group, Sex), nrow)
    names(cases)[names(cases) == "V1"] <- "Cases"
    cases_subgroups[[outbreak]] <- cases
  }
}

# Data Need 2: Reference Population: Calculate the Number of People in Each Age and Sex Group  **************************

# 1st - transform data so that total pop & provinces are no longer the columns
prop_all <- melt(prop_data, id.vars = c("Age_Group", "Sex"), variable.name = "Prov_Number", value.name = "Proportion_by_Sex")
prop_all$Proportion_by_Sex <- prop_all$Proportion_by_Sex / 100 

# 2nd - break out the REFERENCE population separately (represented by "Total" under province number)

prop_ref <- prop_all[(prop_all$Prov_Number == "Total"), ]

# 3rd - drop "Both" sex category since will be using sex-specific rates, then remove "Prov_Number" column b/c do not need it anymore
prop_ref <- prop_ref[!(prop_ref$Sex == "Both"), ]
drop_col <- "Prov_Number"
prop_ref <- prop_ref[, !(names(prop_ref) %in% drop_col)]

# 4th - calculate total males and females in reference group
ref_pop_male <- sum(count_data$Male)
ref_pop_female <- sum(count_data$Female)

# 5th - assign df to a new name to reflect data that will be added
prop_count_ref <- prop_ref

# 6th - calculate the number of people nationally in each subgroup
prop_count_ref[which(prop_count_ref$Sex == "Male"), "Ref_Population"] <- (prop_count_ref[which(prop_count_ref$Sex == "Male"), "Proportion_by_Sex"]) * ref_pop_male
prop_count_ref[which(prop_count_ref$Sex == "Female"), "Ref_Population"] <- (prop_count_ref[which(prop_count_ref$Sex == "Female"), "Proportion_by_Sex"]) * ref_pop_female

# 7th - calculate the proportion of TOTAL people in each subgroup (initially provided data was proportion of males and females in each subgroup), and replace the old proportion value
prop_count_ref$Weight <- prop_count_ref$Ref_Population / sum(prop_count_ref$Ref_Population)

# Data Need 3: Study Population: Calculate the Number of People in Each Age and Sex Group by Province  **************************

# 1st - drop "Both" sex category since will be using sex-specific rates, and remove "Total" from "Prov_Number" since that's for the reference population
prop_prov <- prop_all[!((prop_all$Sex == "Both") | (prop_all$Prov_Number == "Total")), ]

# 2nd - change columns from factors to characters so join function will work later
prop_prov$Prov_Number <- as.character(prop_prov$Prov_Number)
prop_prov$Sex <- as.character(prop_prov$Sex)

# 3rd - remove "X" from start of Prov_Number that was result of how Excel imports numeric columns
convert_prov_string_to_integer <- function(x){
  new_name <- substring(x, 2, nchar(x))
  as.numeric(new_name)
}

prop_prov$Prov_Number <- sapply(prop_prov$Prov_Number, convert_prov_string_to_integer)

# 4th - drop columns don't need
drop_col <- c("Prov_Name", "Both")
count_data <- count_data[, !(names(count_data) %in% drop_col)]
count_prov <- melt(count_data, id.vars = c("Prov_Number"), variable.name = "Sex", value.name = "Prov_Population")

# 5th - change class from factor to character so doesn't mess up join later
count_prov$Sex <- as.character(count_prov$Sex)

# 6th - join the proportion data with the population count data
prop_count_prov <- left_join(prop_prov, count_prov, by = c("Prov_Number", "Sex"))

# 7th - calculate the number of people in each age and sex group by province
prop_count_prov$Group_Population <- (prop_count_prov$Proportion_by_Sex) * prop_count_prov$Prov_Population 

# STEP 3: CALCULATE ADJUSTED RATES BY PROVINCE AND OUTBREAK ##########################

# 1st - calculate cases / study population for each age and sex group, adding rows where there are 0 cases so that population denominator stays the same

outbreaks <- names(cases_subgroups)

for (outbreak in outbreaks){
    dat <- cases_subgroups[[outbreak]]
    dat <- left_join(prop_count_prov[, c("Prov_Number", "Age_Group", "Sex", "Group_Population")], dat, by = c("Prov_Number", "Age_Group", "Sex"))
    dat[which(is.na(dat$Cases) == TRUE), "Cases"] <- 0
    dat$Crude_AR <- dat$Cases / dat$Group_Population
    cases_subgroups[[outbreak]] <- dat
}

# 2nd - multiply crude rate for each age and sex group by reference population weight (# people in subgroup / total reference pop) for the matching age and sex group
outbreaks <- names(cases_subgroups)

for (outbreak in outbreaks){
  dat <- cases_subgroups[[outbreak]]
  dat <- left_join(dat, prop_count_ref[, c("Age_Group", "Sex", "Weight")], by = c("Age_Group", "Sex"))
  dat$Expected_Events <- dat$Crude_AR * dat$Weight
  cases_subgroups[[outbreak]] <- dat
}

# 3rd - sum over the expected events to get a total for each province and each outbreaks
outbreaks <- names(cases_subgroups)
rate_list <- list()

for (outbreak in outbreaks){
  dat <- cases_subgroups[[outbreak]]
  dat <- ddply(dat, .(Prov_Number), summarize, Cases = sum(Cases), Population = sum(Group_Population), Crude_AR_per_10K = (sum(Cases) / sum(Group_Population)) * 10000, Adj_AR_per_10K = (sum(Expected_Events)) * 10000)
  dat$Crude_Adj_Diff <- dat$Crude_AR_per_10K - dat$Adj_AR_per_10K
  rate_list[[outbreak]] <- dat
}

# 4th - save down files as one large dataframe
 
# add outbreak name to each item in list
outbreaks <- names(cases_subgroups)

for (outbreak in outbreaks) {
  dat <- rate_list[[outbreak]]
  dat$Outbreak <- outbreak
  rate_list[[outbreak]] <- dat
}

rate_list_combined <- do.call(rbind, rate_list)
# file_root <- SET
file_name <- paste0(file_root, "all_outbreaks_crude_vs_adj_AR.csv")
write.csv(rate_list_combined, file_name)

# 5th - save down files with each outbreak separately (if prefer)
# outbreaks <- names(rate_list)
# 
# for (outbreak in outbreaks) {
#   dat <- rate_list[[outbreak]]
##   file_root <- SET
#   file_name <- paste0(file_root, outbreak, "_crude_vs_adj_AR", ".csv")
#   write.csv(dat, file_name)
# }

