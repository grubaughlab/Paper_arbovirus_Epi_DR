# File: Proportion of Dengue Cases Reporting Rash over Time by Province

# This script calculates the proportion of reported dengue cases reporting rash over time for the Southwestern region versus the rest of the country to assess whether some cases might have been misdiagnosed Zika cases (for which rash is a common symptom). We do not do this for Chikungunya as we have data from only one outbreak, and therefore cannot compare over time.

# Data needed:
## Dengue case data over time with province and symptom details.
## Data mapping provinces to regions so can compare the Southwestern Region with the rest of the country.

# STEP 1: LOAD DATA ########################################################

# 1st - load libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)

# 2nd - load cleaned diagnosis data into list with all outbreaks of interest
dat <- read.csv("clean_dx_data_DENV.csv", stringsAsFactors = FALSE)

dat$Outbreak <- file_name
dat$Date.of.Onset.of.Symptoms <- as.Date(dat$Date.of.Onset.of.Symptoms, format = "%m/%d/%Y")
colnames(dat)[which(colnames(dat) == "Date.of.Onset.of.Symptoms")] <- "Date_Symptom_Onset"

keep_cols <- c("Outbreak", "Date_Symptom_Onset", "Rash", "Province")
dat <- dat[, keep_cols]

# 2nd - clean up province name (Spanish characters sometimes corrupted or have typos / alternate spellings)
dat_prov <- separate(dat, "Province",  c("Prov_Number", "Prov_Name"), 2)
dat_prov$Prov_Number <- as.numeric(dat_prov$Prov_Number)
dat_prov <- dat_prov[which(dat_prov$Prov_Number != 99), ] # extranjero

dat_prov_match <- read.csv("Province_name_code.csv", stringsAsFactors = FALSE)

dat_prov_matched <- left_join(dat_prov, dat_prov_match, by = c("Prov_Number" = "Code_Num"))
keep_cols <- c("Outbreak", "Date_Symptom_Onset", "Rash", "Prov_Number", "Province")
dat_prov_matched <- dat_prov_matched[, keep_cols]

# 3rd - match region to province in DENV data
dat_region <- read.csv("Region_province_mapping.csv", stringsAsFactors = FALSE)

## fixing typos in region Excel data
dat_region[which(dat_region$Provincia == "Salcedo"), "Provincia"] <- "Hermanas Mirabal" # province name change
dat_region[which(dat_region$Provincia == "María Trinidad  Sánchez"), "Provincia"] <- "María Trinidad Sánchez"
dat_region[which(dat_region$Provincia == "Valverde "), "Provincia"] <- "Valverde"
dat_region[which(dat_region$Provincia == "Azua "), "Provincia"] <- "Azua"

dat_prov_matched_reg <- left_join(dat_prov_matched, dat_region, by = c("Province" = "Provincia"))
keep_cols <- c("Outbreak", "Date_Symptom_Onset", "Rash", "Prov_Number", "Province", "Región.número", "Región")
dat_prov_matched_reg <- dat_prov_matched_reg[, keep_cols]
colnames(dat_prov_matched_reg)[colnames(dat_prov_matched_reg) == "Región.número"] <- "Reg_Number"
colnames(dat_prov_matched_reg)[colnames(dat_prov_matched_reg) == "Región"] <- "Region"

dat <- dat_prov_matched_reg

# STEP 2: Calculate Monthly Proportion ########################################################

dat$Count <- rep(1, nrow(dat))
dat$Week <- isoweek(dat$Date_Symptom_Onset)
dat$Month_Year <- format(as.Date(dat$Date_Symptom_Onset), "%Y-%m")
dat$Year <- format(as.Date(dat$Date_Symptom_Onset), "%Y")

# 1st - group data by week
dat_prop_rash_wk <- ddply(dat, .(Region, Week), summarize, Prop_Rash = sum(Rash) / sum(Count))
ggplot(dat_prop_rash_wk, aes(x = Week, y = Prop_Rash, group = Region, colour = Region)) + geom_line() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# 2nd - group data by month/year
dat_prop_rash_mth <- ddply(dat, .(Region, Month_Year), summarize, Prop_Rash = sum(Rash) / sum(Count))
ggplot(dat_prop_rash_mth, aes(x = Month_Year, y = Prop_Rash, group = Region, colour = Region)) + geom_line() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# 3rd - group data by year
dat_prop_rash_yr <- ddply(dat, .(Region, Year), summarize, Prop_Rash = sum(Rash) / sum(Count))
ggplot(dat_prop_rash_yr, aes(x = Year, y = Prop_Rash, group = Region, colour = Region)) + geom_line() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# 4th - over full country, different time horizons
dat_prop_rash_wk_cty <- ddply(dat, .(Week), summarize, Prop_Rash = sum(Rash) / sum(Count))
dat_prop_rash_mth_cty <- ddply(dat, .(Month_Year), summarize, Prop_Rash = sum(Rash) / sum(Count))
dat_prop_rash_yr_cty <- ddply(dat, .(Year), summarize, Prop_Rash = sum(Rash) / sum(Count))

ggplot(dat_prop_rash_wk_cty, aes(x = Week, y = Prop_Rash, group = 1)) + geom_line() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(dat_prop_rash_mth_cty, aes(x = Month_Year, y = Prop_Rash, group = 1)) + geom_line() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(dat_prop_rash_yr_cty, aes(x = Year, y = Prop_Rash, group = 1)) + geom_line() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# 5th - Enriquillo (SW province) vs. rest of country
dat_enriquillo <- dat[which(dat$Region == "Enriquillo"), ]
dat_other_region <- dat[which(dat$Region != "Enriquillo"), ]

## various time horizons
dat_enriquillo_wk <- ddply(dat_enriquillo, .(Region, Week), summarize, Prop_Rash = sum(Rash) / sum(Count))
dat_enriquillo_mth <- ddply(dat_enriquillo, .(Region, Month_Year), summarize, Prop_Rash = sum(Rash) / sum(Count))
dat_enriquillo_yr <- ddply(dat_enriquillo, .(Region, Year), summarize, Prop_Rash = sum(Rash) / sum(Count))

### across all regions
dat_other_region_wk <- ddply(dat_other_region, .(Week), summarize, Prop_Rash = sum(Rash) / sum(Count))
dat_other_region_mth <- ddply(dat_other_region, .(Month_Year), summarize, Prop_Rash = sum(Rash) / sum(Count))
dat_other_region_yr <- ddply(dat_other_region, .(Year), summarize, Prop_Rash = sum(Rash) / sum(Count))

dat_other_region_wk$Region <- "Other Regions Combined"
dat_other_region_mth$Region <- "Other Regions Combined"
dat_other_region_yr$Region <- "Other Regions Combined"

### combine enriquillo and other regions combined for plotting
dat_enriq_other_wk <- rbind(dat_enriquillo_wk, dat_other_region_wk, stringsAsFactors = FALSE)
dat_enriq_other_mth <- rbind(dat_enriquillo_mth, dat_other_region_mth, stringsAsFactors = FALSE)
dat_enriq_other_yr <- rbind(dat_enriquillo_yr, dat_other_region_yr, stringsAsFactors = FALSE)

ggplot(dat_enriq_other_wk, aes(x = Week, y = Prop_Rash, group = Region, colour = Region)) + geom_line() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(dat_enriq_other_mth, aes(x = Month_Year, y = Prop_Rash, group = Region, colour = Region)) + geom_line() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(dat_enriq_other_yr, aes(x = Year, y = Prop_Rash, group = Region, colour = Region)) + geom_line() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# STEP 3: Save Down Files of Interest ########################################################

## region-specific plots each time horizon
# file_path <- ## SET PATH NAME
# file_name <- "dat_prop_rash_wk"
# write.csv(dat_prop_rash_wk, paste0(file_path, file_name, ".csv"))
# file_name <- "dat_prop_rash_mth"
# write.csv(dat_prop_rash_mth, paste0(file_path, file_name, ".csv"))
# file_name <- "dat_prop_rash_yr"
# write.csv(dat_prop_rash_yr, paste0(file_path, file_name, ".csv"))
# 
# ## Enriquillo versus other regions combine
# file_name <- "dat_enriq_other_wk"
# write.csv(dat_enriq_other_wk, paste0(file_path, file_name, ".csv"))
# file_name <- "dat_enriq_other_mth"
# write.csv(dat_enriq_other_mth, paste0(file_path, file_name, ".csv"))
# file_name <- "dat_enriq_other_yr"
# write.csv(dat_enriq_other_yr, paste0(file_path, file_name, ".csv"))
