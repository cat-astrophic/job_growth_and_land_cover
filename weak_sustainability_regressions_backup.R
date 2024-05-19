# This script gets census data and runs regressions

# Loading libraries

library(AER)
library(dplyr)
library(lmtest)
library(tigris)
library(ggplot2)
library(leaflet)
library(sandwich)
library(stargazer)
library(tidycensus)
library(modelsummary)

# Project directory

direc <- 'D:/weak_sustainability/'

# Reading in the county level land cover proportions panel data set

pd <- read.csv(paste0(direc, 'data/county_level_proportions_2001_2011_2021.csv'))

# Updating the FIPS codes in pd to make them all five digies

pd$County <- as.character(pd$County)
pd$County <- ifelse(nchar(pd$County) < 5, paste0('0', pd$County), pd$County)

# Get the counties land area from tigris

us_counties <- counties(cb = TRUE)

# Getting county level ACS data

states <- c('01', '04', '05', '06', '08', '09', '10', '11', '12', '13', '16', '17', '18', '19', '20', '21', '22',
            '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36', '37', '38',
            '39', '40', '41', '42', '44', '45', '46', '47', '48', '49', '50', '51', '53', '54', '55', '56')

c21 <- as.data.frame(NULL)
c11 <- as.data.frame(NULL)

for (s in states) {
  
  print(paste0('Collecting ACS data for state FIPS ', s, '.......'))
  
  tmp21 <- get_acs(state = s, geography = 'county', year = 2021, variables = c('DP05_0001', 'DP03_0062', 'DP02_0068P', 'DP02_0067P', 'DP02_0059', 'DP04_0001', 'DP03_0009P', 'DP02_0053', 'DP02_0053P', 'DP02_0083P', 'DP03_0019', 'DP03_0021', 'DP03_0033', 'DP03_0034', 'DP03_0035', 'DP03_0036', 'DP03_0037', 'DP03_0038', 'DP03_0039', 'DP03_0040', 'DP03_0041', 'DP03_0042', 'DP03_0043', 'DP03_0044', 'DP03_0045'))
  tmp11 <- get_acs(state = s, geography = 'county', year = 2011, variables = c('DP05_0001', 'DP03_0062', 'DP02_0068P', 'DP02_0067P', 'DP02_0059', 'DP04_0001', 'DP03_0009P', 'DP02_0053', 'DP02_0053P', 'DP02_0083P', 'DP03_0019', 'DP03_0021', 'DP03_0033', 'DP03_0034', 'DP03_0035', 'DP03_0036', 'DP03_0037', 'DP03_0038', 'DP03_0039', 'DP03_0040', 'DP03_0041', 'DP03_0042', 'DP03_0043', 'DP03_0044', 'DP03_0045'))
  
  c21 <- rbind(c21, tmp21)
  c11 <- rbind(c11, tmp11)
  
}

# Merging the ACS data and the NLCD data

data <- pd %>% filter(Year >= 2010)

pop <- c()
inc <- c()
ed <- c()
ed2 <- c()
emp <- c()
school <- c()
hunits <- c()
movers <- c()
comms <- c()
pt <- c()
outdo <- c()
cons <- c()
manu <- c()
whole <- c()
retail <- c()
trans <- c()
fin <- c()
info <- c()
pro <- c()
sw <- c()
amen <- c()
other <- c()
pad <- c()

for (i in 1:(nrow(data)/2)) {
  
  print(paste0('Creating 2021 ACS data for county ', i, ' of 3,108.......'))
  
  tmp21 <- c21 %>% filter(GEOID == data$County[i])
  
  pop <- c(pop, log(tmp21[which(tmp21$variable == 'DP05_0001'),]$estimate[1]))
  inc <- c(inc, log(tmp21[which(tmp21$variable == 'DP03_0062'),]$estimate[1]))
  ed <- c(ed, tmp21[which(tmp21$variable == 'DP02_0068P'),]$estimate[1])
  ed2 <- c(ed2, tmp21[which(tmp21$variable == 'DP02_0067P'),]$estimate[1])
  emp <- c(emp, tmp21[which(tmp21$variable == 'DP03_0009P'),]$estimate[1])
  movers <- c(movers, tmp21[which(tmp21$variable == 'DP02_0083P'),]$estimate[1])
  comms <- c(comms, log(tmp21[which(tmp21$variable == 'DP03_0019'),]$estimate[1]))
  pt <- c(pt, max(0, log(tmp21[which(tmp21$variable == 'DP03_0021'),]$estimate[1])))
  hunits <- c(hunits, log(tmp21[which(tmp21$variable == 'DP04_0001'),]$estimate[1]))
  outdo <- c(outdo, tmp21[which(tmp21$variable == 'DP03_0033'),]$estimate[1])
  cons <- c(cons, tmp21[which(tmp21$variable == 'DP03_0034'),]$estimate[1])
  manu <- c(manu, tmp21[which(tmp21$variable == 'DP03_0035'),]$estimate[1])
  whole <- c(whole, tmp21[which(tmp21$variable == 'DP03_0036'),]$estimate[1])
  retail <- c(retail, tmp21[which(tmp21$variable == 'DP03_0037'),]$estimate[1])
  trans <- c(trans, tmp21[which(tmp21$variable == 'DP03_0038'),]$estimate[1])
  fin <- c(fin, tmp21[which(tmp21$variable == 'DP03_0039'),]$estimate[1])
  info <- c(info, tmp21[which(tmp21$variable == 'DP03_0040'),]$estimate[1])
  pro <- c(pro, tmp21[which(tmp21$variable == 'DP03_0041'),]$estimate[1])
  sw <- c(sw, tmp21[which(tmp21$variable == 'DP03_0042'),]$estimate[1])
  amen <- c(amen, tmp21[which(tmp21$variable == 'DP03_0043'),]$estimate[1])
  other <- c(other, tmp21[which(tmp21$variable == 'DP03_0044'),]$estimate[1])
  pad <- c(pad, tmp21[which(tmp21$variable == 'DP03_0045'),]$estimate[1])
  
}

for (i in 3109:6216) {
  
  print(paste0('Creating 2011 ACS data for county ', i-3108, ' of 3,108.......'))
  
  tmp11 <- c11 %>% filter(GEOID == data$County[i])
  
  pop <- c(pop, log(tmp11[which(tmp11$variable == 'DP05_0001'),]$estimate[1]))
  inc <- c(inc, log(tmp11[which(tmp11$variable == 'DP03_0062'),]$estimate[1]))
  ed <- c(ed, tmp11[which(tmp11$variable == 'DP02_0067P'),]$estimate[1])
  ed2 <- c(ed2, tmp11[which(tmp11$variable == 'DP02_0067P'),]$estimate[1])
  emp <- c(emp, tmp11[which(tmp11$variable == 'DP03_0009P'),]$estimate[1])
  movers <- c(movers, tmp11[which(tmp11$variable == 'DP02_0083P'),]$estimate[1])
  comms <- c(comms, log(tmp11[which(tmp11$variable == 'DP03_0019'),]$estimate[1]))
  pt <- c(pt, max(0, log(tmp11[which(tmp11$variable == 'DP03_0021'),]$estimate[1])))
  hunits <- c(hunits, log(tmp11[which(tmp11$variable == 'DP04_0001'),]$estimate[1]))
  outdo <- c(outdo, tmp11[which(tmp11$variable == 'DP03_0033'),]$estimate[1])
  cons <- c(cons, tmp11[which(tmp11$variable == 'DP03_0034'),]$estimate[1])
  manu <- c(manu, tmp11[which(tmp11$variable == 'DP03_0035'),]$estimate[1])
  whole <- c(whole, tmp11[which(tmp11$variable == 'DP03_0036'),]$estimate[1])
  retail <- c(retail, tmp11[which(tmp11$variable == 'DP03_0037'),]$estimate[1])
  trans <- c(trans, tmp11[which(tmp11$variable == 'DP03_0038'),]$estimate[1])
  fin <- c(fin, tmp11[which(tmp11$variable == 'DP03_0039'),]$estimate[1])
  info <- c(info, tmp11[which(tmp11$variable == 'DP03_0040'),]$estimate[1])
  pro <- c(pro, tmp11[which(tmp11$variable == 'DP03_0041'),]$estimate[1])
  sw <- c(sw, tmp11[which(tmp11$variable == 'DP03_0042'),]$estimate[1])
  amen <- c(amen, tmp11[which(tmp11$variable == 'DP03_0043'),]$estimate[1])
  other <- c(other, tmp11[which(tmp11$variable == 'DP03_0044'),]$estimate[1])
  pad <- c(pad, tmp11[which(tmp11$variable == 'DP03_0045'),]$estimate[1])
  
}

data <- cbind(data, pop, inc, ed, ed2, emp, movers, comms, pt, hunits, outdo, cons, manu, whole, retail, trans, fin, info, pro, sw, amen, other, pad)

colnames(data) <- c('County', 'Year', 'Water', 'Development', 'Barren', 'Forests', 'Shrublands', 'Grasslands', 'Agriculture', 'Wetlands', 'Population',
                    'Income', 'Education_BS', 'Education_HS', 'Unemployment', 'New_Residents', 'Commute_Solo_By_Car', 'Public_Transit', 'Housing_Units',
                    'Outdoors', 'Construction', 'Manufacturing', 'Wholesale', 'Retial', 'Transportation', 'Finance', 'Information', 'Professional',
                    'Social', 'Amenities', 'Other_Jobs', 'Public_Administration')

# Adding a total jobs column to data

data$Jobs <- data$Outdoors + data$Construction + data$Manufacturing + data$Wholesale + data$Retial + data$Transportation + data$Finance + data$Information + data$Professional + data$Social + data$Amenities + data$Other_Jobs + data$Public_Administration

# Creating a differenced data set

counties <- data$County[1:3108]
water <- data$Water[1:3108] - data$Water[3109:6216]
development <- data$Development[1:3108] - data$Development[3109:6216]
barren <- data$Barren[1:3108] - data$Barren[3109:6216]
forests <- data$Forests[1:3108] - data$Forests[3109:6216]
shrublands <- data$Shrublands[1:3108] - data$Shrublands[3109:6216]
grasslands <- data$Grasslands[1:3108] - data$Grasslands[3109:6216]
agriculture <- data$Agriculture[1:3108] - data$Agriculture[3109:6216]
wetlands <- data$Wetlands[1:3108] - data$Wetlands[3109:6216]
population <- data$Population[1:3108] - data$Population[3109:6216]
income <- 1.36*data$Income[1:3108] - data$Income[3109:6216]
education <- data$Education_BS[1:3108] - data$Education_BS[3109:6216]
education2 <- data$Education_HS[1:3108] - data$Education_HS[3109:6216]
unemployment <- data$Unemployment[1:3108] - data$Unemployment[3109:6216]
turnover <- data$New_Residents[1:3108] - data$New_Residents[3109:6216]
commy_car <- data$Commute_Solo_By_Car[1:3108] - data$Commute_Solo_By_Car[3109:6216]
public_commy <- data$Public_Transit[1:3108] - data$Public_Transit[3109:6216]
housing_units <- data$Housing_Units[1:3108] - data$Housing_Units[3109:6216]
outdoors <- data$Outdoors[1:3108] - data$Outdoors[3109:6216]
construction <- data$Construction[1:3108] - data$Construction[3109:6216]
manufacturing <- data$Manufacturing[1:3108] - data$Manufacturing[3109:6216]
wholesale <- data$Wholesale[1:3108] - data$Wholesale[3109:6216]
retail <- data$Retial[1:3108] - data$Retial[3109:6216]
transportation <- data$Transportation[1:3108] - data$Transportation[3109:6216]
finance <- data$Finance[1:3108] - data$Finance[3109:6216]
information <- data$Information[1:3108] - data$Information[3109:6216]
professional <- data$Professional[1:3108] - data$Professional[3109:6216]
social <- data$Social[1:3108] - data$Social[3109:6216]
amenities <- data$Amenities[1:3108] - data$Amenities[3109:6216]
other_jobs <- data$Other_Jobs[1:3108] - data$Other_Jobs[3109:6216]
public_administration <- data$Public_Administration[1:3108] - data$Public_Administration[3109:6216]
jobs <- data$Jobs[1:3108] - data$Jobs[3109:6216]

df <- as.data.frame(cbind(water, development, barren, forests, shrublands, grasslands, agriculture, wetlands, population, income,
                          education, education2, unemployment, turnover, commy_car, public_commy, housing_units, outdoors,
                          construction, manufacturing, wholesale, retail, transportation, finance, information, professional,
                          social, amenities, other_jobs, public_administration, jobs))

colnames(df) <- c('Water', 'Development', 'Barren', 'Forests', 'Shrublands', 'Grasslands', 'Agriculture', 'Wetlands', 'Population',
                  'Income', 'Education_BS', 'Education_HS', 'Unemployment', 'New_Residents', 'Commute_Solo_By_Car', 'Public_Transit',
                  'Housing_Units', 'Outdoors', 'Construction', 'Manufacturing', 'Wholesale', 'Retial', 'Transportation', 'Finance',
                  'Information', 'Professional', 'Social', 'Amenities', 'Other_Jobs', 'Public_Administration', 'Jobs')

df$County <- counties

# Adding an aggregate employment growth rate to df

egr <- c()

for (i in 1:nrow(df)) {
  
  print(paste0('Creating employment growth rate data for county ', i, ' of 3,108.......'))
  
  tmp <- data %>% filter(County == df$County[i])
  
  tmp_rates <- c((tmp$Outdoors[1] / tmp$Outdoors[2])^(1/10) - 1, 
                 (tmp$Construction[1] / tmp$Construction[2])^(1/10) - 1, 
                 (tmp$Manufacturing[1] / tmp$Manufacturing[2])^(1/10) - 1, 
                 (tmp$Wholesale[1] / tmp$Wholesale[2])^(1/10) - 1, 
                 (tmp$Retial[1] / tmp$Retial[2])^(1/10) - 1, 
                 (tmp$Transportation[1] / tmp$Transportation[2])^(1/10) - 1, 
                 (tmp$Finance[1] / tmp$Finance[2])^(1/10) - 1, 
                 (tmp$Information[1] / tmp$Information[2])^(1/10) - 1, 
                 (tmp$Professional[1] / tmp$Professional[2])^(1/10) - 1, 
                 (tmp$Social[1] / tmp$Social[2])^(1/10) - 1, 
                 (tmp$Amenities[1] / tmp$Amenities[2])^(1/10) - 1, 
                 (tmp$Other_Jobs[1] / tmp$Other_Jobs[2])^(1/10) - 1, 
                 (tmp$Public_Administration[1] / tmp$Public_Administration[2])^(1/10) - 1)
  
  tmp_rates[is.infinite(tmp_rates)] <- 0
  tmp_rates[is.na(tmp_rates)] <- 0
  val <- tmp$Outdoors[2]/tmp$Jobs[2]*tmp_rates[1] + tmp$Construction[2]/tmp$Jobs[2]*tmp_rates[2] + tmp$Manufacturing[2]/tmp$Jobs[2]*tmp_rates[3] + tmp$Wholesale[2]/tmp$Jobs[2]*tmp_rates[4] + tmp$Retial[2]/tmp$Jobs[2]*tmp_rates[5] + tmp$Transportation[2]/tmp$Jobs[2]*tmp_rates[6] + tmp$Finance[2]/tmp$Jobs[2]*tmp_rates[7] + tmp$Information[2]/tmp$Jobs[2]*tmp_rates[8] + tmp$Professional[2]/tmp$Jobs[2]*tmp_rates[9] + tmp$Social[2]/tmp$Jobs[2]*tmp_rates[10] + tmp$Amenities[2]/tmp$Jobs[2]*tmp_rates[11] + tmp$Other_Jobs[2]/tmp$Jobs[2]*tmp_rates[12] + tmp$Public_Administration[2]/tmp$Jobs[2]*tmp_rates[13]
  egr <- c(egr, val)
  
}

df <- cbind(df, egr)
colnames(df)[ncol(df)] <- 'Employment_Growth_Rate'

# Creating the Bartik instrument

baseline <- data %>% filter(Year == 2011)
update <- data %>% filter(Year == 2021)

out.rate <- (sum(update$Outdoors, na.rm = TRUE) / sum(baseline$Outdoors, na.rm = TRUE))^(1/10) - 1
con.rate <- (sum(update$Construction, na.rm = TRUE) / sum(baseline$Construction, na.rm = TRUE))^(1/10) - 1
man.rate <- (sum(update$Manufacturing, na.rm = TRUE) / sum(baseline$Manufacturing, na.rm = TRUE))^(1/10) - 1
who.rate <- (sum(update$Wholesale, na.rm = TRUE) / sum(baseline$Wholesale, na.rm = TRUE))^(1/10) - 1
ret.rate <- (sum(update$Retial, na.rm = TRUE) / sum(baseline$Retial, na.rm = TRUE))^(1/10) - 1
tra.rate <- (sum(update$Transportation, na.rm = TRUE) / sum(baseline$Transportation, na.rm = TRUE))^(1/10) - 1
fin.rate <- (sum(update$Finance, na.rm = TRUE) / sum(baseline$Finance, na.rm = TRUE))^(1/10) - 1
inf.rate <- (sum(update$Information, na.rm = TRUE) / sum(baseline$Information, na.rm = TRUE))^(1/10) - 1
pro.rate <- (sum(update$Professional, na.rm = TRUE) / sum(baseline$Professional, na.rm = TRUE))^(1/10) - 1
soc.rate <- (sum(update$Social, na.rm = TRUE) / sum(baseline$Social, na.rm = TRUE))^(1/10) - 1
ame.rate <- (sum(update$Amenities, na.rm = TRUE) / sum(baseline$Amenities, na.rm = TRUE))^(1/10) - 1
oth.rate <- (sum(update$Other_Jobs, na.rm = TRUE) / sum(baseline$Other_Jobs, na.rm = TRUE))^(1/10) - 1
pub.rate <- (sum(update$Public_Administration, na.rm = TRUE) / sum(baseline$Public_Administration, na.rm = TRUE))^(1/10) - 1

national_rates <- c(out.rate, con.rate, man.rate, who.rate, ret.rate, tra.rate, fin.rate, inf.rate, pro.rate, soc.rate, ame.rate, oth.rate, pub.rate)

bartik <- c()

for (i in 1:nrow(df)) {
  
  print(paste0('Creating Bartik instrument for county ', i, ' of 3,108.......'))
  
  tmp <- data %>% filter(County == df$County[i])
  
  tmp_rates[is.infinite(tmp_rates)] <- 0
  tmp_rates[is.na(tmp_rates)] <- 0
  val <- tmp$Outdoors[2]/tmp$Jobs[2]*national_rates[1] + tmp$Construction[2]/tmp$Jobs[2]*national_rates[2] + tmp$Manufacturing[2]/tmp$Jobs[2]*national_rates[3] + tmp$Wholesale[2]/tmp$Jobs[2]*national_rates[4] + tmp$Retial[2]/tmp$Jobs[2]*national_rates[5] + tmp$Transportation[2]/tmp$Jobs[2]*national_rates[6] + tmp$Finance[2]/tmp$Jobs[2]*national_rates[7] + tmp$Information[2]/tmp$Jobs[2]*national_rates[8] + tmp$Professional[2]/tmp$Jobs[2]*national_rates[9] + tmp$Social[2]/tmp$Jobs[2]*national_rates[10] + tmp$Amenities[2]/tmp$Jobs[2]*national_rates[11] + tmp$Other_Jobs[2]/tmp$Jobs[2]*national_rates[12] + tmp$Public_Administration[2]/tmp$Jobs[2]*national_rates[13]
  bartik <- c(bartik, val)
  
}

df <- cbind(df, bartik)
colnames(df)[ncol(df)] <- 'Bartik'

# Adding a state fixed effect

df$State <- substr(df$County, 1, 2)

# Adding historical values of land cover

df$Water.2011 <- data$Water[3109:6216]
df$Development.2011 <- data$Development[3109:6216]
df$Barren.2011 <- data$Barren[3109:6216]
df$Forests.2011 <- data$Forests[3109:6216]
df$Shrublands.2011 <- data$Shrublands[3109:6216]
df$Grasslands.2011 <- data$Grasslands[3109:6216]
df$Agriculture.2011 <- data$Agriculture[3109:6216]
df$Wetlands.2011 <- data$Wetlands[3109:6216]

# Adding urban-rural continuum codes

ur <- read.csv(paste0(direc, 'data/ur_codes.csv'))

ur$FIPS <- ifelse(ur$FIPS < 10000, paste0('0', as.character(ur$FIPS)), as.character(ur$FIPS))
ur$Large <- ifelse(ur$Code <= 3, 1, 0)
ur$Small <- ifelse(ur$Code %in% c(4,5), 1, 0)
ur$Rural <- ifelse(ur$Code == 6, 1, 0)

large <- c()
small <- c()
rural <- c()

for (i in 1:nrow(df)) {
  
  print(paste0('Designating rural-urban status for county ', i, ' of 3,108.......'))
  
  tmp <- ur %>% filter(FIPS == df$County[i])
  
  large <- c(large, tmp$Large[1])
  small <- c(small, tmp$Small[1])
  rural <- c(rural, tmp$Rural[1])
  
}

df$Large <- large
df$Small <- small
df$Rural <- rural

# Adding land area data

land.area <- c()

for (i in 1:nrow(df)) {
  
  print(paste0('Getting land area for county ', i, ' of 3,108.......'))
  
  tmp <- us_counties %>% filter(GEOID == df$County[i])
  land.area <- c(land.area, tmp$ALAND[1])
  
}

df$Land_Area <- land.area

df$Development_Area <- df$Land_Area * df$Development.2011
df$Barren_Area <- df$Land_Area * df$Barren.2011
df$Forests_Area <- df$Land_Area * df$Forests.2011
df$Shrublands_Area <- df$Land_Area * df$Shrublands.2011
df$Grasslands_Area <- df$Land_Area * df$Grasslands.2011
df$Agriculture_Area <- df$Land_Area * df$Agriculture.2011
df$Wetlands_Area <- df$Land_Area * df$Wetlands.2011

# How consistent are these data sets? Use water area percentages

water.area <- c()

for (i in 1:nrow(df)) {
  
  print(paste0('Getting water area for county ', i, ' of 3,108.......'))
  
  tmp <- us_counties %>% filter(GEOID == df$County[i])
  water.area <- c(water.area, tmp$AWATER[1])
  
}

df$Water_Area <- water.area
df$Water_Pct <- df$Water_Area / (df$Water_Area + df$Land_Area)
df$Water <- df$Land_Area * df$Water.2011
df$Water_Diff <- df$Water.2011 - df$Water_Pct

ggplot(data = df, aes(x = Water.2011, y = Water_Pct)) +
  geom_point() +
  theme_bw() +
  ggtitle('Comparing the consistency of the two land cover data sets on 2011 proportions') +
  ylab('Census Bureau') +
  xlab('National Land Cover Database') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df) +
  theme_bw() +
  ggtitle('Comparing the consistency of the two land cover data sets on 2011 proportions') +
  geom_histogram(aes(x = Water.2011, fill = 'NLCD'),  alpha = 0.2, bins = 200) + 
  geom_histogram(aes(x = Water_Pct, fill = 'USCB'), alpha = 0.2, bins = 200) +
  scale_fill_manual(values = c('blue2', 'yellow2')) +
  ylab('Frequency') +
  xlab('Proportion') +
  guides(fill = guide_legend(title = 'Data Set')) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = df) +
  theme_bw() +
  ggtitle('Comparing the consistency of the two land cover data sets on 2011 proportions') +
  geom_histogram(aes(x = Water_Diff, fill = 'fuck', col = 'orange'),  alpha = 1, bins = 200) + 
  scale_fill_manual(values = 'red4') +
  ylab('Frequency') +
  xlab('Difference') + 
  xlim(c(-0.25,0.25)) +
  theme(legend.position = 'none') +
  theme(plot.title = element_text(hjust = 0.5))

checkerooni <- lm(Water_Pct ~ Water.2011, data = df)

stargazer(checkerooni, type = 'text')

# Running regressions for all counties

water.mod <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                   + Housing_Units + Water.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

development.mod <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

barren.mod <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                    + Housing_Units + Barren.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

forests.mod <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Forests.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

shrublands.mod <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Shrublands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

grasslands.mod <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Grasslands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

agriculture.mod <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Agriculture.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

wetlands.mod <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

water.modx <- coeftest(water.mod, vcov = vcovCL(water.mod, type = 'HC1'))
development.modx <- coeftest(development.mod, vcov = vcovCL(development.mod, type = 'HC1'))
barren.modx <- coeftest(barren.mod, vcov = vcovCL(barren.mod, type = 'HC1'))
forests.modx <- coeftest(forests.mod, vcov = vcovCL(forests.mod, type = 'HC1'))
shrublands.modx <- coeftest(shrublands.mod, vcov = vcovCL(shrublands.mod, type = 'HC1'))
grasslands.modx <- coeftest(grasslands.mod, vcov = vcovCL(grasslands.mod, type = 'HC1'))
agriculture.modx <- coeftest(agriculture.mod, vcov = vcovCL(agriculture.mod, type = 'HC1'))
wetlands.modx <- coeftest(wetlands.mod, vcov = vcovCL(wetlands.mod, type = 'HC1'))

stargazer(development.mod, barren.mod, forests.mod, shrublands.mod, grasslands.mod, agriculture.mod, wetlands.mod,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.modx, barren.modx, forests.modx, shrublands.modx, grasslands.modx, agriculture.modx, wetlands.modx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for rural counties

water.rural <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Water.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

development.rural <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Development.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

barren.rural <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Barren.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

forests.rural <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Forests.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

shrublands.rural <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Shrublands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

grasslands.rural <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Grasslands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

agriculture.rural <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Agriculture.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

wetlands.rural <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Wetlands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

water.ruralx <- coeftest(water.rural, vcov = vcovCL(water.rural, type = 'HC1'))
development.ruralx <- coeftest(development.rural, vcov = vcovCL(development.rural, type = 'HC1'))
barren.ruralx <- coeftest(barren.rural, vcov = vcovCL(barren.rural, type = 'HC1'))
forests.ruralx <- coeftest(forests.rural, vcov = vcovCL(forests.rural, type = 'HC1'))
shrublands.ruralx <- coeftest(shrublands.rural, vcov = vcovCL(shrublands.rural, type = 'HC1'))
grasslands.ruralx <- coeftest(grasslands.rural, vcov = vcovCL(grasslands.rural, type = 'HC1'))
agriculture.ruralx <- coeftest(agriculture.rural, vcov = vcovCL(agriculture.rural, type = 'HC1'))
wetlands.ruralx <- coeftest(wetlands.rural, vcov = vcovCL(wetlands.rural, type = 'HC1'))

stargazer(development.rural, barren.rural, forests.rural, shrublands.rural, grasslands.rural, agriculture.rural, wetlands.rural,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.ruralx, barren.ruralx, forests.ruralx, shrublands.ruralx, grasslands.ruralx, agriculture.ruralx, wetlands.ruralx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all non-rural counties

water.urban <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Water.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

development.urban <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Development.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

barren.urban <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Barren.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

forests.urban <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Forests.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

shrublands.urban <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Shrublands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

grasslands.urban <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Grasslands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

agriculture.urban <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Agriculture.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

wetlands.urban <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Wetlands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

water.urbanx <- coeftest(water.urban, vcov = vcovCL(water.urban, type = 'HC1'))
development.urbanx <- coeftest(development.urban, vcov = vcovCL(development.urban, type = 'HC1'))
barren.urbanx <- coeftest(barren.urban, vcov = vcovCL(barren.urban, type = 'HC1'))
forests.urbanx <- coeftest(forests.urban, vcov = vcovCL(forests.urban, type = 'HC1'))
shrublands.urbanx <- coeftest(shrublands.urban, vcov = vcovCL(shrublands.urban, type = 'HC1'))
grasslands.urbanx <- coeftest(grasslands.urban, vcov = vcovCL(grasslands.urban, type = 'HC1'))
agriculture.urbanx <- coeftest(agriculture.urban, vcov = vcovCL(agriculture.urban, type = 'HC1'))
wetlands.urbanx <- coeftest(wetlands.urban, vcov = vcovCL(wetlands.urban, type = 'HC1'))

stargazer(development.urban, barren.urban, forests.urban, shrublands.urban, grasslands.urban, agriculture.urban, wetlands.urban,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.urbanx, barren.urbanx, forests.urbanx, shrublands.urbanx, grasslands.urbanx, agriculture.urbanx, wetlands.urbanx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all large metropolitan counties (UR codes 1-3)

water.large <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Water.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

development.large <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Development.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

barren.large <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Barren.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

forests.large <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Forests.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

shrublands.large <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Shrublands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

grasslands.large <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Grasslands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

agriculture.large <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Agriculture.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

wetlands.large <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Wetlands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

water.largex <- coeftest(water.large, vcov = vcovCL(water.large, type = 'HC1'))
development.largex <- coeftest(development.large, vcov = vcovCL(development.large, type = 'HC1'))
barren.largex <- coeftest(barren.large, vcov = vcovCL(barren.large, type = 'HC1'))
forests.largex <- coeftest(forests.large, vcov = vcovCL(forests.large, type = 'HC1'))
shrublands.largex <- coeftest(shrublands.large, vcov = vcovCL(shrublands.large, type = 'HC1'))
grasslands.largex <- coeftest(grasslands.large, vcov = vcovCL(grasslands.large, type = 'HC1'))
agriculture.largex <- coeftest(agriculture.large, vcov = vcovCL(agriculture.large, type = 'HC1'))
wetlands.largex <- coeftest(wetlands.large, vcov = vcovCL(wetlands.large, type = 'HC1'))

stargazer(development.large, barren.large, forests.large, shrublands.large, grasslands.large, agriculture.large, wetlands.large,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.largex, barren.largex, forests.largex, shrublands.largex, grasslands.largex, agriculture.largex, wetlands.largex,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all small metropolitan counties (UR codes 4 and 5)

water.small <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Water.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

development.small <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Development.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

barren.small <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Barren.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

forests.small <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Forests.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

shrublands.small <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Shrublands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

grasslands.small <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Grasslands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

agriculture.small <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Agriculture.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

wetlands.small <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Wetlands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

water.smallx <- coeftest(water.small, vcov = vcovCL(water.small, type = 'HC1'))
development.smallx <- coeftest(development.small, vcov = vcovCL(development.small, type = 'HC1'))
barren.smallx <- coeftest(barren.small, vcov = vcovCL(barren.small, type = 'HC1'))
forests.smallx <- coeftest(forests.small, vcov = vcovCL(forests.small, type = 'HC1'))
shrublands.smallx <- coeftest(shrublands.small, vcov = vcovCL(shrublands.small, type = 'HC1'))
grasslands.smallx <- coeftest(grasslands.small, vcov = vcovCL(grasslands.small, type = 'HC1'))
agriculture.smallx <- coeftest(agriculture.small, vcov = vcovCL(agriculture.small, type = 'HC1'))
wetlands.smallx <- coeftest(wetlands.small, vcov = vcovCL(wetlands.small, type = 'HC1'))

stargazer(development.small, barren.small, forests.small, shrublands.small, grasslands.small, agriculture.small, wetlands.small,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.smallx, barren.smallx, forests.smallx, shrublands.smallx, grasslands.smallx, agriculture.smallx, wetlands.smallx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Repeat analyses for all counties with initial proportions for all land cover types as a robustness test

water.mod2 <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + Water.2011
                    + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                    + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

development.mod2 <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + Water.2011
                          + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

barren.mod2 <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + Water.2011
                     + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                     + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

forests.mod2 <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + Water.2011
                      + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                      + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

shrublands.mod2 <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + Water.2011
                         + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

grasslands.mod2 <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + Water.2011
                         + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

agriculture.mod2 <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + Water.2011
                          + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

wetlands.mod2 <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + Water.2011
                       + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                       + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

water.mod2x <- coeftest(water.mod2, vcov = vcovCL(water.mod2, type = 'HC1'))
development.mod2x <- coeftest(development.mod2, vcov = vcovCL(development.mod2, type = 'HC1'))
barren.mod2x <- coeftest(barren.mod2, vcov = vcovCL(barren.mod2, type = 'HC1'))
forests.mod2x <- coeftest(forests.mod2, vcov = vcovCL(forests.mod2, type = 'HC1'))
shrublands.mod2x <- coeftest(shrublands.mod2, vcov = vcovCL(shrublands.mod2, type = 'HC1'))
grasslands.mod2x <- coeftest(grasslands.mod2, vcov = vcovCL(grasslands.mod2, type = 'HC1'))
agriculture.mod2x <- coeftest(agriculture.mod2, vcov = vcovCL(agriculture.mod2, type = 'HC1'))
wetlands.mod2x <- coeftest(wetlands.mod2, vcov = vcovCL(wetlands.mod2, type = 'HC1'))

stargazer(development.mod2, barren.mod2, forests.mod2, shrublands.mod2, grasslands.mod2, agriculture.mod2, wetlands.mod2,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.mod2x, barren.mod2x, forests.mod2x, shrublands.mod2x, grasslands.mod2x, agriculture.mod2x, wetlands.mod2x,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Saving results

write.csv(stargazer(development.modx, barren.modx, forests.modx, shrublands.modx, grasslands.modx, agriculture.modx, wetlands.modx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/main.txt'), row.names = FALSE)
write.csv(stargazer(development.ruralx, barren.ruralx, forests.ruralx, shrublands.ruralx, grasslands.ruralx, agriculture.ruralx, wetlands.ruralx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/rural.txt'), row.names = FALSE)
write.csv(stargazer(development.urbanx, barren.urbanx, forests.urbanx, shrublands.urbanx, grasslands.urbanx, agriculture.urbanx, wetlands.urbanx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/non_rural.txt'), row.names = FALSE)
write.csv(stargazer(development.largex, barren.largex, forests.largex, shrublands.largex, grasslands.largex, agriculture.largex, wetlands.largex, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/large_urban.txt'), row.names = FALSE)
write.csv(stargazer(development.smallx, barren.smallx, forests.smallx, shrublands.smallx, grasslands.smallx, agriculture.smallx, wetlands.smallx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/small_urban.txt'), row.names = FALSE)
write.csv(stargazer(development.mod2x, barren.mod2x, forests.mod2x, shrublands.mod2x, grasslands.mod2x, agriculture.mod2x, wetlands.mod2x, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/main_2.txt'), row.names = FALSE)

f.stats.main <- c(66.15, 76.30, 71.42, 63.58, 68.38, 104.73, 76.30)
f.stats.rural <- c(23.82, 27.11, 23.55, 18.33, 22.76, 34.19, 25.09)
f.stats.urban <- c(67.37, 88.68, 88.51, 83.01, 83.22, 111.49, 91.98)
f.stats.large <- c(4.49, 27.14, 28.21, 28.62, 28.18, 25.53, 28.29)
f.stats.small <- c(44.20, 50.17, 48.25, 45.88, 47.29, 66.59, 52.98)
f.stats.main2 <- c(65.04, 65.04, 65.04, 65.04, 65.04, 65.04, 65.04)

nobs.main <- c(3106, 3106, 3106, 3106, 3106, 3106, 3106)
nobs.rural <- c(1309, 1309, 1309, 1309, 1309, 1309, 1309)
nobs.urban <- c(1797, 1797, 1797, 1797, 1797, 1797, 1797)
nobs.large <- c(805, 805, 805, 805, 805, 805, 805)
nobs.small <- c(992, 992, 992, 992, 992, 992, 992)
nobs.main2 <- c(3106, 3106, 3106, 3106, 3106, 3106, 3106)

additional.stats <- as.data.frame(rbind(f.stats.main, f.stats.rural, f.stats.urban, f.stats.large, f.stats.small, f.stats.main2,
                                        nobs.main, nobs.rural, nobs.urban, nobs.large, nobs.small, nobs.main2))

write.csv(additional.stats, paste0(direc, 'results/additional_stats.txt'), row.names = TRUE)

# Running regressions for all counties - controlling with areas instead of percentages

water.mod <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                   + Housing_Units + log(Water_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

development.mod <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + log(Development_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

barren.mod <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                    + Housing_Units + log(Barren_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

forests.mod <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Forests_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

shrublands.mod <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Shrublands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

grasslands.mod <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Grasslands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

agriculture.mod <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + log(Agriculture_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

wetlands.mod <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

water.modx <- coeftest(water.mod, vcov = vcovCL(water.mod, type = 'HC1'))
development.modx <- coeftest(development.mod, vcov = vcovCL(development.mod, type = 'HC1'))
barren.modx <- coeftest(barren.mod, vcov = vcovCL(barren.mod, type = 'HC1'))
forests.modx <- coeftest(forests.mod, vcov = vcovCL(forests.mod, type = 'HC1'))
shrublands.modx <- coeftest(shrublands.mod, vcov = vcovCL(shrublands.mod, type = 'HC1'))
grasslands.modx <- coeftest(grasslands.mod, vcov = vcovCL(grasslands.mod, type = 'HC1'))
agriculture.modx <- coeftest(agriculture.mod, vcov = vcovCL(agriculture.mod, type = 'HC1'))
wetlands.modx <- coeftest(wetlands.mod, vcov = vcovCL(wetlands.mod, type = 'HC1'))

stargazer(development.mod, barren.mod, forests.mod, shrublands.mod, grasslands.mod, agriculture.mod, wetlands.mod,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.modx, barren.modx, forests.modx, shrublands.modx, grasslands.modx, agriculture.modx, wetlands.modx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for rural counties

water.rural <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Water_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

development.rural <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Development_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

barren.rural <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Barren_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

forests.rural <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + log(Forests_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

shrublands.rural <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Shrublands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

grasslands.rural <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Grasslands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

agriculture.rural <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Agriculture_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

wetlands.rural <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Wetlands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

water.ruralx <- coeftest(water.rural, vcov = vcovCL(water.rural, type = 'HC1'))
development.ruralx <- coeftest(development.rural, vcov = vcovCL(development.rural, type = 'HC1'))
barren.ruralx <- coeftest(barren.rural, vcov = vcovCL(barren.rural, type = 'HC1'))
forests.ruralx <- coeftest(forests.rural, vcov = vcovCL(forests.rural, type = 'HC1'))
shrublands.ruralx <- coeftest(shrublands.rural, vcov = vcovCL(shrublands.rural, type = 'HC1'))
grasslands.ruralx <- coeftest(grasslands.rural, vcov = vcovCL(grasslands.rural, type = 'HC1'))
agriculture.ruralx <- coeftest(agriculture.rural, vcov = vcovCL(agriculture.rural, type = 'HC1'))
wetlands.ruralx <- coeftest(wetlands.rural, vcov = vcovCL(wetlands.rural, type = 'HC1'))

stargazer(development.rural, barren.rural, forests.rural, shrublands.rural, grasslands.rural, agriculture.rural, wetlands.rural,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.ruralx, barren.ruralx, forests.ruralx, shrublands.ruralx, grasslands.ruralx, agriculture.ruralx, wetlands.ruralx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all non-rural counties

water.urban <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Water_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

development.urban <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Development_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

barren.urban <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Barren_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

forests.urban <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + log(Forests_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

shrublands.urban <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Shrublands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

grasslands.urban <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Grasslands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

agriculture.urban <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Agriculture_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

wetlands.urban <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Wetlands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

water.urbanx <- coeftest(water.urban, vcov = vcovCL(water.urban, type = 'HC1'))
development.urbanx <- coeftest(development.urban, vcov = vcovCL(development.urban, type = 'HC1'))
barren.urbanx <- coeftest(barren.urban, vcov = vcovCL(barren.urban, type = 'HC1'))
forests.urbanx <- coeftest(forests.urban, vcov = vcovCL(forests.urban, type = 'HC1'))
shrublands.urbanx <- coeftest(shrublands.urban, vcov = vcovCL(shrublands.urban, type = 'HC1'))
grasslands.urbanx <- coeftest(grasslands.urban, vcov = vcovCL(grasslands.urban, type = 'HC1'))
agriculture.urbanx <- coeftest(agriculture.urban, vcov = vcovCL(agriculture.urban, type = 'HC1'))
wetlands.urbanx <- coeftest(wetlands.urban, vcov = vcovCL(wetlands.urban, type = 'HC1'))

stargazer(development.urban, barren.urban, forests.urban, shrublands.urban, grasslands.urban, agriculture.urban, wetlands.urban,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.urbanx, barren.urbanx, forests.urbanx, shrublands.urbanx, grasslands.urbanx, agriculture.urbanx, wetlands.urbanx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all large metropolitan counties (UR codes 1-3)

water.large <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Water_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

development.large <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Development_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

barren.large <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Barren_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

forests.large <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + log(Forests_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

shrublands.large <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Shrublands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

grasslands.large <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Grasslands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

agriculture.large <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Agriculture_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

wetlands.large <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Wetlands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

water.largex <- coeftest(water.large, vcov = vcovCL(water.large, type = 'HC1'))
development.largex <- coeftest(development.large, vcov = vcovCL(development.large, type = 'HC1'))
barren.largex <- coeftest(barren.large, vcov = vcovCL(barren.large, type = 'HC1'))
forests.largex <- coeftest(forests.large, vcov = vcovCL(forests.large, type = 'HC1'))
shrublands.largex <- coeftest(shrublands.large, vcov = vcovCL(shrublands.large, type = 'HC1'))
grasslands.largex <- coeftest(grasslands.large, vcov = vcovCL(grasslands.large, type = 'HC1'))
agriculture.largex <- coeftest(agriculture.large, vcov = vcovCL(agriculture.large, type = 'HC1'))
wetlands.largex <- coeftest(wetlands.large, vcov = vcovCL(wetlands.large, type = 'HC1'))

stargazer(development.large, barren.large, forests.large, shrublands.large, grasslands.large, agriculture.large, wetlands.large,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.largex, barren.largex, forests.largex, shrublands.largex, grasslands.largex, agriculture.largex, wetlands.largex,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all small metropolitan counties (UR codes 4 and 5)

water.small <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Water_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

development.small <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Development_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

barren.small <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Barren_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

forests.small <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + log(Forests_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

shrublands.small <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Shrublands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

grasslands.small <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Grasslands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

agriculture.small <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Agriculture_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

wetlands.small <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Wetlands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

water.smallx <- coeftest(water.small, vcov = vcovCL(water.small, type = 'HC1'))
development.smallx <- coeftest(development.small, vcov = vcovCL(development.small, type = 'HC1'))
barren.smallx <- coeftest(barren.small, vcov = vcovCL(barren.small, type = 'HC1'))
forests.smallx <- coeftest(forests.small, vcov = vcovCL(forests.small, type = 'HC1'))
shrublands.smallx <- coeftest(shrublands.small, vcov = vcovCL(shrublands.small, type = 'HC1'))
grasslands.smallx <- coeftest(grasslands.small, vcov = vcovCL(grasslands.small, type = 'HC1'))
agriculture.smallx <- coeftest(agriculture.small, vcov = vcovCL(agriculture.small, type = 'HC1'))
wetlands.smallx <- coeftest(wetlands.small, vcov = vcovCL(wetlands.small, type = 'HC1'))

stargazer(development.small, barren.small, forests.small, shrublands.small, grasslands.small, agriculture.small, wetlands.small,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.smallx, barren.smallx, forests.smallx, shrublands.smallx, grasslands.smallx, agriculture.smallx, wetlands.smallx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Repeat analyses for all counties with initial proportions for all land cover types as a robustness test

water.mod2 <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                    + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                    + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

development.mod2 <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                          + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                          + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

barren.mod2 <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                     + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                     + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

forests.mod2 <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                      + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                      + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

shrublands.mod2 <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                         + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                         + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

grasslands.mod2 <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                         + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                         + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

agriculture.mod2 <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                          + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                          + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

wetlands.mod2 <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                       + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                       + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

water.mod2x <- coeftest(water.mod2, vcov = vcovCL(water.mod2, type = 'HC1'))
development.mod2x <- coeftest(development.mod2, vcov = vcovCL(development.mod2, type = 'HC1'))
barren.mod2x <- coeftest(barren.mod2, vcov = vcovCL(barren.mod2, type = 'HC1'))
forests.mod2x <- coeftest(forests.mod2, vcov = vcovCL(forests.mod2, type = 'HC1'))
shrublands.mod2x <- coeftest(shrublands.mod2, vcov = vcovCL(shrublands.mod2, type = 'HC1'))
grasslands.mod2x <- coeftest(grasslands.mod2, vcov = vcovCL(grasslands.mod2, type = 'HC1'))
agriculture.mod2x <- coeftest(agriculture.mod2, vcov = vcovCL(agriculture.mod2, type = 'HC1'))
wetlands.mod2x <- coeftest(wetlands.mod2, vcov = vcovCL(wetlands.mod2, type = 'HC1'))

stargazer(development.mod2, barren.mod2, forests.mod2, shrublands.mod2, grasslands.mod2, agriculture.mod2, wetlands.mod2,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.mod2x, barren.mod2x, forests.mod2x, shrublands.mod2x, grasslands.mod2x, agriculture.mod2x, wetlands.mod2x,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Saving results

write.csv(stargazer(development.modx, barren.modx, forests.modx, shrublands.modx, grasslands.modx, agriculture.modx, wetlands.modx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/area_main.txt'), row.names = FALSE)
write.csv(stargazer(development.ruralx, barren.ruralx, forests.ruralx, shrublands.ruralx, grasslands.ruralx, agriculture.ruralx, wetlands.ruralx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/area_rural.txt'), row.names = FALSE)
write.csv(stargazer(development.urbanx, barren.urbanx, forests.urbanx, shrublands.urbanx, grasslands.urbanx, agriculture.urbanx, wetlands.urbanx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/area_non_rural.txt'), row.names = FALSE)
write.csv(stargazer(development.largex, barren.largex, forests.largex, shrublands.largex, grasslands.largex, agriculture.largex, wetlands.largex, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/area_large_urban.txt'), row.names = FALSE)
write.csv(stargazer(development.smallx, barren.smallx, forests.smallx, shrublands.smallx, grasslands.smallx, agriculture.smallx, wetlands.smallx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/area_small_urban.txt'), row.names = FALSE)
write.csv(stargazer(development.mod2x, barren.mod2x, forests.mod2x, shrublands.mod2x, grasslands.mod2x, agriculture.mod2x, wetlands.mod2x, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/area_main_2.txt'), row.names = FALSE)

f.stats.main <- c(63.95, 74.48, 67.27, 74.33, 76.55, 122.98, 74.81)
f.stats.rural <- c(19.63, 23.58, 20.08, 24.87, 26.23, 45.88, 23.93)
f.stats.urban <- c(78.41, 88.62, 86.18, 86.21, 84.70, 100.65, 87.25)
f.stats.large <- c(24.45, 27.36, 25.77, 24.05, 22.52, 25.81, 26.27)
f.stats.small <- c(47.68, 50.46, 45.25, 50.71, 49.68, 62.05, 50.31)
f.stats.main2 <- c(74.67, 74.67, 74.67, 74.67, 74.67, 74.67, 74.67)

nobs.main <- c(3106, 3106, 3106, 3106, 3106, 3106, 3106)
nobs.rural <- c(1309, 1309, 1309, 1309, 1309, 1309, 1309)
nobs.urban <- c(1797, 1797, 1797, 1797, 1797, 1797, 1797)
nobs.large <- c(805, 805, 805, 805, 805, 805, 805)
nobs.small <- c(992, 992, 992, 992, 992, 992, 992)
nobs.main2 <- c(3106, 3106, 3106, 3106, 3106, 3106, 3106)

additional.stats <- as.data.frame(rbind(f.stats.main, f.stats.rural, f.stats.urban, f.stats.large, f.stats.small, f.stats.main2,
                                        nobs.main, nobs.rural, nobs.urban, nobs.large, nobs.small, nobs.main2))

write.csv(additional.stats, paste0(direc, 'results/area_additional_stats.txt'), row.names = TRUE)

# Creating a summary statistics figure

sdf <- df[,c(33, 2:8, 9:11, 13:17, 46)]

sdf$Education_BS <- sdf$Education_BS / 100
sdf$New_Residents <- sdf$New_Residents / 100

colnames(sdf)[colnames(sdf) == 'Population'] <- 'Population (log Change)'
colnames(sdf)[colnames(sdf) == 'Unemployment'] <- 'Unemployment Rate (Change)'
colnames(sdf)[colnames(sdf) == 'Income'] <- 'Income (log Change in 2021 USD)'
colnames(sdf)[colnames(sdf) == 'Education_BS'] <- "At Least a Bachelor's Degree (Change in Proportion)"
colnames(sdf)[colnames(sdf) == 'Commute_Solo_By_Car'] <- 'Commuting Solo by Car (log Change)'
colnames(sdf)[colnames(sdf) == 'Public_Transit'] <- 'Commuting by Public Transit (log Change)'
colnames(sdf)[colnames(sdf) == 'New_Residents'] <- 'New Residents (Change in Proportion)'
colnames(sdf)[colnames(sdf) == 'Housing_Units'] <- 'Housing Units (log Change)'
colnames(sdf)[colnames(sdf) == 'Employment_Growth_Rate'] <- 'Employment Growth Rate (Change)'
colnames(sdf)[colnames(sdf) == 'Development'] <- 'Developed Land (Change in Proportion)'
colnames(sdf)[colnames(sdf) == 'Barren'] <- 'Barren Land (Change in Proportion)'
colnames(sdf)[colnames(sdf) == 'Forests'] <- 'Forested Land (Change in Proportion)'
colnames(sdf)[colnames(sdf) == 'Shrublands'] <- 'Shrublands (Change in Proportion)'
colnames(sdf)[colnames(sdf) == 'Grasslands'] <- 'Grassands (Change in Proportion)'
colnames(sdf)[colnames(sdf) == 'Agriculture'] <- 'Agricultural Land (Change in Proportion)'
colnames(sdf)[colnames(sdf) == 'Wetlands'] <- 'Wetlands (Change in Proportion)'

datasummary_skim(sdf, fmt = '%.3f')

# Ensuring the area data results are valid by removing outliers

# Which observations are consistent (in water) across data sets

dfx <- df %>% filter(Water_Diff > mean(df$Water_Diff) - 2*sd(df$Water_Diff)) %>% filter(Water_Diff < mean(df$Water_Diff) + 2*sd(df$Water_Diff))

ggplot(data = dfx) +
  theme_bw() +
  ggtitle('Comparing the consistency of the two land cover data sets on 2011 proportions') +
  geom_histogram(aes(x = Water_Diff, fill = 'fuck', col = 'orange'),  alpha = 1, bins = 200) + 
  scale_fill_manual(values = 'red4') +
  ylab('Frequency') +
  xlab('Difference') + 
  theme(legend.position = 'none') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = dfx, aes(x = Water.2011, y = Water_Pct)) +
  geom_point() +
  theme_bw() +
  ggtitle('Comparing the consistency of the two land cover data sets on 2011 proportions') +
  ylab('Census Bureau') +
  xlab('National Land Cover Database') +
  theme(plot.title = element_text(hjust = 0.5))

checkerooni2 <- lm(Water_Pct ~ Water.2011, data = dfx)

stargazer(checkerooni, checkerooni2, type = 'text')

# Running regressions for all counties - controlling with areas instead of percentages

water.mod <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                   + Housing_Units + log(Water_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx)

development.mod <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + log(Development_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx)

barren.mod <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                    + Housing_Units + log(Barren_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx)

forests.mod <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Forests_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx)

shrublands.mod <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Shrublands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx)

grasslands.mod <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Grasslands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx)

agriculture.mod <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + log(Agriculture_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx)

wetlands.mod <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx)

water.modx <- coeftest(water.mod, vcov = vcovCL(water.mod, type = 'HC1'))
development.modx <- coeftest(development.mod, vcov = vcovCL(development.mod, type = 'HC1'))
barren.modx <- coeftest(barren.mod, vcov = vcovCL(barren.mod, type = 'HC1'))
forests.modx <- coeftest(forests.mod, vcov = vcovCL(forests.mod, type = 'HC1'))
shrublands.modx <- coeftest(shrublands.mod, vcov = vcovCL(shrublands.mod, type = 'HC1'))
grasslands.modx <- coeftest(grasslands.mod, vcov = vcovCL(grasslands.mod, type = 'HC1'))
agriculture.modx <- coeftest(agriculture.mod, vcov = vcovCL(agriculture.mod, type = 'HC1'))
wetlands.modx <- coeftest(wetlands.mod, vcov = vcovCL(wetlands.mod, type = 'HC1'))

stargazer(development.mod, barren.mod, forests.mod, shrublands.mod, grasslands.mod, agriculture.mod, wetlands.mod,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.modx, barren.modx, forests.modx, shrublands.modx, grasslands.modx, agriculture.modx, wetlands.modx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for rural counties

water.rural <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Water_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Rural == 1),])

development.rural <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Development_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Rural == 1),])

barren.rural <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Barren_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Rural == 1),])

forests.rural <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + log(Forests_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Rural == 1),])

shrublands.rural <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Shrublands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Rural == 1),])

grasslands.rural <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Grasslands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Rural == 1),])

agriculture.rural <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Agriculture_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Rural == 1),])

wetlands.rural <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Wetlands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Rural == 1),])

water.ruralx <- coeftest(water.rural, vcov = vcovCL(water.rural, type = 'HC1'))
development.ruralx <- coeftest(development.rural, vcov = vcovCL(development.rural, type = 'HC1'))
barren.ruralx <- coeftest(barren.rural, vcov = vcovCL(barren.rural, type = 'HC1'))
forests.ruralx <- coeftest(forests.rural, vcov = vcovCL(forests.rural, type = 'HC1'))
shrublands.ruralx <- coeftest(shrublands.rural, vcov = vcovCL(shrublands.rural, type = 'HC1'))
grasslands.ruralx <- coeftest(grasslands.rural, vcov = vcovCL(grasslands.rural, type = 'HC1'))
agriculture.ruralx <- coeftest(agriculture.rural, vcov = vcovCL(agriculture.rural, type = 'HC1'))
wetlands.ruralx <- coeftest(wetlands.rural, vcov = vcovCL(wetlands.rural, type = 'HC1'))

stargazer(development.rural, barren.rural, forests.rural, shrublands.rural, grasslands.rural, agriculture.rural, wetlands.rural,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.ruralx, barren.ruralx, forests.ruralx, shrublands.ruralx, grasslands.ruralx, agriculture.ruralx, wetlands.ruralx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all non-rural counties

water.urban <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Water_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Rural == 0),])

development.urban <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Development_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Rural == 0),])

barren.urban <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Barren_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Rural == 0),])

forests.urban <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + log(Forests_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Rural == 0),])

shrublands.urban <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Shrublands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Rural == 0),])

grasslands.urban <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Grasslands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Rural == 0),])

agriculture.urban <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Agriculture_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Rural == 0),])

wetlands.urban <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Wetlands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Rural == 0),])

water.urbanx <- coeftest(water.urban, vcov = vcovCL(water.urban, type = 'HC1'))
development.urbanx <- coeftest(development.urban, vcov = vcovCL(development.urban, type = 'HC1'))
barren.urbanx <- coeftest(barren.urban, vcov = vcovCL(barren.urban, type = 'HC1'))
forests.urbanx <- coeftest(forests.urban, vcov = vcovCL(forests.urban, type = 'HC1'))
shrublands.urbanx <- coeftest(shrublands.urban, vcov = vcovCL(shrublands.urban, type = 'HC1'))
grasslands.urbanx <- coeftest(grasslands.urban, vcov = vcovCL(grasslands.urban, type = 'HC1'))
agriculture.urbanx <- coeftest(agriculture.urban, vcov = vcovCL(agriculture.urban, type = 'HC1'))
wetlands.urbanx <- coeftest(wetlands.urban, vcov = vcovCL(wetlands.urban, type = 'HC1'))

stargazer(development.urban, barren.urban, forests.urban, shrublands.urban, grasslands.urban, agriculture.urban, wetlands.urban,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.urbanx, barren.urbanx, forests.urbanx, shrublands.urbanx, grasslands.urbanx, agriculture.urbanx, wetlands.urbanx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all large metropolitan counties (UR codes 1-3)

water.large <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Water_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Large == 1),])

development.large <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Development_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Large == 1),])

barren.large <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Barren_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Large == 1),])

forests.large <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + log(Forests_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Large == 1),])

shrublands.large <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Shrublands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Large == 1),])

grasslands.large <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Grasslands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Large == 1),])

agriculture.large <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Agriculture_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Large == 1),])

wetlands.large <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Wetlands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Large == 1),])

water.largex <- coeftest(water.large, vcov = vcovCL(water.large, type = 'HC1'))
development.largex <- coeftest(development.large, vcov = vcovCL(development.large, type = 'HC1'))
barren.largex <- coeftest(barren.large, vcov = vcovCL(barren.large, type = 'HC1'))
forests.largex <- coeftest(forests.large, vcov = vcovCL(forests.large, type = 'HC1'))
shrublands.largex <- coeftest(shrublands.large, vcov = vcovCL(shrublands.large, type = 'HC1'))
grasslands.largex <- coeftest(grasslands.large, vcov = vcovCL(grasslands.large, type = 'HC1'))
agriculture.largex <- coeftest(agriculture.large, vcov = vcovCL(agriculture.large, type = 'HC1'))
wetlands.largex <- coeftest(wetlands.large, vcov = vcovCL(wetlands.large, type = 'HC1'))

stargazer(development.large, barren.large, forests.large, shrublands.large, grasslands.large, agriculture.large, wetlands.large,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.largex, barren.largex, forests.largex, shrublands.largex, grasslands.largex, agriculture.largex, wetlands.largex,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all small metropolitan counties (UR codes 4 and 5)

water.small <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Water_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Small == 1),])

development.small <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Development_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Small == 1),])

barren.small <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Barren_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Small == 1),])

forests.small <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + log(Forests_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Small == 1),])

shrublands.small <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Shrublands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Small == 1),])

grasslands.small <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Grasslands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Small == 1),])

agriculture.small <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Agriculture_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Small == 1),])

wetlands.small <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Wetlands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx[which(dfx$Small == 1),])

water.smallx <- coeftest(water.small, vcov = vcovCL(water.small, type = 'HC1'))
development.smallx <- coeftest(development.small, vcov = vcovCL(development.small, type = 'HC1'))
barren.smallx <- coeftest(barren.small, vcov = vcovCL(barren.small, type = 'HC1'))
forests.smallx <- coeftest(forests.small, vcov = vcovCL(forests.small, type = 'HC1'))
shrublands.smallx <- coeftest(shrublands.small, vcov = vcovCL(shrublands.small, type = 'HC1'))
grasslands.smallx <- coeftest(grasslands.small, vcov = vcovCL(grasslands.small, type = 'HC1'))
agriculture.smallx <- coeftest(agriculture.small, vcov = vcovCL(agriculture.small, type = 'HC1'))
wetlands.smallx <- coeftest(wetlands.small, vcov = vcovCL(wetlands.small, type = 'HC1'))

stargazer(development.small, barren.small, forests.small, shrublands.small, grasslands.small, agriculture.small, wetlands.small,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.smallx, barren.smallx, forests.smallx, shrublands.smallx, grasslands.smallx, agriculture.smallx, wetlands.smallx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Repeat analyses for all counties with initial proportions for all land cover types as a robustness test

water.mod2 <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                    + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                    + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx)

development.mod2 <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                          + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                          + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx)

barren.mod2 <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                     + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                     + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx)

forests.mod2 <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                      + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                      + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx)

shrublands.mod2 <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                         + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                         + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx)

grasslands.mod2 <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                         + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                         + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx)

agriculture.mod2 <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                          + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                          + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx)

wetlands.mod2 <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                       + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                       + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = dfx)

water.mod2x <- coeftest(water.mod2, vcov = vcovCL(water.mod2, type = 'HC1'))
development.mod2x <- coeftest(development.mod2, vcov = vcovCL(development.mod2, type = 'HC1'))
barren.mod2x <- coeftest(barren.mod2, vcov = vcovCL(barren.mod2, type = 'HC1'))
forests.mod2x <- coeftest(forests.mod2, vcov = vcovCL(forests.mod2, type = 'HC1'))
shrublands.mod2x <- coeftest(shrublands.mod2, vcov = vcovCL(shrublands.mod2, type = 'HC1'))
grasslands.mod2x <- coeftest(grasslands.mod2, vcov = vcovCL(grasslands.mod2, type = 'HC1'))
agriculture.mod2x <- coeftest(agriculture.mod2, vcov = vcovCL(agriculture.mod2, type = 'HC1'))
wetlands.mod2x <- coeftest(wetlands.mod2, vcov = vcovCL(wetlands.mod2, type = 'HC1'))

stargazer(development.mod2, barren.mod2, forests.mod2, shrublands.mod2, grasslands.mod2, agriculture.mod2, wetlands.mod2,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.mod2x, barren.mod2x, forests.mod2x, shrublands.mod2x, grasslands.mod2x, agriculture.mod2x, wetlands.mod2x,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Saving results

write.csv(stargazer(development.modx, barren.modx, forests.modx, shrublands.modx, grasslands.modx, agriculture.modx, wetlands.modx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/robust_main.txt'), row.names = FALSE)
write.csv(stargazer(development.ruralx, barren.ruralx, forests.ruralx, shrublands.ruralx, grasslands.ruralx, agriculture.ruralx, wetlands.ruralx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/robust_rural.txt'), row.names = FALSE)
write.csv(stargazer(development.urbanx, barren.urbanx, forests.urbanx, shrublands.urbanx, grasslands.urbanx, agriculture.urbanx, wetlands.urbanx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/robust_non_rural.txt'), row.names = FALSE)
write.csv(stargazer(development.largex, barren.largex, forests.largex, shrublands.largex, grasslands.largex, agriculture.largex, wetlands.largex, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/robust_large_urban.txt'), row.names = FALSE)
write.csv(stargazer(development.smallx, barren.smallx, forests.smallx, shrublands.smallx, grasslands.smallx, agriculture.smallx, wetlands.smallx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/robust_small_urban.txt'), row.names = FALSE)
write.csv(stargazer(development.mod2x, barren.mod2x, forests.mod2x, shrublands.mod2x, grasslands.mod2x, agriculture.mod2x, wetlands.mod2x, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/robust_main_2.txt'), row.names = FALSE)

f.stats.main <- c(59.52, 69.07, 61.64, 69.76, 73.25, 118.80, 70.32)
f.stats.rural <- c(19.71, 23.11, 19.80, 24.55, 26.24, 46.34, 23.64)
f.stats.urban <- c(77.40, 88.93, 85.47, 86.62, 86.24, 100.97, 87.91)
f.stats.large <- c(21.89, 24.52, 22.87, 21.31, 20.18, 23.03, 23.48)
f.stats.small <- c(48.54, 51.15, 44.81, 51.95, 51.31, 64.02, 51.54)
f.stats.main2 <- c(68.62, 68.62, 68.62, 68.62, 68.62, 68.62, 68.62)

nobs.main <- c(3032, 3032, 3032, 3032, 3032, 3032, 3032)
nobs.rural <- c(1276, 1276, 1276, 1276, 1276, 1276, 1276)
nobs.urban <- c(1756, 1756, 1756, 1756, 1756, 1756, 1756)
nobs.large <- c(785, 785, 785, 785, 785, 785, 785)
nobs.small <- c(971, 971, 971, 971, 971, 971, 971)
nobs.main2 <- c(3032, 3032, 3032, 3032, 3032, 3032, 3032)

additional.stats <- as.data.frame(rbind(f.stats.main, f.stats.rural, f.stats.urban, f.stats.large, f.stats.small, f.stats.main2,
                                        nobs.main, nobs.rural, nobs.urban, nobs.large, nobs.small, nobs.main2))

write.csv(additional.stats, paste0(direc, 'results/robust_additional_stats.txt'), row.names = TRUE)

# Ensuring the results are valid by removing ACS outliers (anything beyond 3 sigma)

m1 <- mean(df$Employment_Growth_Rate, na.rm = TRUE)
m2 <- mean(df$Population, na.rm = TRUE)
m3 <- mean(df$Income, na.rm = TRUE)
m4 <- mean(df$Education_BS, na.rm = TRUE)
m5 <- mean(df$Unemployment, na.rm = TRUE)
m6 <- mean(df$New_Residents, na.rm = TRUE)
m7 <- mean(df$Commute_Solo_By_Car, na.rm = TRUE)
m8 <- mean(df$Public_Transit, na.rm = TRUE)
m9 <- mean(df$Housing_Units, na.rm = TRUE)

s1 <- sd(df$Employment_Growth_Rate, na.rm = TRUE)
s2 <- sd(df$Population, na.rm = TRUE)
s3 <- sd(df$Income, na.rm = TRUE)
s4 <- sd(df$Education_BS, na.rm = TRUE)
s5 <- sd(df$Unemployment, na.rm = TRUE)
s6 <- sd(df$New_Residents, na.rm = TRUE)
s7 <- sd(df$Commute_Solo_By_Car, na.rm = TRUE)
s8 <- sd(df$Public_Transit, na.rm = TRUE)
s9 <- sd(df$Housing_Units, na.rm = TRUE)

outliers <- c()

for (i in 1:nrow(df)) {
  
  val <- 0
  
  if (NA %in% c(df$Population[i], df$Income[i])) {
    
    val <- 1
    
  } else {
    
    if (df$Employment_Growth_Rate[i] < (m1 - 3*s1) || df$Employment_Growth_Rate[i] > (m1 + 3*s1)) {
      
      val <- 1
      
    }
    
    if ((df$Population[i] < m2 - 3*s2) || (df$Population[i] > m2 + 3*s2)) {
      
      val <- 1
      
    }
    
    if ((df$Income[i] < m3 - 3*s3) || (df$Income[i] > m3 + 3*s3)) {
      
      val <- 1
      
    }
    
    if ((df$Education_BS[i] < m4 - 3*s4) || (df$Education_BS[i] > m4 + 3*s4)) {
      
      val <- 1
      
    }
    
    if ((df$Unemployment[i] < m5 - 3*s5) || (df$Unemployment[i] > m5 + 3*s5)) {
      
      val <- 1
      
    }
    
    if ((df$New_Residents[i] < m6 - 3*s6) || (df$New_Residents[i] > m6 + 3*s6)) {
      
      val <- 1
      
    }
    
    if ((df$Commute_Solo_By_Car[i] < m7 - 3*s7) || (df$Commute_Solo_By_Car[i] > m7 + 3*s7)) {
      
      val <- 1
      
    }
    
    if ((df$Public_Transit[i] < m8 - 3*s8) || (df$Public_Transit[i] > m8 + 3*s8)) {
      
      val <- 1
      
    }
    
    if ((df$Housing_Units[i] < m9 - 3*s9) || (df$Housing_Units[i] > m9 + 3*s9)) {
      
      val <- 1
      
    }
    
  }
  
  if (val == 1) {
    
    outliers <- c(outliers, i)
    
  }
  
}

df$ID <- 1:nrow(df)
kdf <- df[!(df$ID %in% outliers),]

# Running regressions for all counties

water.mod <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                   + Housing_Units + Water.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

development.mod <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

barren.mod <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                    + Housing_Units + Barren.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

forests.mod <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Forests.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

shrublands.mod <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Shrublands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

grasslands.mod <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Grasslands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

agriculture.mod <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Agriculture.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

wetlands.mod <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

water.modx <- coeftest(water.mod, vcov = vcovCL(water.mod, type = 'HC1'))
development.modx <- coeftest(development.mod, vcov = vcovCL(development.mod, type = 'HC1'))
barren.modx <- coeftest(barren.mod, vcov = vcovCL(barren.mod, type = 'HC1'))
forests.modx <- coeftest(forests.mod, vcov = vcovCL(forests.mod, type = 'HC1'))
shrublands.modx <- coeftest(shrublands.mod, vcov = vcovCL(shrublands.mod, type = 'HC1'))
grasslands.modx <- coeftest(grasslands.mod, vcov = vcovCL(grasslands.mod, type = 'HC1'))
agriculture.modx <- coeftest(agriculture.mod, vcov = vcovCL(agriculture.mod, type = 'HC1'))
wetlands.modx <- coeftest(wetlands.mod, vcov = vcovCL(wetlands.mod, type = 'HC1'))

stargazer(development.mod, barren.mod, forests.mod, shrublands.mod, grasslands.mod, agriculture.mod, wetlands.mod,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.modx, barren.modx, forests.modx, shrublands.modx, grasslands.modx, agriculture.modx, wetlands.modx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for rural counties

water.rural <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Water.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 1),])

development.rural <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Development.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 1),])

barren.rural <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Barren.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 1),])

forests.rural <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Forests.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 1),])

shrublands.rural <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Shrublands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 1),])

grasslands.rural <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Grasslands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 1),])

agriculture.rural <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Agriculture.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 1),])

wetlands.rural <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Wetlands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 1),])

water.ruralx <- coeftest(water.rural, vcov = vcovCL(water.rural, type = 'HC1'))
development.ruralx <- coeftest(development.rural, vcov = vcovCL(development.rural, type = 'HC1'))
barren.ruralx <- coeftest(barren.rural, vcov = vcovCL(barren.rural, type = 'HC1'))
forests.ruralx <- coeftest(forests.rural, vcov = vcovCL(forests.rural, type = 'HC1'))
shrublands.ruralx <- coeftest(shrublands.rural, vcov = vcovCL(shrublands.rural, type = 'HC1'))
grasslands.ruralx <- coeftest(grasslands.rural, vcov = vcovCL(grasslands.rural, type = 'HC1'))
agriculture.ruralx <- coeftest(agriculture.rural, vcov = vcovCL(agriculture.rural, type = 'HC1'))
wetlands.ruralx <- coeftest(wetlands.rural, vcov = vcovCL(wetlands.rural, type = 'HC1'))

stargazer(development.rural, barren.rural, forests.rural, shrublands.rural, grasslands.rural, agriculture.rural, wetlands.rural,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.ruralx, barren.ruralx, forests.ruralx, shrublands.ruralx, grasslands.ruralx, agriculture.ruralx, wetlands.ruralx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all non-rural counties

water.urban <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Water.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 0),])

development.urban <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Development.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 0),])

barren.urban <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Barren.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 0),])

forests.urban <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Forests.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 0),])

shrublands.urban <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Shrublands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 0),])

grasslands.urban <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Grasslands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 0),])

agriculture.urban <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Agriculture.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 0),])

wetlands.urban <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Wetlands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 0),])

water.urbanx <- coeftest(water.urban, vcov = vcovCL(water.urban, type = 'HC1'))
development.urbanx <- coeftest(development.urban, vcov = vcovCL(development.urban, type = 'HC1'))
barren.urbanx <- coeftest(barren.urban, vcov = vcovCL(barren.urban, type = 'HC1'))
forests.urbanx <- coeftest(forests.urban, vcov = vcovCL(forests.urban, type = 'HC1'))
shrublands.urbanx <- coeftest(shrublands.urban, vcov = vcovCL(shrublands.urban, type = 'HC1'))
grasslands.urbanx <- coeftest(grasslands.urban, vcov = vcovCL(grasslands.urban, type = 'HC1'))
agriculture.urbanx <- coeftest(agriculture.urban, vcov = vcovCL(agriculture.urban, type = 'HC1'))
wetlands.urbanx <- coeftest(wetlands.urban, vcov = vcovCL(wetlands.urban, type = 'HC1'))

stargazer(development.urban, barren.urban, forests.urban, shrublands.urban, grasslands.urban, agriculture.urban, wetlands.urban,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.urbanx, barren.urbanx, forests.urbanx, shrublands.urbanx, grasslands.urbanx, agriculture.urbanx, wetlands.urbanx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all large metropolitan counties (UR codes 1-3)

water.large <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Water.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Large == 1),])

development.large <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Development.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Large == 1),])

barren.large <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Barren.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Large == 1),])

forests.large <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Forests.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Large == 1),])

shrublands.large <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Shrublands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Large == 1),])

grasslands.large <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Grasslands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Large == 1),])

agriculture.large <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Agriculture.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Large == 1),])

wetlands.large <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Wetlands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Large == 1),])

water.largex <- coeftest(water.large, vcov = vcovCL(water.large, type = 'HC1'))
development.largex <- coeftest(development.large, vcov = vcovCL(development.large, type = 'HC1'))
barren.largex <- coeftest(barren.large, vcov = vcovCL(barren.large, type = 'HC1'))
forests.largex <- coeftest(forests.large, vcov = vcovCL(forests.large, type = 'HC1'))
shrublands.largex <- coeftest(shrublands.large, vcov = vcovCL(shrublands.large, type = 'HC1'))
grasslands.largex <- coeftest(grasslands.large, vcov = vcovCL(grasslands.large, type = 'HC1'))
agriculture.largex <- coeftest(agriculture.large, vcov = vcovCL(agriculture.large, type = 'HC1'))
wetlands.largex <- coeftest(wetlands.large, vcov = vcovCL(wetlands.large, type = 'HC1'))

stargazer(development.large, barren.large, forests.large, shrublands.large, grasslands.large, agriculture.large, wetlands.large,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.largex, barren.largex, forests.largex, shrublands.largex, grasslands.largex, agriculture.largex, wetlands.largex,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all small metropolitan counties (UR codes 4 and 5)

water.small <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Water.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Small == 1),])

development.small <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Development.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Small == 1),])

barren.small <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Barren.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Small == 1),])

forests.small <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Forests.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Small == 1),])

shrublands.small <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Shrublands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Small == 1),])

grasslands.small <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Grasslands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Small == 1),])

agriculture.small <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Agriculture.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Small == 1),])

wetlands.small <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Wetlands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Small == 1),])

water.smallx <- coeftest(water.small, vcov = vcovCL(water.small, type = 'HC1'))
development.smallx <- coeftest(development.small, vcov = vcovCL(development.small, type = 'HC1'))
barren.smallx <- coeftest(barren.small, vcov = vcovCL(barren.small, type = 'HC1'))
forests.smallx <- coeftest(forests.small, vcov = vcovCL(forests.small, type = 'HC1'))
shrublands.smallx <- coeftest(shrublands.small, vcov = vcovCL(shrublands.small, type = 'HC1'))
grasslands.smallx <- coeftest(grasslands.small, vcov = vcovCL(grasslands.small, type = 'HC1'))
agriculture.smallx <- coeftest(agriculture.small, vcov = vcovCL(agriculture.small, type = 'HC1'))
wetlands.smallx <- coeftest(wetlands.small, vcov = vcovCL(wetlands.small, type = 'HC1'))

stargazer(development.small, barren.small, forests.small, shrublands.small, grasslands.small, agriculture.small, wetlands.small,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.smallx, barren.smallx, forests.smallx, shrublands.smallx, grasslands.smallx, agriculture.smallx, wetlands.smallx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Repeat analyses for all counties with initial proportions for all land cover types as a robustness test

water.mod2 <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + Water.2011
                    + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                    + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

development.mod2 <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + Water.2011
                          + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

barren.mod2 <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + Water.2011
                     + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                     + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

forests.mod2 <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + Water.2011
                      + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                      + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

shrublands.mod2 <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + Water.2011
                         + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

grasslands.mod2 <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + Water.2011
                         + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

agriculture.mod2 <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + Water.2011
                          + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

wetlands.mod2 <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + Water.2011
                       + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                       + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

water.mod2x <- coeftest(water.mod2, vcov = vcovCL(water.mod2, type = 'HC1'))
development.mod2x <- coeftest(development.mod2, vcov = vcovCL(development.mod2, type = 'HC1'))
barren.mod2x <- coeftest(barren.mod2, vcov = vcovCL(barren.mod2, type = 'HC1'))
forests.mod2x <- coeftest(forests.mod2, vcov = vcovCL(forests.mod2, type = 'HC1'))
shrublands.mod2x <- coeftest(shrublands.mod2, vcov = vcovCL(shrublands.mod2, type = 'HC1'))
grasslands.mod2x <- coeftest(grasslands.mod2, vcov = vcovCL(grasslands.mod2, type = 'HC1'))
agriculture.mod2x <- coeftest(agriculture.mod2, vcov = vcovCL(agriculture.mod2, type = 'HC1'))
wetlands.mod2x <- coeftest(wetlands.mod2, vcov = vcovCL(wetlands.mod2, type = 'HC1'))

stargazer(development.mod2, barren.mod2, forests.mod2, shrublands.mod2, grasslands.mod2, agriculture.mod2, wetlands.mod2,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.mod2x, barren.mod2x, forests.mod2x, shrublands.mod2x, grasslands.mod2x, agriculture.mod2x, wetlands.mod2x,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Saving results

write.csv(stargazer(development.modx, barren.modx, forests.modx, shrublands.modx, grasslands.modx, agriculture.modx, wetlands.modx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/all_gone_main.txt'), row.names = FALSE)
write.csv(stargazer(development.ruralx, barren.ruralx, forests.ruralx, shrublands.ruralx, grasslands.ruralx, agriculture.ruralx, wetlands.ruralx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/all_gone_rural.txt'), row.names = FALSE)
write.csv(stargazer(development.urbanx, barren.urbanx, forests.urbanx, shrublands.urbanx, grasslands.urbanx, agriculture.urbanx, wetlands.urbanx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/all_gone_non_rural.txt'), row.names = FALSE)
write.csv(stargazer(development.largex, barren.largex, forests.largex, shrublands.largex, grasslands.largex, agriculture.largex, wetlands.largex, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/all_gone_large_urban.txt'), row.names = FALSE)
write.csv(stargazer(development.smallx, barren.smallx, forests.smallx, shrublands.smallx, grasslands.smallx, agriculture.smallx, wetlands.smallx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/all_gone_small_urban.txt'), row.names = FALSE)
write.csv(stargazer(development.mod2x, barren.mod2x, forests.mod2x, shrublands.mod2x, grasslands.mod2x, agriculture.mod2x, wetlands.mod2x, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/all_gone_main_2.txt'), row.names = FALSE)

f.stats.main <- c(67.27, 84.92, 86.14, 79.70, 71.95, 120.80, 87.98)
f.stats.rural <- c(42.04, 42.84, 40.09, 38.62, 33.39, 57.70, 42.59)
f.stats.urban <- c(7.14, 26.05, 28.03, 24.28, 25.04, 34.99, 29.40)
f.stats.large <- c(2.19, 21.07, 21.91, 23.67, 21.40, 18.35, 22.56)
f.stats.small <- c(3.85, 8.61, 8.76, 7.51, 8.21, 16.35, 11.25)
f.stats.main2 <- c(73.64, 73.64, 73.64, 73.64, 73.64, 73.64, 73.64)

nobs.main <- c(2892, 2892, 2892, 2892, 2892, 2892, 2892)
nobs.rural <- c(1180, 1180, 1180, 1180, 1180, 1180, 1180)
nobs.urban <- c(1712, 1712, 1712, 1712, 1712, 1712, 1712)
nobs.large <- c(764, 764, 764, 764, 764, 764, 764)
nobs.small <- c(948, 948, 948, 948, 948, 948, 948)
nobs.main2 <- c(2892, 2892, 2892, 2892, 2892, 2892, 2892)

additional.stats <- as.data.frame(rbind(f.stats.main, f.stats.rural, f.stats.urban, f.stats.large, f.stats.small, f.stats.main2,
                                        nobs.main, nobs.rural, nobs.urban, nobs.large, nobs.small, nobs.main2))

write.csv(additional.stats, paste0(direc, 'results/all_gone_additional_stats.txt'), row.names = TRUE)

# Running regressions for all counties - controlling with areas instead of percentages

water.mod <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                   + Housing_Units + log(Water_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

development.mod <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + log(Development_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

barren.mod <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                    + Housing_Units + log(Barren_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

forests.mod <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Forests_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

shrublands.mod <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Shrublands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

grasslands.mod <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Grasslands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

agriculture.mod <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + log(Agriculture_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

wetlands.mod <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

water.modx <- coeftest(water.mod, vcov = vcovCL(water.mod, type = 'HC1'))
development.modx <- coeftest(development.mod, vcov = vcovCL(development.mod, type = 'HC1'))
barren.modx <- coeftest(barren.mod, vcov = vcovCL(barren.mod, type = 'HC1'))
forests.modx <- coeftest(forests.mod, vcov = vcovCL(forests.mod, type = 'HC1'))
shrublands.modx <- coeftest(shrublands.mod, vcov = vcovCL(shrublands.mod, type = 'HC1'))
grasslands.modx <- coeftest(grasslands.mod, vcov = vcovCL(grasslands.mod, type = 'HC1'))
agriculture.modx <- coeftest(agriculture.mod, vcov = vcovCL(agriculture.mod, type = 'HC1'))
wetlands.modx <- coeftest(wetlands.mod, vcov = vcovCL(wetlands.mod, type = 'HC1'))

stargazer(development.mod, barren.mod, forests.mod, shrublands.mod, grasslands.mod, agriculture.mod, wetlands.mod,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.modx, barren.modx, forests.modx, shrublands.modx, grasslands.modx, agriculture.modx, wetlands.modx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for rural counties

water.rural <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Water_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 1),])

development.rural <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Development_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 1),])

barren.rural <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Barren_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 1),])

forests.rural <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + log(Forests_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 1),])

shrublands.rural <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Shrublands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 1),])

grasslands.rural <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Grasslands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 1),])

agriculture.rural <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Agriculture_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 1),])

wetlands.rural <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Wetlands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 1),])

water.ruralx <- coeftest(water.rural, vcov = vcovCL(water.rural, type = 'HC1'))
development.ruralx <- coeftest(development.rural, vcov = vcovCL(development.rural, type = 'HC1'))
barren.ruralx <- coeftest(barren.rural, vcov = vcovCL(barren.rural, type = 'HC1'))
forests.ruralx <- coeftest(forests.rural, vcov = vcovCL(forests.rural, type = 'HC1'))
shrublands.ruralx <- coeftest(shrublands.rural, vcov = vcovCL(shrublands.rural, type = 'HC1'))
grasslands.ruralx <- coeftest(grasslands.rural, vcov = vcovCL(grasslands.rural, type = 'HC1'))
agriculture.ruralx <- coeftest(agriculture.rural, vcov = vcovCL(agriculture.rural, type = 'HC1'))
wetlands.ruralx <- coeftest(wetlands.rural, vcov = vcovCL(wetlands.rural, type = 'HC1'))

stargazer(development.rural, barren.rural, forests.rural, shrublands.rural, grasslands.rural, agriculture.rural, wetlands.rural,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.ruralx, barren.ruralx, forests.ruralx, shrublands.ruralx, grasslands.ruralx, agriculture.ruralx, wetlands.ruralx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all non-rural counties

water.urban <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Water_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 0),])

development.urban <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Development_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 0),])

barren.urban <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Barren_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 0),])

forests.urban <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + log(Forests_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 0),])

shrublands.urban <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Shrublands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 0),])

grasslands.urban <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Grasslands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 0),])

agriculture.urban <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Agriculture_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 0),])

wetlands.urban <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Wetlands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Rural == 0),])

water.urbanx <- coeftest(water.urban, vcov = vcovCL(water.urban, type = 'HC1'))
development.urbanx <- coeftest(development.urban, vcov = vcovCL(development.urban, type = 'HC1'))
barren.urbanx <- coeftest(barren.urban, vcov = vcovCL(barren.urban, type = 'HC1'))
forests.urbanx <- coeftest(forests.urban, vcov = vcovCL(forests.urban, type = 'HC1'))
shrublands.urbanx <- coeftest(shrublands.urban, vcov = vcovCL(shrublands.urban, type = 'HC1'))
grasslands.urbanx <- coeftest(grasslands.urban, vcov = vcovCL(grasslands.urban, type = 'HC1'))
agriculture.urbanx <- coeftest(agriculture.urban, vcov = vcovCL(agriculture.urban, type = 'HC1'))
wetlands.urbanx <- coeftest(wetlands.urban, vcov = vcovCL(wetlands.urban, type = 'HC1'))

stargazer(development.urban, barren.urban, forests.urban, shrublands.urban, grasslands.urban, agriculture.urban, wetlands.urban,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.urbanx, barren.urbanx, forests.urbanx, shrublands.urbanx, grasslands.urbanx, agriculture.urbanx, wetlands.urbanx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all large metropolitan counties (UR codes 1-3)

water.large <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Water_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Large == 1),])

development.large <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Development_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Large == 1),])

barren.large <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Barren_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Large == 1),])

forests.large <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + log(Forests_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Large == 1),])

shrublands.large <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Shrublands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Large == 1),])

grasslands.large <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Grasslands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Large == 1),])

agriculture.large <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Agriculture_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Large == 1),])

wetlands.large <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Wetlands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Large == 1),])

water.largex <- coeftest(water.large, vcov = vcovCL(water.large, type = 'HC1'))
development.largex <- coeftest(development.large, vcov = vcovCL(development.large, type = 'HC1'))
barren.largex <- coeftest(barren.large, vcov = vcovCL(barren.large, type = 'HC1'))
forests.largex <- coeftest(forests.large, vcov = vcovCL(forests.large, type = 'HC1'))
shrublands.largex <- coeftest(shrublands.large, vcov = vcovCL(shrublands.large, type = 'HC1'))
grasslands.largex <- coeftest(grasslands.large, vcov = vcovCL(grasslands.large, type = 'HC1'))
agriculture.largex <- coeftest(agriculture.large, vcov = vcovCL(agriculture.large, type = 'HC1'))
wetlands.largex <- coeftest(wetlands.large, vcov = vcovCL(wetlands.large, type = 'HC1'))

stargazer(development.large, barren.large, forests.large, shrublands.large, grasslands.large, agriculture.large, wetlands.large,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.largex, barren.largex, forests.largex, shrublands.largex, grasslands.largex, agriculture.largex, wetlands.largex,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all small metropolitan counties (UR codes 4 and 5)

water.small <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Water_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Small == 1),])

development.small <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Development_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Small == 1),])

barren.small <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Barren_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Small == 1),])

forests.small <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + log(Forests_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Small == 1),])

shrublands.small <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Shrublands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Small == 1),])

grasslands.small <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + log(Grasslands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Small == 1),])

agriculture.small <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + log(Agriculture_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Small == 1),])

wetlands.small <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + log(Wetlands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf[which(kdf$Small == 1),])

water.smallx <- coeftest(water.small, vcov = vcovCL(water.small, type = 'HC1'))
development.smallx <- coeftest(development.small, vcov = vcovCL(development.small, type = 'HC1'))
barren.smallx <- coeftest(barren.small, vcov = vcovCL(barren.small, type = 'HC1'))
forests.smallx <- coeftest(forests.small, vcov = vcovCL(forests.small, type = 'HC1'))
shrublands.smallx <- coeftest(shrublands.small, vcov = vcovCL(shrublands.small, type = 'HC1'))
grasslands.smallx <- coeftest(grasslands.small, vcov = vcovCL(grasslands.small, type = 'HC1'))
agriculture.smallx <- coeftest(agriculture.small, vcov = vcovCL(agriculture.small, type = 'HC1'))
wetlands.smallx <- coeftest(wetlands.small, vcov = vcovCL(wetlands.small, type = 'HC1'))

stargazer(development.small, barren.small, forests.small, shrublands.small, grasslands.small, agriculture.small, wetlands.small,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.smallx, barren.smallx, forests.smallx, shrublands.smallx, grasslands.smallx, agriculture.smallx, wetlands.smallx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Repeat analyses for all counties with initial proportions for all land cover types as a robustness test

water.mod2 <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                    + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                    + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

development.mod2 <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                          + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                          + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

barren.mod2 <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                     + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                     + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

forests.mod2 <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                      + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                      + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

shrublands.mod2 <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                         + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                         + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

grasslands.mod2 <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                         + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                         + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

agriculture.mod2 <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                          + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                          + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

wetlands.mod2 <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents + Housing_Units + log(Water_Area + 1)
                       + log(Development_Area + 1) + log(Barren_Area + 1) + log(Forests_Area + 1) + log(Shrublands_Area + 1) + log(Grasslands_Area + 1) + log(Agriculture_Area + 1)
                       + log(Wetlands_Area + 1) + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = kdf)

water.mod2x <- coeftest(water.mod2, vcov = vcovCL(water.mod2, type = 'HC1'))
development.mod2x <- coeftest(development.mod2, vcov = vcovCL(development.mod2, type = 'HC1'))
barren.mod2x <- coeftest(barren.mod2, vcov = vcovCL(barren.mod2, type = 'HC1'))
forests.mod2x <- coeftest(forests.mod2, vcov = vcovCL(forests.mod2, type = 'HC1'))
shrublands.mod2x <- coeftest(shrublands.mod2, vcov = vcovCL(shrublands.mod2, type = 'HC1'))
grasslands.mod2x <- coeftest(grasslands.mod2, vcov = vcovCL(grasslands.mod2, type = 'HC1'))
agriculture.mod2x <- coeftest(agriculture.mod2, vcov = vcovCL(agriculture.mod2, type = 'HC1'))
wetlands.mod2x <- coeftest(wetlands.mod2, vcov = vcovCL(wetlands.mod2, type = 'HC1'))

stargazer(development.mod2, barren.mod2, forests.mod2, shrublands.mod2, grasslands.mod2, agriculture.mod2, wetlands.mod2,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development.mod2x, barren.mod2x, forests.mod2x, shrublands.mod2x, grasslands.mod2x, agriculture.mod2x, wetlands.mod2x,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Saving results

write.csv(stargazer(development.modx, barren.modx, forests.modx, shrublands.modx, grasslands.modx, agriculture.modx, wetlands.modx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/all_gone__area_main.txt'), row.names = FALSE)
write.csv(stargazer(development.ruralx, barren.ruralx, forests.ruralx, shrublands.ruralx, grasslands.ruralx, agriculture.ruralx, wetlands.ruralx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/all_gone__area_rural.txt'), row.names = FALSE)
write.csv(stargazer(development.urbanx, barren.urbanx, forests.urbanx, shrublands.urbanx, grasslands.urbanx, agriculture.urbanx, wetlands.urbanx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/all_gone__area_non_rural.txt'), row.names = FALSE)
write.csv(stargazer(development.largex, barren.largex, forests.largex, shrublands.largex, grasslands.largex, agriculture.largex, wetlands.largex, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/all_gone__area_large_urban.txt'), row.names = FALSE)
write.csv(stargazer(development.smallx, barren.smallx, forests.smallx, shrublands.smallx, grasslands.smallx, agriculture.smallx, wetlands.smallx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/all_gone__area_small_urban.txt'), row.names = FALSE)
write.csv(stargazer(development.mod2x, barren.mod2x, forests.mod2x, shrublands.mod2x, grasslands.mod2x, agriculture.mod2x, wetlands.mod2x, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/all_gone__area_main_2.txt'), row.names = FALSE)

f.stats.main <- c(74.77, 82.20, 74.04, 86.01, 81.52, 111.80, 84.58)
f.stats.rural <- c(37.58, 36.53, 27.07, 42.83, 42.21, 56.03, 41.25)
f.stats.urban <- c(19.33, 26.55, 27.20, 25.47, 22.24, 28.62, 25.06)
f.stats.large <- c(18.77, 21.85, 20.40, 19.34, 17.84, 15.87, 20.59)
f.stats.small <- c(8.51, 8.70, 7.79, 9.38, 8.08, 13.91, 8.61)
f.stats.main2 <- c(52.02, 52.02, 52.02, 52.02, 52.02, 52.02, 52.02)

nobs.main <- c(2892, 2892, 2892, 2892, 2892, 2892, 2892)
nobs.rural <- c(1180, 1180, 1180, 1180, 1180, 1180, 1180)
nobs.urban <- c(1712, 1712, 1712, 1712, 1712, 1712, 1712)
nobs.large <- c(764, 764, 764, 764, 764, 764, 764)
nobs.small <- c(948, 948, 948, 948, 948, 948, 948)
nobs.main2 <- c(2892, 2892, 2892, 2892, 2892, 2892, 2892)

additional.stats <- as.data.frame(rbind(f.stats.main, f.stats.rural, f.stats.urban, f.stats.large, f.stats.small, f.stats.main2,
                                        nobs.main, nobs.rural, nobs.urban, nobs.large, nobs.small, nobs.main2))

write.csv(additional.stats, paste0(direc, 'results/all_gone__area_additional_stats.txt'), row.names = TRUE)

# Income-based analysis

# Split the data into income halves

flerp <- data %>% filter(Year == 2021)
flerp <- flerp[order(flerp$Income),]
flerp$Science <- 1:nrow(flerp)

squinp <- c()

for (i in 1:nrow(df)) {
  
  squinp <- c(squinp, ceiling(flerp[which(flerp$County == df$County[i]),]$Science[1] / 1554))
  
}

df$Quart <- squinp

# Running regressions for lower income counties

water1 <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                + Housing_Units + Water.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 1),])

development1 <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Development.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 1),])

barren1 <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                 + Housing_Units + Barren.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 1),])

forests1 <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                  + Housing_Units + Forests.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 1),])

shrublands1 <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Shrublands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 1),])

grasslands1 <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Grasslands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 1),])

agriculture1 <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Agriculture.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 1),])

wetlands1 <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                   + Housing_Units + Wetlands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 1),])

water1x <- coeftest(water1, vcov = vcovCL(water1, type = 'HC1'))
development1x <- coeftest(development1, vcov = vcovCL(development1, type = 'HC1'))
barren1x <- coeftest(barren1, vcov = vcovCL(barren1, type = 'HC1'))
forests1x <- coeftest(forests1, vcov = vcovCL(forests1, type = 'HC1'))
shrublands1x <- coeftest(shrublands1, vcov = vcovCL(shrublands1, type = 'HC1'))
grasslands1x <- coeftest(grasslands1, vcov = vcovCL(grasslands1, type = 'HC1'))
agriculture1x <- coeftest(agriculture1, vcov = vcovCL(agriculture1, type = 'HC1'))
wetlands1x <- coeftest(wetlands1, vcov = vcovCL(wetlands1, type = 'HC1'))

stargazer(development1, barren1, forests1, shrublands1, grasslands1, agriculture1, wetlands1,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development1x, barren1x, forests1x, shrublands1x, grasslands1x, agriculture1x, wetlands1x,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for wealthier half of counties

water2 <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                + Housing_Units + Water.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 2),])

development2 <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Development.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 2),])

barren2 <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                 + Housing_Units + Barren.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 2),])

forests2 <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                  + Housing_Units + Forests.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 2),])

shrublands2 <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Shrublands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 2),])

grasslands2 <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Grasslands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 2),])

agriculture2 <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Agriculture.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 2),])

wetlands2 <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                   + Housing_Units + Wetlands.2011 + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 2),])

water2x <- coeftest(water2, vcov = vcovCL(water2, type = 'HC1'))
development2x <- coeftest(development2, vcov = vcovCL(development2, type = 'HC1'))
barren2x <- coeftest(barren2, vcov = vcovCL(barren2, type = 'HC1'))
forests2x <- coeftest(forests2, vcov = vcovCL(forests2, type = 'HC1'))
shrublands2x <- coeftest(shrublands2, vcov = vcovCL(shrublands2, type = 'HC1'))
grasslands2x <- coeftest(grasslands2, vcov = vcovCL(grasslands2, type = 'HC1'))
agriculture2x <- coeftest(agriculture2, vcov = vcovCL(agriculture2, type = 'HC1'))
wetlands2x <- coeftest(wetlands2, vcov = vcovCL(wetlands2, type = 'HC1'))

stargazer(development2, barren2, forests2, shrublands2, grasslands2, agriculture2, wetlands2,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development2x, barren2x, forests2x, shrublands2x, grasslands2x, agriculture2x, wetlands2x,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Saving results

write.csv(stargazer(development1x, barren1x, forests1x, shrublands1x, grasslands1x, agriculture1x, wetlands1x, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/income1.txt'), row.names = FALSE)
write.csv(stargazer(development2x, barren2x, forests2x, shrublands2x, grasslands2x, agriculture2x, wetlands2x, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/income2.txt'), row.names = FALSE)

f.stats1 <- c(27.96, 32.38, 33.28, 29.17, 30.19, 44.70, 32.88)
f.stats2 <- c(61.15, 83.62, 79.54, 61.35, 78.47, 99.02, 80.98)

nobs1 <- c(1553, 1553, 1553, 1553, 1553, 1553, 1553)
nobs2 <- c(1553, 1553, 1553, 1553, 1553, 1553, 1553)

additional.stats <- as.data.frame(rbind(f.stats1, f.stats2, nobs1, nobs2))

write.csv(additional.stats, paste0(direc, 'results/additional_stats_income.txt'), row.names = TRUE)

# Repeating the income analysis controlling with areas instead of percentages

# Running regressions for lower income counties

water1 <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                + Housing_Units + log(Water_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 1),])

development1 <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Development_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 1),])

barren1 <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                 + Housing_Units + log(Barren_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 1),])

forests1 <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                  + Housing_Units + log(Forests_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 1),])

shrublands1 <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Shrublands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 1),])

grasslands1 <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Grasslands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 1),])

agriculture1 <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Agriculture_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 1),])

wetlands1 <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                   + Housing_Units + log(Wetlands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 1),])

water1x <- coeftest(water1, vcov = vcovCL(water1, type = 'HC1'))
development1x <- coeftest(development1, vcov = vcovCL(development1, type = 'HC1'))
barren1x <- coeftest(barren1, vcov = vcovCL(barren1, type = 'HC1'))
forests1x <- coeftest(forests1, vcov = vcovCL(forests1, type = 'HC1'))
shrublands1x <- coeftest(shrublands1, vcov = vcovCL(shrublands1, type = 'HC1'))
grasslands1x <- coeftest(grasslands1, vcov = vcovCL(grasslands1, type = 'HC1'))
agriculture1x <- coeftest(agriculture1, vcov = vcovCL(agriculture1, type = 'HC1'))
wetlands1x <- coeftest(wetlands1, vcov = vcovCL(wetlands1, type = 'HC1'))

stargazer(development1, barren1, forests1, shrublands1, grasslands1, agriculture1, wetlands1,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development1x, barren1x, forests1x, shrublands1x, grasslands1x, agriculture1x, wetlands1x,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for wealthier half of counties

water2 <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                + Housing_Units + log(Water_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 2),])

development2 <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Development_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 2),])

barren2 <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                 + Housing_Units + log(Barren_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 2),])

forests2 <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                  + Housing_Units + log(Forests_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 2),])

shrublands2 <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Shrublands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 2),])

grasslands2 <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + log(Grasslands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 2),])

agriculture2 <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + log(Agriculture_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 2),])

wetlands2 <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                   + Housing_Units + log(Wetlands_Area + 1) + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Quart == 2),])

water2x <- coeftest(water2, vcov = vcovCL(water2, type = 'HC1'))
development2x <- coeftest(development2, vcov = vcovCL(development2, type = 'HC1'))
barren2x <- coeftest(barren2, vcov = vcovCL(barren2, type = 'HC1'))
forests2x <- coeftest(forests2, vcov = vcovCL(forests2, type = 'HC1'))
shrublands2x <- coeftest(shrublands2, vcov = vcovCL(shrublands2, type = 'HC1'))
grasslands2x <- coeftest(grasslands2, vcov = vcovCL(grasslands2, type = 'HC1'))
agriculture2x <- coeftest(agriculture2, vcov = vcovCL(agriculture2, type = 'HC1'))
wetlands2x <- coeftest(wetlands2, vcov = vcovCL(wetlands2, type = 'HC1'))

stargazer(development2, barren2, forests2, shrublands2, grasslands2, agriculture2, wetlands2,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(development2x, barren2x, forests2x, shrublands2x, grasslands2x, agriculture2x, wetlands2x,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Saving results

write.csv(stargazer(development1x, barren1x, forests1x, shrublands1x, grasslands1x, agriculture1x, wetlands1x, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/income1.txt'), row.names = FALSE)
write.csv(stargazer(development2x, barren2x, forests2x, shrublands2x, grasslands2x, agriculture2x, wetlands2x, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/income2.txt'), row.names = FALSE)

f.stats1 <- c(18.30, 30.90, 30.60, 32.53, 34.79, 57.74, 32.67)
f.stats2 <- c(66.64, 81.15, 77.38, 74.66, 75.76, 97.13, 80.19)

nobs1 <- c(1553, 1553, 1553, 1553, 1553, 1553, 1553)
nobs2 <- c(1553, 1553, 1553, 1553, 1553, 1553, 1553)

additional.stats <- as.data.frame(rbind(f.stats1, f.stats2, nobs1, nobs2))

write.csv(additional.stats, paste0(direc, 'results/area_additional_stats_income.txt'), row.names = TRUE)

# Additional figures - plots of wetland percentage and change in wetland percentage by county

# Getting county shapefiles for US

shp <- as.data.frame(NULL)

for (s in states) {
  
  print(paste0('Collecting all county shapefiles for state FIPS ', s, '.......'))
  
  ship <- counties(state = s, year = 2021)
  shp <- rbind(shp, ship)
  
}

# Add the wetlands and wetlands change data

w <- c()
w.c <- c()

for (i in 1:nrow(shp)) {
  
  print(paste0('Adding wetlands data for county ', i, ' of 3,108.......'))
  
  tmp <- df %>% filter(County == shp$GEOID[i])
  tmp2 <- data %>% filter(County == shp$GEOID[i]) %>% filter(Year == 2021)
  
  w <- c(w, tmp2$Wetlands[1])
  w.c <- c(w.c, tmp$Wetlands[1])
  
}

shp$Wetlands <- w
shp$Wetlands.Change <- w.c

q <- quantile(shp$Wetlands.Change, probs = c(0, 0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
wc <- c()

for (i in 1:nrow(shp)) {
  
  wc <- c(wc, max(1,max(which(q < shp$Wetlands.Change[i]))))
  
}

shp$WC <- wc

# Creating leaflets

pal1 <- colorNumeric(palette = c('white', 'blue'), domain = shp$Wetlands)
pal2 <- colorNumeric(palette = c('red', 'pink', 'white', 'lightblue', 'blue'), domain = shp$WC)

wet.map <- leaflet(shp$geometry) %>% addTiles() %>% addPolygons(weight = 1.0, smoothFactor = 1.0, opacity = 1.0, fillOpacity = 1.0, color = 'black', fillColor = pal1(shp$Wetlands))
change.map <- leaflet(shp$geometry) %>% addTiles() %>% addPolygons(weight = 1.0, smoothFactor = 1.0, opacity = 1.0, fillOpacity = 1.0, color = 'black', fillColor = pal2(shp$WC))

wet.map
change.map

# Next up is ag change

# Add the ag and ag change data

a <- c()
a.c <- c()

for (i in 1:nrow(shp)) {
  
  print(paste0('Adding agriculture data for county ', i, ' of 3,108.......'))
  
  tmp <- df %>% filter(County == shp$GEOID[i])
  tmp2 <- data %>% filter(County == shp$GEOID[i]) %>% filter(Year == 2021)
  
  a <- c(a, tmp2$Agriculture[1])
  a.c <- c(a.c, tmp$Agriculture[1])
  
}

shp$Ag <- a
shp$Ag.Change <- a.c

q <- quantile(shp$Ag.Change, probs = c(0, 0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
ac <- c()

for (i in 1:nrow(shp)) {
  
  ac <- c(ac, max(1,max(which(q < shp$Ag.Change[i]))))
  
}

shp$AC <- ac

# Creating leaflets

pal1 <- colorNumeric(palette = c('white', 'blue'), domain = shp$Ag)
pal2 <- colorNumeric(palette = c('red', 'pink', 'white', 'lightblue', 'blue'), domain = shp$AC)

ag.map <- leaflet(shp$geometry) %>% addTiles() %>% addPolygons(weight = 1.0, smoothFactor = 1.0, opacity = 1.0, fillOpacity = 1.0, color = 'black', fillColor = pal1(shp$Ag))
ag.change.map <- leaflet(shp$geometry) %>% addTiles() %>% addPolygons(weight = 1.0, smoothFactor = 1.0, opacity = 1.0, fillOpacity = 1.0, color = 'black', fillColor = pal2(shp$AC))

ag.map
ag.change.map

# Next up is developed land change

# Add the dev and dev change data

d <- c()
d.c <- c()

for (i in 1:nrow(shp)) {
  
  print(paste0('Adding developed land data for county ', i, ' of 3,108.......'))
  
  tmp <- df %>% filter(County == shp$GEOID[i])
  tmp2 <- data %>% filter(County == shp$GEOID[i]) %>% filter(Year == 2021)
  
  d <- c(d, tmp2$Development[1])
  d.c <- c(d.c, tmp$Development[1])
  
}

shp$Dev <- d
shp$Dev.Change <- d.c

q <- quantile(shp$Dev.Change, probs = c(0, 0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
dc <- c()

for (i in 1:nrow(shp)) {
  
  dc <- c(dc, max(1,max(which(q < shp$Dev.Change[i]))))
  
}

shp$DC <- dc

# Creating leaflets

pal1 <- colorNumeric(palette = c('white', 'blue'), domain = shp$Dev)
pal2 <- colorNumeric(palette = c('red', 'pink', 'white', 'lightblue', 'blue'), domain = shp$DC)

dev.map <- leaflet(shp$geometry) %>% addTiles() %>% addPolygons(weight = 1.0, smoothFactor = 1.0, opacity = 1.0, fillOpacity = 1.0, color = 'black', fillColor = pal1(shp$Dev))
dev.change.map <- leaflet(shp$geometry) %>% addTiles() %>% addPolygons(weight = 1.0, smoothFactor = 1.0, opacity = 1.0, fillOpacity = 1.0, color = 'black', fillColor = pal2(shp$DC))

dev.map
dev.change.map

# Additional figures - plots of jobs and job growth by county

# Add the jobs and jobs change data

j <- c()
j.c <- c()

for (i in 1:nrow(shp)) {
  
  print(paste0('Adding jobs data for county ', i, ' of 3,108.......'))
  
  tmp <- df %>% filter(County == shp$GEOID[i])
  tmp2 <- data %>% filter(County == shp$GEOID[i]) %>% filter(Year == 2021)
  
  j <- c(j, tmp2$Jobs[1])
  j.c <- c(j.c, tmp$Jobs[1])
  
}

shp$Jobs <- log(j+1)
shp$Jobs.Change <- j.c

q <- quantile(shp$Jobs.Change, probs = c(0, 0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
jc <- c()

for (i in 1:nrow(shp)) {
  
  jc <- c(jc, max(1,max(which(q < shp$Jobs.Change[i]))))
  
}

shp$JC <- jc

# Creating leaflets

pal1 <- colorNumeric(palette = c('white', 'blue'), domain = shp$Jobs^2)
pal2 <- colorNumeric(palette = c('red', 'pink', 'white', 'lightblue', 'blue'), domain = shp$JC)

job.map <- leaflet(shp$geometry) %>% addTiles() %>% addPolygons(weight = 1.0, smoothFactor = 1.0, opacity = 1.0, fillOpacity = 1.0, color = 'black', fillColor = pal1(shp$Jobs^2))
jc.map <- leaflet(shp$geometry) %>% addTiles() %>% addPolygons(weight = 1.0, smoothFactor = 1.0, opacity = 1.0, fillOpacity = 1.0, color = 'black', fillColor = pal2(shp$JC))

job.map
jc.map

# Ok, fuck it, lets do a leaflet with Bartik residuals for shit gigglin'

bar.tick <- lm(Employment_Growth_Rate ~ Bartik, data = df)
shp$bartik <- c(abs(bar.tick$residuals[1:3107]), NA)
shp$bartik2 <- c(bar.tick$fitted.values[1:3107], NA)
q <- quantile(abs(bar.tick$residuals), probs = c(0, 0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
tickies <- c()

for (i in 1:nrow(shp)) {
    
    tickies <- c(tickies, max(1,max(which(q < shp$bartik[i]))))
  
}

shp$tickies <- tickies

tick.pal <- colorNumeric(palette = c('blue', 'lightblue', 'white', 'pink', 'red'), domain = shp$tickies)
tick.map <- leaflet(shp$geometry) %>% addTiles() %>% addPolygons(weight = 1.0, smoothFactor = 1.0, opacity = 1.0, fillOpacity = 1.0, color = 'black', fillColor = tick.pal(shp$tickies))
tick.map

# Creating some scatter plots to motivate the analysis

ggplot(data = shp, aes(x = bartik2, y = Dev.Change)) +
  geom_point() +
  theme_bw() +
  stat_smooth(method = lm) +
  ggtitle('Relationship between Job Growth and Developed Land') +
  ylab('Change in Developed Land Proportion') +
  xlab('Exogenous Component of Change in Employment Rate') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = shp, aes(x = bartik2, y = Wetlands.Change)) +
  geom_point() +
  theme_bw() +
  stat_smooth(method = lm) +
  ggtitle('Relationship between Job Growth and Wetlands') +
  ylab('Change in Wetland Proportion') +
  xlab('Exogenous Component of Change in Employment Rate') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = shp, aes(x = bartik2, y = Ag.Change)) +
  geom_point() +
  theme_bw() +
  stat_smooth(method = lm) +
  ggtitle('Relationship between Job Growth and Agricultural Land') +
  ylab('Change in Agricultural Land Proportion') +
  xlab('Exogenous Component of Change in Employment Rate') +
  theme(plot.title = element_text(hjust = 0.5))

