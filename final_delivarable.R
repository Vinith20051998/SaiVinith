#import and read the dataset
d <- read.csv("Vaccination_Coverage_among_Pregnant_Women.csv")
View(d)

#look at the first few rows of the dataset
head(d)

#look at the ladt few rows of the dataset
tail(d)

#look at the summary of the dataset
summary(d)

#get the data for each state
#no need to create a subset of influenza vaccination in Alaska as there id no data for Tdap in Alaska 
alaska <- subset(d, Geography == "Alaska")
alaska
View(alaska)
str(alaska)

alaska_omit_na <- subset(alaska, Estimate....!="NR*")
alaska_omit_na
View(alaska_omit_na)
str(alaska_omit_na)

#plot to look for different years for influenza vaccinated in Alaska
x = alaska_omit_na$Survey.Year.Influenza.Season
y = alaska_omit_na$Estimate....
plot(x,y)

#plot to see the estimate of vaccination against influenza season in Alaska
hispanic_alaska <- subset(alaska_omit_na, Dimension=="Hispanic")
hispanic_alaska
View(hispanic_alaska)

#to do the plotting we need to arrange the survey year in ascending order
survey_year_asc <- order(hispanic_alaska$Survey.Year.Influenza.Season)
survey_year_asc

hispanic_alaska[survey_year_asc,]
  
x = hispanic_alaska[survey_year_asc, ]$Survey.Year.Influenza.Season
y = hispanic_alaska$Estimate....
plot(x, y, xlab = "Influenza Season(year)", ylab = "Estimated Vaccine Coverage(%)",
     main = "Influenza Vaccine Coverage in Alaska (2013 - 2020)", type = "o",
     pch = 20, lwd = 2, col = "blue")

#creating a bar diagram containing 5 states with vaccine estimate in a year.
#virginia, Washington, Pennsylvania, Delaware, Michigan

#Creating a subset of Tdap vaccine
tdap_vaccine <- subset(d, Vaccine == "Tdap")
tdap_vaccine

#summary of tdap vaccine
summary(tdap_vaccine)

#creating a subset of tdap vaccines in virginia in 2020 of white, non- hispanic 
tdap_2020_virginia <- subset(tdap_vaccine, Survey.Year.Influenza.Season == "2020" & 
                               Geography == "Virginia" & Dimension == "White, Non-Hispanic")
tdap_2020_virginia
View(tdap_2020_virginia)


#creating a subset of tdap vaccines in michigan in 2020 of white, non-hispanic
tdap_2020_michigan <- subset(tdap_vaccine, Survey.Year.Influenza.Season == "2020" 
                               & Geography == "Michigan" & Dimension == "White, Non-Hispanic")
tdap_2020_michigan
View(tdap_2020_michigan)

#creating a subset of tdap vaccines in Pennsylvania in 2020 of white, non-hispanic
tdap_2020_pennsylvania <- subset(tdap_vaccine, Survey.Year.Influenza.Season == "2020" 
                             & Geography == "Pennsylvania" & Dimension == "White, Non-Hispanic")
tdap_2020_pennsylvania
View(tdap_2020_pennsylvania)

#creating a subset of tdap vaccines in washington in 2020 of white, non-hispanic
tdap_2020_washington <- subset(tdap_vaccine, Survey.Year.Influenza.Season == "2020" 
                                 & Geography == "Washington" & Dimension == "White, Non-Hispanic")
tdap_2020_washington
View(tdap_2020_washington)

#creating a subset of tdap vaccines in delaware in 2020 of white, non-hispanic
tdap_2020_delaware <- subset(tdap_vaccine, Survey.Year.Influenza.Season == "2020" 
                               & Geography == "Delaware" & Dimension == "White, Non-Hispanic")
tdap_2020_delaware
View(tdap_2020_delaware)

#now, create a bar plot of vaccination % in Virginia, Michigan, Pennsylvania, Washington, Delaware
data <- c(as.numeric(tdap_2020_virginia$Estimate....), as.numeric(tdap_2020_michigan$Estimate....), 
          as.numeric(tdap_2020_pennsylvania$Estimate....), as.numeric(tdap_2020_washington$Estimate....),
          as.numeric(tdap_2020_delaware$Estimate....))
states <- c("Virginia", "Michigan", "Pennsylvania", "Washington", "Delaware")
barplot(data)

barplot(data, xlab = "States", ylab = "Vaccination %", main = "Estimated Tdap Vaccine Coverage in 
        2020 among 5 States", names.arg = states, col = "purple", border = "red",
        ylim = c(0,100))

#creating a pie chart of estimated vaccine % from 2015-2020 in black, non-hispanic people in United States

#Creating a subset of Tdap vaccine
tdap_vaccine <- subset(d, Vaccine == "Tdap")
tdap_vaccine
View(tdap_vaccine)

##creating a subset of vaccinated tdap % of black, non-hispanic people in united states in 2015
tdap_2015_us <- subset(tdap_vaccine, Geography == "United States" & Survey.Year.Influenza.Season == "2015"
                       & Dimension == "Black, Non-Hispanic")
tdap_2015_us

##creating a subset of vaccinated tdap % of black, non-hispanic people in united states in 2016
tdap_2016_us <- subset(tdap_vaccine, Geography == "United States" & Survey.Year.Influenza.Season == "2016"
                       & Dimension == "Black, Non-Hispanic")
tdap_2016_us


##creating a subset of vaccinated tdap % of black, non-hispanic people in united states in 2017
tdap_2017_us <- subset(tdap_vaccine, Geography == "United States" & Survey.Year.Influenza.Season == "2017"
                       & Dimension == "Black, Non-Hispanic")
tdap_2017_us

##creating a subset of vaccinated tdap % of black, non-hispanic people in united states in 2018
tdap_2018_us <- subset(tdap_vaccine, Geography == "United States" & Survey.Year.Influenza.Season == "2018"
                       & Dimension == "Black, Non-Hispanic")
tdap_2018_us


##creating a subset of vaccinated tdap % of black, non-hispanic people in united states in 2019
tdap_2019_us <- subset(tdap_vaccine, Geography == "United States" & Survey.Year.Influenza.Season == "2019"
                       & Dimension == "Black, Non-Hispanic")
tdap_2019_us


##creating a subset of vaccinated tdap % of black, non-hispanic people in united states in 2020
tdap_2020_us <- subset(tdap_vaccine, Geography == "United States" & Survey.Year.Influenza.Season == "2020"
                       & Dimension == "Black, Non-Hispanic")
tdap_2020_us

values <- c(as.numeric(tdap_2015_us$Estimate....), as.numeric(tdap_2016_us$Estimate....), 
as.numeric(tdap_2017_us$Estimate....), as.numeric(tdap_2018_us$Estimate....), as.numeric(tdap_2019_us$Estimate....), 
as.numeric(tdap_2020_us$Estimate....))
years <- c("2015 - 50.1%", "2016 - 65.9%", "2017 - 66.3%", "2018 - 65.8%", "2019 - 70.7%", "2020 - 70.7%")

pie(values, labels = years, main = "Tdap Vaccination % in United States from 2015-2020")
