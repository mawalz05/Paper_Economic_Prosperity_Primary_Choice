rm(list=ls())

#Importing economic prosperity csv from git hub
data = read.csv("https://raw.githubusercontent.com/mawalz05/Paper_Economic_Prosperity_Primary_Choice/master/Economic_Propserity.csv")

#Converting Density to numeric data to run the model
data$density = as.numeric(data$density)

#Model in Table 2
#DV is PD (Binary where 1 = primary, 0 = caucus)
model = glm(PD ~ density+population+stategdp_percap+uni_dem+Incumbent_R+favsonD+citi_1+nominate_dim1+South_ext+Tashijan+counter2+spline+factor(Year), data = data, family = "binomial")
summary(model)

#Correlations of variables of interest (population, density, and gdp)
pop = data$population
dens = data$density
gdp = data$stategdp_percap

new = as.data.frame(cbind(pop, dens, gdp))
cor(new)

####################################################
############Data for Figures########################
#Import caucus_primary_count from github
data2 = read.csv("https://raw.githubusercontent.com/mawalz05/Paper_Economic_Prosperity_Primary_Choice/master/caucus_primary_count.csv")

#Turning wide data into long data for bar plot
library(tidyr)
data2 = gather(data2, Type, Proportion, -year)

#Code to produce Figure 1
library(dplyr)
library(ggplot2)
data2 %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = Proportion, fill = Type)) + 
  geom_bar(position = "fill", stat = "identity")  + scale_fill_grey() + theme_classic()

#Code to Produce the US Map of caucuses vs primaries 2016 (Figure 2)
#Data Wrangling
poor = data %>%
  filter(Year == 2016) 
poorvar = c("statename","Year","PD")
poor = poor[poorvar]

fips = c("01","02","04","05","06","08","09","10","12","13","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","44","45","46","47","48","49","50","51","53","54","55","56")

poor2 = cbind(poor, fips)
str(poor2)
colnames(poor2)[colnames(poor2)=="statename"] <- "full"

#Producing Figure 2
library(usmap)
plot_usmap("states", data = poor2, values = "PD") + 
  ggplot2::scale_fill_continuous(low = "gray28", high = "gray68", guide = FALSE)

state_map <- us_map(regions = "states")
str(state_map)

#Comparing the mean gdp per capita between the South and the rest of the US
data %>%
  group_by(South_ext) %>%
  summarize(mean(stategdp_percap))
