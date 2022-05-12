#install.packages("PL94171")
install.packages("ggExtra")
library(sf)
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(PL94171)
library(tmap)
library(ggExtra)


KS_counties <- st_read("KS_counties.shp")

KS_counties <- KS_counties[-c(1:5,7:14)] #Reduce to data I need

names(KS_counties)[names(KS_counties) == "name"] <- "County"

head(KS_counties)

ggplot() +
  geom_sf(data = KS_counties)

##Base data

base_data <- read.csv("BASE_Grant_KS_Data.csv")

head(base_data)

base_data <- base_data %>% 
  select(County, Amount)

head(base_data)

base_data <- base_data[-36,]

base_data

base_totals <- aggregate(Amount ~ County, base_data, sum) ##Sum by county

base_totals

ggplot(data = base_totals, aes(x = "", y = Amount, 
                               fill = County)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "BASE Grant Awards by County") +
  coord_polar("y")

tibble::as_tibble(base_totals)


##Pull in GDP

KS_gdp <- read.csv("CAGDP1_KS_2001_2020.csv")

colnames(KS_gdp)

KS_gdp[1,2]

KS_gdp_2020 <- KS_gdp[-c(1:3),-c(1,3,4,6,9:27)] #Remove data I don't need

KS_gdp_2020 <- filter(KS_gdp_2020, LineCode == 3)

KS_gdp_2020 <- KS_gdp_2020[-c(2:4)]

names(KS_gdp_2020)[names(KS_gdp_2020) == "GeoName"] <- "County"
names(KS_gdp_2020)[names(KS_gdp_2020) == "X2020"] <- "GDP"

KS_gdp_2020 <- KS_gdp_2020 %>% 
  mutate_at("County", str_replace, ", KS", "")

KS_gdp_2020

tibble::as_tibble(KS_gdp_2020)

##Pull in Population
KS_pop <- pl_read("ks2020.pl")

KS_pop <- pl_subset(KS_pop, sumlev = "050") #050 filters to county level

head(KS_pop)
tail(KS_pop)
KS_pop[,"BASENAME"] #387
KS_pop[,"POP100"] #Filter to these two
names(KS_pop)

KS_pop <- KS_pop %>%
  select(BASENAME, POP100)

names(KS_pop)[names(KS_pop) == "BASENAME"] <- "County"
names(KS_pop)[names(KS_pop) == "POP100"] <- "Population"

KS_pop

tibble::as_tibble(KS_pop)

ggplot(data = KS_pop, aes(x = "", y = Population, 
                               fill = County)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "2020 Population by County") +
  coord_polar("y") ###This is not readable, try a bar chart



#Merge Base, GDP, Population

## Add 0 counties


award_gdp <- dplyr::left_join(KS_gdp_2020, base_totals, by = "County")

all_data <- dplyr::left_join(award_gdp, KS_pop, by = "County")

all_data[is.na(all_data)] <- 0

all_data #This has BASE award amount, GPD, and Population all by county and all by 2020 nubmers.

#Regular Plots

all_data %>%
  ggplot(.) +
    geom_bar(aes(x = County, y = Amount), stat = "identity", fill = "#2b8cbe") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = -0.1)) +
    ggtitle("BASE Grant Award Totals by County") + ylab("Amount in Dollars")

all_data %>%
  ggplot(.) +
  geom_bar(aes(x = County, y = GDP), stat = "identity", fill = "#2b8cbe") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1)) +
  ggtitle("2020 GDP by County") + ylab("GDP in Thousands of Dollars")

all_data %>%
  ggplot(.) +
  geom_bar(aes(x = County, y = Population), stat = "identity", fill = "#2b8cbe") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1)) +
  ggtitle("2020 Population by County") + ylab("Population")


#Joing spatial data with our other data

mapped_data <- left_join(KS_counties, all_data, by = "County")

head(mapped_data)

#Plot each variable on the map

ggplot() +
  geom_sf(data = mapped_data, aes(fill = Amount))+
  labs(fill = "Amount in Dollars") +
  ggtitle("BASE Grant Award Totals by County")+
  scale_fill_viridis_c(option="magma", begin=0.25)

ggplot() +
  geom_sf(data = mapped_data, aes(fill = GDP))+
  labs(fill = "GDP in Thousands of Dollars") +
  ggtitle("2020 GDP by County")+
  scale_fill_viridis_c(option="magma", begin=0.25)

ggplot() +
  geom_sf(data = mapped_data, aes(fill = Population))+
  ggtitle("2020 Population by County")+
  scale_fill_viridis_c(option="magma", begin=0.25)

#Interactive Maps

tmap_mode("view")

tm_shape(mapped_data) +
  tm_polygons("Amount") +
  tm_bubbles("GDP", col="green")

tm_shape(mapped_data) +
  tm_polygons("Amount") +
  tm_bubbles("Population", col="blue")




#Run regression using population and GDP
base_pop <- ggplot(data = mapped_data, aes(x = mapped_data$Population, y = mapped_data$Amount)) +
  geom_point() +
  theme(legend.position = "none") +
  xlab("Population") +
  ylab("Award Amount in Dollars")

ggMarginal(base_pop, type = "histogram")

base_gdp <- ggplot(data = mapped_data, aes(x = mapped_data$GDP, y = mapped_data$Amount)) +
  geom_point() +
  theme(legend.position = "none") +
  xlab("Population") +
  ylab("GDP in Thousands of Dollars")

ggMarginal(base_gdp, type = "histogram")

##### 
#Parking lot

#Create % Columns for each row in "all_data"

ggplot(data = base_data, aes(x = County, y = Amount)) + 
  geom_boxplot(fill = "blue", alpha = 0.5, outlier.size = 0.25, 
               position = position_dodge( width = 2, preserve = "single")) + 
  coord_flip() 


##Rural/Urban classification....likely abandon this
KS_rucc <- read_excel("ruralurbancodes2013.xls")
KS_rucc <- KS_rucc %>%
  select()

head(KS_rucc)

KS_rucc

#Need to filter to just Kansas, remove population