#installing packages
install.packages("skimr")
install.packages("janitor")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("lubridate")
install.packages("hms")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("htmltools")

#Usage library
library(dplyr)
library(skimr)
library(janitor)
library(tidyverse)
library(lubridate)
library(hms)
library(tidyr)
library(ggplot2)
library(plotly)

#Read 3 csv files
wh2023 <- read.csv("World happiness 2023/world-happiness-2023.csv")
wd2023 <- read.csv("World happiness 2023/world-data-2023.csv")
continents2023 <- read.csv("World happiness 2023/continents-data-2023.csv")

#extract specific columns from continent dataset
continent2023 <- continents2023 %>%
  select(name,Country_code,Region,Sub_region) %>%
  rename(Country = name)
wh2023 <- wh2023 %>%
  rename(Country = Country_name)
#extract specific columns from world-data dataset
wd2023 <- wd2023 %>%
  select(Country,Abbreviation,Unemployment.rate,Urban_population,Population,Official.language) %>%
rename(Unemployment_rate =Unemployment.rate,Official_language = Official.language)


#merge continents,world data and world happiness dataset 
wh2023 <- merge(wh2023,continent2023,by = "Country",all.x = TRUE)
wh2023 <- wh2023 %>%
  relocate(Country_code,Region,Sub_region,.after = Country)
wh2023 <- merge(wh2023,wd2023,by = "Country",all.x = TRUE)%>%
  relocate(Abbreviation,.after = Country)
View(wh2023)

#check null rows
wh2023[wh2023==""]<-NA
null_values <- colSums(is.na(wh2023))
print(null_values)

#check duplicate
duplicate_index <- anyDuplicated(wh2023)
# Print the results
if (duplicate_index > 0) {
  print(paste("Duplicate rows found at index:",duplicate_index, "\n"))
} else {
  cat("No duplicate rows found.\n")
}

#Clear null values
wh2023 <- wh2023[complete.cases(wh2023), ]
View(wh2023)

#Save as txt file
write.table(wh2023,"World happiness 2023/world_happiness_report.txt",fileEncoding = "UTF-8", quote = FALSE)

#fill data for 11 null regions for analysis
# filled_wh2023 <- wh2023 %>%
#   mutate(
#     Region = case_when(
#       Country == "Bosnia and Herzegovina" ~ "Europe",Country == "Congo (Kinshasa)" ~ "Africa",Country == "Czechia" ~ "Europe",
#       Country == "Congo (Brazzaville)" ~ "Africa",Country == "Hong Kong S.A.R. of China" ~ "Asia",Country == "Ivory Coast" ~ "Africa",
#       Country == "Kosovo" ~ "Europe",Country == "North Macedonia" ~ "Europe",Country == "State of Palestine" ~ "Asia",
#       Country == "Taiwan Province of China" ~ "Asia",Country == "Turkiye" ~ "Asia",
#       TRUE ~ Region
#     )
#     Sub_region = case_when(
#       Country == "Bosnia and Herzegovina" ~ "South-eastern Europe",Country == "Congo (Kinshasa)" ~ "Central Africa",Country == "Czechia" ~ "Central Europe",
#       Country == "Congo (Brazzaville)" ~ "Central Africa",Country == "Hong Kong S.A.R. of China" ~ "Eastern Asia",Country == "Ivory Coast" ~ "Western Africa",
#       Country == "Kosovo" ~ "South-eastern Europe",Country == "North Macedonia" ~ "South-eastern Europe",Country == "State of Palestine" ~ "Western Asia",
#       Country == "Taiwan Province of China" ~ "Eastern Asia",Country == "Turkiye" ~ "Western Asia",
#       TRUE ~ Sub_region
#     ))
# view(filled_wh2023)

#Analyze
head(wh2023)
#summary of dataset
summary(wh2023)

# min, max and average of ladder score
avg_ladder <- mean(wh2023$Ladder_score)
ladder_point <- wh2023 %>%
  summarise(avg_ladder,
            min_ladder = min(Ladder_score),
            max_ladder = max(Ladder_score))
View(ladder_point)
# min, max and average of freedom life choices score
avg_flc <- mean(wh2023$Freedom_life_choices)
flc_point <- wh2023 %>%
  summarise(avg_flc,
            min_flc = min(Freedom_life_choices),
            max_flc = max(Freedom_life_choices))
View(flc_point)

#top 10 highest ladder score
highest_ladder <- wh2023 %>%
  select(Country,Ladder_score,Region,Generosity,Freedom_life_choices) %>%
  arrange(desc(Ladder_score)) %>%
  top_n(10,wt=Ladder_score)
view(highest_ladder)

#top 10 lowest ladder score 
lowest_ladder <- wh2023 %>%
  select(Country,Ladder_score,Region,Generosity,Freedom_life_choices) %>%
      arrange(desc(Ladder_score)) %>%
      tail(10)
View(lowest_ladder)

#top 10 highest generosity
highest_generosity <- wh2023 %>%
  select(Country,Ladder_score,Region,Generosity,Freedom_life_choices) %>%
  arrange(desc(Generosity)) %>%
  top_n(10,wt=Generosity)
view(highest_generosity)

#average,max,min ladder score by Continents
regions <- wh2023 %>%
  group_by(Region) %>%
  summarise(avg_ladder_by_continent = mean(Ladder_score),
            sum_ladder = sum(Ladder_score),
            country_count = length(Country),
            max_ladder = max(Ladder_score),
            min_ladder = min(Ladder_score))
View(regions)

#average,max,min ladder score by sub regions
sub_region <- wh2023 %>%
  group_by(Sub_region) %>%
  summarise(avg_ladder_by_regions = mean(Ladder_score),
            sum_ladder = sum(Ladder_score),
            country_count = length(Country),
            max_ladder = max(Ladder_score),
            min_ladder = min(Ladder_score))
View(sub_region)

#chart 01 - World happiness Report: ladder scores by continents
ggplot(wh2023, aes(x = Region, y = Ladder_score  ,fill = Region)) +
  geom_boxplot() +
  labs(title="Chart 01 - Distribution of Ladder scores by continents",
       caption = paste0("Data from: ", 2023, "  to  ", 2023),
       fill="Continents",
       x = "Continents",
       y = "Ladder Score")+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.caption = element_text(face="bold"),
        plot.title =element_text(face="bold"))


#chart 02 - World happiness Report: ladder score by sub region
ggplot(wh2023, aes(x = Sub_region, y = Ladder_score  ,fill = Sub_region)) +
  geom_boxplot() +
  labs(title="Chart 02 - Distribution of ladder scores by sub regions",
       caption = paste0("Data from: ", 2023, "  to  ", 2023),
       fill= "Sub Region",
       x = "Sub Regions",
       y = "Ladder Score")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.caption = element_text(face="bold"),
        plot.title =element_text(face="bold"))

#chart 03 - World happiness Report: highest ladder score by countries
ggplot(highest_ladder, aes(Country,y = Ladder_score,fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title="Chart 03 - Distribution of top 10 countries with highest ladder scores",
       caption = paste0("Data from: ", 2023, "  to  ", 2023),
       fill= "Country",
       x = "Country",
       y = "Ladder Score")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.caption = element_text(face="bold"),
        plot.title =element_text(face="bold"))

#chart 04 - World happiness Report: lowest ladder score by countries
ggplot(lowest_ladder, aes(Country,y = Ladder_score,fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title="Chart 04 - Distribution of top 10 countries with lowest ladder scores",
       caption = paste0("Data from: ", 2023, "  to  ", 2023),
       fill= "Country",
       x = "Country",
       y = "Ladder Score")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.caption = element_text(face="bold"),
        plot.title =element_text(face="bold"))

#chart 05 - World happiness Report: ladder score with GDP per capita
ggplot(wh2023, aes(x = Logged_GDP_per_capita, y = Ladder_score)) +
  geom_point(color = "skyblue")+
  facet_wrap(~Region)+
  labs(title="Chart 05 - Distribution of ladder scores with GDP per capita by Continents",
       caption = paste0("Data from: ", 2023, "  to  ", 2023),
       x = "GPD Per Capita",
       y = "Ladder Score")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.caption = element_text(face="bold"),
        plot.title =element_text(face="bold"))

#chart 06 - World happiness Report: ladder score with Social support
ggplot(wh2023, aes(x = Social_support, y = Ladder_score)) +
  geom_point(color = "skyblue")+
  facet_wrap(~Region)+
  labs(title="Chart 06 - Distribution of ladder scores with Social Support by Continents",
       caption = paste0("Data from: ", 2023, "  to  ", 2023),
       x = "Social Support",
       y = "Ladder Score")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.caption = element_text(face="bold"),
        plot.title =element_text(face="bold"))

#chart 07 - World happiness Report: ladder score with Generosity
ggplot(wh2023, aes(x = Generosity, y = Ladder_score)) +
  geom_point(color = "skyblue")+
  facet_wrap(~Region)+
  labs(title="Chart 07 - Distribution of ladder scores with Generosity by Continents",
       caption = paste0("Data from: ", 2023, "  to  ", 2023),
       x = "Generosity",
       y = "Ladder Score")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.caption = element_text(face="bold"),
        plot.title =element_text(face="bold"))

# create data for world coordinates using  
# map_data() function 
# world_coordinates <- map_data("world")
# world_map <- merge(world_coordinates,wh2023)
# # create world map using ggplot() function 
# ggplot() + 
#   geom_map( 
#     data = world_map, map = world_coordinates, 
#     aes(long,lat,map_id = region,fill = Ladder_score,color = "white", size = 0.25))+
#   labs(title="Chart 07 - Distribution of ladder scores with Generosity by Continents",
#        caption = paste0("Data from: ", 2023, "  to  ", 2023),
#        fill= "Ladder Score")+
#   theme(axis.text.x = element_text(angle = 90,hjust = 1),
#         axis.title.x = element_text(face="bold"),
#         axis.title.y = element_text(face="bold"),
#         plot.caption = element_text(face="bold"),
#         plot.title =element_text(face="bold"))


# Create choropleth map
choropleth_map <- plot_ly(
  type = "choropleth",
  locationmode = "country names",
  locations = wh2023$Country,
  z = wh2023$Ladder_score,
  colorscale = "Viridis",
) %>% 
  layout(
    title = list(
      text = "<b>World Happiness Report: Ladder score by country</b>"
    ),
    geo = list(showframe = FALSE, showcoastlines = FALSE),
    margin= list(b=-10,t=100)
  )%>%
  colorbar(
    title = list(
      text= "<b>Ladder Score</b>"
    )
  )
# Display the map
choropleth_map
