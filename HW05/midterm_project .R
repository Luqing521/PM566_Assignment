---
title: "midterm-project"
author: "Luqing Ren"
date: "10/4/2020"
output: html_document

---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction: 
Cardiovascular disease (CVD) is a leading cause of death both in California and the United States.There are many risk factors for developing CVD including diet, lack of exercise, smoking, and air pollution.This data includes indicators which reflect environmental conditions or a population’s vulnerability to environmental pollutants of all communities in California State.
The main purpose of this project is to analyze whether air pollution associated with CVD rate in counties of California state and the pollution particle effect to CVD rate. Is CVD rate associated with communities category.

## Methods:
The data was downloaded from https://data.ca.gov/dataset/calenviroscreen-3-0-results. through API and formed into a data table.The key variables are extracted and formed into a new data frame.The data table includes several air pollution types,cardiovascular disease rate and socioeconomic indicators. 
 
```{r,eval = TRUE} 
# load the data set by API
library(httr)
library(data.table)
go_query <- GET("https://data.ca.gov/api/3/action/datastore_search?resource_id=89b3f4e9-0bf8-4690-8c6f-715a717f3fae&limit=10000")
dat <- content(go_query)
dat <- as.list(dat)
dat_1 <- do.call(rbind,lapply(dat$result$records, rbind))
dat_1[dat_1 == "NULL"]= NA
ces <- as.data.frame(dat_1) 
ces <- as.data.table(lapply(ces, unlist))
ces <- as.data.table(lapply(ces,function(x) type.convert(as.character(x), as.is = TRUE)))
```


## Create a new data set including key variables and check the data
```{r,eval = TRUE}
## create a new data set including key variables
library(tidyverse)
ces_2 <-
  ces %>% 
  select(`Census Tract`, ZIP, `California County`,Latitude, Longitude,
         `SB 535 Disadvantaged Community`,Ozone,`Ozone Pctl`,
         PM2.5, `PM2.5 Pctl`,Traffic,`Traffic Pctl`, `Cardiovascular Disease`,
         Education, Poverty) %>% 
  as.data.table()
#rename column 
colnames(ces_2) = c("census_trct","ZIP","county","latitude","longitude","community_cat",
                    "ozone","ozone_pct","PM2.5","PM2.5_pct","traffic","traffic_pct",
                    "cardio","education","poverty")
# check the data
dim(ces_2)
```
- After briefly check the data by skim(), most of the variables are complete and the missing data is less than 5%, which is not a huge amount. 



## Examine the relationship between CVD rate and community category
```{r,eval = TRUE}
# get the average of each exposure indicator:
library(data.table)
library(htmlTable)
ces_2[, PM2.5_mean := mean(PM2.5, na.rm = TRUE), by = county]
ces_2[, ozone_mean := mean(ozone, na.rm = TRUE), by = county]
ces_2[, traffic_mean := mean(traffic, na.rm = TRUE), by = county]
ces_2[, cardio_mean := mean(cardio, na.rm = TRUE), by = county]

# make a table of top 10 counties with CVD rate and community category
table1 <-
ces_2 %>% 
  summarise(county,cardio_mean,community_cat) %>% 
  arrange(desc(cardio_mean))
table1<-head(as.data.table(table1)[,.SD[1],by="county"], 10)  
    
htmlTable(table1,header = c("county","CVD rate","community category"),
          caption = "Table1: The top 10 counties with CVD rate and their comunity category")

# cardiovascular disease occurrence by community category
ces_2 %>%  
  ggplot(mapping= aes(x = as.factor(community_cat), y= cardio))+
  geom_boxplot()+
labs(x=("community category"),title = ("Figure1.cardiovascular disease occurrence"))
```

- In the top 20 CVD rate counties, more than half of those belongs to disadvantageous communities.The boxplot shows that disadvantageous communities have relatively higher average of CVD rate than those not.  


## Examine the pollution exposure by community category
```{r,eval = TRUE}
# pollution exposure by community category
to_map <- ces_2 %>% 
  select(cardio_mean, county, PM2.5_mean, ozone_mean,traffic_mean,community_cat)

#select unique county and cardio_mean.
to_map<-as.data.table(to_map)[, .SD[1], by="county"]

g1<-to_map %>%  
filter(!(PM2.5_mean %in% NA)) %>% 
  ggplot(mapping= aes(x= PM2.5_mean, y = cardio_mean))+
  geom_point()+ 
  geom_smooth(method = "lm",size=0.5, se=FALSE)+
labs(x=("PM2.5"),y=("cardio"),title = ("PM2.5 and CVD rate"))

g2<-to_map %>%  
filter(!(ozone_mean %in% NA)) %>% 
  ggplot(mapping= aes(x = ozone_mean, y = cardio_mean))+
  geom_point()+ 
  geom_smooth(method = "lm",size=0.5,se=FALSE)+
  labs(x=("ozone"),y=("cardio"),title = ("ozone and CVD rate"))

g3<-to_map %>%  
filter(!(traffic_mean %in% NA)) %>% 
  ggplot(mapping= aes(x = traffic_mean, y= cardio_mean))+
  geom_point()+ 
  geom_smooth(method = "lm",size=0.5,se=FALSE)+
  labs(x=("traffic density"),y=("cardio"),title = ("traffic density and CVD rate"))

#combine four graphs 
library(ggpubr)
figure <- ggarrange(g1, g2, g3,
                  labels = c("A", "B", "C"),
                  ncol = 2, nrow = 2)
annotate_figure(figure, top = text_grob("Figure2.pollution exposure by community category "))
```

- From the matrix graphs, disadvantageous communities have relatively higher PM2.5 and Ozone pollution exposure and traffic density. Both PM2.5 and ozone are positively corelated with CVD rate in disadvantageous communities. In all communities, traffic density is negatively related with CVD rate.



## Visualize the polution burden and CVD rate in state map  
```{r,eval = TRUE}
# create a new variable for pollution burden score.Pollution Burden is calculated as the average of pollution exposures percentile (Ozone_Pct,PM2.5_Pct,Traffic_Pct,tox_Pct), and scaled by the statewide maximum average percentile. 

ces_2[, pol_score := (ozone_pct+PM2.5_pct+traffic_pct)/3]
ces_2[,pol_score1 := (pol_score*10)/max(ces_2$pol_score, na.rm = TRUE)]
ces_2[, score_mean := mean(pol_score1, na.rm = TRUE), by = county]
# make a state map of pollution burden score and CVD rate 
 library(urbnmapr)
 library(ggpubr)
#trim white space in county.
county_names <- trimws(ces_2$county, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
#append "County" to the trimmed county_names and add it as column "county_name" in ces_2
ces_2 <-ces_2 %>% 
  mutate(county_name = paste(county_names, "County", sep = " "))
#construct a new dataset that contains only cardio_mean and county_name
to_map <- ces_2 %>% 
  select(cardio_mean, county_name, pol_score,PM2.5_mean, ozone_mean,traffic_mean)
#select unique county and cadio_mean.
to_map<-as.data.table(to_map)[, .SD[1], by="county_name"]
#urbanmap data from library(urbnmapr)...
urbnmap_data <- countydata %>% 
  left_join(counties, by = "county_fips") %>% 
  filter(state_name =="California")
#merge to_map with urbanmap_data so that we can draw an urban map of cardio_mean by county.
#name the merged data as new_data
new_data<-merge(x=urbnmap_data, y=to_map, 
      by="county_name",
      all.x = TRUE,
      all.y = FALSE)

#draw polution index map.
g6<- new_data%>% 
  ggplot(mapping = aes(long, lat, group = group, fill=pol_score ))+
  geom_polygon(color = "#ffffff", size = .25)+
  scale_fill_gradientn(colours = c("#CFE8F3", "#A2D4EC", "#73BFE2", "#46ABDB", "#1696D2", "#12719E","#0A4C6A", "#062635"),guide = guide_colorbar(title.position = "top")) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(title=("Pollution score"),
       fill = "Pollution_score")

#draw cardiovascular disease rate map.
g5<- new_data%>% 
  ggplot(mapping = aes(long, lat, group = group, fill=cardio_mean ))+
  geom_polygon(color = "#ffffff", size = .25)+
  scale_fill_gradientn(colours = c("#CFE8F3", "#A2D4EC", "#73BFE2", "#46ABDB", "#1696D2", "#12719E","#0A4C6A", "#062635"),guide = guide_colorbar(title.position = "top")) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(title=("Cardiovascular disease rate"),
       fill = "CVD rate")

map<-ggarrange(g5, g6,
          labels = c("A", "B"),
          ncol = 2, nrow = 2)
annotate_figure(map, top = text_grob("Figure3. pollution exposure by community category "))         
```

- An area with a high pollution score is the one that experiences higher pollution burden than areas with low scores. From the state map, we can see Madera, Fresno, Los Angeles, San Joaquin,San Bernadino and Riverside show higher pollution score, this is in consistent with a higher CVD rate in those counties. However, the pollution is not a sufficient cause of CVD rate, because some communities with lower pollution score have higher CVD rate.


## The association between pollution burden score and CVD rate
```{r,eval = TRUE}
table2 <-
ces_2 %>% 
  summarise(county,pol_score1,cardio_mean) %>% 
  arrange(desc(pol_score1))
 table2 <-head(as.data.table(table2)[,.SD[1],by="county"], 10)
 
 htmlTable(table1,header = c("county","pollution score","CVD rate"),
          caption = "Table2: The top 10 counties with CVD rate and their pollution score")  
 
 ces_2%>% 
  filter(!pol_score1 %in% NA) %>% 
  ggplot(mapping = aes(x = score_mean, y = cardio_mean))+
  geom_point(aes(color = "red"))+
  geom_smooth(method = "lm",color = "black",size=0.5)+
  labs(x=("pollution score"),y=("cardio"),title = ("Figure4. pollution score and CVD rate by community category"))
```

-From this graph, pollution burden is positively related with CVD rate. The more pollution exposure associate with higher rate of CVD occurrence.



## Conclusion:
- The data indicates that air pollution is positively correlated with CVD occurrence rate, and disadvantageous communities have a relatively higher CVD rate. 
- PM2.5 and Ozone is correlated with CVD rate. Higher PM2.5 and Ozone correspondent to higher CVD rate.
- A higher pollution score indicates that the place experiences higher pollution burden. In top 20 high score counties, more than half of them are disadvantageous communities.
- Pollution exposure is not the sufficient cause of CVD rate. Some communities with lower pollution score have a relative higher CVD rate.
- Some socioeconomic indicators can be used for further analysis.













