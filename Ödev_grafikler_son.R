skim(wh_report_2021)
## Reorganizing & Renaming Data
wh_report_2021 <- wh_report_2021 %>% select(-`Standard error of ladder score`,- upperwhisker,- lowerwhisker) %>%
  rename(
    country = `Country name`,
    regional_Ind = `Regional indicator`,
    ladder_score = `Ladder score`,
    GDP_per_Capita = `Logged GDP per capita`,
    Soc_support = `Social support`,
    Healthy_life_exp = `Healthy life expectancy`,
    Freedom = `Freedom to make life choices`,
    Corrupt_perception = `Perceptions of corruption`,
    Dyst_ladder_score = `Ladder score in Dystopia`,
    Dyst_GDP_per_Capita = `Explained by: Log GDP per capita`,
    Dyst_Soc_support = `Explained by: Social support`,
    Dyst_Healthy_life_exp = `Explained by: Healthy life expectancy`,
    Dyst_Freedom = `Explained by: Freedom to make life choices`,
    Dyst_Generosity = `Explained by: Generosity`,
    Dyst_Corrupt_perception = `Explained by: Perceptions of corruption`,
    Dyst_residual = `Dystopia + residual`
    
  )
head(wh_report_2021)
wh_report_2021 %>% count(regional_Ind)

#Happiness Score distribution across Regions
wh_report_2021 %>% mutate(region = fct_reorder(regional_Ind,ladder_score,median)) %>% 
  ggplot(aes(ladder_score))+
  geom_boxplot(aes(fill=region))+
  coord_flip()+
  labs(title = "Western Europe & North America have high mean Happiness_score")+
  xlab("Happiness Score / 10")+
  ylab("")+
  theme_classic()
# Average Happiness_Score comparism across Regions
wh_report_2021 %>% group_by(regional_Ind) %>% 
  summarise(avg_happiness = mean(ladder_score)) %>% 
  ggplot(aes(avg_happiness,regional_Ind))+
  geom_col(aes(fill=regional_Ind))+
  labs(title = " Happiness_score")+
  xlab("Happy Score / 10")+
  ylab("Region")+
  theme_light()+
  theme(axis.text.y = element_text(angle = 360),legend.position = "bottom")

#Exploring GDP_Per_Capita against ladder_score

wh_report_2021 %>% 
  ggplot(aes(ladder_score,Freedom))+
  geom_point(aes(color=regional_Ind))+
  labs(title = "Happiness_Index increases with Fredoom",
       subtitle = "Freedom importance in the state of a country")+
  xlab("Happy Score / 10")+
  ylab("Freedom")+
  theme_bw()
#Exploring Healthy_life_exp against ladder_score

wh_report_2021 %>% 
  ggplot(aes(ladder_score,Healthy_life_exp))+
  geom_point(aes(color=regional_Ind))+
  labs(title = " Healthy_life_exp increases with Happiness_Index",
       subtitle = "Happiness_Index is an important factor Healthy_life")+
  xlab("Happy Score / 10")+
  ylab("Healthy_life")+
  theme_bw()










# Exploring GDP_Per_Capita against ladder_score
wh_report_2021 %>% 
  ggplot(aes(ladder_score,GDP_per_Capita))+
  geom_point(aes(color=regional_Ind))+
  labs(title = "Happiness_Index increases with GDP per Capita",
       subtitle = "Economic importance in the state of a country")+
  xlab("Happy Score / 10")+
  ylab("GDP per Capita")+
  theme_bw()



#EDA FOR Middle East and North Africa

#Arabian Peninsula:Bahrain, Kuwait, Oman, Qatar, Saudi Arabia, United Arab Emirates, Yemen.
#North Africa=c("Libya","Algeria","Tunisia","Egypt")
#Near East:Iraq, Israel, Jordan, Lebanon, Palestine, Syrian Arab Republic, Turkey.

#Categorizing the Middle East and North Africa countries into subregions

Arabian_Peninsula <- c("Bahrain","Kuwait","Saudi Arabia","United Arab Emirates","Yemen")

Near_East<- c("Iraq","Israel","Jordan","Lebanon","Palestinian Territories","Morocco","Turkey","Iran")

North_Africa<- c("Libya","Algeria","Tunisia","Egypt")

MiddleEast_NorthAfrica <- wh_report_2021 %>% filter(regional_Ind == "Middle East and North Africa") %>%
  select(country,ladder_score,GDP_per_Capita,Soc_support,Healthy_life_exp,Freedom,Corrupt_perception,Generosity) %>%
  mutate(MiddleEast_NorthAfrica_Subregion = case_when(
    country %in% Arabian_Peninsula ~ "Arabian Peninsula",
    country %in% Near_East ~ "Near East",
    country %in% North_Africa ~ "North Africa",
    TRUE ~ as.character(country)
  ))

#Visualization of countries in the subregions

MiddleEast_NorthAfrica %>%
  ggplot(aes(MiddleEast_NorthAfrica_Subregion))+
  geom_bar(aes(fill = country),alpha = 0.8)+
  labs(title = "Countries under Subregions")+
  xlab("Subregions")+
  ylab("No of countries")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))
#Exploration of Data Subregional

MiddleEast_NorthAfrica %>% mutate(MiddleEast_NorthAfrica_Subregion = fct_reorder(MiddleEast_NorthAfrica_Subregion,ladder_score,mean)) %>% 
  ggplot(aes(ladder_score))+
  geom_boxplot(aes(fill=MiddleEast_NorthAfrica_Subregion))+
  coord_flip()+
  labs(title = "Arabian Peninsula records the highest mean happiness Index")+
  xlab("Happiness_Score / 10")+
  ylab("")+
  theme_classic()

#Explore Healthy Life expectancy

MiddleEast_NorthAfrica %>% mutate(country = fct_reorder(country,Healthy_life_exp)) %>% 
  ggplot(aes(Healthy_life_exp,country))+
  geom_col(aes(fill=country))+
  labs(title = "Life expectancy does not significantly vary among the countries",
       subtitle ="Israel is an exception")+
  xlab("Healthy Life expectancy")+
  ylab("Country")+
  theme_bw()

#Explore GDP_Per_Capita

MiddleEast_NorthAfrica %>% mutate(country = fct_reorder(country,GDP_per_Capita)) %>% 
  ggplot(aes(GDP_per_Capita,country))+
  geom_col(aes(fill=country))+
  labs(title = "GDP Per Capita across the Middle East and North Africa",
       subtitle ="United Arab Emirates has the highest GDP Per Capita" )+
  xlab("GDP Per Capita")+
  ylab("Country")+
  theme_bw()+
  theme(legend.position = "right")

#Ladder_Scores amongst African countries

MiddleEast_NorthAfrica %>% mutate(country = fct_reorder(country,ladder_score)) %>% 
  ggplot(aes(ladder_score,country))+
  geom_col(aes(fill=country))+
  labs(title = "Israel has the highest Happiness Score")+
  xlab("Happiness_Score / 10")+
  ylab("Country")+
  geom_text(aes(label = round(ladder_score,1)),nudge_x =0.2)+
  theme_bw()


## PERCEPTION OF CORRUPTION

MiddleEast_NorthAfrica %>% mutate(country = fct_reorder(country,Corrupt_perception)) %>% 
  ggplot(aes(Corrupt_perception,country))+
  geom_col(aes(fill=country))+
  labs(title = "Lebanon & Iraq are perceived as most corrupt")+
  xlab("Corruption Perception")+
  ylab("Country")+
  geom_text(aes(label = round(ladder_score,1)),nudge_x =0.08)+
  theme_gray()
#FREEDOM

MiddleEast_NorthAfrica %>% mutate(country = fct_reorder(country,Freedom)) %>% 
  ggplot(aes(Freedom,country))+
  geom_col(aes(fill=country))+
  labs(title = "Lebanon & Iraq are perceived as most corrupt")+
  xlab("Corruption Perception")+
  ylab("Country")+
  geom_text(aes(label = round(ladder_score,1)),nudge_x =0.08)+
  theme_gray()



