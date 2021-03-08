


# C. McClintock 
# March 2021 
# ACLU inquiry 

# .............................................................................

# load libraries
library(tidyverse)
library(extrafont)
library(Cairo)
library(RColorBrewer)
library(scales)

# load fonts and set theme
loadfonts()
theme <- theme(text = element_text(family = "Source Sans Pro"),
               legend.position = "bottom", panel.grid.minor.x=element_blank())

# .............................................................................

depop <- read_csv("data/depop-specific.csv", n_max = 47)

depop$County <- paste0(depop$County, " County")


jan <- select(depop, 1, 3,)
depop <- select(depop, 1, `Mar26`:`Dec 31`)


pop <- read_csv("data/pop.csv", skip=2)[,1:3]
names(pop) <- str_to_lower(gsub(" ", "", names(pop)))

pop <- arrange(pop, -estimatedpopulation)

depop <- subset(depop, County %in% pop[2:12,]$jurisdictionbygeography)

depop <- gather(depop, `Mar26`:`Dec 31`, key="date", value="count")

covid_adp <- depop %>% group_by(County) %>% summarize(`Mar-Dec ADP`=mean(count, na.rm=T))

covid_adp <- left_join(covid_adp, jan, by="County")

covid_adp$changeinjailpop <- ((covid_adp$`Mar-Dec ADP`/covid_adp$`ADP in January`)-1)

jailpop <- select(covid_adp, 1,4)
names(jailpop) <- c("county", "perc_jailpop")

# .............................................................................


county <- read_csv("data/county-year.csv", skip=3)[,1:3]


names(county) <- str_to_lower(gsub(" ", "", names(county)))
county <- spread(county, key="incidentdate", value="numberofcrimes")
county <- county[,1:3]
county$perc_crime <- (county$`2020`/county$`2019`) -1

change <- left_join(jailpop, county[,c(1,4)], by=c("county"="jurisdictionbygeography"))

change <- gather(change, perc_jailpop:perc_crime, key="metric", value="perc")
change <- mutate(change, 
                 metric=fct_recode(metric, 
                                   "% Change in Jail Population"="perc_jailpop", 
                                   "% Change in Crime (2019-2020)"="perc_crime"))


change <- left_join(change, pop, by=c("county"="jurisdictionbygeography"))
change$county <- paste0(change$county, " (", prettyNum(change$estimatedpopulation, big.mark=","), ")")

ggplot(subset(change, estimatedpopulation>100000), aes(x=reorder(county, -estimatedpopulation), y=perc, fill=metric)) + 
  geom_bar(stat="identity", position="dodge", width = 0.7) +
  theme_minimal() + theme +
  scale_x_discrete(labels=wrap_format(10))  +
  labs(title="Colorado Change in Jail Population and Number of Crimes by County, 2019 to 2020", 
       x="", y="% Change", fill="",
       caption="Data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021 \nAnnotations below county name give 2020 estimated county population.") +
  scale_y_continuous(labels=percent, limits=c(-0.6, 0.6)) +
  geom_text(aes(label=paste0(round(100*perc), "%")), family="Source Sans Pro", 
            position = position_dodge(width = 0.7), size=4, vjust="outward") +
  geom_hline(aes(yintercept=0))

g <- ggplot(subset(change, estimatedpopulation>100000), aes(x=reorder(county, -estimatedpopulation), y=perc, fill=metric)) + 
  geom_bar(stat="identity", position="dodge", width = 0.7) +
  theme_minimal() + theme +
  scale_x_discrete(labels=wrap_format(10))  +
  labs(title="No association between change in jail population and change in crime",
       subtitle="Colorado Percent Change in Jail Population and Percent Change in Number of Crimes by County \n2019 to 2020, for 11 largest counties collectively representing 85% of Colorado's population", 
       x="", y="% Change", fill="",
       caption="2019 and 2020 crime data from Colorado Crime Stats by the Colorado Bureau of Investigation, retrieved 6 March 2021 
       2020 jail population data from individual county reporting in response to ACLU requests. % change in jail population 
       compares average daily population from January 2020 to data from March 24th to December 31st of 2020. 
       Annotations below county name give 2020 estimated county population.") +
  scale_y_continuous(labels=percent, limits=c(-0.8, 0.6)) +
  geom_text(aes(label=paste0(round(100*perc), "%")), family="Source Sans Pro", 
            position = position_dodge(width = 0.7), size=5, vjust="outward") +
  geom_hline(aes(yintercept=0)) + 
  theme(plot.title=element_text(face="bold", size = 16), 
        plot.subtitle=element_text(size = 12), 
        legend.text=element_text(size = 12), 
        axis.text.x=element_text(size = 10)) 
  
g + annotate("text", x=8, y=0.5, label="Larimer County and Douglas County also had similar decreases \nin jail population but crime decreased by 13% in Larimer County \nand increased by 11% in Douglas County.", 
           size=4, family="Source Sans Pro", lineheight = 1) +
  geom_segment(aes(x=6.5,xend=6, y=0.38,yend=0.025), size=0.25)+
  geom_segment(aes(x=6.8,xend=6.8, y=0.38,yend=0.17), size=0.25)+ 
  theme(plot.title=element_text(face="bold")) + 
  annotate("text", x=7, y=-0.65, label="Jefferson County had the greatest decrease \nin jail population and saw no change in overall crime.", 
           size=4, family="Source Sans Pro", lineheight = 1) +
  geom_segment(aes(x=5.1, xend=4.3, y=-0.62,yend=-0.58), size=0.25) + 
  annotate("text", x=3, y=0.5, label="El Paso County and Arapahoe County had similar decreases \nin jail population but crime decreased by 2% in El Paso County \nand increased by 9% in Arapahoe County.", 
           size=4, family="Source Sans Pro", lineheight = 1) +
  geom_segment(aes(x=2.5,xend=2, y=0.38,yend=0.025), size=0.25)+
  geom_segment(aes(x=2.8,xend=2.8, y=0.38,yend=0.17), size=0.25) +
theme(plot.caption=element_text(face="italic"))


arb <- subset(change, estimatedpopulation>100000)[1:11,]
sum(arb$estimatedpopulation) # 4888569







