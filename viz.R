
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

# read in data
co20 <- read_csv("co-2020.csv", skip=3)
co19 <- read_csv("co-2019.csv", skip=3)
type20 <- read_csv("co-2020-bytype.csv", skip=3)[,1:3]
type19 <- read_csv("co-2019-bytype.csv", skip=3)[,1:3]

# clean names
names(co20) <- str_to_lower(gsub(" ", "", names(co20)))
names(co19) <- str_to_lower(gsub(" ", "", names(co19)))
names(type19) <- str_to_lower(gsub(" ", "", names(type19)))
names(type20) <- str_to_lower(gsub(" ", "", names(type20)))
names(co20) <- gsub("[^[:alnum:] ]", "", names(co20))
names(co19) <- gsub("[^[:alnum:] ]", "", names(co19))

# add year column to data
co20$year <- 2020
co19$year <- 2019
type20$year <- 2020
type19$year <- 2019

# .............................................................................

# NUMBER OF CRIMES, 2019 VS. 2020

# select counts and join both years
count <- full_join(select(co19, jurisdictionbygeography, numberofcrimes, year), 
                   select(co20, jurisdictionbygeography, numberofcrimes, year), 
                   by=c("jurisdictionbygeography", "numberofcrimes", "year"))

# drop the colorado total
count <- subset(count, !jurisdictionbygeography=="Colorado")

# visualize
ggplot(count, aes(reorder(jurisdictionbygeography, numberofcrimes), 
                  numberofcrimes, fill=as.factor(year))) + 
  geom_bar(stat="identity", position="dodge") + coord_flip() +
  geom_text(aes(label=prettyNum(numberofcrimes, big.mark=",")), 
            position = position_dodge(width = 1), size=2.5, hjust=-.25) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 80000)) + theme_minimal() + 
  labs(y="Number of Crimes", x="County", fill="Year", 
       title="Number of Crimes in Fourteen Largest CO Counties, 2019 vs. 2020", 
       caption="Data from Colorado Crime States by the Colorado Bureau of Investigation") + 
  theme + scale_fill_manual(values=brewer.pal(9, "Greens")[c(3,7)])

# pdf("event-attend-type-extra.pdf", family="Source Sans Pro", width=7, height=8)
# dev.off()
# embed_fonts("event-attend-type-extra.pdf", outfile="event-attend-type-extra.pdf")

# .............................................................................

# CRIME RATE, 2019 VS. 2020

# select crime rate and join
rate <- full_join(select(co19, jurisdictionbygeography, crimerateper100000, year), 
                   select(co20, jurisdictionbygeography, crimerateper100000, year), 
                   by=c("jurisdictionbygeography", "crimerateper100000", "year"))

# drop colorado
rate <- subset(rate, !jurisdictionbygeography=="Colorado")

# visualize
ggplot(rate, aes(reorder(jurisdictionbygeography, crimerateper100000), 
                 crimerateper100000, fill=as.factor(year))) + 
  geom_bar(stat="identity", position="dodge") + coord_flip() +
  geom_text(aes(label=prettyNum(round(crimerateper100000), big.mark=",")), 
            position = position_dodge(width = 1), size=2.5, hjust=-.25) +
  theme_minimal() + scale_y_continuous(labels = scales::comma, limits = c(0, 15000)) +
  labs(y="Crime Rate (per 100,000)", x="County", fill="Year", 
       title="Crime Rate per 100,000 in Fourteen Largest CO Counties, 2019 vs. 2020", 
       caption="Data from Colorado Crime States by the Colorado Bureau of Investigation") + 
  theme + scale_fill_manual(values=brewer.pal(9, "Greens")[c(3,7)])

# .............................................................................

# comparison - total crime 

type <- full_join(type19, type20, by=names(type19))

type_co <- subset(type, jurisdictionbygeography=="Colorado" & 
                    !offensetype %in% c("Missing", "Non-Reportable NIBRS Offense"))

ggplot(type_co, aes(factor(year), y=numberofcrimes, fill=offensetype)) + 
  geom_bar(stat="identity") + theme_minimal() + theme +
  annotate("text", x=c(1,2), y=c(350000,365000), label=c("340,228", "353,367"), family = "Source Sans Pro")+ 
  theme + scale_fill_manual(values=brewer.pal(9, "Greens")[c(4,5,6)])+
  labs(y="Number of Crimes", x="Year", fill="Offense Type", 
       title="Colorado Number of Crimes, 2019 vs. 2020", 
       subtitle="Crimes against society decreased from 2019 to 2020, crimes against persons stayed the same, \nincrease in property crime accounts for the entirety of the increase in crime from 2019 to 2020",
       caption="Data from Colorado Crime States by the Colorado Bureau of Investigation")+
  scale_y_continuous(labels = scales::comma) + 
  annotate("text", x=c(1,1,1), y=c(310000, 175000, 22000), label=c("64,104","233,807","42,317"), family = "Source Sans Pro")+ 
  annotate("text", x=c(2,2,2), y=c(320000, 180000, 15000), label=c("64,117", "257,982", "31,268"), family = "Source Sans Pro")

# add descriptive labels?
# crimes against society decreased, crimes against persons stayed the same, only 
# property crime increased 

# .............................................................................

ggplot(type_co, aes(offensetype, y=numberofcrimes, fill=factor(year))) + 
  geom_bar(stat="identity", position = "dodge") + theme_minimal()+
  theme + scale_fill_manual(values=brewer.pal(9, "Greens")[c(4,6)])+
  labs(y="Number of Crimes", x="Year", fill="Offense Type", 
       title="Colorado Crimes by Offense Type, 2019 vs. 2020", 
       subtitle="Crimes against society decreased from 2019 to 2020, crimes against persons stayed the same, \nincrease in property crime accounts for the entirety of the increase in crime from 2019 to 2020",
       caption="Data from Colorado Crime States by the Colorado Bureau of Investigation") +
  geom_text(aes(label=prettyNum(numberofcrimes, big.mark=",")), 
            position = position_dodge(width = 1), size=3.5, vjust=-.45)+
  scale_y_continuous(labels = scales::comma, limits = c(0, 300000)) 

# .............................................................................

type <- subset(type, !jurisdictionbygeography=="Colorado" & 
                    !offensetype %in% c("Missing", "Non-Reportable NIBRS Offense"))

ggplot(type, aes(x=offensetype, y=numberofcrimes, fill=factor(year))) +
  geom_bar(stat="identity", position="dodge")  + 
  facet_wrap(~jurisdictionbygeography, scales = "free_y", ncol=2) + theme_minimal() +
  theme + scale_fill_manual(values=brewer.pal(9, "Greens")[c(4,6)]) +
  scale_x_discrete(labels=wrap_format(15))+
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label=prettyNum(numberofcrimes, big.mark=",")), 
            position = position_dodge(width = 1), size=2.5, vjust=1.4)+
  labs(y="Number of Crimes", fill="Year", x="Offense Type", 
       title="Colorado Crimes by County and Offense Type, 2019 vs. 2020", 
       caption="Data from Colorado Crime States by the Colorado Bureau of Investigation") 


# .............................................................................

type_wide <- spread(type, key="year", value="numberofcrimes")
type_wide$perc <- (type_wide$`2020`/type_wide$`2019`)-1
type_wide$diff <- (type_wide$`2020`-type_wide$`2019`)


person_wide <- subset(type_wide, offensetype=="Crimes Against Person")

ggplot(person_wide, aes(reorder(jurisdictionbygeography, perc), perc, fill=perc)) + 
  geom_bar(stat="identity") + theme_minimal() + theme +
  coord_flip() + 
  labs(title="Percent Change in Crimes Against Persons, 2019 to 2020", x="County", y="% Change", 
       caption="Data from Colorado Crime States by the Colorado Bureau of Investigation
       Note: Adams County reported exactly the same number of violent crimes (14,018) in 2019 and 2020, 
       which suggest a possible error in source data.") +
  scale_fill_gradient2(low = "#2166ac", mid = "#e4eff5", high="#b2182b") +
  scale_y_continuous(labels=percent, limits=c(-0.23, 0.2)) + guides(fill=F)+
  geom_text(aes(label=paste0(prettyNum(round(100*perc, 1)), "%")), 
            position = position_dodge(width = 1), size=2.9, hjust=1.2) +
  annotate("text", x=c(1:14), y=rep(0.2, 14), 
           label=paste0("n = ", arrange(person_wide, perc)$diff), 
           family="Source Sans Pro", size=2.8)

# .............................................................................


ggplot(type_wide, aes(offensetype, perc, fill=perc)) + 
  geom_bar(stat="identity", position="dodge") + theme_minimal() + theme +
  facet_wrap(~jurisdictionbygeography, ncol=2)+
  scale_fill_gradient2(low = "#2166ac", mid = "#e4eff5", high="#b2182b") +
  scale_y_continuous(labels=percent, limits=c(-0.65, 0.3)) + guides(fill=F) +
  labs(title="Percent Change in Number of Crimes by Offense Type, 2019 to 2020", 
       x="", y="% Change",
       caption="Data from Colorado Crime States by the Colorado Bureau of Investigation
       Note: Adams County reported exactly the same number of violent crimes (14,018) in 2019 and 2020, 
       which suggest a possible error in source data.")+
  scale_x_discrete(labels=wrap_format(15), position="top") +
  geom_text(aes(label=paste0(round(100*perc, 1), "%", " (", diff, ")")), 
            position = position_dodge(width = 1), size=2.5, vjust=1.2)

ggplot(subset(type_wide, jurisdictionbygeography=="Boulder County"), 
       aes(offensetype, perc, fill=perc)) + 
  geom_bar(stat="identity", position="dodge") +
  labs(title="Boulder County Change in Crime by Offense Type, 2019 to 2020", 
       x="Offense Type", y="% Change",
       caption="Data from Colorado Crime States by the Colorado Bureau of Investigation") +
  geom_text(aes(label=paste0(round(100*perc, 1), "%", " (", diff, ")")), 
            position = position_dodge(width = 1), size=4, vjust=1.2, 
            family="Source Sans Pro") +
  guides(fill=F)+
  scale_y_continuous(labels=percent, limits=c(-0.4, 0.4)) +
  geom_hline(aes(yintercept=0))

  
# .............................................................................


jailpop <- read_csv("changeinjailpop.csv")
jailpop$perc_jailpop <- as.numeric(gsub("%", "", jailpop$changeinjailpop))/100

jailpop <- jailpop[,c(1, 3)]

county <- read_csv("county-year.csv", skip=3)[,1:3]


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

pop <- read_csv("pop.csv", skip=2)[,1:3]
names(pop) <- str_to_lower(gsub(" ", "", names(pop)))

change <- left_join(change, pop, by=c("county"="jurisdictionbygeography"))
change$county <- paste0(change$county, " (", prettyNum(change$estimatedpopulation, big.mark=","), ")")

ggplot(change, aes(x=reorder(county, -estimatedpopulation), y=perc, fill=metric)) + 
  geom_bar(stat="identity", position="dodge", width = 0.7) +
  theme_minimal() + theme +
  scale_x_discrete(labels=wrap_format(10))  +
  labs(title="Colorado Change in Jail Population and Number of Crimes by County, 2019 to 2020", 
       x="", y="% Change", fill="",
       caption="Data from Colorado Crime States by the Colorado Bureau of Investigation \nAnnotations below county name give 2020 estimated county population.") +
  scale_y_continuous(labels=percent, limits=c(-0.6, 0.6)) +
  geom_text(aes(label=paste0(round(100*perc), "%")), family="Source Sans Pro", 
            position = position_dodge(width = 0.7), size=3, vjust=1.2) +
  geom_hline(aes(yintercept=0))



# .............................................................................

ctype <- read_csv("county-type.csv", skip=3)[,1:4]

names(ctype) <- str_to_lower(gsub(" ", "", names(ctype)))
ctype <- spread(ctype, key="incidentdate", value="numberofcrimes")
ctype <- ctype[,1:4]
ctype$perc_crime <- (ctype$`2020`/ctype$`2019`)-1
ctype$diff <- (ctype$`2020` - ctype$`2019`)

diff <- select(ctype, jurisdictionbygeography, offensetype, diff)

ctype <- pivot_wider(ctype[,c(1,2,5)], id_cols="jurisdictionbygeography", names_from="offensetype", values_from=c("perc_crime"))

jail <- left_join(jailpop, ctype, by=c("county"="jurisdictionbygeography"))
jail <- gather(jail, perc_jailpop:`Crimes Against Society`, key="metric", value="perc")
jail <- left_join(jail, diff, by=c("county"="jurisdictionbygeography", "metric"="offensetype"))

jail <- mutate(jail, 
                  metric=fct_recode(metric, 
                                    "% Change in Jail Population"="perc_jailpop", 
                                    "% Change in Violent Crime"="Crimes Against Person", 
                                    "% Change in Property Crime"="Crimes Against Property", 
                                    "% Change in Other Crime"="Crimes Against Society"))

jail$metric <- fct_relevel(jail$metric, "% Change in Jail Population")

pop <- read_csv("pop.csv", skip=2)[,1:3]
names(pop) <- str_to_lower(gsub(" ", "", names(pop)))

jail <- left_join(jail, pop, by=c("county"="jurisdictionbygeography"))
jail$county <- paste0(jail$county, " (", prettyNum(jail$estimatedpopulation, big.mark=","), ")")

ggplot(subset(jail, !county %in% c("Washington County (4,899)", "La Plata County (56,721)")), 
       aes(x=reorder(county, -estimatedpopulation), y=perc, fill=metric)) + 
  geom_bar(stat="identity", position="dodge", width = 0.8) +
  theme_minimal() + theme  +
  scale_x_discrete(labels=wrap_format(10)) +
  labs(title="Colorado Change in Jail Population and Number of Crimes by County and Offense Type, 2019 to 2020", 
       x="", y="% Change", fill="",
       caption="Data from Colorado Crime States by the Colorado Bureau of Investigation \nAnnotations below county name give 2020 estimated county population.") +
  scale_y_continuous(labels=percent, limits=c(-0.6, 0.6)) +
  geom_text(aes(label=paste0(round(100*perc), "%")), family="Source Sans Pro",
            position = position_dodge(width = 0.8), size=2.3, vjust=1.2) +
  geom_hline(aes(yintercept=0)) + 
  scale_fill_manual(values=brewer.pal(9, "Spectral")[c(9,6,7,8)])


ggplot(subset(jail, !county %in% c("Washington County (4,899)", "La Plata County (56,721)")), aes(x=county, y=perc, fill=metric)) + 
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() + theme + theme(axis.text.x=element_blank()) +
  scale_x_discrete(labels=wrap_format(10)) +
  labs(title="Colorado Change in Jail Population and Number of Crimes by County and Offense Type, 2019 to 2020", 
       x="", y="% Change", fill="",
       caption="Data from Colorado Crime States by the Colorado Bureau of Investigation \nAnnotations below county name give 2020 estimated county population.") +
  scale_y_continuous(labels=percent, limits=c(-0.6, 0.6)) +
  geom_text(aes(label=paste0(round(100*perc), "%")), family="Source Sans Pro",
            position = position_dodge(width = 0.8), size=2.3, vjust=1.2) +
  geom_hline(aes(yintercept=0)) + facet_wrap(~reorder(county, -estimatedpopulation), nrow=2, scales="free_x") +
  scale_fill_manual(values=brewer.pal(9, "Spectral")[c(9,6,7,8)])

# recommend removing washington and la plata, numbers are too small

pop <- arrange(pop, -estimatedpopulation)

# I had Eagle and not La Plata in the original ones
pop[1:15,]$jurisdictionbygeography[!pop[1:15,]$jurisdictionbygeography %in% co19$jurisdictionbygeography]
co19$jurisdictionbygeography[!co19$jurisdictionbygeography %in% pop[1:15,]$jurisdictionbygeography]

jailpop$county[!jailpop$county %in% co19$jurisdictionbygeography]
co19$jurisdictionbygeography[!co19$jurisdictionbygeography %in% jailpop$county] 

# .............................................................................

# TO DO: 
# crimes in CO by month, 2019 vs. 2020?


