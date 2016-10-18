# What: Software Carpentry Workshop
# When: October 18, 2016
# Who: John Erb-Downward
# Where: Hatcher Graduate Library, UM

#Packages necessary for analysis
library(dplyr)
library(tidyr)
library(ggplot2)
library(RSQLite)

# Using RSQLite
conn<-dbConnect(SQLite(),dbname="~/Desktop/Software_Carpentry/survey.sqlite")
tables<-dbListTables(conn)
surveys<-dbGetQuery(conn,'SELECT * FROM surveys')
head(surveys)
surveys<-dbGetQuery(conn,
                    'SELECT * FROM surveys
                    JOIN species ON surveys.species_id=species.species_id
                    JOIN plots ON surveys.plot_id=plots.plot_id;')
dbDisconnect(conn)
rm(conn)

## There are some duplicate columns so we are going to just load in ecology.csv
surveys<-read.csv("~/Desktop/Software_Carpentry/ecology.csv.txt",header=T)
colnames(surveys)
head(surveys)
class(surveys)
typeof(surveys) # Data representation

# examples of data type creation
df <- data.frame(x1=c(TRUE,FALSE,TRUE),x2=c(1,"red",2))
df
list(99,TRUE,"balloons") #mixed data types
str(surveys)
surveys["year"] # data frame with year vector
str(surveys)
levels(surveys$sex)
nlevels(surveys$sex)

spice<-factor(c("low","medium","low","high"),levels=c('low','medium','high'),ordered=T)
spice
# [1] low    medium low    high  
# Levels: low < medium < high

spice<-ordered(spice,levels=c('high','medium','low'))
max(spice)

tabulation<-table(surveys$taxa)
tabulation
barplot(tabulation)
levels(surveys$taxa)
sort(tabulation,decreasing=T)
barplot(sort(tabulation,decreasing=T))
# or
barplot(tabulation[c('Rodent','Bird','Rabbit','Reptile')])
# or
taxa2<-surveys$taxa
head(taxa2)
taxa2<-ordered(taxa2,levels=c("Rodent","Bird","Rabbit","Reptile"))
barplot(table(taxa2))

# Cross tabulation
table(surveys$year,surveys$taxa)
with(surveys,table(year,taxa))

# Question: What was the median weight of each rodent species between 1980 and 1990?
head(surveys$taxa=='Rodent')
dim(surveys[surveys$taxa=='Rodent',])
survey.sub<-surveys[surveys$year>=1980 & surveys$year<=1990,]
dim(survey.sub)
#surveys[surveys$year %in% seq.int(1980,1990)&survey$taxa=='Rodent',]  #Probably better
#surveys[(surveys$year >= 1980 & survey$year <= 1990 &survey$taxa=='Rodent',]
# | or symbol
# dplyr
output<-select(surveys,year,taxa,weight)
head(output)
filter(surveys, taxa=='Rodent')

#piplines
surveys %>% 
  filter(taxa == 'Rodent') %>%
  select(year,taxa,weight)

surveys.sub2<-surveys %>%
  filter((year %in% seq.int(1980,1990), taxa=='Rodent')) %>%    # comma assumes AND
  select(year,taxa,weight)
  
dim(surveys.sub2)

surveys %>% 
  mutate(weight_kg=weight/1000) %>%
  head()

#all.equal()  Good tool for matching output.


# Split, Apply, Combine Workflow
surveys %>%
  filter(!is.na(weight),taxa=="Rodent") %>%
  group_by(species_id) %>%
  summarize(med_weight=median(weight)) %>%
  print(n=25)

survey.complete<-surveys %>%
  filter(!is.na(weight),species_id!='',!is.na(hindfoot_length),sex!='',taxa=="Rodent")
  
common_species<-survey.complete %>%
  group_by(species_id) %>%
  tally() %>%
  filter (n >=50) %>%
  select (species_id)

common_surveys<-survey.complete %>%
  filter(species_id %in% common_species$species_id)

## ggplot2
ggplot(data=common_surveys,
       aes(x=weight,y=hindfoot_length,color=species_id))+
  geom_point()

