####### NAIVE METHOD FOR ESTIMATING VOTES OF PEOPLE IN CHARITY 2 DATABASE ######
###### LOADING LIBRARY ######

library(ggplot2)
library(reshape2)

###### LOADING DATA ######

## Load contacts data with zipcode and code_geo 
contacts <- read.csv('charity2_contacts.csv',sep=';')

# Remove useless columns of contacts 

contacts$id <- NULL
contacts$prefix_id <- NULL
contacts$birth_date <- NULL
contacts$age <- NULL
contacts$first_name_clean <- NULL

## Load 2012 first round french presidential election data, for each city in France, the distribution of votes for 
## the different candidates


Election_2012_with_INSEE <- read.csv("Election_2012_with_INSEE_ROUND1.csv",sep=',')


## Build what we need : for each city, the distribution of the votes for the candidates

Election <- Election_2012_with_INSEE[,c("Code.INSEE","Code.du.département","Libellé.du.département",
                                        "Code.de.la.commune","Libellé.de.la.commune", "Inscrits", 
                                        "Abstentions", "Votants", "Blancs.et.nuls", "Exprimés")]

Election["JOLY"] <- Election_2012_with_INSEE[,20]
Election["LE PEN"] <- Election_2012_with_INSEE[,26]
Election["SARKOZY"] <- Election_2012_with_INSEE[,32]
Election["MELENCHON"] <- Election_2012_with_INSEE[,38]
Election["POUTOU"] <- Election_2012_with_INSEE[,44]
Election["ARTHAUD"] <- Election_2012_with_INSEE[,50]
Election["CHEMINADE"] <- Election_2012_with_INSEE[,56]
Election["BAYROU"] <- Election_2012_with_INSEE[,62]
Election["DUPONT-AIGNAN"] <- Election_2012_with_INSEE[,68]
Election["HOLLANDE"] <- Election_2012_with_INSEE[,74]

rm(Election_2012_with_INSEE)

## Load the correspondance table between zip code and insee code

zip_insee_code <- read.csv('correspondance-code-insee-code-postal.csv',sep=',')

## Merge Election table with zip_insee_code to have zip code in Election table 

Election <- merge(Election,zip_insee_code[,c('Code.INSEE','Code.Postal')],on='Code.INSEE')

###### ELECTION TABLE PROCESSING ###### 

## for each person of the charity base, we attribute the one who gets the most votes in its city

# Merge contacts and its city votes during first round 
Election_res <- Election[,c('Code.Postal','Inscrits',
                     'Abstentions','Blancs.et.nuls',
                     'JOLY','LE PEN','SARKOZY','MELENCHON','POUTOU','ARTHAUD','CHEMINADE',
                     'BAYROU','DUPONT-AIGNAN','HOLLANDE')]

Election_agg_by_zipcode = aggregate(Election_res[,c('Inscrits',
                       'Abstentions','Blancs.et.nuls',
                       'JOLY','LE PEN','SARKOZY','MELENCHON','POUTOU','ARTHAUD','CHEMINADE',
                       'BAYROU','DUPONT-AIGNAN','HOLLANDE')],by=list(CodePostal=Election_res$Code.Postal), FUN=sum)


# Some cities are represented by different zip code, e.g Nice with 06000/06100/06200/06300



new_table = c()
for (i in c(1:dim(Election_agg_by_zipcode)[1]))
{
  list_zip = unlist(strsplit(as.character(Election_agg_by_zipcode[i,1]),'/'))
  
  for (zip_code in list_zip)
  {
    new_table = rbind(new_table,c(zip_code,as.numeric(Election_agg_by_zipcode[i,c(2:14)])))
  }
  
}

# This table separates the different values after grouping 
# e.g before we had one line for Nice corresponding to '06000/06100/06200/06300' 
# now we have 4 lines, each one corresponding to one of these zip codes 
# This will allow us to join more easily with the contact table 

new_Election_agg_by_zipcode  = data.frame(new_table)
colnames(new_Election_agg_by_zipcode) <- colnames(Election_agg_by_zipcode)


###### NAIVE COUNT ######

contacts_votes <- merge(contacts,new_Election_agg_by_zipcode[,c('CodePostal','Inscrits',
                                             'Abstentions','Blancs.et.nuls',
                                             'JOLY','LE PEN','SARKOZY','MELENCHON','POUTOU','ARTHAUD','CHEMINADE',
                                             'BAYROU','DUPONT-AIGNAN','HOLLANDE')],by.x='zip_code',by.y='CodePostal')


# After merging, only 63994 remaining people, the other one did not live in France 
# or zipcode are not present in the election results table

# Then for each person, assign the candidate who had the most votes in the corresponding city 

votes = colnames(contacts_votes[,c(5:16)])[apply(contacts_votes[,c(5:16)],1,which.max)]

final_charity_votes = cbind(contacts_votes[,c(1:3)],votes)


# Distribution of the votes for our charity database

nb_abs = as.numeric(table(final_charity_votes$votes)[1])

proportion_with_abs = table(final_charity_votes$votes)/dim(final_charity_votes)[1]
proportion_without_abs = (table(final_charity_votes$votes)/(dim(final_charity_votes)[1]-nb_abs))[2:6]
# On our target population, we had 11.6 % abstentions
# For the expressed votes, Hollande got 50.64 % of the votes, Sarkozy 44.07 %, Le Pen 5.14 %, Melenchon 0.065 % 
# Dupont Aignan : 0.085 % and nothing for the others candidates

# To compare with the true distribution on the whole french population : 
# Hollande : 28.63 %, Sarkozy : 27.18 %, Le Pen : 17.9 %, Melenchon : 11.1 %, Dupont Aignan : 1.79 % 

# Plot true vs estimated with our target population

data = data.frame(rbind(c('True',28.63,27.18,17.9,11.1,1.79,20.52),
             c('Estimated',50.64,44.07,5.14,0.065,0.085,11.6)))
colnames(data) = c('model','Hollande','Sarkozy','Le Pen','Melenchon','Dupont-Aignan','Abs')

melt_data <-melt(data,id.vars="model")
melt_data$value <- as.numeric(melt_data$value)

bar = ggplot(melt_data,aes(x=variable,y=value,fill=model))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)+
  xlab("Candidates")+ylab("Percentage of votes") + labs(title='Percentages of votes for candidates Targeted Population with Naive Count')


## CONCLUSION 
# We don't know if our target population in the charity database significantly represents the whole french population 
# However, with the naive method, we do not fin results with the reference population, so our database is surely not
# representative of the whole french population.


bar <- ggplot()
