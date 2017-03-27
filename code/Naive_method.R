####### NAIVE METHOD FOR ESTIMATING VOTES OF PEOPLE IN CHARITY 2 DATABASE ######
###### LOADING LIBRARY ######

library(ggplot2)
library(reshape2)

###### LOADING DATA ######

## Load contacts data with zipcode and code_geo 
contacts <- read.csv('data/charity2_contacts.csv',sep=';')

# Remove useless columns of contacts 

contacts$id <- NULL
contacts$prefix_id <- NULL
contacts$birth_date <- NULL
contacts$age <- NULL
contacts$first_name_clean <- NULL

## Load 2012 first round french presidential election data, for each city in France, the distribution of votes for 
## the different candidates


Election_2012_with_INSEE <- read.csv("data/Election_2012_with_INSEE_Round1.csv",sep=',')


## Build what we need : for each city, the distribution of the votes for the candidates

Election <- Election_2012_with_INSEE[,c("Code.INSEE","Code.du.département","Libellé.du.département",
                                        "Code.de.la.commune","Libellé.de.la.commune", "Inscrits", 
                                        "Abstentions", "Votants", "Blancs.et.nuls", "Exprimés")]

Election["JOLY"] <- Election_2012_with_INSEE[,22]
Election["LE PEN"] <- Election_2012_with_INSEE[,28]
Election["SARKOZY"] <- Election_2012_with_INSEE[,34]
Election["MELENCHON"] <- Election_2012_with_INSEE[,40]
Election["POUTOU"] <- Election_2012_with_INSEE[,46]
Election["ARTHAUD"] <- Election_2012_with_INSEE[,52]
Election["CHEMINADE"] <- Election_2012_with_INSEE[,58]
Election["BAYROU"] <- Election_2012_with_INSEE[,64]
Election["DUPONT-AIGNAN"] <- Election_2012_with_INSEE[,70]
Election["HOLLANDE"] <- Election_2012_with_INSEE[,76]


rm(Election_2012_with_INSEE)


# Add 0 to code insee in Election table when the initial insee code has only 4 digits, for merge purpose

add_0toinsee <- function(code)
{
  if (length(unlist(strsplit(code,''))) == 4){
    
    return(paste('0',code,sep=''))
  }else {
    return(code)
  }
}

insee <- as.character(Election$Code.INSEE)
transformed_insee <- as.vector(sapply(insee,add_0toinsee))

# change the insee column of Election 

Election$Code.INSEE <- transformed_insee


###### NAIVE COUNT ######

# Merge contacts and its city votes during first round, we are merging on code_geo and Code.INSEE

contacts_votes <- merge(contacts, Election[,c('Code.INSEE','Inscrits',
                                                                'Abstentions','Blancs.et.nuls',
                                                                'JOLY','LE PEN','SARKOZY','MELENCHON','POUTOU','ARTHAUD','CHEMINADE',
                                                                'BAYROU','DUPONT-AIGNAN','HOLLANDE')],by.x='code_geo',by.y='Code.INSEE')


# After merging, 74 302 remaining people, the other one did not live in France 
# or zipcode are not present in the election results table ( like DOM TOM)

# Then for each person, assign the candidate who had the most votes in the corresponding city 

votes = colnames(contacts_votes[,c(4:15)])[apply(contacts_votes[,c(4:15)],1,which.max)]

final_charity_votes = cbind(contacts_votes[,c(1:3)],votes)


# Distribution of the votes for our charity database

nb_abs = as.numeric(table(final_charity_votes$votes)[1])

proportion_with_abs = table(final_charity_votes$votes)/dim(final_charity_votes)[1]
proportion_without_abs = (table(final_charity_votes$votes)/(dim(final_charity_votes)[1]-nb_abs))[-1]

# On our target population, we had 13.36 % abstentions
# For the expressed votes, Hollande got 52.96 % of the votes, Sarkozy 40.73 %, Le Pen 5.94 %, Melenchon 0.24 % 
# Dupont Aignan : 0.075 %, Bayrou : 0.038 %, Joly : 0.0031 %, Blank votes : 0.0016 %and nothing for the others candidates

# To compare with the true distribution on the whole french population : 
# Hollande : 28.63 %, Sarkozy : 27.18 %, Le Pen : 17.9 %, Melenchon : 11.1 %, Dupont Aignan : 1.79 % 

# Plot true vs estimated with our target population

data = data.frame(rbind(c('True',28.63,27.18,17.9,11.1,9.13,2.31,1.79,1.52,20.52),
             c('Estimated',52.96,40.73,5.94,0.24,0.075,0.039,0.0031,0.0016,13.36)))
colnames(data) = c('model','Hollande','Sarkozy','Le Pen','Melenchon','Bayrou','Joly','Dupont-Aignan','Blank','Abs')

melt_data <-melt(data,id.vars="model")
melt_data$value <- as.numeric(melt_data$value)

bar = ggplot(melt_data,aes(x=variable,y=value,fill=model))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)+
  xlab("Candidates")+ylab("Percentage of votes") + labs(title='Percentages of votes for candidates Targeted Population with Naive Count')

plot(bar)

## CONCLUSION 
# We don't know if our target population in the charity database significantly represents the whole french population 
# However, with the naive method, we do not fin results with the reference population, so our database is surely not
# representative of the whole french population.



