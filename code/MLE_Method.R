
### set working directory to hw3

###### LOADING LIBRARY ######

library(ggplot2)
library(reshape2)

# Loading data


# Load contacts data with zipcode and code_geo 
contacts <- read.csv('data/charity2_contacts.csv',sep=';')

# Remove useless columns of contacts 

contacts$id <- NULL
contacts$prefix_id <- NULL
contacts$birth_date <- NULL
contacts$age <- NULL
contacts$first_name_clean <- NULL

Election_2012_with_INSEE <- read.csv("data/Election_2012_with_INSEE_Round1.csv",sep=',')
# Creation of BDD

Election <- Election_2012_with_INSEE[,c("Code.INSEE","Code.du.département","Libellé.du.département","Code.de.la.commune","Libellé.de.la.commune", "Inscrits", "Exprimés", "Votants", "Blancs.et.nuls", "Abstentions")]
Election["JOLY"] <- Election_2012_with_INSEE[,22]
Election["LE PEN"] <- Election_2012_with_INSEE[,28]
Election["SARKOZY"] <- Election_2012_with_INSEE[,34]
Election["MÉLENCHON"] <- Election_2012_with_INSEE[,40]
Election["POUTOU"] <- Election_2012_with_INSEE[,46]
Election["ARTHAUD"] <- Election_2012_with_INSEE[,52]
Election["CHEMINADE"] <- Election_2012_with_INSEE[,58]
Election["BAYROU"] <- Election_2012_with_INSEE[,64]
Election["DUPONT-AIGNAN"] <- Election_2012_with_INSEE[,70]
Election["HOLLANDE"] <- Election_2012_with_INSEE[,76]


# Creation of counts per vote type 
results <- c(sum(Election["Blancs.et.nuls"]),sum(Election["Abstentions"]), sum(Election["JOLY"]), sum(Election["LE PEN"]), sum(Election["SARKOZY"]), sum(Election["MÉLENCHON"]), sum(Election["POUTOU"]), sum(Election["ARTHAUD"]), sum(Election["CHEMINADE"]), sum(Election["BAYROU"]), sum(Election["DUPONT-AIGNAN"]), sum(Election["HOLLANDE"]))
counts <- cbind(colnames(Election[,9:20]),results)

# Creation of weights : creation function MLE & optimization of MLE with our votes BDD
Ydonné = t(Election[,c("Blancs.et.nuls","Abstentions","JOLY","LE PEN","SARKOZY","MÉLENCHON","POUTOU","ARTHAUD","CHEMINADE","BAYROU","DUPONT-AIGNAN","HOLLANDE")])
mle_Ydonné <- function(w0){
  w <- c(w0[1],0,w0[2:11])
  
  # Facteur 
  fact <- results%*%exp(w)
  
  # Creation BDD modifée avec poids
  Elections_points <- matrix(nrow = nrow(Election), ncol = 0)
  for(i in 1:12){
    col <- Election[,i+8]*exp(w[i])/fact
    Elections_points = cbind(Elections_points, col)
  }
  Elections_points <- cbind(Election["Code.INSEE"],Elections_points)
  
  # L creation 
  L <- rowSums(Elections_points[,c(2:13)])
  logL <- log(L)*Election_2012_with_INSEE[,"Pop.charity.per.code.postal"]
  Elections_points <- cbind(Elections_points, L, logL)
  colnames(Elections_points)[14:15] <- c("L","logL")
  logLrouge <- sum(logL)
  
  return(-logLrouge)
}

winit0 <- rep(1,11)
optim_weights = optim(winit0, mle_Ydonné) 
weights = optim_weights$par
weights = c(weights[1],0,weights[2:11])


# BDD
Elections_points <- matrix(nrow = nrow(Election), ncol = 0)
for(i in 1:12){
    col <- Election[,i+8]*exp(weights[i])/fact
    Elections_points = cbind(Elections_points, col)
}
Elections_points <- cbind(Election["Code.INSEE"],Elections_points)


# Export results
weightsNames <- cbind(c("Blancs et nuls","Abstentions","JOLY","LE PEN","SARKOZY","MÉLENCHON","POUTOU","ARTHAUD","CHEMINADE","BAYROU","DUPONT-AIGNAN","HOLLANDE"), weights)
prop <- exp(weights)/sum(exp(weights))
weightsNames <- cbind(weightsNames, prop)

# Deriving the vote of each person of the donor database 

data_weights <- as.data.frame(weightsNames)
weights <- as.numeric(as.vector(data_weights$weights))

weights_tot = apply(exp(weights)*Election[,c(9:20)],1,sum)
weights_j = exp(weights)*Election[,c(9:20)]
prob = weights_j/weights_tot

Res2 <- cbind(Election$Code.INSEE,Election$Exprimés*prob)

# Add 0 to reference insee code
add_0toinsee <- function(code)
{
  if (length(unlist(strsplit(code,''))) == 4){
    
    return(paste('0',code,sep=''))
  }else {
    return(code)
  }
}

insee <- as.character(Res2[,1])
transformed_insee <- as.vector(sapply(insee,add_0toinsee))
Res2[,1] <- transformed_insee

contacts_votes_bis <- merge(contacts, Res2[,c('Election$Code.INSEE',
                                              'Blancs.et.nuls','Abstentions',
                                              'JOLY','LE PEN','SARKOZY','MÉLENCHON','POUTOU','ARTHAUD','CHEMINADE',
                                              'BAYROU','DUPONT-AIGNAN','HOLLANDE')],by.x='code_geo',by.y='Election$Code.INSEE')


votes = cbind(as.character(contacts_votes_bis$code_geo),colnames(contacts_votes_bis[,c(3:14)])[apply(contacts_votes_bis[,c(3:14)],1,which.max)])
nb_abs = as.numeric(table(votes[,2])[1])
proportion_with_abs = table(votes[,2])/dim(votes)[1]
proportion_without_abs = (table(votes[,2])/(dim(votes)[1]-nb_abs))[-1]


# plot data true vs estimated

data = data.frame(rbind(c('True',28.63,27.18,17.9,11.1,9.13,2.31,1.79,0.56,0.25,1.52,20.52),
                        c('Estimated MLE',18.85,11.15,7.96,17.84,11.96,5.94,7.62,3.09,0.15,7.80,8.03)))
colnames(data) = c('model','Hollande','Sarkozy','Le Pen','Melenchon','Bayrou','Joly','Dupont-Aignan','Arthaud','Cheminade','Blank','Abs')

melt_data <-melt(data,id.vars="model")
melt_data$value <- as.numeric(melt_data$value)

bar = ggplot(melt_data,aes(x=variable,y=value,fill=model))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)+
  xlab("Candidates")+ylab("Percentage of votes") + labs(title='Percentages of votes for candidates Targeted Population with MLE')

plot(bar)



