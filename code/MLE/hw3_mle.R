library(readxl)
Election_2012_with_INSEE <- read_excel("~/Documents/ECP/3A/Marketing Analytics/HW3_top/hw3/code/MLE/Election_2012_with_INSEE_forMLE.xls")

# Creation of BDD
View(Election_2012_with_INSEE)
Election <- Election_2012_with_INSEE[,c("Code INSEE","Code du département","Libellé du département","Code de la commune","Libellé de la commune", "Inscrits", "Exprimés", "Votants", "Blancs et nuls", "Abstentions")]
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
View(Election)

# Creation of counts per vote type 
results <- c(sum(Election["Blancs et nuls"]),sum(Election["Abstentions"]), sum(Election["JOLY"]), sum(Election["LE PEN"]), sum(Election["SARKOZY"]), sum(Election["MÉLENCHON"]), sum(Election["POUTOU"]), sum(Election["ARTHAUD"]), sum(Election["CHEMINADE"]), sum(Election["BAYROU"]), sum(Election["DUPONT-AIGNAN"]), sum(Election["HOLLANDE"]))
counts <- cbind(colnames(Election[,9:20]),results)

# Creation of weights : creation function MLE & optimization of MLE with our votes BDD
Ydonné = t(Election[,c("Blancs et nuls","Abstentions","JOLY","LE PEN","SARKOZY","MÉLENCHON","POUTOU","ARTHAUD","CHEMINADE","BAYROU","DUPONT-AIGNAN","HOLLANDE")])
mle_Ydonné <- function(w){
    # Facteur 
    fact <- results%*%exp(w)
    
    # Creation BDD modifée avec poids
    Elections_points <- matrix(nrow = nrow(Election), ncol = 0)
    for(i in 1:12){
        col <- Election[,i+8]*exp(w[i])/fact
        Elections_points = cbind(Elections_points, col)
    }
    Elections_points <- cbind(Election["Code INSEE"],Elections_points)
    
    # L creation 
    L <- rowSums(Elections_points[,c("Blancs et nuls","Abstentions","JOLY","LE PEN","SARKOZY","MÉLENCHON","POUTOU","ARTHAUD","CHEMINADE","BAYROU","DUPONT-AIGNAN","HOLLANDE")])
    logL <- log(L)*Election_2012_with_INSEE[,"Pop charity per code postal"]
    Elections_points <- cbind(Elections_points, L, logL)
    colnames(Elections_points)[14:15] <- c("L","logL")
    logLrouge <- sum(logL)
    
    return(-logLrouge)
}
winit <- rep(1,12)
optim_weights = optim(winit, mle_Ydonné) 
weights = optim_weights$par

# BDD
Elections_points <- matrix(nrow = nrow(Election), ncol = 0)
for(i in 1:12){
    col <- Election[,i+8]*exp(weights[i])/fact
    Elections_points = cbind(Elections_points, col)
}
Elections_points <- cbind(Election["Code INSEE"],Elections_points)
View(Elections_points[6130,])

# Export results
weightsNames <- cbind(c("Blancs et nuls","Abstentions","JOLY","LE PEN","SARKOZY","MÉLENCHON","POUTOU","ARTHAUD","CHEMINADE","BAYROU","DUPONT-AIGNAN","HOLLANDE"), weights)
prop <- exp(weights)/sum(exp(weights))
weightsNames <- cbind(weightsNames, prop)

write.table(weightsNames, file = "~/Documents/ECP/3A/Marketing Analytics/HW3_top/hw3/code/MLE/weights.csv", sep = ";", row.names = F) 
write.table(Elections_points, file = "~/Documents/ECP/3A/Marketing Analytics/HW3_top/hw3/code/MLE/MLEresults.csv", sep = ";", row.names = F) 

