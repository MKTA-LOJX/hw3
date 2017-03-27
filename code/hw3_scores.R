library(readr)
charity2_contacts <- read_delim("~/Documents/ECP/3A/Marketing Analytics/HW3_top/Projet_MarketingAnalytics/charity2_contacts.csv", 
                                +     ";", escape_double = FALSE, trim_ws = TRUE)
library(readxl)
Election_2012_with_INSEE <- read_excel("~/Documents/ECP/3A/Marketing Analytics/HW3_top/Projet_MarketingAnalytics/Election_2012_with_INSEE.xls")
View(Election_2012_with_INSEE)

# Creation of BDD
Election <- Election_2012_with_INSEE[,c("Code INSEE","Code du département","Libellé du département","Code de la commune","Libellé de la commune", "Inscrits", "Abstentions", "Votants", "Blancs et nuls", "Exprimés")]
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
results <- c(sum(Election["Abstentions"]),sum(Election["Votants"]),sum(Election["Blancs et nuls"]),sum(Election["Exprimés"]), sum(Election["JOLY"]), sum(Election["LE PEN"]), sum(Election["SARKOZY"]), sum(Election["MÉLENCHON"]), sum(Election["POUTOU"]), sum(Election["ARTHAUD"]), sum(Election["CHEMINADE"]), sum(Election["BAYROU"]), sum(Election["DUPONT-AIGNAN"]), sum(Election["HOLLANDE"]))
counts <- cbind(colnames(Election[,7:20]),results)

# Creation of MLE function
mle <- function(w,Y){
    r <- rep(0,ncol(Y))
    Yn <- rowSums(Y)
    denom = w%*%Yn
    for(i in 1:ncol(Y)){
        m = w%*%Y[,i]/denom
        r[i] = log(m)        
    }
    return(sum(r))
}

# Creation of weights : optimization of MLE with our votes BDD
Ydonné = t(Election[,c("Abstentions","JOLY","LE PEN","SARKOZY","MÉLENCHON","POUTOU","ARTHAUD","CHEMINADE","BAYROU","DUPONT-AIGNAN","HOLLANDE")])
mle_Ydonné <- function(w){
    return(-mle(w,Ydonné))
}
winit <- rep(1,11)
optim_weights = optim(winit, mle_Ydonné) #optim does minimization so we take -MLE in mle_Ydonné
weights = cbind(optim_weights$par)

# Facteur 
fact <- results[4:14]%*%exp(weights)

# Creation BDD modifée avec poids
Elections_points <- matrix(nrow = 36565, ncol = 0)
for(i in 1:11){
    col <- Election[,i+9]*exp(weights[i])/fact
    Elections_points = cbind(Elections_points, col)
}
Elections_points <- cbind(Election["Code INSEE"],Elections_points)
View(Elections_points)

# L creation 
L <- rowSums(Elections_points[,c("Exprimés","JOLY","LE PEN","SARKOZY","MÉLENCHON","POUTOU","ARTHAUD","CHEMINADE","BAYROU","DUPONT-AIGNAN","HOLLANDE")])
logL <- log(L)*Election_2012_with_INSEE[,"Pop charity per code postal"]
Elections_points <- cbind(Elections_points, L, logL)
colnames(Elections_points)[13:14] <- c("L","logL")

# On enlève les 3 communes qui ont aucun vote exprimés
logLrouge <- colSums(Elections_points[Elections_points["L"] > 0,]["logL"])



