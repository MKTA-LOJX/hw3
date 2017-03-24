#-----------------------------------------------------
#               A Quick Look at our Dear
#                       Donators
#-----------------------------------------------------


# Libraries loading 
library(RODBC)
library(ggplot2)
library(readxl)

# Link to MySQL server 
db = odbcConnect("mysql_server_64", uid="root") # change if needed with name of your ODBC DNS


#----------------------------------------------------
# What data ?

query1 ="SELECT id, code_geo
FROM charity2.contacts"

data = sqlQuery(db,query1)
head(data)

data$code_geo = as.numeric(data$code_geo)

# Missing data
100*sum(is.na(data$code_geo))/nrow(data)
# 1.5% of missing data

query2 ="SELECT id, code_geo, town_clean
FROM charity2.contacts
WHERE code_geo IS NULL"

missing_code_geo = sqlQuery(db,query2)
head(missing_code_geo)
# TODO: Fill these NAs


# Ignore missing values for the moment
data = data[complete.cases(data),]

# Some information
summary(data)

# Plot distribution by code insee
gr1 = ggplot(data= data, aes(x=code_geo))+
    geom_histogram(alpha=.7, stat="count", position="dodge")+
    labs(title = "Distribution des contacts par code insee",y= "Nombre de contacts", x= "Code Insee")
print(gr1)


#------------------------------------------
# Distribution

query3 ="SELECT code_geo, town_clean, count(code_geo) as nb_contacts
FROM charity2.contacts
WHERE code_geo IS NOT NULL
Group by 1"

donators = sqlQuery(db,query3)
head(donators)

donators[,'perc_contacts'] = donators$nb_contacts/sum(donators$nb_contacts)
donators$code_geo = as.character(donators$code_geo) 

# Correct code geo for Paris, Marseille and Lyon
donators[donators$town_clean=="MARSEILLE",1] = "13055"
donators[donators$town_clean=="PARIS",1] = "75056"
donators[donators$town_clean=="LYON",1] = "69123"

# Load national data
inscrits <- read_excel("~/MScDSBA/MKTA/hw3/inscrits.xlsx", col_types = c("text", "numeric"))
colnames(inscrits) = c('code_geo', "nb_inscrits")
inscrits[,'perc_pop'] = inscrits$nb_inscrits/sum(inscrits$nb_inscrits)
head(inscrits)


dim(donators)

# Merge with
dist = merge(x = donators, y = inscrits, by = "code_geo", all.x = TRUE)
dist[,'relat_dist'] = dist$perc_contacts/dist$perc_pop

# Mismatches
dist[is.na(dist$nb_inscrits),]

# Ignore DOM TOM for the moment
dist=dist[complete.cases(dist),]

# Convert code_geo to numeric for ggplot

gr2 = ggplot(data= dist, aes(x=code_geo, y=relat_dist))+
    geom_histogram(alpha=.7, stat="sum")+
    labs(title = "Distribution relative des contacts par code insee",y= "Distribution relative", x= "Code Insee")
print(gr2)

