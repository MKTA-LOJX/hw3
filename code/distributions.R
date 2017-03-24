#----------------------------------------------------------------
#                   A Quick Look at our Dear
#                           Donators
#----------------------------------------------------------------


# Libraries loading 
library(RODBC)
library(ggplot2)
library(readxl)

# Link to MySQL server 
db = odbcConnect("mysql_server_64", uid="root") # change if needed with name of your ODBC DNS


#----------------------------------------------------------------
# Distribution

# Get the data
query3 ="SELECT id, zip_code, code_geo, town_clean, count(*) as nb_contacts
FROM charity2.contacts_processed
GROUP BY code_geo"
donators = sqlQuery(db,query3)
head(donators)

# Compute percentage by code_geo and set to text format
donators[,'perc_contacts'] = donators$nb_contacts/sum(donators$nb_contacts)
donators$code_geo = as.character(donators$code_geo)
head(donators)

# Load national data and compute percentages
inscrits <- read_excel("~/MScDSBA/MKTA/hw3/inscrits.xlsx", col_types = c("text", "numeric"))
colnames(inscrits) = c('code_geo', "nb_inscrits")
inscrits[,'perc_pop'] = inscrits$nb_inscrits/sum(inscrits$nb_inscrits)
head(inscrits)

# Merge the two
dist = merge(x = donators, y = inscrits, by = "code_geo", all.x = TRUE)
dist[,'relat_dist'] = dist$perc_contacts/dist$perc_pop
head(dist)

# Mismatches... TODO
dist[is.na(dist$nb_inscrits),]

# Ignore DOM TOM for the moment
dist=dist[complete.cases(dist),]

# Convert code_geo to numeric for ggplot
gr2 = ggplot(data= dist, aes(x=code_geo, y=relat_dist))+
    geom_histogram(alpha=.7, stat="sum")+
    theme(axis.text.x = element_blank())+
    labs(title = "Distribution relative des contacts par code insee",y= "Distribution relative", x= "Code Insee")
print(gr2)

