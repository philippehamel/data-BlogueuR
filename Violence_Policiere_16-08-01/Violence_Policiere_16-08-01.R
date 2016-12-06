# Package
library(lubridate)
library(reshape2)

##########################################################################
#                                                                        #
#                    Violence policère au État-Unis                      #
#                                                                        #
##########################################################################

############################# Importation
data.police.violence <- read.csv2(file = "MPVDatasetDownload.csv")


############################ Nettoyage
# Eliminer les colonnes vides
data.police.violence <- data.police.violence[ , 1:19]

# Supprimer les dates non desirer
data.police.violence$Date.of.injury.resulting.in.death..month.day.year. <-
    as.POSIXct(data.police.violence$Date.of.injury.resulting.in.death..month.day.year., 
            format = "%y-%m-%d")

data.police.violence <- data.police.violence[which(
    data.police.violence$Date.of.injury.resulting.in.death..month.day.year. <="2016-06-30",
), ]

# Transformer l'age en numeric
data.police.violence$Victim.s.age <- 
    as.numeric(levels(data.police.violence$Victim.s.age))[data.police.violence$Victim.s.age]

# Alleger le data.frame en gardant seulement les colonnes necessaire
data.light.VP <- data.police.violence[ , c(2, 3, 4, 6, 9)]
colnames(data.light.VP) <- c("age", "genre", "race", "date", "state")


############################# Donnees mensuel
# Donnees
data.chrono <- data.light.VP
data.chrono$date <- substr(data.chrono$date, 1, 7)
data.chrono <- as.data.frame(table(data.chrono$date))
data.chrono$Var1 <- paste(data.chrono$Var1, "-01", sep = "")
data.chrono$Var1 <-as.POSIXct(data.chrono$Var1, format = "%Y-%m-%d")
# Cette parti la est deg, mais j'etais faqtigue quand je l'ai ecrite et ca marche
# faque genez-vous pas si vous connaissez une maniere plus efficace.


############################# Donne de population

# Importation
pop <- read.csv(file = "NST-EST2015-01.csv", skip = 3, header = T)
pop <- pop[c(6:56), c(1, 7:9)]

# Creation du data.frame par etat
# data par etat
data.pop <- data.light.VP
data.pop$date <- substr(data.pop$date, 1, 4)
data.pop <- data.pop[which(data.pop$date < 2016), ]

data.pop <- as.data.frame(table(data.pop$state, data.pop$date))
colnames(data.pop) <-(c("state", "annee", "freq"))
    
# creation variable commune
state <- rep(NA, nrow(pop)) 

state[pop$X == ".Alabama"]              <- "AL"
state[pop$X == ".Alaska"]               <- "AK"
state[pop$X == ".Arizona"]              <- "AZ"
state[pop$X == ".Arkansas"]             <- "AR"
state[pop$X == ".California"]           <- "CA"
state[pop$X == ".Colorado"]             <- "CO"
state[pop$X == ".Connecticut"]          <- "CT"
state[pop$X == ".Delaware"]             <- "DE"
state[pop$X == ".District of Columbia"] <- "DC"
state[pop$X == ".Florida"]              <- "FL"
state[pop$X == ".Georgia"]              <- "GA"
state[pop$X == ".Hawaii"]               <- "HI"
state[pop$X == ".Idaho"]                <- "ID"
state[pop$X == ".Illinois"]             <- "IL"
state[pop$X == ".Indiana"]              <- "IN"
state[pop$X == ".Iowa"]                 <- "IA"
state[pop$X == ".Kansas"]               <- "KS"
state[pop$X == ".Kentucky"]             <- "KY"
state[pop$X == ".Louisiana"]            <- "LA"
state[pop$X == ".Maine"]                <- "ME"
state[pop$X == ".Maryland"]             <- "MD"
state[pop$X == ".Massachusetts"]        <- "MA"
state[pop$X == ".Michigan"]             <- "MI"
state[pop$X == ".Minnesota"]            <- "MN"
state[pop$X == ".Mississippi"]          <- "MS"
state[pop$X == ".Missouri"]             <- "MO"
state[pop$X == ".Montana"]              <- "MT"
state[pop$X == ".Nebraska"]             <- "NE"
state[pop$X == ".Nevada"]               <- "NV"
state[pop$X == ".New Hampshire"]        <- "NH"
state[pop$X == ".New Jersey"]           <- "NJ"
state[pop$X == ".New Mexico"]           <- "NM"
state[pop$X == ".New York"]             <- "NY"
state[pop$X == ".North Carolina"]       <- "NC"
state[pop$X == ".North Dakota"]         <- "ND"
state[pop$X == ".Ohio"]                 <- "OH"
state[pop$X == ".Oklahoma"]             <- "OK"
state[pop$X == ".Oregon"]               <- "OR"
state[pop$X == ".Pennsylvania"]         <- "PA"
state[pop$X == ".Rhode Island"]         <- "RI"
state[pop$X == ".South Carolina"]       <- "SC"
state[pop$X == ".South Dakota"]         <- "SD"
state[pop$X == ".Tennessee"]            <- "TN"
state[pop$X == ".Texas"]                <- "TX"
state[pop$X == ".Utah"]                 <- "UT"
state[pop$X == ".Vermont"]              <- "VT"
state[pop$X == ".Virginia"]             <- "VA"
state[pop$X == ".Washington"]           <- "WA"
state[pop$X == ".West Virginia"]        <- "WV"
state[pop$X == ".Wisconsin"]            <- "WI"
state[pop$X == ".Wyoming"]              <- "WY"

# Creation du data.frame
pop <- data.frame(state, pop$X2013, pop$X2014, pop$X2015)
colnames(pop) <- c("state", "2013", "2014", "2015")
pop <- melt(pop, id = "state")

pop$value <- as.numeric(gsub(",", "", pop$value, fixed = TRUE))
colnames(pop) <- c("state", "annee", "pop")

# Merge
data.pop.VP <- merge(pop, data.pop)

# Creation de la variable frequence realtive
data.pop.VP$freq.rel <- ((data.pop.VP$freq/data.pop.VP$pop)*1000000)


############################# Eportation
# J'exporte mes fichiers de donnees transformer afin de travailler dans un autre
# document ou je peut me concentrer sur mes infographiques

save("data.pop.VP", "data.chrono", "data.light.VP", file = "data_transformed.R")