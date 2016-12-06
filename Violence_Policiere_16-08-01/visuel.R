############################# Packaage
library(ggmap)
library(ggthemes)
library(ggplot2)
library(gridExtra)


############################# Importation
load(file = "data_transformed.Rdata")


############################# Graphique chrono

tend.chrono <- ggplot(data = data.chrono, aes(x = Var1, y = Freq)) +
    theme(axis.text.x = element_text(angle = 90)) + theme_hc() +
    stat_smooth(method = "lm", se = F) +
    geom_point() +
    labs(title = "Meurtre par policier aux États-Unis\n de janvier 2013 à juin 2016",
         x = "Date", y = "Fréquence")

summary(data.chrono$Freq)


############################# Map des etats
# Trouver et formater la carte
map <- map_data("state")

# Variable commune
data.pop.VP$state <- gsub("AL", "alabama", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("AK", "alaska", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("AZ", "arizona", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("AR", "arkansas", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("CA", "california", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("CO", "colorado", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("CT", "connecticut", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("DE", "delaware", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("DC", "district of columbia", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("FL", "florida", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("GA", "georgia", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("HI", "Hawaii", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("ID", "idaho", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("IL", "illinois", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("IN", "indiana", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("IA", "iowa", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("KS", "kansas", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("KY", "kentucky", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("LA", "louisiana", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("ME", "maine", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("MD", "maryland", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("MA", "massachusetts", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("MI", "michigan", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("MN", "minnesota", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("MS", "mississippi", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("MO", "missouri", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("MT", "montana", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("NE", "nebraska", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("NV", "nevada", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("NH", "new hampshire", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("NJ", "new jersey", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("NM", "new mexico", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("NY", "new york", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("NC", "north carolina", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("ND", "north dakota", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("OH", "ohio", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("OK", "oklahoma", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("OR", "oregon", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("PA", "pennsylvania", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("RI", "rhode island", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("SC", "south carolina", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("SD", "south dakota", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("TN", "tennessee", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("TX", "texas", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("UT", "utah", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("VT", "vermont", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("VA", "virginia", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("WA", "washington", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("WV", "west virginia", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("WI", "wisconsin", data.pop.VP$state, fixed = TRUE)
data.pop.VP$state <- gsub("WY", "wyoming", data.pop.VP$state, fixed = TRUE)

colnames(data.pop.VP) <- c("region", "annee", "pop", "freq", "freq.rel")


############# Map
map.data <- merge(map, data.pop.VP)

carte.freq <- ggplot() +
    geom_polygon(data = map.data, aes(x = long, y = lat, group = group, fill = freq),
                 color = "black") +
    scale_fill_distiller(direction = 1) +
    facet_wrap(~ annee) +
    theme_nothing(legend = T) +
    coord_map() +
    ggtitle("Meutre commis par des policiers par État \n Fréquence absolue")

carte.rel <- ggplot() +
    geom_polygon(data=map.data, aes(x=long, y=lat, group = group, fill=freq.rel), 
                 color = "black") +
    scale_fill_distiller(direction = 1) +
    facet_wrap(~ annee) +
    theme_nothing(legend = T) +
    coord_map() +
    ggtitle("Meurtre commis par des policiers par État \n Par millions d'habitant")

