#install.packages("ggplot2")
#install.packages("maps")
#install.packages("ggforce")
#install.packages("tidyverse")
#install.packages("maps")
library("ggplot2")
library("maps")
library("ggforce")
library("tidyverse")
library("maps")
library("sf")
# install.packages("OpenStreetMap")
library(OpenStreetMap)
#date = read.csv2('C:\\Users\\tschl\\Desktop\\Datenvisualisierung2024')

#setwd("C:/Users/tschl/Desktop/") 

#data <- read.csv2('C:\\Users\\Public\\Documents')
data3 <- read.csv2('C:/Users/Public/Documents/global_power_plant_database_subset.csv') 

#fehler: ich habe die endung .csv vergessen


#Sys.setlocale("LC_MESSAGES","en_US.UTF-8")#19 26

#plan:
#
#weltkarte als heatmap für gesamt mw produktion eines landes
#kuchendiagramme pro kontinent - gas/öl/solar anteil
#deutschlandkarte mit punkten in versch farben von jeder power plant

data3

graph1 <- ggplot(data = data3, 
                 mapping = aes (x = capacity_mw, 
                                y = latitude)) +
  geom_point(aes(color = factor(primary_fuel)))
graph1

#

graph2 <- ggplot(data = data3, 
                 mapping = aes (x = capacity_mw, 
                                y = longitude)) +
  geom_point(aes(color = factor(primary_fuel)))

graph2

##

graph3 <- ggplot(data = data3, 
                 mapping = aes (x = latitude, 
                                y = longitude)) +
  geom_point(aes(color = factor(capacity_mw)))

graph3

##

sorted_mw <- sort(data3$capacity_mw)

graph4 <- ggplot(sorted_mw,
                 mapping = aes )+ theme-dark()

barplot(sorted_mw, 
        col=rgb(0.8,0.1,1)
        ) # sorted pareto distribution

barplot(data3$capacity_mw) # unsorted

graph5 <- data3 %>% 
  arrange(capacity_mw) %>% 
  mutate(index = sequence(n())) %>% 
  ggplot(aes(x = index, y = capacity_mw)) +
  geom_line() +
  facet_zoom(xlim = c(0, 10000),ylim = c(0, 10))

graph5 # pareto distribution rangezoomt

##

graph6 <- data3 %>% 
  arrange(primary_fuel) %>%
  arrange(capacity_mw) %>% 
  arrange(country) %>%
  mutate(index = sequence(n())) %>% 
  ggplot(aes(x = index, y = capacity_mw)) +
  geom_point((aes(color = factor(primary_fuel)))) +
  facet_zoom(xlim = c(24712, 34544),ylim = c(0, 10000)) # die USA

graph6 # rangezoomt an ein Land

##

#install.packages("sf")

# Import the data with coordinates
world <- map_data("world")

# Plot the map. group = group connects the points in the correct order
ggplot(data = world, aes(x = long, y = lat, group = group)) + 
  geom_polygon() + 
  geom_point(aes(color = facor()))

# Equivalent to:
ggplot(data3, aes(map_id = region)) +
  geom_map(data = world, map = world,
           aes(x = longitude, y = latitude, map_id = region))

ggplot(world) +
  geom_sf(color = "white", aes(color = data3$country)) +
  theme(legend.position = "none")

# Import a geojson or shapefile
map <- read_sf("https://raw.githubusercontent.com/R-CoderDotCom/data/main/shapefile_spain/spain.geojson")

ggplot(map) +
  geom_sf(color = "white", aes(fill = name)) +
  theme(legend.position = "none")

# habe ich nicht für die präsi genutzt

# install.packages("sf")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("giscoR")

library(giscoR)
# library(dplyr)
# library(sf)
# library(ggplot2)

epsg_code <- 3035

# European countries
EU_countries <- gisco_get_countries(region = "EU") %>%
  st_transform(epsg_code)

# Countries centroids
symbol_pos <- st_centroid(EU_countries, of_largest_polygon = TRUE)

data3_spatial <- st_transform(data3, epsg_code)

# We can create a quick plot to get a first sight of our data:

# Plot
ggplot(EU_countries) +
  geom_sf() +
  xlim(c(2200000, 7150000)) +
  ylim(c(1380000, 5500000)) +
  # Airports
  geom_sf(
    data = airports,
    pch = 3,
    cex = 1,
    color = "red"
  ) +
  # Labels position (centroids)
  geom_sf(data = symbol_pos, color = "blue")

# habe ich nicht in der Präsi benutzt

#------------------------------------------

points = st_as_sf(data3, coords = c("longitude", "latitude"), crs = 4326)

plot(st_geometry(points), pch=16, col="navy") #decontextualized points


Sys.setenv(NOAWT=1)



upperLeftUSA = c(53.00, -135.00)
lowerRightUSA = c(20.00, -65.00)
usa_map  = openmap(upperLeft, lowerRight, type="osm")
plot(usa_map) # usa map

upperLeftWorld = c(-85.00, 175.00)
lowerRightWorld = c(85.00, -175.00)
world_map  = openmap(upperLeftWorld, lowerRightWorld, type="osm")
plot(world_map) # world map

points = st_transform(points, osm())

graph7 <- plot(st_geometry(points), pch=16, col="navy", cex=0.5, add=T)
# USA Karte mit allen Kraftwerken als Punkt

##

data3_reduced_to_coal <- filter(data3, primary_fuel=="Coal")
data3_reduced_to_oil <- filter(data3, primary_fuel=="Oil")

pointsCoal = st_as_sf(data3_reduced_to_coal, coords = c("longitude", "latitude"), crs = 4326)
pointsCoal = st_transform(pointsCoal, osm())

pointsOil = st_as_sf(data3_reduced_to_oil, coords = c("longitude", "latitude"), crs = 4326)
pointsOil = st_transform(pointsOil, osm())

#pointsCoal = st_as_sf(data3ReducedToCoal, coords = c("longitude", "latitude"), crs = 4326)
#pointsOil = st_as_sf(data3_reduced_to_oil, coords = c("longitude", "latitude"), crs = 4326)

graph8_0 <- plot(usa_map) 
graph8_1 <- plot(st_geometry(pointsCoal), pch=16, col="black", cex=0.5, add=T) 
graph8_2 <- plot(st_geometry(pointsOil), pch=16, col="brown", cex=0.5, add=T)


legend("bottomright",   # Position
       inset = 0.15,          # Position
       legend = c("Kohle", "Öl"),  # Legend texts
       fill = c(1,2), # box colors
       bty = "n", # Removes the legend box
       )            
                      
graph8_1 # kp wieso ich das nicht pipen kann, aber es geht einzeln
# nur die Kohlekraftwerke und ölkraftwerke in schwarz und braun

##

graph9_0 <- plot(world_map) 
graph9_1 <- plot(st_geometry(pointsCoal), pch=1, col=alpha("black", 0.4), cex=0.4, add=T) 
graph9_2 <- plot(st_geometry(pointsOil), pch=1, col=alpha("brown", 0.4), cex=0.4, add=T)

legend("bottom",   # Position
       inset = 0.15,          # Position
       legend = c("Kohle", "Öl"),  # Legend texts
       fill = c(1,2), # box colors
       bty = "n", # Removes the legend box
)   

##

#6. Deutschlandkarte, Größe der Punkte soll dabei der 
#Gesamtkapazität und die Farbe der primary fuel-Größe entsprechen.


upperLeftDe = c(55.27, 6.00)
lowerRightDe = c(47.20, 15.00)
de_map  = openmap(upperLeftDe, lowerRightDe, type="osm")
plot(de_map) # Deutschlandkarte

colors <- c("Biomass"="brown","Coal" = "black",
            "Cogeneration"="green", "Gas" = "red",
            "Geotherman"="maroon","Nuclear" = "green",
            "Hydro" = "blue", "Oil"="grey",
            "Wind" = "yellow", "Other"="white",
            "Solar" = "lightblue", "Petcoke"="darkgrey",
            "Storage" = "blue",
            "Waste" = "brown",
            "wave and Tidal" = "blue",
            "Wind"="lightgrey"
)
            
#points$primary_fuel <- data3$primary_fuel

plot(st_geometry(points), pch=16, col=factor(points$primary_fuel), cex=0.5, add=T)

#plot(st_geometry(points), pch=16, col=colors[primary_fuel], cex=0.5, add=T) 
        # bing ais vorschlag mit farbvektor (unbenutzt da es nicht funktioniert)


data3_reduced_to_nuclear <- filter(data3, primary_fuel=="Nuclear")
data3_reduced_to_gas <- filter(data3, primary_fuel=="Gas")
data3_reduced_to_hydro <- filter(data3, primary_fuel=="Hydro")


pointsNuclear = st_as_sf(data3_reduced_to_nuclear, coords = c("longitude", "latitude"), crs = 4326)
pointsNuclear = st_transform(pointsNuclear, osm())

pointsGas = st_as_sf(data3_reduced_to_gas, coords = c("longitude", "latitude"), crs = 4326)
pointsGas = st_transform(pointsGas, osm())

pointsHydro = st_as_sf(data3_reduced_to_hydro, coords = c("longitude", "latitude"), crs = 4326)
pointsHydro = st_transform(pointsHydro, osm())


plot(st_geometry(pointsCoal), pch=19, col=alpha("black", 0.9), cex=0.9, add=T) 
plot(st_geometry(pointsOil), pch=19, col=alpha("brown", 0.9), cex=0.9, add=T)
plot(st_geometry(pointsNuclear), pch=19, col=alpha("darkgreen", 0.9), cex=0.9, add=T)
plot(st_geometry(pointsGas), pch=19, col=alpha("orange", 0.9), cex=0.9, add=T)
plot(st_geometry(pointsHydro), pch=19, col=alpha("blue", 0.9), cex=0.9, add=T)#cex=factor(pointsHydro$capacity_mw), add=T)
# sources ausgewählt nach den 50. größten Kraftwerken, deren primary fuel

legend("bottomright",   # Position
       inset = 0.15,          # Position
       legend = c("Kohle", "Öl", "Nuklear", "Gas", "Hydro"),  # Legend texts
       fill = c("black","brown","darkgreen","orange","blue"), # box colors
       #bty = "n", # Removes the legend box
)   


plot(data3)

ggplot2(points)

ggplot(points, add=T) +
  geom_sf(color = "red", aes(color = points$primary_fuel)) +
  theme(legend.position = "none")


head(points, n=3)

head(pointsCoal, n=3)


graph99 <- ggplot2(data3, aes = ())




##



# vlt esquisse benutzen um graphen zu erstellen da es eine drag and drop anwendung ist

#count number of occurrences of each value in column

table(data3$country) # USA hat die meisten Kraftwerke

## deutschlandkarte

## erneuerbare vs nicht erneuerbar einfärben

## Beschriftung lieber 4 mio anstatt 4000000

## anzahl der kraftwerke pro primary fuel 

## akkumulierte kapaziät pro primary fuel

## auf dem balken oben drauf steht der wert

## world map with total capacity

# top energieproduzenten als balkendiagramm, schriftart groß genug wählen 
#   (schlechte beamer qualität)

## kuchendiagramm für jede energiequelle, gut das die prozente drinn stehen

# vlt in der Weltkarte 









head(data3, n=4)
# vlt primäre sortierung ist das Land, sekundär capacity_mw
# vlt primäre sortierung primary fuel, sekundär capacity_mw oder 
# primäre sortierung primary_fuel, sekundär land, tertriär capacity_mw

##

# multi line comment mit Command + Shift + C

#str(data3)

data_capacity_mw <- data3[,"capacity_mw"]
print(data_capacity_mw, quote = TRUE, row.names = FALSE)

dataGermany <- data3[country_long="Germany"]








#javascript:(function(){document.body.parentElement.style = 'filter: grayscale(1) invert(1) contrast(0.8)';})()
#firefox pdf dark mode

# zum testen ob RStudio gefreezed ist
number <- 2+2
print(number)
