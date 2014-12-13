## Scrap HTML page
# install.packages("ggmap")
install.packages("rvest")

## Load package
library(rvest)
library(XML)
library(ggmap)
library(plyr)
library(googleVis)


## Open HTML
activity  <- html("/media/hp/024C-1DE4//index.php_contextid=28&roleid=0&id=3&perpage=10200")

## Import data
tables <- readHTMLTable(activity)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
df <- tables[[which.max(n.rows)]]

## Choose nice names
colnames(df) <- c("select","photo", "name", "id","email", "city", "country", "lastaccess")

## as.character
df$select <-  as.character(df$select)
df$photo <-  as.character(df$photo)
df$name <-  as.character(df$name)
df$id <-  as.character(df$id)
df$email <-  as.character(df$email)
df$city <-  as.character(df$city)
df$country <-  as.character(df$country)
df$lastaccess <-  as.character(df$lastaccess)

## Create var - access
df$access  <- ifelse(df$lastaccess == "Nunca", "Accessed", "Not accessed")

########################
## JOIN CITIES DATA ----
########################

teste  <- tapply(df$access, df$city, table, simplify=FALSE)
dfTeste <- ldply(teste, data.frame)


## Geocoding ---
## You should not use this commented lines below. Google API has query limit - 2500. You should use the data from csv.

#geocodes1  <- geocode(dfTeste$.id[1:250])
#geocodes2  <- geocode(dfTeste$.id[251:500])
#geocodes3  <- geocode(dfTeste$.id[501:750])
#geocodes4  <- geocode(dfTeste$.id[751:1000])
#geocodes5  <- geocode(dfTeste$.id[1001:1250])
#geocodes6  <- geocode(dfTeste$.id[1251:1500])
#geocodes7  <- geocode(dfTeste$.id[1501:1636])


########################
## MAPS ----
########################

## Import part 1
geocodes1  <- read.csv("geocodes1-1000.csv")

## Import part 2 
geocodes2  <- read.csv("geocodes1001-1636.csv")

## Bind dataframes
geocodes <- rbind(geocodes1, geocodes2)

## Remove previous dataframes
rm(geocodes1, geocodes2)
geocodes <- geocodes[, -1]

## Bind dataframes
geocodesFinal  <- cbind(dfTeste, geocodes)

## Write csv
# write.csv(geocodesFinal, "geocodesFinal.csv")



###################
## GGPLOT2 MAPS ##
##################

## Open Created csv
geocodesFinal  <- read.csv("geocodesFinal.csv")

## Download Map
map  <-  get_map(location=c(lon = -47.40306, lat = -18.48686), zoom = 6, maptype="roadmap",
                 source = "google", color = "bw")

## Create ggplot2 graph
moodle.map <- ggmap(map, base_layer = ggplot(aes(x = lon, y = lat), data = geocodesFinal), extent = "device")

# Users
moodle.map + stat_density2d(data = subset(geocodesFinal, geocodesFinal$Var1 == "Accessed") , aes(x = lon, y = lat,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 50, geom = 'polygon') + scale_fill_gradient(low = "#F7FBFF", high = "#08306B") + scale_alpha(range = c(0.00, 0.15), guide = FALSE) +  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))

# Dropouts
moodle.map + stat_density2d(data = subset(geocodesFinal, geocodesFinal$Var1 == "Not accessed") , aes(x = lon, y = lat,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 50, geom = 'polygon') + scale_fill_gradient(low = "#FFF5EB", high = "#7F2704") + scale_alpha(range = c(0.00, 0.30), guide = FALSE) +  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))


# Dropouts vs Users
moodle.map + stat_density2d(data = geocodesFinal, aes(x = lon, y = lat,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 25, geom = 'polygon') + scale_fill_gradient(low = "#F7FBFF", high = "#08306B") + scale_alpha(range = c(0.00, 0.15), guide = FALSE) +  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12)) + facet_grid(. ~ Var1)


###################
## R GOOGLEVIS   ##
###################

geocodesFinal$gv  <- paste0(geocodesFinal$lat,":",geocodesFinal$lon)
gvDf  <- geocodesFinal[1:100, c(2,6)]

dropouts  <- gvisMap(gvDf, gvDf$gv,  
                     options=list(showTip=TRUE, 
                                  enableScrollWheel=TRUE,
                                  mapType='terrain', 
                                  useMapTypeControl=TRUE))


## apgomide@gmail.com; Alexandre123; 
## psn games BH
## Anotar logins e senhas
## Ver o passo-a-passo de como instalar
## Instalar no videogame