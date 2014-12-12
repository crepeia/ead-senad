## Scrap HTML page
# install.packages("ggmap")

## Load package
library(XML)
library(ggmap)
library(plyr)


## Open HTML
activity  <- html("/media//convidado/024C-1DE4//index.php_contextid=28&roleid=0&id=3&perpage=10200")

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


## Geocoding

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

newData  <- cbind(dfTeste, geocodes)

## Download Map
map  <-  get_map(location=c(lon = -47.40306, lat = -18.48686), zoom = 6, maptype="toner",
                source = "stamen", color = "color")

## Create ggplot2 graph
sp.map.2012 <- ggmap(map, base_layer = ggplot(aes(x = lon, y = lat), data = newData), extent = "device")

## Plot map
sp.map.2012 + geom_point(aes(colour=newData$Freq, fill = factor(newData$Var1)), alpha = 1/10, size = I(5))

