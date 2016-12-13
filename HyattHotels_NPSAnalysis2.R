install.packages("NPS")
library("NPS")
install.packages("ggplot2")
install.packages("ggmap")
library('ggplot2')
library('ggmap')

# jandata

Jan <- read.csv(file="out-201501.csv", header=TRUE, sep=",")
JanSurvey <- Jan[!is.na(Jan$Survey_ID_H),]
usJan <- subset(JanSurvey, Country_PL == "United States")

jandata <- subset(usJan, Location_PL == "Airport") 

hotels <- aggregate(jandata$Likelihood_Recommend_H, list(jandata$Property_ID_PL), mean, na.rm = TRUE)
jandata[,175]

locations <- aggregate(jandata[,175:176], list(jandata$Property_ID_PL), unique, na.rm = TRUE)

hotel_location <- merge(hotels, locations)
colnames(hotel_location) <- c('hotelID', 'likelihood', 'lat', 'lon')
us <- map_data('state')


hotelmap <- ggplot() + 
  geom_map(data=us, aes(x=long, y=lat, group = group, map_id = region),
           colour="white", fill="grey", map = us) +
  geom_point(data = hotel_location,
             aes(x = lon, y = lat, color = likelihood), size = 5, alpha = 0.7, shape = 16) +
  scale_color_gradient(low = 'yellow', high = 'black') + 
  coord_map(xlim=c(-130,-62), ylim=c(23, 50))

hotelmap <- hotelmap + labs(x="", y="") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = 'bold', size = 20),
        legend.position = c(0.9, 0.2)) +
  ggtitle('likelihood to recommend of hotel located near the aiports in the United States Jan ')


hotelmap
















View(jandata1)

nps(jandata1$Likelihood_Recommend_H, breaks = list(0:6, 7:8, 9:10))
#jan ksvm
install.packages("kernlab")
library("kernlab")






#April data
april <- read.csv(file="out-201404.csv", header=TRUE, sep=",")

aprSurvey <- april[!is.na(april$Survey_ID_H),]
usapr <- subset(aprSurvey, Country_PL == "United States")

aprdata <- subset(usapr, Location_PL == "Airport") 

hotels2 <- aggregate(aprdata$Likelihood_Recommend_H, list(aprdata$Property_ID_PL), mean, na.rm = TRUE)
jandata[,175]

locations2 <- aggregate(aprdata[,175:176], list(aprdata$Property_ID_PL), unique, na.rm = TRUE)

hotel_location2 <- merge(hotels2, locations2)
colnames(hotel_location2) <- c('hotelID', 'likelihood', 'lat', 'lon')
us2 <- map_data('state')


hotelmap2 <- ggplot() + 
  geom_map(data=us2, aes(x=long, y=lat, group = group, map_id = region),
           colour="white", fill="grey", map = us) +
  geom_point(data = hotel_location2,
             aes(x = lon, y = lat, color = likelihood), size = 5, alpha = 0.7, shape = 16) +
  scale_color_gradient(low = 'yellow', high = 'black') + 
  coord_map(xlim=c(-130,-62), ylim=c(23, 50))

hotelmap2 <- hotelmap2 + labs(x="", y="") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = 'bold', size = 20),
        legend.position = c(0.9, 0.2)) +
  ggtitle('likelihood to recommend of hotel located near the aiports in the United States April ')





#julydata

July <- read.csv(file="out-201407.csv", header=TRUE, sep=",")

julySurvey <- July[!is.na(July$Survey_ID_H),]
usjuly <- subset(julySurvey, Country_PL == "United States")

julydata <- subset(usjuly, Location_PL == "Airport") 

hotels3 <- aggregate(julydata$Likelihood_Recommend_H, list(julydata$Property_ID_PL), mean, na.rm = TRUE)


locations3 <- aggregate(julydata[,175:176], list(julydata$Property_ID_PL), unique, na.rm = TRUE)

hotel_location3 <- merge(hotels3, locations3)
colnames(hotel_location3) <- c('hotelID', 'likelihood', 'lat', 'lon')
us3 <- map_data('state')


hotelmap3 <- ggplot() + 
  geom_map(data=us3, aes(x=long, y=lat, group = group, map_id = region),
           colour="white", fill="grey", map = us) +
  geom_point(data = hotel_location3,
             aes(x = lon, y = lat, color = likelihood), size = 5, alpha = 0.7, shape = 16) +
  scale_color_gradient(low = 'yellow', high = 'black') + 
  coord_map(xlim=c(-130,-62), ylim=c(23, 50))

hotelmap3 <- hotelmap3 + labs(x="", y="") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = 'bold', size = 20),
        legend.position = c(0.9, 0.2)) +
  ggtitle('likelihood to recommend of hotel located near the aiports in the United States July ')



hotelmap
hotelmap2
hotelmap3

#oct

octSurvey <- Oct[!is.na(Oct$Survey_ID_H),]
usoct <- subset(octSurvey, Country_PL == "United States")

octdata <- subset(usoct, Location_PL == "Airport") 

hotels4 <- aggregate(octdata$Likelihood_Recommend_H, list(octdata$Property_ID_PL), mean, na.rm = TRUE)


locations4 <- aggregate(octdata[,175:176], list(octdata$Property_ID_PL), unique, na.rm = TRUE)

hotel_location4 <- merge(hotels4, locations4)
colnames(hotel_location4) <- c('hotelID', 'likelihood', 'lat', 'lon')
us4 <- map_data('state')


hotelmap4 <- ggplot() + 
  geom_map(data=us4, aes(x=long, y=lat, group = group, map_id = region),
           colour="white", fill="grey", map = us) +
  geom_point(data = hotel_location4,
             aes(x = lon, y = lat, color = likelihood), size = 5, alpha = 0.7, shape = 16) +
  scale_color_gradient(low = 'yellow', high = 'black') + 
  coord_map(xlim=c(-130,-62), ylim=c(23, 50))

hotelmap4 <- hotelmap4 + labs(x="", y="") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = 'bold', size = 20),
        legend.position = c(0.9, 0.2)) +
  ggtitle('likelihood to recommend of hotel located near the aiports in the United States Oct. ')




hotelmap
hotelmap2
hotelmap3
hotelmap4