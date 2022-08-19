library(prism)
library(reshape2)
library(dplyr)
library(tidyr)

get_prism_monthlys(type='ppt', keepZip=F, mon=1:12, keep_pre81_months = FALSE , years = 2021)
get_prism_monthlys(type='tmean', keepZip=F, mon=1:12, keep_pre81_months = FALSE , years = 2021)
get_prism_monthlys(type='tmean', keepZip=F, mon=1:12, keep_pre81_months = FALSE , years = c(2011, 2001, 1991, 1981))
get_prism_monthlys(type='ppt', keepZip=F, mon=1:12, keep_pre81_months = FALSE , years = c(2011, 2001, 1991, 1981))

ppt_stack <- prism_stack(prism_archive_subset(type='ppt', temp_period = 'monthly'))
tmean_stack <- prism_stack(prism_archive_subset(type='tmean', temp_period = 'monthly'))

cities <- read.csv('climate-locations.csv')

cities.spdf<-SpatialPointsDataFrame(coords=cities[,c('Long','Lat')], 
                                    data=cities, proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))

city.ppt <- extract(ppt_stack, cities.spdf,  fun=mean, na.rm=TRUE)
city.tmean <- extract(tmean_stack, cities.spdf,  fun=mean, na.rm=TRUE)

city.ppt<-cbind(cities[,1:2], city.ppt)
city.tmean<-cbind(cities[,1:2], city.tmean)

precip.data <- melt(city.ppt, value.name = 'precip') %>%
  separate(col=variable, sep = '_', into = c(NA, NA, NA,NA, 'data',NA) )%>%
  separate(col=data, sep=4, into=c('year', 'month'))

temp.data<- melt(city.tmean, value.name = 'temp') %>%
  separate(col=variable, sep = '_', into = c(NA, NA, NA,NA, 'data',NA) )%>%
  separate(col=data, sep=4, into=c('year', 'month'))

write.csv(precip.data, 'precipitation.csv', row.names = F)
write.csv(temp.data, 'temperature.csv', row.names = F)

climate<-full_join(temp.data, precip.data)
climate <- climate %>%
  mutate(City = recode(City, 'Ajo '= 'Ajo'))
write.csv(climate, 'climate.csv', row.names = F)



ggplot(filter(climate, year==2011, City=='Ajo'), aes(x=month)) +
  geom_point(aes(y=temp), col='red')+
  geom_line(aes(y=temp), col='red')+
  geom_point(aes(y=precip), col = 'blue')+
  geom_line(aes(y=precip), col = 'blue')+
  scale_y_continuous( sec.axis = sec_axis(~.*2)) + theme_bw()

clim.sum<- climate %>%
  group_by(City, State, month) %>%
  summarize(mean_temp = mean(temp), mean_precip = mean(precip))

ggplot(filter(clim.sum, City=='Raleigh'), aes(x=as.numeric(month))) +
  geom_point(aes(y=mean_temp), col='red')+
  geom_line(aes(y=mean_temp), col='red')+
  geom_point(aes(y=mean_precip), col = 'blue')+
  geom_line(aes(y=mean_precip), col = 'blue')+
  scale_y_continuous('Mean Temperature (C)',  sec.axis = sec_axis(~.*2, 'Mean Precipitation (mm)')) + theme_bw() +
  ggtitle('Climate Diagram for Raleigh, NC') +
  scale_x_continuous('Month', breaks=1:12,  labels=c('J', 'F', 'M', 'A', 'M', 'J','J','A','S','O', 'N','D'))


ggplot(filter(clim.sum, City=='Ajo '), aes(x=as.numeric(month))) +
  geom_point(aes(y=mean_temp), col='red')+
  geom_line(aes(y=mean_temp), col='red')+
  geom_point(aes(y=mean_precip), col = 'blue')+
  geom_line(aes(y=mean_precip), col = 'blue')+
  scale_y_continuous('Mean Temperature (C)',  sec.axis = sec_axis(~.*2, 'Mean Precipitation (mm)')) + theme_bw() +
  ggtitle('Climate Diagram for Flagstaff, AZ') +
  scale_x_continuous('Month', breaks=1:12,  labels=c('J', 'F', 'M', 'A', 'M', 'J','J','A','S','O', 'N','D'))


library(maps)
States <- map_data("state")
ggplot() + 
  geom_polygon( data=States, aes(x=long, y=lat, group=group),
                color="black", fill="white" , size=.8) +
  geom_point(data = cities, aes(Long, Lat), col='red', size=3) + theme_classic() 

ggsave('city-map.pdf')
