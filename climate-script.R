# Exercise: class height
# first you will need to make a vector of all the heights in the class (in inches)
height <- c(...)

# Next, do some summary statistics (for example, mean, median, max, etc.) on class height
...(height)

# Make a histogram of the height data
...(height)

# install.packages
install.packages("dplyr")
install.packages("ggplot2")

# Load climate data
climate <- ...('...')

# View data for Raleigh
...(dplyr)
...(climate, ...)
...(climate, City == ..., year == ...)

climate %>%
  filter(...) ...
  filter(...)

#summarize the temperature and precipitation
climate ... 
  ...(City =="Raleigh") %>%
  ...(mean.temp = ..., ... = mean(precip))

# plot temperature by month
library(...)
  
...(data=climate, ...(x=..., y=...)) +
  geom_...()

# Summarize the entire data set and plot temperature
climate_sum <- ... %>%
  group_by(...) %>%
  summarize(mean.temp = mean(temp), mean.precip=mean(precip))

ggplot(data = ..., aes(x = month, y = ...)) +
  geom_point()

ggplot(data = ..., aes(x = month, y = ...)) +
  geom_...()

# make all the points red
ggplot(data = climate_sum, aes(x = month, y = mean.temp)) +
  geom_point()

#Change the color in the aesthetics
ggplot(data = climate_sum, aes(x = month, y = mean.precip, color= ... )) +
  geom_point()

# Plot Walter's Climate diagram for Raleigh in 2021
## Start with temperature
ggplot(data = filter(climate, City == "...", year =="..." ), aes(x=...)) +
  geom_point(...(y=...), col='red')

ggplot(data = filter(climate, City == "Raleigh", year =="2021" ), aes(x=month)) +
  geom_point(aes(y=temp), color='red') +
  ...(aes(y=temp), color = '...')

temp_raleigh_21 <- ggplot(data = filter(climate, City == "Raleigh", year =="2021" ), aes(x=month)) +
  geom_point(aes(y=temp), col='red')+
  geom_line(aes(y=temp), col='red')

## add precipitation
climate_raleigh_21 <- ... + 
  geom_point(aes(y=...), color='...')+
  geom_line(aes(y=...), color='...') +
  scale_y_continuous( sec.axis = sec_axis(~.*2)) 

# Make it pretty
... +
  scale_y_continuous('...',  
                     sec.axis = sec_axis(~.*2, '...')) +
  scale_x_continuous('...', breaks=1:12,  
                     labels=c('J', 'F', 'M', 'A', 'M', 'J','J','A','S','O', 'N','D')) + 
  ggtitle('...') +
  theme_...()

ggplot(data = filter(..., City=='...'), aes(x=...)) +
  geom_...(aes(y=...), col='red')+
  geom_...(aes(y=...), col='red')+
  geom_...(aes(y=mean.precip), col = '...')+
  geom_...(aes(y=mean.precip), col = '...')+
  scale_y_continuous('...',  
                     sec.axis = sec_axis(~.*2, '...')) +
  scale_x_continuous('...', breaks=1:12,  
                     labels=c('J', 'F', 'M', 'A', 'M', 'J','J','A','S','O', 'N','D')) + 
  ggtitle('...') +
  theme_...()

ggsave("...")


#Exercise 2: Compare climate diagrams
#In this part of lab, make at least **three** additional climate diagrams in R. Include your code here
