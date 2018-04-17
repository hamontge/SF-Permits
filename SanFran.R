################### Reading Data #########################################
install.packages("maps")
library(maps)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("ggmap")
library(ggmap)
install.packages("stringr")
library(stringr)
install.packages("lubridate")
library(lubridate)

build = read.csv("Building_Permits.csv")

###################### Cleaning ###########################################
# Seperate Longitude and Latitude String, remove (), numeric
build$latitude = str_split_fixed(build$Location, ",", 2)[ , 1]
build$latitude = as.numeric(gsub("[^0-9\\.\\-]", "", build$latitude))
build$longitude = str_split_fixed(build$Location, ",", 2)[ , 2]
build$longitude = as.numeric(gsub("[^0-9\\.\\-]", "", build$longitude))

# Date into month and year columns
build$Creation_Month = month(as.POSIXlt(build$Permit.Creation.Date, format="%m/%d/%Y"))
build$Creation_Year = year(as.POSIXlt(build$Permit.Creation.Date, format="%m/%d/%Y"))


# Remove NAs from select columns

build=build[complete.cases(build[ , 4]),]
build=build[complete.cases(build[ , 40]),]
build=build[complete.cases(build[ , 41]),]
build=build[complete.cases(build[ , 42]),]

####################### Statistics #########################################
# Trends in Permitting by creation year
build %>% group_by(Creation_Year,Current.Status) %>% summarise(tot=sum(Creation_Year)) %>% filter(Creation_Year<2018) %>%
  ggplot(aes(x=factor(Creation_Year),y=tot,group=Current.Status,fill=Current.Status))+
  ylab("Output")+xlab("Year")+
  geom_point(size=3,shape=21)

build %>% group_by(Creation_Year,Current.Status) %>% summarise(tot=sum(Creation_Year)) %>% filter(Creation_Year<2018) %>%
  ggplot(aes(x=factor(Creation_Year),y=tot,group=Current.Status,fill=Current.Status))+
  ylab("Output")+xlab("Year")+
  geom_point(size=3,shape=21)

# Looking at building permits by zipcode
build %>% group_by(Creation_Year,Zipcode) %>% summarise(tot=sum(Creation_Year)) %>% filter(Creation_Year<2018) %>%
  ggplot(aes(x=factor(Creation_Year),y=tot,group=Zipcode,fill=Zipcode))+
  ylab("Output")+xlab("Year")+
  geom_point(size=3,shape=21)+facet_wrap(~Zipcode)
## Hotest zip codes are 94110 (dropping off 2017), 94114, 94117

## What type of work is being done in these areas?
levels(build$Permit.Type.Definition)
build %>% group_by(Permit.Type.Definition, Zipcode) %>% filter(Zipcode==94110|Zipcode==94114|Zipcode==94117) %>%
  summarise(tot=length(Permit.Type.Definition)) %>%
  ggplot(aes(x=factor(Permit.Type.Definition),y=tot,group=Zipcode,fill=Zipcode))+
  ylab("Output")+xlab("Permit.Type.Definition")+
  geom_point(size=3,shape=21)

## What neighborhoods are in these zip codes?
build %>% filter(Zipcode==94110)%>%  distinct(Neighborhoods...Analysis.Boundaries)
build %>% filter(Zipcode==94114)%>%  distinct(Neighborhoods...Analysis.Boundaries)
build %>% filter(Zipcode==94117)%>%  distinct(Neighborhoods...Analysis.Boundaries)

## Compare to work elsewhere
levels(build$Permit.Type.Definition)
build %>% group_by(Permit.Type.Definition, Zipcode) %>%
  summarise(tot=length(Permit.Type.Definition)) %>%
  ggplot(aes(x=factor(Permit.Type.Definition),y=tot,group=Zipcode,fill=Zipcode))+
  ylab("Output")+xlab("Permit.Type.Definition")+
  geom_point(size=3,shape=21)
## Majority of the work is "additions alterations or repairs" and "otc alterations permit"
## The 3 major area codes would be good canidaties to market a company helping with OTC permits
## or with additions

# Looking at building permits by area
build %>% group_by(Creation_Year,Neighborhoods...Analysis.Boundaries) %>% summarise(tot=sum(Creation_Year)) %>% filter(Creation_Year<2018) %>%
  ggplot(aes(x=factor(Creation_Year),y=tot,group=Neighborhoods...Analysis.Boundaries,fill=Neighborhoods...Analysis.Boundaries))+
  ylab("Output")+xlab("Year")+
  geom_point(size=3,shape=21)+facet_wrap(~Neighborhoods...Analysis.Boundaries)
## "Financial District/South Beach" and "Mission" is extremely busy with permits


################################## Maps ###########################################################
SFgoogle <- qmap("San Francisco", zoom = 13, color = "bw", legend = "topleft")

# All permits
SFgoogle + stat_density2d(data = build,
                          aes(x= longitude, y = latitude, fill = ..level.., alpha = ..level..), geom = "polygon") +
  scale_fill_gradient(low= "green", high = "#bd0026")+facet_wrap(~Creation_Year)
# Shows the financial district is consistently the hot bed of activity year over year

# Plot zipcodes and neighborhoods
SFgoogle + geom_point(data=build,aes(x= longitude, y = latitude, group = Neighborhoods...Analysis.Boundaries ,color = Neighborhoods...Analysis.Boundaries, size = Neighborhoods...Analysis.Boundaries))
SFgoogle + geom_point(data=build,aes(x= longitude, y = latitude, group = Zipcode ,color = Zipcode, size = Zipcode))

# Map for the areas marked earlier for hot areas
BuildMainArea = filter(build, Zipcode==94110|Zipcode==94114|Zipcode==94117)
SFgoogle + stat_density2d(data = BuildMainArea,
                          aes(x= longitude, y = latitude, fill = ..level.., alpha = ..level..), geom = "polygon") +
  scale_fill_gradient(low= "green", high = "#bd0026")+facet_wrap(~Creation_Year)
# consistent areas over time

BuildMainArea2 = filter(build, Neighborhoods...Analysis.Boundaries=="Financial District/South Beach")
SFgoogle + stat_density2d(data = BuildMainArea2,
                          aes(x= longitude, y = latitude, fill = ..level.., alpha = ..level..), geom = "polygon") +
  scale_fill_gradient(low= "green", high = "#bd0026")+facet_wrap(~Creation_Year)
# consistent areas over time

#Remove hot areas
BuildOtherAreas = filter(build, Neighborhoods...Analysis.Boundaries!="Financial District/South Beach")
SFgoogle + stat_density2d(data = BuildOtherAreas,
                          aes(x= longitude, y = latitude, fill = ..level.., alpha = ..level..), geom = "polygon") +
  scale_fill_gradient(low= "green", high = "#bd0026")+facet_wrap(~Creation_Year)
##

levels(build$Permit.Type.Definition)
levels(build$Existing.Use)
filter(build,Permit.Type.Definition=="new construction wood frame")


temp = filter(build,Existing.Use=="vacant lot")
#2012-2018 aggregated
SFgoogle + geom_point(data=temp,aes(x= longitude, y = latitude, 
                                    group = Neighborhoods...Analysis.Boundaries ,color = Neighborhoods...Analysis.Boundaries, 
                                    size = Neighborhoods...Analysis.Boundaries))
#With time
SFgoogle + geom_point(data=temp,aes(x= longitude, y = latitude, 
                                    group = Neighborhoods...Analysis.Boundaries ,color = Neighborhoods...Analysis.Boundaries, 
                                    size = Neighborhoods...Analysis.Boundaries))+facet_wrap(~Creation_Year)
