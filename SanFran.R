########################## Reading Data #########################################
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
library(dplyr)

build = read.csv("Building_Permits.csv")

############################## Cleaning ###########################################
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

# Make Zipcodes character format for charting
build$Zipcode = as.character(build$Zipcode)

############################ Statistics #########################################
# Trends in Permitting by creation year
build %>% group_by(Creation_Year,Current.Status) %>% summarise(tot=sum(Creation_Year)) %>% filter(Creation_Year<2018) %>%
  ggplot(aes(x=factor(Creation_Year),y=tot,group=Current.Status,fill=Current.Status))+
  ylab("Output")+xlab("Year")+
  geom_point(size=3,shape=21)+ 
  ggtitle("Number of Various Permit Types")

# Permit Status by creation date
build %>% group_by(Current.Status,Creation_Year) %>% summarise(tot=length(Current.Status)) %>%
  ggplot(aes(x=factor(Current.Status),y=tot,group=Current.Status,fill=Current.Status))+
  ylab("Output")+xlab("Permit Status")+
  geom_point(size=3,shape=21)+facet_wrap(~Creation_Year)+
  theme(axis.text.x = element_text(angle = -60, hjust = 0))+ 
  ggtitle("Permit Status by Creation Date")+guides(fill=FALSE)

# Timeframes to get permit filed to issued
build$file.to.issue = mdy(build$Issued.Date) - mdy(build$Filed.Date)
mean(na.omit(build$file.to.issue))
# 26 Days

# Timeframes to get permit issued to construction start
build$issue.to.const = mdy(build$First.Construction.Document.Date) - mdy(build$Issued.Date)
mean(na.omit(build$issue.to.const))
# 2 days

# Timeframes to get permit construction start to completed
build$const.to.compl = mdy(build$Completed.Date) - mdy(build$First.Construction.Document.Date)
mean(na.omit(build$const.to.compl))
# 162 days

# Overall timeframe
build$file.to.compl = mdy(build$Completed.Date) - mdy(build$Filed.Date)
mean(na.omit(build$file.to.compl))
# 187 days

# Permiting Range of Overall Timeframes
ggplot(build, aes(build$file.to.compl)) + geom_bar() +  xlab("Number of Days") + 
  ggtitle("Permiting overall Timeframe")

# Boxplot of Range of Overall Timeframes
build %>% group_by(Zipcode) %>% 
  ggplot(aes(x=factor(Zipcode),y=build$file.to.compl))+xlab("Zipcode")+
  geom_boxplot(outlier.colour = "blue", outlier.color = "blue",
               outlier.fill = "blue", outlier.shape = 1, outlier.size = .5, outlier.stroke = .1)+
  theme(axis.text.x = element_text(angle = -60, hjust = 0))+ ylab("Number of Days")+
  ggtitle("Permiting Overall Timeframe")

# Overall timeframe to zipcodes
build %>% group_by(Zipcode) %>% summarise(tot=mean(na.omit(file.to.compl))) %>%
  ggplot(aes(x=factor(Zipcode),y=tot,group=Zipcode,fill=Zipcode))+
  ylab("Output")+xlab("Zipcode")+
  geom_point(size=3,shape=21)+geom_hline(yintercept = mean(na.omit(build$file.to.compl)), color="red",linetype="dashed")+
  theme(axis.text.x = element_text(angle = -60, hjust = 0))+ 
  ggtitle("Mean Permiting Overall Timeframe")+guides(fill=FALSE)

# Looking at building permits by zipcode
build %>% group_by(Creation_Year,Zipcode) %>% summarise(tot=sum(Creation_Year)) %>% filter(Creation_Year<2018) %>%
  ggplot(aes(x=factor(Creation_Year),y=tot,group=Zipcode,fill=Zipcode))+xlab("Year")+ylab("Total")+
  geom_point(size=3,shape=21)+facet_wrap(~Zipcode)+
  theme(axis.text.x = element_text(angle = -60, hjust = 0))+ guides(fill=FALSE)+ 
  ggtitle("Number of Permits by Zip Code")
## Hotest zip codes are 94110 (dropping off 2017), 94114, 94117

## What type of work is being done in these areas?
levels(build$Permit.Type.Definition)
build %>% group_by(Permit.Type.Definition, Zipcode) %>% filter(Zipcode==94110|Zipcode==94114|Zipcode==94117) %>%
  summarise(tot=length(Permit.Type.Definition)) %>%
  ggplot(aes(x=factor(Permit.Type.Definition),y=tot,group=Zipcode,fill=Zipcode))+
  xlab("Permit Type")+ylab("Total")+
  geom_point(size=3,shape=21)+
  theme(axis.text.x = element_text(angle = -60, hjust = 0))+ 
  ggtitle("Number by Type of Permit for 94110, 94114, 94117")

## What neighborhoods are in these zip codes?
build %>% filter(Zipcode==94110)%>%  distinct(Neighborhoods...Analysis.Boundaries)
build %>% filter(Zipcode==94114)%>%  distinct(Neighborhoods...Analysis.Boundaries)
build %>% filter(Zipcode==94117)%>%  distinct(Neighborhoods...Analysis.Boundaries)

## Compare to work elsewhere
levels(build$Permit.Type.Definition)
build %>% group_by(Permit.Type.Definition, Zipcode) %>%
  summarise(tot=length(Permit.Type.Definition)) %>%
  ggplot(aes(x=factor(Permit.Type.Definition),y=tot,group=Zipcode,fill=Zipcode))+
  ylab("Output")+xlab("Permit Type")+
  geom_point(size=3,shape=21)+
  theme(axis.text.x = element_text(angle = -60, hjust = 0))+ 
  ggtitle("Number by Type of Permit")
## Majority of the work is "additions alterations or repairs" and "otc alterations permit"
## The 3 major area codes would be good canidaties to market a company helping with OTC permits
## or with additions

# Looking at building permits by area
build %>% group_by(Creation_Year,Neighborhoods...Analysis.Boundaries) %>% summarise(tot=sum(Creation_Year)) %>% filter(Creation_Year<2018) %>%
  ggplot(aes(x=factor(Creation_Year),y=tot,group=Neighborhoods...Analysis.Boundaries,fill=Neighborhoods...Analysis.Boundaries))+
  ylab("Total")+xlab("Year")+
  geom_point(size=3,shape=21)+facet_wrap(~Neighborhoods...Analysis.Boundaries)+
  theme(axis.text.x = element_text(angle = -60, hjust = 0))+ guides(fill=FALSE)+ 
  ggtitle("Number of Permits by Neighborhood")
## "Financial District/South Beach" and "Mission" is extremely busy with permits

## Exisitng number of stories for permits
ggplot(build, aes(build$Number.of.Existing.Stories)) + geom_bar() +  xlab("Number of Stories") + 
  ggtitle("Permiting overall Timeframe")


################################## Maps ###########################################################
SFgoogle <- qmap("San Francisco", zoom = 13, color = "bw", legend = "topleft")

# All permits
SFgoogle + stat_density2d(data = build,
                          aes(x= longitude, y = latitude, fill = ..level.., alpha = ..level..), geom = "polygon") +
  scale_fill_gradient(low= "green", high = "#bd0026")+facet_wrap(~Creation_Year)+ 
  theme(legend.position="right")+ 
  ggtitle("Heatmap of all Permits")

# Shows the financial district is consistently the hot bed of activity year over year

# Plot zipcodes and neighborhoods
SFgoogle + geom_point(data=build,aes(x= longitude, y = latitude, group = Neighborhoods...Analysis.Boundaries ,
                                     color = Neighborhoods...Analysis.Boundaries, size = Neighborhoods...Analysis.Boundaries))+ 
                                    theme(legend.position="right")+ 
  ggtitle("Neighborhood Locations")

SFgoogle + geom_point(data=build,aes(x= longitude, y = latitude, group = Zipcode ,color = Zipcode, size = Zipcode))+ 
  theme(legend.position="right")+ 
  ggtitle("Zipcode Locations")

# Map for the areas marked earlier for hot areas
BuildMainArea = filter(build, Zipcode==94110|Zipcode==94114|Zipcode==94117)
SFgoogle + stat_density2d(data = BuildMainArea,
                          aes(x= longitude, y = latitude, fill = ..level.., alpha = ..level..), geom = "polygon") +
  scale_fill_gradient(low= "green", high = "#bd0026")+facet_wrap(~Creation_Year)+ 
  theme(legend.position="right")+
  ggtitle("Permit Heatmap for 94110, 94114, & 94117")
# consistent areas over time

BuildMainArea2 = filter(build, Neighborhoods...Analysis.Boundaries=="Financial District/South Beach")
SFgoogle + stat_density2d(data = BuildMainArea2,
                          aes(x= longitude, y = latitude, fill = ..level.., alpha = ..level..), geom = "polygon") +
  scale_fill_gradient(low= "green", high = "#bd0026")+facet_wrap(~Creation_Year)+ 
  theme(legend.position="right")+
  ggtitle("Permit Heatmap for Financial District/South Beach")
# consistent areas over time

#Remove hot areas
BuildOtherAreas = filter(build, Neighborhoods...Analysis.Boundaries!="Financial District/South Beach")
SFgoogle + stat_density2d(data = BuildOtherAreas,
                          aes(x= longitude, y = latitude, fill = ..level.., alpha = ..level..), geom = "polygon") +
  scale_fill_gradient(low= "green", high = "#bd0026")+facet_wrap(~Creation_Year)+ 
  theme(legend.position="right")+
  ggtitle("Permit Heatmap for without Finance District")
## Area in the middle of Map is always low on permitting, might be a good target for an area to buy property 
## in order renovate and resale. 

# Look at different permit types and existing land use to find neat thing to look at
levels(build$Permit.Type.Definition)
levels(build$Existing.Use)
filter(build,Permit.Type.Definition=="new construction wood frame")

# Decided to look at vacant lots
temp = filter(build,Existing.Use=="vacant lot")
##2012-2018 aggregated
SFgoogle + geom_point(data=temp,aes(x= longitude, y = latitude, 
                                    group = Neighborhoods...Analysis.Boundaries ,color = Neighborhoods...Analysis.Boundaries, 
                                    size = Neighborhoods...Analysis.Boundaries))+ 
  theme(legend.position="right")+
  ggtitle("Vacant Lots by Neighborhood")
##With time
SFgoogle + geom_point(data=temp,aes(x= longitude, y = latitude, 
                                    group = Neighborhoods...Analysis.Boundaries ,color = Neighborhoods...Analysis.Boundaries, 
                                    size = Neighborhoods...Analysis.Boundaries))+facet_wrap(~Creation_Year)+ 
  theme(legend.position="right")+
  ggtitle("Vacant Lots by Neighborhood")

##Overlay of heatmap and vacant lots
SFgoogle + stat_density2d(data = BuildOtherAreas,
                          aes(x= longitude, y = latitude, fill = ..level.., alpha = ..level..), geom = "polygon") +
  scale_fill_gradient(low= "green", high = "#bd0026")+facet_wrap(~Creation_Year)+ 
  theme(legend.position="right")+
  ggtitle("Permit Heatmap for without Finance District Overlaid by Vacant Lots")+
  geom_point(data=temp,aes(x= longitude, y = latitude,
                           group = Neighborhoods...Analysis.Boundaries ,color = Neighborhoods...Analysis.Boundaries,
                           size = Neighborhoods...Analysis.Boundaries))

