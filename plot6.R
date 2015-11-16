# The data can be downloaded from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# needs to be unzipped into the folder where this file is in, which sould also be the working directory
# read in the data 
# read in the data 
pm_data <- readRDS("exdata-data-NEI_data//summarySCC_PM25.rds")
pm_map <- readRDS("exdata-data-NEI_data//Source_Classification_Code.rds")

# create plot6.png 
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

# Decided to use SCC.Level.Two only in grepl-search because SCC.Level.Three includes EI.Sector variables 
# [5] Solvent - Industrial Surface Coating & Solvent Use
# [6] Miscellaneous Non-Industrial NEC                  
# [7] Gas Stations  
# and does not include the SCC.level.two varaibles 
# [3] Off-highway Vehicle Gasoline, 
# [4] 2-Stroke Off-highway Vehicle Gasoline, 4-Stroke
# [5] Off-highway Vehicle Diesel  
# which in my opinion are part of motor vehicles, whereas solvent, industrial surface coating etc. are not.
# I did an additional check for Gas Stations and it turned out that Gas stations of SCC.level.Three are included in highway vehicles in Level.Two
# But it also includes Aircraft, Surface Coating, Agriculture Production... 
# it is therefore not necessary to add gas stations

# search for SCC.Level.Two values that contian the term vehicle
vehicle <- grepl("[Vv]ehicle", pm_map$SCC.Level.Two)
# save the names of those values
vehicle_names <- unique(pm_map$SCC.Level.Two[vehicle])
# get the SCC code for the found SCC.Level.Two values, that contain motor vehicle sources
vehicle_SCC <- pm_map$SCC[vehicle]

# subset the actual pm_data dataframe with the SCC codes from the map dataframe to 
# collect all the data-entries with with the found SCC-values 
pm_vehicle <- subset(pm_data, SCC %in% vehicle_SCC)

# Filter the pm_vehicle set for the fips code for LA county and Balitcomre city
pm_filt <- filter(pm_vehicle, fips =="06037" | fips == "24510" )
# Create three dataframes with different kinds of aggreagations: sum, mean and length
# The reason is: The amount data collection varies per year and the total emission
# has to be seen with causion. Therefore I also calculate the mean and the count of entries
# using the length function
pm_agg_filt_sum<- aggregate(Emissions ~ year+ fips, pm_filt, FUN=sum)
pm_agg_filt_mean<- aggregate(Emissions ~ year+ fips, pm_filt, FUN=mean)
pm_agg_filt_count<- aggregate(Emissions ~ year+ fips, pm_filt, FUN=length)

# the numberformatter-function are used to format the y-axis according for better
# readibility and substitutes 4000 with 4K
numberformatter <- function(x) {
  x <- x/1000
  res <- paste(x, "K")
}


# Change the levels of the fips column, to make sure the levels are displayed 
# correctly
pm_agg_filt_sum$fips<- factor(pm_agg_filt_sum$fips)
levels(pm_agg_filt_sum$fips) <- c("LA-County", "BC")

pm_agg_filt_count$fips<- factor(pm_agg_filt_count$fips)
levels(pm_agg_filt_count$fips) <- c("LA-County", "BC")

pm_agg_filt_mean$fips<- factor(pm_agg_filt_mean$fips)
levels(pm_agg_filt_mean$fips) <- c("LA-County", "BC")


# I decided to show the changes as total amount, but want to point out, that all the changes in Los Angeles
# have to be read with causion. This is why I created three ggplots and use 
# the gridExtra library to display them in a gridplot side by side.
g_agg1 = ggplot(pm_agg_filt_sum, aes(x=factor(year),y=Emissions,fill=factor(fips))) +
  geom_bar(stat = "identity",color="black") + 
  facet_grid(.~fips) +
  stat_smooth(aes(group=fips, color="black"),method="lm",se=FALSE,show_guide=FALSE, size=1) +
  xlab("Years") +
  ylab("Total emission [tons]") + 
  scale_fill_discrete(name="Counties",labels=c("LA-County","BC"),breaks=unique(pm_agg_filt_sum$fips),guide=FALSE) +
  scale_color_manual(name="Trend",labels=c("Linear Regression"),values="black") +
  scale_y_continuous(label=numberformatter) +
  ggtitle("Total emission \n for motor vehicle sources") +
  theme(plot.margin = unit(c(0.5, 0.5, 1.85, 0.5), "cm"),plot.title = element_text(lineheight=1, face="bold",vjust=2,size=rel(1)))

 

# The count of data entries is important to understand the total and average emission values
# for Los Angeles County, so I added to in the 3 column plot
g_agg2 = ggplot(pm_agg_filt_count, aes(x=factor(year),y=Emissions,fill=factor(fips))) +#,color=factor(type))) +
  geom_bar(stat = "identity",color="black") + 
  facet_grid(.~fips) +
  #stat_smooth(aes(group=fips, color="black"),method="lm",se=FALSE,show_guide=FALSE, size=1) +
  xlab("Years") +
  ylab("Total number of data entries") + 
  scale_fill_discrete(name="Cities",labels=c("LA","BA"),breaks=unique(pm_agg_filt_count$fips)) +
  #scale_color_manual(name=" ",labels=c("Linear Regression"),values="black") +
  #scale_y_continuous(label=numberformatter)
  ggtitle("Total count of data entries \n of motor vehicle sources") +
  theme(legend.position="bottom",plot.title = element_text(lineheight=1, face="bold",vjust=2,size=rel(1)))

# print(g_agg2)

# I add the average emissions per city to show how the number of entries 
# actually controls the total emissions especially in Los Angeles. The total amount
# number is therefore to be taken with a grain of salt, as in 2002 and 2005 a lot more 
# entries have been collected. This can bee seen, looking at the entries. It might be 
# a flaw in the dataset. 
# The Baltimore City data on the other hand doesn't have fluctuations in the number of 
# data entries and a clear trend can be seen
g_agg3 = ggplot(pm_agg_filt_mean, aes(x=factor(year),y=Emissions,fill=factor(fips))) +#,color=factor(type))) +
  geom_bar(stat = "identity",color="black") + 
  facet_grid(.~fips) +
  stat_smooth(aes(group=fips, color="black"),method="lm",se=FALSE,show_guide=TRUE, size=1) +
  xlab("Years") +
  ylab("Average emission [tons]") + 
  scale_fill_discrete(name="Counties",labels=c("LA","BA"),breaks=unique(pm_agg_filt_mean$fips),guide=FALSE) +
  scale_color_manual(name="Trend",labels=c("Linear Regression"),values="black") +
  #scale_y_continuous(label=numberformatter)
  ggtitle("Average emission \n for motor vehicle sources") +
  theme(legend.position="bottom",plot.title = element_text(lineheight=1, face="bold",vjust=2,size=rel(1)))

# The most important message is: Los Angeles has A LOT more emission from motor vehicles
# than Baltimore City. Who would have thought... ;-) 
# The trend for Los Angeles is most likely errorprone and has to be read with causion 
png("plot6.png", height=500, width=1000)
grid.arrange(g_agg1,g_agg2,g_agg3, nrow=1)
dev.off()
