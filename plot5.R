# The data can be downloaded from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# needs to be unzipped into the folder where this file is in, which sould also be the working directory
# read in the data 
# read in the data 
pm_data <- readRDS("exdata-data-NEI_data//summarySCC_PM25.rds")
pm_map <- readRDS("exdata-data-NEI_data//Source_Classification_Code.rds")

# create plot5.png 
library(dplyr)
library(ggplot2)
library(gridExtra)

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
# filter baltimore city vehicles only
pm_filt <- filter(pm_vehicle, fips == "24510" )
# group and summarize the motor vehicle-source-dataset and calculate the mean, the count and the sum of 
# the emission entries
# The reason is: The amount data collection can vary per year and the total emission
# has to be seen taking the amount of data entries into account. 
# Therefore I also calculate the amount of entries using the length function
vehicle_year <- pm_filt %>% group_by(year) %>% summarise(mean= mean(Emissions),
                                                        emissions=sum(Emissions),
                                                        count=length(Emissions))

# the numberformatter-functions are used to format the y-axis according for better
# readibility and substitutes 4000 with 4K or 4e^06 with 4M .
numberformatter <- function(x) {
  x <- x/1000
  res <- paste(x, "K")
}

numberformatterM <- function(x) {
  x <- x/1000000
  res <- paste(x, "M")
}
# in the following I create three ggplots and use the gridExtra library to display them
# in a gridplot.
# Create plot with total amount of emission and tendency over time
g = ggplot(vehicle_year, aes(x=factor(year),y=emissions)) +
  geom_bar(stat = "identity",  colour="black", fill="#CC99FF") + 
  stat_smooth(aes(group=1,color="#990066"),method="lm", se=FALSE,show_guide=FALSE, size=1) +
  scale_y_continuous(label=numberformatter) +
  scale_color_manual(name="Trend",labels=c("Linear Regression"),values="#990066") +
  xlab("Year") +
  ylab("Total emission [tons]") +
  ggtitle("Total emission of \n motor vehicle sources in Baltimore City") +
  theme(plot.title = element_text(lineheight=1, face="bold",vjust=2,size=rel(1.2)))

#print(g)

# Create plot with the amount of data entries per year
g2 = ggplot(vehicle_year, aes(x=factor(year),y=mean)) +
  geom_bar(stat = "identity",  colour="black", fill="#CC99FF") + 
  stat_smooth(aes(group=1,color="#990066"),method="lm", se=FALSE, size=1.2) +
  #scale_y_continuous(label=numberformatter) +
  scale_color_manual(name="Trend",labels=c("Linear Regression"),values="#990066") +
  xlab("Year") +
  ylab("Average emission [tons]") +
  ggtitle("Average emission of \n motor vehicle sources in Baltimore City") +
  theme(plot.title = element_text(lineheight=1, face="bold",vjust=2,size=rel(1.2)))

#print(g3)
# Create plot with average emission and tendency over time
g3 = ggplot(vehicle_year, aes(x=factor(year),y=count)) +
  geom_bar(stat = "identity",  colour="black", fill="#CC99FF") + 
  #stat_smooth(aes(group=1,color="#990066"),method="lm", se=FALSE, size=1) +
  scale_y_continuous(label=numberformatterM) +
  #scale_color_manual(name="Trend",labels=c("Linear Regression\n of total emission\n values"),values="#990066") +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Count of data entries of motor\n vehicle sources in Baltimore City") +
  theme(plot.title = element_text(lineheight=1, face="bold",vjust=2,size=rel(1.2)))

#print(g4)

png("plot5.png",height=800, width=700)
g_s <- grid.arrange(g,g3, nrow=1)
g_done <- grid.arrange(g_s,g2,nrow=2)
dev.off()