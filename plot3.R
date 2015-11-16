# create plot1.png 
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
# read in the data, which was created by read_in_data.R
# source('C:/Users/chris/Dropbox/data analysis/Coursera/exploratory data analysis/Project2/read_in_data.R')

# The data can be downloaded from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# needs to be unzipped into the folder where this file is in, which sould also be the working directory
# read in the data 
pm_data <- readRDS("exdata-data-NEI_data//summarySCC_PM25.rds")
pm_map <- readRDS("exdata-data-NEI_data//Source_Classification_Code.rds")


# subsetting the pm_data to only get the baltimore values
balt <- subset(pm_data, fips == "24510")

# aggregate the baltimore data emission by type and year and build the sum
# for each year and each data source type
agg_balt <- aggregate(Emissions ~ type+year, data = balt,FUN=sum)

# the numberformatter-function are used to format the y-axis according for better
# readibility and substitutes 2000 with 2K
numberformatter <- function(x) {
  x <- x/1000
  res <- paste(x, "K")
}

# create a facet grid for different source types to show the total emission development 
# in Baltimore City
g1 = ggplot(agg_balt, aes(x=factor(year),y=Emissions,fill=factor(type))) +#,color=factor(type))) +
   geom_bar(stat = "identity",color="black") + 
   facet_grid(type~.) +
   stat_smooth(aes(group=type, color="black"),method="lm",se=FALSE,show_guide=TRUE, size=1) +
   xlab("Years") +
   ylab("Total emission [tons]") + 
   scale_fill_discrete(name="Emission source types") +
   scale_color_manual(name="Trend",labels=c("Linear Regression\n of total emission\n values"),values="black") +
   scale_y_continuous(label=numberformatter) +
   ggtitle("Development of total emissions in Baltimore City \n for different source types") +
   theme(plot.title = element_text(lineheight=1, face="bold",vjust=2,size=17))

# save it with ggsave
ggsave(filename="plot3.png")