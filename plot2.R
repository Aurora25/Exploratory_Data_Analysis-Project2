# create plot1.png 
library(dplyr)

# The data can be downloaded from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# needs to be unzipped into the folder where this file is in, which sould also be the working directory
# read in the data 
pm_data <- readRDS("exdata-data-NEI_data//summarySCC_PM25.rds")
pm_map <- readRDS("exdata-data-NEI_data//Source_Classification_Code.rds")

# 
balt <- subset(pm_data, fips == "24510")

# group the values by each year and then summarize each group with the sum function to get the total amount of Emission 
# for that year
emission.sum.by.year <- summarize(group_by(balt, year),Emissions = sum(Emissions))


# To show the trend of the development of the yearly total emission of Balimore city
# predict a linear regression model. 
# In order to have the linear regression line displayed correctly, we have to understand how the barplot works and 
# align the bars on the x-axis, such that:
# the middle of the bars are at the x-axis ticks 1,2,3,4 and learn a linear model, with the vector c(1,2,3,4) as feature.
# Another possibility would be to align the bars around the x-ticks values c(1999,2002,2005,2008) as centers and learn the
# linear regression model with the actual years feature, but as it is more expensive computation wise and it doesn't have
# any other advantages, I went with the first model.
# 1st step:
# create a vector with the x-axis ticks, that will be the center of the plot, call it points
points <- c(1,2,3,4)-0.4
# step 2:
# learn a linear regression model for these points
model = lm(emission.sum.by.year$Emissions ~ points)

# step 3:
# plot the total sum of pm2.5 per year using a bar plot, add more space for the y-axis label and ticks
png("plot2.png")
par(mar = c(4,5,4,4), mgp = c(3,1,0))
with(emission.sum.by.year, barplot(Emissions,  col="mediumorchid2", #names.arg = year,
                                   #set width of the bars to 0.8 with a space of 0.2 (space is given as space = spacewidth/barwidth)
                                   width = 0.8,space=0.25,
                                   xlab = "Years",
                                   ylim = c(0,4000),
                                   #ylab="Total emission of PM2.5 per year [tons]",
                                   main = "Total emission of PM2.5 in Baltimore City"
                                   ,yaxt = "n"
                                   ,family = "serif"
                                   ,cex.lab = 1.3, cex.main = 1.3
                                   
)
)
# These points are seen as comments in order for the user to test and see that the middle of the bar plots 
# actually represent the points for which the linear regression model was trained
# points(c(1,2,3,4)-0.4,emission.sum.by.year$Emissions)
# Modify the y-axis tick labels to show a clear picture of the data values: changes labels from 4000 to 4K
sequence <- seq(0,4000,500) 
axis(at = sequence,side=2,
     , labels = paste(as.character(sequence/1000), rep.int("K",length(sequence)),sep = " ")
     ,las = 1
     ,family = "serif"
)
# Step 4:
# modify the x-axis to have the xticks in the middle of the bar plots and label them as the years
axis(at = c(1,2,3,4)-0.4,
     labels = as.character(emission.sum.by.year$year), side=1)
# Add title of y-axis seperately, as it needs to move one line further out because of the horizontal alignment of the y-ticks
title(ylab="Total emission of PM2.5 per year [tons]",mgp=c(4,1,0),family = "serif",cex.lab = 1.3)
# Step 5:
# draw the linear regression line and add text to clarify 
abline(model,lwd=2)
text(3.5,2600, labels=c("Linear \n regression"))
dev.off()