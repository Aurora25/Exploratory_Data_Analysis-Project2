# The data can be downloaded from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# needs to be unzipped into the folder where this file is in, which sould also be the working directory
# read in the data 
# read in the data 
pm_data <- readRDS("exdata-data-NEI_data//summarySCC_PM25.rds")
pm_map <- readRDS("exdata-data-NEI_data//Source_Classification_Code.rds")

# create plot1.png 
library(dplyr)

# group the values by each year and then summarize each group with the sum function to get the total amount of Emission 
# for that year
emission.sum.by.year <- summarize(group_by(pm_data, year),Emissions = sum(Emissions))


# open pdf plotter 
png("plot1.png")
# plot the total sum of pm2.5 per year using a bar plot (creates the clear message of one value per year)
with(emission.sum.by.year, barplot(Emissions, names.arg = year, col="mediumorchid2",
                                   xlab = "Years", 
                                   ylab="Total emission of PM2.5 per year [tons]",
                                   main = "Total emission of PM2.5 for each year"
                                   ,yaxt = "n"
                                   ,family = "serif"
                                   ,cex.lab = 1.3, cex.main = 1.3
                                   )
)
# Modify the y-axis tick labels to show a clear picture of the data values: changes labels from 4e06 to 4M
sequence <- seq(0,8000000,1000000) 
axis(at = sequence,side=2,
     , labels = paste(as.character(sequence/1000000), rep.int("M",length(sequence)),sep = " ")
     ,las = 1
     ,family = "serif"
     )

dev.off()