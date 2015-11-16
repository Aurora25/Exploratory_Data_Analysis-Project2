# The data can be downloaded from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# needs to be unzipped into the folder where this file is in, which sould also be the working directory
# read in the data 
pm_data <- readRDS("exdata-data-NEI_data//summarySCC_PM25.rds")
pm_map <- readRDS("exdata-data-NEI_data//Source_Classification_Code.rds")

# create plot1.png 
library(dplyr)
library(ggplot2)
library(gridExtra)

# Define the pattern to use with grepl to find all coal combustion related sources
pattern<-"^(?=.*\\bcoal\\b)(?=.*(\\bcomb\\b)).*$"

# use grepl to produce a logical vector for all the coal combustions related sources in the EI.Sector column
ah <- grepl(pattern, pm_map$EI.Sector, ignore.case=T, perl=T)

# save all coal cumbustion related sources by name in the vector coal
# I decided to use the coal combustion sources from the EI.Sector variable only, as
# I think it containes all necessary sources
coal <- pm_map$EI.Sector[ah]
# save the SCC numbers for all coal combustion related sources to fetch them from the pm_data dataframe
coal_SCC <- pm_map$SCC[ah]

# Subset the pm_data, such that only the coal combustion related sources are included
pm_coal <- subset(pm_data, SCC %in% coal_SCC)

# group and summarize the coal-related-source-dataset and calculate the mean, the count and the sum of 
# the emission entries
coal_year <- pm_coal %>% group_by(year) %>% summarise(Emission=sum(Emissions),
                                                      mean_emi=mean(Emissions),
                                                      sd_emi = sd(Emissions),
                                                      max_emi =max(Emissions),
                                                      count=length(Emissions))

# the numberformatter-functions are used to format the y-axis according for better
# readibility and substitutes 4000 with 4K or 4e^06 with 4M .
numberformatter <- function(x) {
  x <- x/1000000
  res <- paste(x, "M")
}

numberformatterK <- function(x) {
  x <- x/1000
  res <- paste(x, "K")
}

# In the following I create 3 ggplots and summarize them to one grid plot with GridExta Library
# I chose to show the total emission values, average and number of data entries, in order
# to be able to fully understand the dataset. 

# Create total amount of emissions produced by coal combustion sources
g = ggplot(coal_year, aes(x=factor(year),y=Emission)) +
    geom_bar(stat = "identity",  colour="black", fill="#CC99FF") + 
    stat_smooth(aes(group=1,color="#990066"),method="lm", se=FALSE,show_guide=FALSE, size=1) +
    scale_y_continuous(label=numberformatter) +
    scale_color_manual(name="Trend",labels=c("Linear Regression"),values="#990066") +  
    xlab("Year") +
    ylab("Total emission [tons]") +
    ggtitle("Total emission of \n coal combustion sources across the U.S.") +
    theme(plot.title = element_text(lineheight=1, face="bold",vjust=2,size=rel(1.2)))
#print(g)

# I add the average emissions to show how the number of entries can control the total emissions.
g1 = ggplot(coal_year, aes(x=factor(year),y=mean_emi)) +
  geom_bar(stat = "identity",  colour="black", fill="#CC99FF") + 
  stat_smooth(aes(group=1,color="#990066"),method="lm", se=FALSE, size=1) +
  #scale_y_continuous(label=numberformatterK) +
  scale_color_manual(name="Trend",labels=c("Linear Regression"),values="#990066") +
  xlab("Year") +
  ylab("Average emission [tons]") +
  ggtitle("Average emission of \n coal combustion sources across the U.S.") +
  theme(plot.title = element_text(lineheight=1, face="bold",vjust=2,size=rel(1.2)))
#print(g1)

# The count of data entries is important to understand the total and average emission values
g2 <- ggplot(coal_year, aes(x=factor(year),y=count)) +
  geom_bar(stat = "identity",  colour="black", fill="#CC99FF") + 
  #stat_smooth(aes(group=1,color="#990066"),method="lm", se=FALSE, size=1) +
  scale_y_continuous(label=numberformatterK) +
  #scale_color_manual(name="Trend",labels=c("Linear Regression\n of total emission\n values"),values="#990066") +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Total number of data entries of\n coal combustion sources across the U.S.") +
  theme(plot.title = element_text(lineheight=1, face="bold",vjust=2,size=rel(1.2)))
#print(g2)

# Save the plot with a png device.
png("plot4.png", height=800, width=700)
g_s <- grid.arrange(g,g2,nrow=1)
grid.arrange(g_s,g1, nrow=2)
dev.off()