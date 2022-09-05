library(ggplot2)
NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")
year_emissions <- NEI[ , c(4,6)]
year_emissions_sum <- aggregate(Emissions ~ year, year_emissions, sum)

png("plot1.png")
plot(year_emissions_sum$year, year_emissions_sum$Emissions, type = "l", pch = 19, xlab = "Year", ylab = "Total US Pm2.5 Emissions in Tons", main = "Total US Pm2.5 Emissions by Year")
dev.off()
maryland_df <- subset(NEI, fips == "24510")
maryland_df <- aggregate(Emissions ~ year, maryland_df, sum)

png("plot2.png")
plot(maryland_df$year, maryland_df$Emissions, xlab = "Year", ylab = "Total Pm2.5 Emissions in Tons", main = "Total US Pm2.5 Emissions by Tons in Maryland", type = "l")
dev.off()

png("plot3.png")
type_emissions <- aggregate(Emissions ~ type * year, maryland_df, sum)
ggplot(type_emissions, aes(x = year, y = Emissions, color = type)) + geom_point() + geom_line() + labs(x = "Year", y = "Total Pm2.5 Emissions in Tons") + ggtitle("Total US Pm2.5 Emissions Per Year by Type")
dev.off()

merge_df <- merge(NEI, SCC, by = "SCC")
has_coal <- grepl("coal", as.character(merge_df$Short.Name), ignore.case = TRUE)
coal_df <- merge_df[has_coal, ]
coal_emissions <- aggregate(Emissions ~ year + type, data = coal_df, sum)
png("plot4.png")
ggplot(coal_emissions, aes(x= year, y = Emissions, color = type)) + geom_point() + geom_line() + labs(x = "Year", y = "Total Pm2.5 Emissions in Tons") + ggtitle("Total Emissions from Coal Combustion-Related Sources by Year")
dev.off()

motor_df <- subset(NEI, fips == "24510" & type == "ON-ROAD" )
baltimore_motor_emissions <- aggregate(Emissions ~ type + year, motor_df, sum)
png("plot5.png")
ggplot(baltimore_motor_emissions, aes(x = year, y=Emissions))+geom_point() + geom_line() + labs(x = "Year", y= "Total Pm2.5 Emissions in Tons") + ggtitle("Total Pm2.5 Emissions in Tons by Year in Baltimore from Motor Vehicles")
dev.off()

baltimore_la_df <- subset(NEI, NEI$fips %in% c("06037", "24510" )& type == "ON-ROAD")
baltimore_la_df_emissions <- aggregate(Emissions ~ type+year+fips, baltimore_la_df, sum)
png("plot6.png")
ggplot(baltimore_la_df_emissions, aes(x=year, y=Emissions, color = fips)) + geom_point() + geom_line() + labs(x="Year", y = "Total Pm2.5 Emissions in Tons") + ggtitle("Total Pm2.5 Emissions in Tons by Year in Baltimore and Los Angeles from Motor Vehicles") + scale_colour_discrete(name = "City", labels = c("Los Angeles", "Baltimore"))
dev.off()

