##imports
library(readr)
library(ggplot2)
##https://scales.r-lib.org/
##Library used for formating numbers on a graph 
library(scales)




##https://cran.r-project.org/web/packages/rio/vignettes/rio.html
##Considered reformatting to CSV but found method to read the tsv file and all regular ggplot methods seem to work the same
##So I just went with this approch. 
custdata <- read_tsv("CSVFiles/custdata.tsv")


##Data set from home work1
Swimming_Data <- read_csv("CSVFiles/Olympic_Swimming_Results_1912to2020.csv")

custdata

##Used to remove all null values from custdata
custdata <- na.omit(custdata)
custdata

##Average of the income in custdaa
mean_income <- mean(custdata$income )


mean_income

##Replaces any vlaues just labeled as zero with the average of the income column
custdata$income[custdata$income == 0] <- mean_income


custdata$income 


##Used to help format the numbers in income
custdata$income(labels = dollar)
##----------------------------------------------------------------------------------------------------------

##https://www.geeksforgeeks.org/how-to-make-density-plots-with-ggplot2-in-r/

##Creates density plot of customer income 
## A density plot is different from a hisogram because it is used to show the distribution of the data values rather
##Than frequency. 


ggplot(custdata, aes(x = income)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean_income), 
             color = "red", linetype = "dashed", size = 1) +
  
scale_x_continuous(labels = dollar) +
 scale_y_continuous(labels = percent_format(accuracy = 0.00001)) +
  labs(title = "Density Plot of Income",x = "Income (USD)",y = "Density (%)")


##------------------------------------------------------------------------------------------------
##Here we have a graph showing customers renting vs customers who own homes with or without mortages 
##The graph shows that it people generally pay slightly more mortages than renting.
##Here's the source of where i found the code for formating the x ticks on the graph
##https://www.geeksforgeeks.org/rotating-x-axis-labels-and-changing-theme-in-ggplot2/


ggplot(custdata) + geom_bar(aes(x=custdata$housing.type), fill="yellow")+
  
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Home owning vs renting costs",x = "Cost", y = "Home owning and renting")


##---------------------------------------------------------------------------------------

##Swimming data set
Swimming_Data

##Mask to filter only Butterfly events
Butterfly_data <- subset(Swimming_Data, Stroke =="Butterfly")
##Mask to filter only Micheal Phelps as the Athlete
Micheal_phelps_Data <- subset(Butterfly_data, Athlete =="Michael Phelps")
##Mask to filter the distance as 100 meters
Micheal_phelps_Data_cleaned <- subset(Micheal_phelps_Data, `Distance (in meters)` =="100m")


##Final data mask with all filters applied
Micheal_phelps_Data_cleaned

##For this assignment i wanted to focus on tracking individual Athletes rather than doing big general analysis
## For this reason i wanted to track time progression using a scatter plot. Here we actually see an interesting trend where Phelps reached the his
##fastest time earlier in his career in 2008 which is typically not regular for most swimmers who are actively training and growing in their ablitles
##So to see this from someone who is considered to be the greatest swimmer to ever live is fairly interesting. 

ggplot(Micheal_phelps_Data_cleaned) + geom_point(aes(x=Year, y=Results), color="magenta", size=4,
                            shape="triangle") +
  labs(title="Micheal Phelps 100 fly time progression",x = "Finishing time", y = "Year Swam")


##------------------------------------------------------------------------------------------------------------
##Filter to so the only Athlete is Katie Ledecky
Katie_Ledecky_Data <- subset(Swimming_Data, Athlete =="Katie Ledecky")

Katie_Ledecky_Data


## This bar graph simply shows the number of gold medals that Katie Ledecky Won from 2012-2020
## This is almost like an inverse trend as we saw previously since as the years went on Katie ended up winning more medals overall
##This is a very  simplistic graph but shows a way we can track overall trends for Athletes over they're olympic career
ggplot(Katie_Ledecky_Data) + geom_bar(aes(x=Year), fill="pink")+
  labs(title = "Number of Gold metals won at the Olympics by Katie Ledecky",x = "Year", y = "Number of gold medals")



##------------------------------------------------------------------------------------------

##Subset used to filter only 50 meter events
Stroke_data <- subset(Swimming_Data, Swimming_Data$`Distance (in meters)` =="50m")

##Used to filter only freestyle events
Stroke_data <-  subset(Swimming_Data, Swimming_Data$Stroke == "Freestyle")


Stroke_data

##was having very unorthidox issues while trying to make this graph speifically So I put this here to try help with the trouble shooting process
##I'm fairly certain there aren't any null values in the subset to begin with but I put this here just to be sure.
Stroke_data_cleaned <- na.omit(Stroke_data)
##-------------------------------------------------
Stroke_data_cleaned
##Group to find the min in the Result column and group by year
Stroke_data_agg <-aggregate(Results ~ Year, data = Stroke_data_cleaned,FUN = max)



print(Stroke_data_agg)

##This shows the regression of the slowest times in the 50 meter free style by year graph shows an overall decrease in the slowest times
##contributing to the overall trend of the olympic getting faster over time.








##---------------------------------------------------------------------------------
swim_data_50 <- subset(Swimming_Data, 
                      `Distance (in meters)` == "100m" & Results < 59.99)


swim_data_50
##Decided to see if there was a correlation between the placement (or number of placements) 
## and the year swam. The score suggests a weak trend between these two columns.
Swim_corr  <- cor(Swim_data_50$Results,Swim_data_50$Year)
print(Swim_corr)


##------------------------------------------------------------------
##Wanted to see if there was a correlation between the location of the olmpics and the relays or number of relays swam at the,
Swim_cont_table = table(Swimming_Data$Results, Swimming_Data$Year)
print(Swim_cont_table)





## P value for chi square test indicates that there exists no stronge correlation between these values. 
chisq.test(Swim_cont_table)




##------------------------------------------------------------------------------------------

Swim_cont_table = table(Swimming_Data$Results, Swimming_Data$Location)
print(Swim_cont_table)





## P value for chi square test indicates that there exists no stronge correlation between these values. 
chisq.test(Swim_cont_table)

Swimming_Data

##----------------------------------------------------------------

library(rcompanion)
cramerV(table(Swimming_Data$Location, Swimming_Data$Results))


Swim_cont_table2 = table(Swimming_Data$Location, Swimming_Data$Results)
##print(Swim_cont_table)


chisq.test(Swim_cont_table2)


library(ggplot2)
ggplot(Swimming_Data, aes(x =Swimming_Data$Location , fill = Swimming_Data$Results)) +
  geom_bar(position = "dodge") +
  labs(title = "Grouped Bar Plot of cat1 vs cat2")











##----------------------------------------------------------------------------------------


nrow(Swim_data_50)
head(Swim_data_50)


Swim_data_50$Year <- as.numeric(as.character(Swim_data_50$Year))
Swim_data_50$Results <- as.numeric(as.character(Swim_data_50$Results))

nrow(Swim_data_50)
head(Swim_data_50)

clean_data <- Swim_data_50[!is.na(Swim_data_50$Year) & !is.na(Swim_data_50$Results), ]


x <- clean_data$Year
y <- clean_data$Results

# Calculate Spearman correlation
result <- cor(x, y, method = "spearman")

# Print the result
cat("Spearman correlation coefficient is:", result, "\n")