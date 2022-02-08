# Loading in necessary packages

library(tidyverse)
library(readxl)
library(dplyr)
library(scales)

# read in the excel file

n <- read_excel("NFL Excel.xlsx")

# Filter the data to create a new column called Region

n$state <-  word(n$DMA, -1)
View(n)
n$region <- ifelse(n$state =="MA"|n$state =="NY"|n$state == "VT"|n$state =="NH"|
                     n$state =="ME"|n$state =="CT"|n$state =="RI"|n$state =="PA"|
                     n$state =="NJ"|n$state =="DE"|n$state =="MD"|n$state =="DC"|
                     n$state=="MD)", "Northeast", NA) 
n$region <- ifelse(n$state == "FL"|n$state == "GA"|n$state =="AL"|n$state =="MS"|
                     n$state =="LA"|n$state =="TN"|n$state =="KY"|n$state =="AR"|
                     n$state =="NC"|n$state =="SC"|n$state =="VA"|n$state =="WV"|
                     n$state =="TN-VA","South", n$region)
n$region <- ifelse(n$state == "OH"|n$state =="IN"|n$state =="IL"|n$state =="MI"|
                     n$state =="WI"|n$state =="MN"|n$state =="ND"|n$state =="SD"|
                     n$state =="IA"|n$state =="NE"|n$state =="MO"|n$state =="KS",
                   "Midwest", n$region)
n$region <- ifelse(n$state =="TX"|n$state =="OK"|n$state =="NM"|n$state =="AZ"|
                     n$state =="UT"|n$state =="CO"|n$state =="WY"|n$state =="MT"|
                     n$state =="ID", "Rockies/SW", n$region)
n$region <- ifelse(n$state =="CA"|n$state =="NV"|n$state =="OR"|n$state =="WA"|
                     n$state =="AK"|n$state =="HI", "West Coast", n$region)

# fixing the column name "trump percentage 2016" to "TrumpP"

names(n)[9]<-"TrumpP"

# creating the new data frame with averages for each region

TP <- n %>% group_by(region) %>% summarise(TrumpAvg = mean(TrumpP))
View(TP)

# 2. base graphs, bar graph with average by region and boxplot with same

ggplot(data = TP, mapping = aes(x = region, y = TrumpAvg, fill = region)) + 
  geom_bar(stat='identity') + ylim(c(0, 1)) + geom_text(aes(label=round(TrumpAvg,digits = 2))) +
  labs(title = 'Trump Popularity by Region',
       x = 'Region', 
       y = 'Average Percentage to vote for Trump (%)')

ggplot(data = n, mapping = aes(x = region,y = TrumpP, fill = region)) + 
  geom_boxplot() + ylim(c(0, 1)) +
  labs(title = 'Trump Popularity by Region',
        x = 'Region', 
        y = 'Average Percentage to vote for Trump')

# Filter by region then compare each DMA per region to the region avg

NE <- n %>% filter(region == 'Northeast')

plot1 <- ggplot(data = NE, mapping = aes(x = DMA, y = TrumpP, fill = DMA)) + 
  geom_bar(stat = 'identity',show.legend = FALSE) + labs(x = "DMA", y = "TrumpP") +
  theme(axis.text.x = element_text(angle = 90, size = 7, 
                                   vjust = .4)) 

plot2 <- plot1 + geom_hline(yintercept = mean(NE$TrumpP)) + 
  geom_text(aes(label=round(NE$TrumpP,digits = 2)))

plot2


# Midwest: NBA by Trump
MW <- n %>% filter(region == 'Midwest')

ggplot(data = MW, mapping = aes(x = TrumpP, y = NBA )) + geom_point()+
  geom_smooth(method = 'lm', se = FALSE) + labs(title = "Midwest")
cor.test(MW$TrumpP, MW$NBA)
sd(MW$TrumpP, MW$NBA)


# West Coast: NBA by Trump
WC <- n %>% filter(region == 'West Coast')

ggplot(data = WC, mapping = aes(x = TrumpP, y = NBA )) + geom_point()+
  geom_smooth(method = 'lm', se = FALSE) + labs(title = "West Coast")
cor.test(WC$TrumpP, WC$NBA)
sd(WC$TrumpP, WC$NBA)

# South Nascar by trump

SO <- n %>% filter(region == 'South')

ggplot(data = SO, mapping = aes(x = TrumpP, y = NASCAR )) + geom_point()+
  geom_smooth(method = 'lm', se = FALSE) + labs(title = "South")
cor.test(SO$TrumpP, SO$NASCAR)
sd(SO$TrumpP, SO$NASCAR)

#Rockies: Nascar by Trump

RSW <- n %>% filter(region == 'Rockies/SW')

ggplot(data = RSW, mapping = aes(x = TrumpP, y = NASCAR )) + geom_point()+
  geom_smooth(method = 'lm', se = FALSE) + labs(title = "Rockies/SW")
cor.test(RSW$TrumpP, RSW$NASCAR)
sd(RSW$TrumpP, RSW$NASCAR)

# Northeast:  Nascar by Trump

NE <- n %>% filter(region == 'Northeast')

ggplot(data = NE, mapping = aes(x = TrumpP, y = NASCAR )) + geom_point()+
  geom_smooth(method = 'lm', se = FALSE) + labs(title = "Northeast")
cor.test(NE$TrumpP, NE$NASCAR)

sd(NE$TrumpP, NE$NASCAR)








       