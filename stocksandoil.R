### Set Up
rm(list=ls())
setwd("C:/Users/dposm/Downloads")
df <- read.csv("stocksandoil.csv")
df$date <- as.Date(df$date)
df$index <- 1:nrow(df)

### EDA and Data Cleaning
dim(df)
str(df)
names(df)
summary(df)

### Test and Validation Data
start <- as.numeric(nrow(data.frame(which(df$date <= "2011-06-01", arr.ind=TRUE)))) 
cutoff <- as.numeric(nrow(data.frame(which(df$date <= "2014-06-01", arr.ind=TRUE))))
end <- as.numeric(nrow(df))

df.test <- df[start:cutoff,] #Selecting training data set from 6/1/2011 through 6/1/2014.
df.valid <- df[(cutoff+1):end, ] #Selecting remainder for validation set.

### Fitting the regression model
reg1 <- lm(diff(log(wti)) ~ diff(log(copper)) + diff(log(dollar)) + 
             diff(tenyear) +0, data = df.test)
summary(reg1)   
#We can see that the results are consistent with Bernanke's 2016 findings.

### Creating the prediction vector, where 
### reg1$coefficients are 0.396 (copper), -0.74296 (dollar), and 0.06294 (tenyear)
prediction_raw <- exp(diffinv(0.396*(diff(log(df.valid$copper))) + (-0.74296)*(diff(log(df.valid$dollar))) +
                                0.06294*(diff(df.valid$tenyear))))
prediction <- prediction_raw*100 #Multiply from 100 because the log changes are in percentage terms

### Indexing and Merging
prediction_df <- data.frame(prediction)
prediction_df$index <- (cutoff+1):end 

df_final <- merge(df, prediction_df, by=c("index"), all = TRUE)
df_final <- data.frame(df_final)

### Data Visualization
ggplot(df_final, aes(x=date)) + #ggplot call
  geom_line(aes(y = prediction), color = "skyblue", alpha = 0.95, size = 1.2) + #WTI
  geom_line(aes(y = wti), color = "steelblue", size = 1.2) + #Oil Demand
  theme_bw() + #Theme (theme_bw closely resembles to chart)
  labs(color = "black", #Title color
       title = "Figure 3: WTI Crude Estimated Demand Effect") + #Title
  theme(panel.grid.major.x = element_blank(), #Remove major horizontal orientation lines
        panel.grid.minor.x = element_blank(), #Remove minor horizontal orientation lines
        axis.title.x=element_blank(), #Remove x-axis title
        axis.text.x = element_text(angle = 90), #Rotate x-axis labels
        axis.title.y=element_text(), #Add y-axis title
        ) + 
  scale_x_date(date_breaks = "2 months") + #Adjust x-axis steps
  annotate("text", x=as.Date("2016-01-01"), y=55, label= "WTI") + #2% Target Annotation
  annotate("text", x = as.Date("2016-01-01"), y=92, label = "Oil Demand Only") + #Core PCE Inflation Annotation
  ylab("Dollars per Barrel")