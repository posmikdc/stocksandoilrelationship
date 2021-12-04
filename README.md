# stocksandoilrelationship
On February 19, 2016, Dr. Bernanke wrote about the relationship between stocks and oil prices. 
It caught my eye because his regression model was interesting to me. Bernanke estimates the model as a zero-intercept model. Additionally, he takes the differences of all variables and the log transform of all but one. This yields the following model:

reg1 <- lm(diff(log(wti)) ~ diff(log(copper)) + diff(log(dollar)) + 
             diff(tenyear) +0, data = df.test)
             
I set out to recreate his analysis and recreate his visualization.  
