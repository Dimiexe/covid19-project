#The aim of this project is, given a strong correlation between total covid19 cases and
#total covid19 caused deaths, to identify the average number of days it takes for a person, 
#who contracted the virus to succumb to it. The method that is followed here is as goes;
#we shift the curve of total cases towards the future, one day at a time, for a maximum of 
#40 days. On each shift the correlation between cases and deaths is calculated. After all
#the shifts are done, we find the one for which the correlation was highest and assume it to
#be the average number of days we are looking for.

#Read the data
data <- read.csv("D:/Work Projects/Healthcare R/Covid-19 project R/datasets/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv");

#Print a general description of the data
library(psych)
describe(data)
#Variables with an asterisk (*) are categorical so the calculated metrics are of no use.
#We can see that some variables have more observations than others and that indicates that NaN values exist in or dataset.
#We notice that the variables describing covid19 cases and deaths have a big std and that's to be expected as this is number
#growing by the day continuously.

#We want to investigate whether the number of total cases is related to the number of
#total deaths. (Since this is an aposteriory study we know that's indeed the case so
#we expect to see a high correlation between the two variables.)
library(corrplot)
## Calculate correlation and show the correlogram
res.cor <- cor(data[,c(3:12)])
corrplot(res.cor)
#We see numerous "?" in the correlation matrix because the function requires the data to have no NaN values.
#So we clear our data, dropping any lines containing NaN values.
cleaned_data <- na.omit(data)
res.cor <- cor(cleaned_data[,c(3:12)])
corrplot(res.cor)
#As expected we see a high, ~1 correlation between the number of cases and the number of deaths.

#Since we established that our ground rule is valid we continue with the rest of the study.
#We reload our full data and see which values the state column contains.
data <- read.csv("D:/Work Projects/Healthcare R/Covid-19 project R/datasets/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv");
states <- unique(data$state)
states

#we notice that the values "NY" and "NYC" could be joined as the population described by
#"NYC" is a sub-category of "NY". So we join them under "NY".
for (cols in colnames(data)){
  data[data$state == "NY",cols] <- data[data$state == "NY",cols] + data[data$state == "NYC",cols]
}
data <- data[data$state != "NYC",]
states <- unique(data$state)

#We are interested in exploring data in the 50 states so we exclude the rest.
states <- states[order(states)]
states <- states[! states %in% c("FSM","PW","AS","GU","VI","RMI","PR","MP", "DC")]
states

#We want all the graphs in one figure so we set the layout.
par(mfrow=c(5,10))

#For each valid state we do the following.
#We want to find the average days it takes for a person to die from covid19 since they
#got the virus. To calculate that, we shift the "tot_cases" curve by up to 40 days to
#the future. We make the hypothesis that the correlation between "tot_cases" and
#"tot_death" will be greater at some point when the shifted "tot_cases" curve fits
#more closely over the "tot_death" curve. The amount of days that the shifted "tot_cases"
#curve and the "tot_death" curve present the highest correlation is chosen as the
#average number of days for a person to succumb to the virus.
shift_per_state <- vector(length = length(states))
k <- 0
for (st in states){
  k<-k+1
  #reload the data for each iteration and keep only the desired columns, we do that
  #because we know that those columns contain no NaN values.
  data <- read.csv("D:/Work Projects/Healthcare R/Covid-19 project R/datasets/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv");
  data <- data[,c("submission_date","state","tot_cases","tot_death")]
  
  #Transform the "submission_date" into a date format readable by R for sorting purposes.
  data$submission_date <- as.Date(data$submission_date, "%m/%d/%Y")
  data <- data[data$state == st,]
  data <- data[order(data$submission_date),]
  rownames(data) <- 1:nrow(data)
  
  #Here, we exclude all days in the beginning of the pandemic when both the 
  #total number of cases and total number of deaths are 0.
  flag <- 0
  for (ii in c(1:length(data$submission_date))){
    if (!(data$tot_cases == 0 & data$tot_death == 0)){
      flag <- ii
      break
    }
  }
  data <- data[flag:length(data$submission_date),]
  rownames(data) <- 1:nrow(data)
  
  #Prepare the variables
  x <- data$submission_date
  y <- data$tot_cases
  y2 <- data$tot_death
  #plot(x,y, col="blue",type = "l")
  #lines(x,y2,col = "red")
  shift <- 40
  c <-vector(,length = shift)
  
  #Perform the shift
  for (ii in c(1:shift)){
    #plot(y[1:(length(y)-ii)],col = "blue",type = "l")
    #lines(y2[(1+ii):length(y2)],col="red")
    c[ii] <- cor(y[1:(length(y)-ii)],y2[(1+ii):length(y2)])
  }
  
  #Find the index (shift) for which we have the maximum correlation
  for (ii in c(1:length(c))){
    if (c[ii]==max(c)){
      #print(ii)
      ind <- ii
      break
    }
  }
  #Plot the graph of correlation with regards to days shifted. A peak in that
  #would indicate the number of shifted days for which we have max. correlation.
  plot(c,type="l", main = paste(st, as.character(ind)))
  shift_per_state[k] <- ind
}
#Return the mfrow var to its original value
par(mfrow=c(1,1))

#calculate mean of all states.
mean1 <- mean(shift_per_state) #10.56
#Calculate mean of states with avg number of days > 1.
mean2 <- mean(shift_per_state[shift_per_state != 1]) #14.66


#We will compare that mean to our whole US data
library(dplyr)
data <- read.csv("D:/Work Projects/Healthcare R/Covid-19 project R/datasets/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv");
data$submission_date <- as.Date(data$submission_date, "%m/%d/%Y")
for (cols in c("submission_date","state","tot_cases","tot_death")){
  data[data$state == "NY",cols] <- data[data$state == "NY",cols] + data[data$state == "NYC",cols]
}
data <- data[data$state %in% states,c("submission_date","state","tot_cases","tot_death")]
df_cases <- data[data$state == "AK",c("submission_date", "tot_cases")]
df_cases <- df_cases[order(df_cases$submission_date),]
rownames(df_cases) <- 1:nrow(df_cases)
df_deaths <- data[data$state == "AK",c("submission_date", "tot_death")]
df_deaths <- df_deaths[order(df_deaths$submission_date),]
rownames(df_deaths) <- 1:nrow(df_deaths)
for (st in states[2:length(states)]){
  temp <- data[data$state == st,]
  temp <- temp[order(temp$submission_date),]
  rownames(temp) <- 1:nrow(temp)
  df_cases$st <- temp[temp$state == st, c("tot_cases")]
  df_deaths$st <- temp[temp$state == st, c("tot_death")]
}

x <- df_cases$submission_date
y <- rowSums(df_cases[,c(2:ncol(df_cases))])
y2 <- rowSums(df_deaths[,c(2:ncol(df_deaths))])
shift <- 40
c <-vector(,length = shift)

for (ii in c(1:shift)){
  c[ii] <- cor(y[1:(length(y)-ii)],y2[(1+ii):length(y2)])
}

for (ii in c(1:length(c))){
  if (c[ii] == max(c)){
    #print(ii)
    ind <- ii
    break
  }
}
plot(c,type="l", main = paste("US", as.character(ind)))

#So, comparing our 3 USA averages we see that the mean of the states, either all of them or only
#those with an average greater than 1, are close to the calculated USA average day by using this
#method. So, as a conclusion we can say that a person in the US will succumb to the virus after an
#average of (10.56 + 14.66 + 13)/3 = ~13 days
