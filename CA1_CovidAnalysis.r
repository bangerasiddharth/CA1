install.packages("ggplot2")                                                     #Install Data Visualization package
install.packages("mice")                                                        #Install mice package for visualizing missing values
install.packages("VIM")                                                         #Install VIM package for visualizing missing values
install.packages("psych")                                                       #Install psych package for correlation analysis between multiple variables
install.packages("magrittr")                                                    #Install magrittr package are only needed the first time you use it
install.packages("dplyr")                                                       #Install dplyr package for faster development processing and improved readability
install.packages("sqldf")                                                       #Package to write sql queries
install.packages("hrbrthemes")                                                  #Package for modified line chart
library(magrittr)                                                               
library(dplyr)                                                                  
library(psych)
library(VIM)
library(ggplot2)
library(mice)
library(plotrix)
library(sqldf)
library(hrbrthemes)
#-------------------------------------------------------------------------------
covid_data <- read.csv("covid.csv", na = "", header = T, stringsAsFactors = T)  #Loading the covid.csv data into covid_data dataframe
covid_data [covid_data == ""] <- NA                                             #Assigning blank spaces with NA
head(covid_data,5)                                                              #Displaying the first 5 sample records
str(covid_data)                                                                 #Verifying the datatypes of the fields
covid_data$date <- as.Date(covid_data$date)                                     #Converting datatype of column date from chr to Date
str(covid_data)                                                                 #Verifying the datatype conversion
nrow(covid_data)                                                                #Displaying the number of rows - 84529
ncol(covid_data)                                                                #Displaying the number of columns - 59
summary(covid_data)                                                             #Displaying the Summary
any(is.na(covid_data))                                                          #Returns true meaning columns in covid_data dataframe has NA values
entire_data <-covid_data[complete.cases(covid_data),]                           #Loading only those entries having no NA records
nrow(entire_data)                                                               #Returns 0 meaning there are no such records having data in all the columns
na_data <-covid_data[!complete.cases(covid_data),]                              #Loading only those entries having NA values in columns
nrow(na_data)                                                                   #Returns 84529 which is equal to the total number of rows. Meaning there are NA values in few columns for all the records
names(which(sapply(covid_data, anyNA)))                                         #53 columns out of 59 columns has NA entries for the given records                                                                
md.pattern((covid_data))                                                        #Visualizing the missing pattern using mice library
incomplete_data <- aggr(covid_data, prop = FALSE, numbers = TRUE)               #Visualizing the missing data using VIM library
summary(incomplete_data)                                                        #Displaying the summary of missing data
#-------------------------------------------------------------------------------

total_cases<-aggregate(covid_data$total_cases~covid_data$location,
                       covid_data,FUN = max)                                    #Grouping countries and their total cases and loading into total_cases dataframe
total_cases5<-total_cases[order(-total_cases$`covid_data$total_cases`),][1:6,]  #Selecting Top 5 continents sorted by the total cases in descending order
total_cases5 <- total_cases5 %>% filter(total_cases5$`covid_data$location`
                                        != 'World')                             #Filter outing worldwide records just to display continent wise order

new_cases<-aggregate(covid_data$new_cases~covid_data$location,
                     covid_data,FUN = max)                                      #Grouping location wise new cases count
new_cases5<-new_cases[order(-new_cases$`covid_data$new_cases`),][1:6,]          #Selecting Top 5 locations sorted by the new cases in descending order
new_cases5 <- new_cases5 %>% filter(new_cases5$`covid_data$location`
                                    != 'World')                                 #Filter outing worldwide records just to display continent wise order

total_deaths<-aggregate(covid_data$total_deaths~covid_data$location,
                        covid_data,FUN = max)                                   #Grouping location wise total deaths
total_deaths5<-total_deaths[order(-total_deaths$`covid_data$total_deaths`),][1:6,]#Selecting Top 5 continents sorted by the total deaths in descending order
total_deaths5 <- total_deaths5 %>% filter(total_deaths5$`covid_data$location`
                                          != 'World')                           #Filter outing worldwide records just to display continent wise order

new_deaths<-aggregate(covid_data$new_deaths~covid_data$location,
                      covid_data,FUN = max)                                     #Grouping location wise total new deaths
new_deaths5<-new_deaths[order(-new_deaths$`covid_data$new_deaths`),][1:6,]      #Selecting Top 5 continents sorted by the total new deaths in descending order
new_deaths5 <- new_deaths5 %>% filter(new_deaths5$`covid_data$location`
                                      != 'World')                               #Filter outing worldwide records just to display continent wise order

barplot(total_cases5$`covid_data$total_cases`,
        names.arg = total_cases5$`covid_data$location`, 
        main="Top 5 locations by total cases",las=3,col="Orange")               #Plotting a bar chart displaying the total cases so far by location in descending order

barplot(new_cases5$`covid_data$new_cases`,
        names.arg = new_cases5$`covid_data$location`,
        main="Top 5 locations by total new cases",las=3,col="orange")           #Plotting a bar chart displaying the new cases by location in descending order

barplot(total_deaths5$`covid_data$total_deaths`,
        names.arg = total_deaths5$`covid_data$location`,
        main="Top 5 locations by total deaths",las=3,col="Red")                 #Plotting a bar chart displaying the total deaths by location in descending order

barplot(new_deaths5$`covid_data$new_deaths`,
        names.arg = new_deaths5$`covid_data$location`,
        main="Top 5 locations by number of new deaths",las=3,col="Red")         #Plotting a bar chart displaying the new deaths by location in descending order


asia_data <- sqldf("select new_cases, new_deaths, date from covid_data 
                   where location = 'Asia'")

europe_data <- sqldf("select new_cases, new_deaths, date from covid_data 
                     where location = 'Europe'")
attach(asia_data)
ggplot(asia_data, aes(x=date, y=new_cases)) +
  geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=2) +
  theme_ipsum() +
  ggtitle("Trend of new cases in Asia")                                         #Plotting dotted line chart to show trend in new cases for Asia

ggplot(asia_data, aes(x=date, y=new_deaths)) +
  geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=2) +
  theme_ipsum() +
  ggtitle("Trend of new deaths in Asia")                                        #Plotting dotted line chart to show trend in new deaths for Asia

attach(europe_data)
ggplot(europe_data, aes(x=date, y=new_cases)) +
  geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=2) +
  theme_ipsum() +
  ggtitle("Trend of new cases in Europe")                                       #Plotting dotted line chart to show trend in new cases for Europe

ggplot(europe_data, aes(x=date, y=new_deaths)) +
  geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=2) +
  theme_ipsum() +
  ggtitle("Trend of new deaths in Europe")                                      #Plotting dotted line chart to show trend in new deaths for Europe

#-------------------------------------------------------------------------------
#                            RESEARCH QUESTIONS
#-------------------------------------------------------------------------------
# 1. Does vaccination has effect on number of patients in ICU in USA?
# H0 : Vaccination has no effect on the number of ICU patients
# H1 : vaccination has effect on the number of ICU patients
#-------------------------------------------------------------------------------

usa_covid_data = subset(covid_data, iso_code == "USA", 
                        select = c("icu_patients", "people_fully_vaccinated"))  #Populating USA specific data in columns icu_patients and people_fully_vaccinated 
md.pattern(usa_covid_data)                                                      #Displaying missing pattern using mice library
summary(usa_covid_data)             

pairs(usa_covid_data, labels = colnames(usa_covid_data), col = "#c68c53" ,
      main = "CORRELATION PLOT")                                                #Displaying a scatterplot to get an idea of relation between icu_patients and people_fully_vaccinated variables

usa_covid_data$people_fully_vaccinated[is.na(usa_covid_data$people_fully_vaccinated)] <- 0
usa_covid_data <- na.omit(usa_covid_data)                                       #Ensuring that NA values in people_fully_vaccinated are replaced with 0 and other NA values from icu_patients are omitted
md.pattern(usa_covid_data)                                                      #Displaying the missing data patterns

pairs.panels(usa_covid_data,                                                    #Displaying correlation between variables
             smooth = TRUE,                                                     #If TRUE, draws loess smooths
             scale = FALSE,                                                     #If TRUE, scales the correlation text font
             density = TRUE,                                                    #If TRUE, adds density plots and histograms
             ellipses = TRUE,                                                   #If TRUE, draws ellipses
             method = "spearman",                                               #Correlation method (also "pearson" or "kendall")
             pch = 21,                                                          #pch symbol
             lm = FALSE,                                                        #If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,                                                        #If TRUE, reports correlations
             jiggle = FALSE,                                                    #If TRUE, data points are jittered
             factor = 2,                                                        #Jittering factor
             hist.col = 4,                                                      #Histograms color
             stars = TRUE,                                                      #If TRUE, adds significance level with stars
             ci = TRUE)                                                         #If TRUE, adds confidence intervals

library("lattice")                                                              #ICU patients : continuous variable, People fully vaccinated : continuous variable(interval)
attach(usa_covid_data)
plot(icu_patients, people_fully_vaccinated, pch = 19, col = "#c68c53")

histogram(~icu_patients | people_fully_vaccinated, 
          data = usa_covid_data, 
          main = "ICU Patients vs People fully vaccinated", 
          xlab = "ICU Patients", 
          ylab = "People fully Vaccinated")                                     #Plotting histogram
tapply(icu_patients, people_fully_vaccinated, median)                           #tapply is used to compute median of the variables

qqnorm(icu_patients)                                                            #Checking normality of data
qqline(icu_patients, col = "#c68c53")                                           #Line representing normal distribution

normality_test <- shapiro.test(usa_covid_data$icu_patients)                     #Checking for normality between variables using Shapiro-Wilks test
normality_test$p.value                                                          #P-value turns out to be 0.000000000000000238
hist(icu_patients)                                                              #Data isnt normally distributed
cor.test(usa_covid_data$people_fully_vaccinated, usa_covid_data$icu_patients,  
         method = "spearman")                                                   #Using Spearman's correlation coefficient

detach(usa_covid_data)

#It can be clearly seen tat the p-value is less than 0.05, meaning we can 
#reject the null hypothesis.
#As evident, there is a negative correlation present between the number of
#ICU patients and the number of people who are fully vaccinated
#-------------------------------------------------------------------------------
# 2. Does vaccination has effect on number of new cases in Ireland?
# H0 : Vaccination has no effect on the number of new cases
# H1 : vaccination has effect on the number of new cases
#-------------------------------------------------------------------------------

IRL_covid_data = subset(covid_data, iso_code == "IRL", 
                        select = c("new_cases", "people_fully_vaccinated"))     #Populating IRL specific data in columns new_cases and people_fully_vaccinated 
md.pattern(IRL_covid_data)                                                      #Displaying missing pattern using mice library
summary(IRL_covid_data)             

pairs(IRL_covid_data, labels = colnames(IRL_covid_data), col = "#00ace6" ,
      main = "CORRELATION PLOT")                                                #Displaying a scatterplot to get an idea of relation between new_cases and people_fully_vaccinated variables

IRL_covid_data$people_fully_vaccinated[is.na(IRL_covid_data$people_fully_vaccinated)] <- 0
IRL_covid_data <- na.omit(IRL_covid_data)                                       #Ensuring that NA values in people_fully_vaccinated are replaced with 0 and other NA values from new_cases are omitted
md.pattern(IRL_covid_data)                                                      #Displaying the missing data patterns

pairs.panels(IRL_covid_data,                                                    #Displaying correlation between variables
             smooth = TRUE,                                                     #If TRUE, draws loess smooths
             scale = FALSE,                                                     #If TRUE, scales the correlation text font
             density = TRUE,                                                    #If TRUE, adds density plots and histograms
             ellipses = TRUE,                                                   #If TRUE, draws ellipses
             method = "spearman",                                               #Correlation method (also "pearson" or "kendall")
             pch = 21,                                                          #pch symbol
             lm = FALSE,                                                        #If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,                                                        #If TRUE, reports correlations
             jiggle = FALSE,                                                    #If TRUE, data points are jittered
             factor = 2,                                                        #Jittering factor
             hist.col = 4,                                                      #Histograms color
             stars = TRUE,                                                      #If TRUE, adds significance level with stars
             ci = TRUE)                                                         #If TRUE, adds confidence intervals

library("lattice")                                                              #ICU patients : continuous variable, People fully vaccinated : continuous variable(interval)
attach(IRL_covid_data)
plot(IRL_covid_data$new_cases, IRL_covid_data$people_fully_vaccinated, pch = 19, 
     col = "#00ace6")

histogram(~IRL_covid_data$new_cases | IRL_covid_data$people_fully_vaccinated, 
          data = IRL_covid_data, 
          main = "New Cases vs People fully vaccinated", 
          xlab = "New Cases", 
          ylab = "People fully Vaccinated")                                     #Plotting histogram
tapply(IRL_covid_data$new_cases, IRL_covid_data$people_fully_vaccinated, median)#tapply is used to compute mean of the variables

qqnorm(IRL_covid_data$new_cases)                                                #Checking normality of data
qqline(IRL_covid_data$new_cases, col = "#00ace6")                                   #Line representing normal distribution

normality_test <- shapiro.test(IRL_covid_data$new_cases)                        #Checking for normality between variables using Shapiro-Wilks test
normality_test$p.value                                                          #P-value turns out to be 0.0000000000000000000000000000124
hist(new_cases)                                                                 #Data isnt normally distributed
cor.test(IRL_covid_data$people_fully_vaccinated, IRL_covid_data$new_cases,  
         method = "spearman")                                                   #Using Spearman's correlation coefficient

detach(IRL_covid_data)
#It can be clearly seen tat the p-value is less than 0.05, meaning we can 
#reject the null hypothesis.
#As evident, number of new cases and people fully vaccinated are inversely related.
#Meaning, we can reject the null hypothesis and accept the alternate hypothesis.
#-------------------------------------------------------------------------------
# 3. Handwashing facilities relates to more covid deaths
# H0 : Handwashing facilities has no effect on covid deaths
# H1 : Handwashing facilities has effect on covid deaths
#-------------------------------------------------------------------------------

handwash_vs_deaths <- subset(covid_data, !is.na(covid_data$handwashing_facilities),
                             select = c("handwashing_facilities", "total_deaths")) #Populating just the columns handwashing_facilities and total_deaths in handwash_vs_deaths
handwash_vs_deaths
md.pattern(handwash_vs_deaths)
summary(handwash_vs_deaths)

pairs(handwash_vs_deaths, labels = colnames(handwash_vs_deaths), 
      main = "CORRELATION PLOT", col=("#00e68a"))

na.omit(handwash_vs_deaths)
handwash_vs_deaths <- na.omit(handwash_vs_deaths)
handwash_vs_deaths

library(psych)
pairs.panels(handwash_vs_deaths,
             smooth = TRUE,                                                     #If TRUE, draws loess smooths
             scale = FALSE,                                                     #If TRUE, scales the correlation text font
             density = TRUE,                                                    #If TRUE, adds density plots and histograms
             ellipses = TRUE,                                                   #If TRUE, draws ellipses
             method = "spearman",                                               #Correlation method (also "pearson" or "kendall")
             pch = 21,                                                          #pch symbol
             lm = FALSE,                                                        #If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,                                                        #If TRUE, reports correlations
             jiggle = FALSE,                                                    #If TRUE, data points are jittered
             factor = 2,                                                        #Jittering factor
             hist.col = 4,                                                      #Histograms color
             stars = TRUE,                                                      #If TRUE, adds significance level with stars
             ci = TRUE)                                                         #If TRUE, adds confidence intervals


library("lattice")
attach(handwash_vs_deaths)
plot(handwash_vs_deaths$total_deaths, handwash_vs_deaths$handwashing_facilities, 
     pch = 19, col = "#00e68a")

histogram(~total_deaths | handwashing_facilities, 
          data = handwash_vs_deaths, 
          main = "Total deaths vs Handwashing facilities", 
          xlab = "Total deaths", 
          ylab = "Handwash facilities")                                         #Plotting histogram
tapply(handwash_vs_deaths$total_deaths, 
       handwash_vs_deaths$handwashing_facilities, 
       median)


qqnorm(handwash_vs_deaths$total_deaths)                                         #Checking normality of data
qqline(handwash_vs_deaths$total_deaths, col = "#00e68a")                        #Line representing normal distribution

temp <- sample_n(handwash_vs_deaths, 5000)                                      #Checking for normality between variables using Shapiro-Wilks test
normality_test <- shapiro.test(handwash_vs_deaths$total_deaths)
normality_test$p.value                                                          #p value turns out to be 0.0000000000000000000000000000000125

hist(handwash_vs_deaths$total_deaths)                                           #not normally distributed

cor.test(handwash_vs_deaths$handwashing_facilities, handwash_vs_deaths$total_deaths,  
         method = "spearman")

detach(handwash_vs_deaths)

#It can be clearly seen tat the p-value is less than 0.05, meaning we can 
#reject the null hypothesis.
#As evident, number of deaths and handwashing facilities relate to less covid 
#deaths.
#Meaning, we can reject the null hypothesis and accept the alternate hypothesis.
#-------------------------------------------------------------------------------
# 4. High stringency_index relates to more number of new covid deaths for UK
# H0 : Stringency_index does not effect number of new covid deaths
# H1 : Stringency_index has effects on number of new covid deaths
#-------------------------------------------------------------------------------

newdeaths_vs_stringency_index = subset(covid_data, iso_code == "GBR", 
                                       select = c("new_deaths", 
                                                  "stringency_index"))          #Populating UK specific data in columns new_deaths and stringency_index 
md.pattern(newdeaths_vs_stringency_index)                                       #Displaying missing pattern using mice library
summary(newdeaths_vs_stringency_index)             

pairs(newdeaths_vs_stringency_index, labels = colnames(newdeaths_vs_stringency_index), 
      col = "#ffbf00" ,
      main = "CORRELATION PLOT")                                                #Displaying a scatterplot to get an idea of relation between new_deaths and stringency_index variables

newdeaths_vs_stringency_index$stringency_index[is.na(newdeaths_vs_stringency_index$stringency_index)] <- 0
newdeaths_vs_stringency_index <- na.omit(newdeaths_vs_stringency_index)         #Ensuring that NA values in stringency_index are replaced with 0 and other NA values from new_deaths are omitted
md.pattern(newdeaths_vs_stringency_index)                                       #Displaying the missing data patterns

pairs.panels(newdeaths_vs_stringency_index,                                     #Displaying correlation between variables
             smooth = TRUE,                                                     #If TRUE, draws loess smooths
             scale = FALSE,                                                     #If TRUE, scales the correlation text font
             density = TRUE,                                                    #If TRUE, adds density plots and histograms
             ellipses = TRUE,                                                   #If TRUE, draws ellipses
             method = "spearman",                                               #Correlation method (also "pearson" or "kendall")
             pch = 21,                                                          #pch symbol
             lm = FALSE,                                                        #If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,                                                        #If TRUE, reports correlations
             jiggle = FALSE,                                                    #If TRUE, data points are jittered
             factor = 2,                                                        #Jittering factor
             hist.col = 4,                                                      #Histograms color
             stars = TRUE,                                                      #If TRUE, adds significance level with stars
             ci = TRUE)                                                         #If TRUE, adds confidence intervals

library("lattice")                                                              
plot(new_deaths, newdeaths_vs_stringency_index$stringency_index, pch = 19, col = "#ffbf00")

histogram(~new_deaths | stringency_index, 
          data = newdeaths_vs_stringency_index, 
          main = "New deaths vs Stringency index", 
          xlab = "New deaths", 
          ylab = "Stringency index")                                            #Plotting histogram
tapply(newdeaths_vs_stringency_index$new_deaths, 
       newdeaths_vs_stringency_index$stringency_index, median)                  #tapply is used to compute mean of the variables

qqnorm(newdeaths_vs_stringency_index$new_deaths)                                #Checking normality of data
qqline(newdeaths_vs_stringency_index$new_deaths, col = "#ffbf00")                   #Line representing normal distribution

normality_test <- shapiro.test(newdeaths_vs_stringency_index$new_deaths)        #Checking for normality between variables using Shapiro-Wilks test
normality_test$p.value                                                          #P-value turns out to be 0.0000000000000002
hist(newdeaths_vs_stringency_index$new_deaths)                                  #Data isnt normally distributed
cor.test(newdeaths_vs_stringency_index$stringency_index, 
         newdeaths_vs_stringency_index$new_deaths,  
         method = "spearman")                                                   #Using Spearman's correlation coefficient

detach(newdeaths_vs_stringency_index)

#It can be clearly seen tat the p-value is less than 0.05, meaning we can 
#reject the null hypothesis.
#As evident, there is a negative correlation present between the stringency
#index and the number of new covid deaths
#-------------------------------------------------------------------------------
# 5. Are total number of covid deaths and diabetes related?
# H0 : Total covid deaths and diabetes prevalence are not related
# H1 : Total covid deaths and diabetes prevalence show some relationship
#-------------------------------------------------------------------------------

attach(covid_data)
deaths_vs_diabetes_subset <- subset(covid_data, 
                                    select = c(iso_code, location, date, total_deaths, 
                                               diabetes_prevalence))
md.pattern(deaths_vs_diabetes_subset)                                           #Using mice library to display NA values with its count
missing_datapoints <- aggr(deaths_vs_diabetes_subset, prop = FALSE, numbers = TRUE) #Using VIM library to display the missing values
summary(missing_datapoints)                                                     #summary of missing values

attach(deaths_vs_diabetes_subset)
plot(deaths_vs_diabetes_subset$total_deaths, 
     deaths_vs_diabetes_subset$diabetes_prevalence, pch = 9, col= "#ffbf80",
     main = "Total deaths vs Diabetes prevalence",
     xlab = "total_deaths",
     ylab = "Diabetes_prevalence")                                              #checking for linearity among the variables

options(scipen = 999)
ggplot(deaths_vs_diabetes_subset, aes(x=total_deaths,y=diabetes_prevalence))+ 
  geom_point(col="lightblue", size=3)

with (deaths_vs_diabetes_subset, {qqplot (total_deaths, diabetes_prevalence,
                                          main = "Total Deaths vs Diabetes",
                                          xlab = "total_deaths",
                                          ylab = "diabetes_prevalence")})       #Checking linear correlation between variables using Quantile-quantile plot (Q-Q plot)
covid_corr_var <- subset(deaths_vs_diabetes_subset,
                         select = c(total_deaths, diabetes_prevalence))         #Correlation coefficient between 2 variables
sample_covid_data<-covid_corr_var[sample(1:nrow(covid_corr_var), 10000, replace = TRUE),]
sample_covid_data
pairs.panels(sample_covid_data,
             smooth = TRUE,                                                     #If TRUE, draws loess smooths
             scale = FALSE,                                                     #If TRUE, scales the correlation text font    
             density = TRUE,                                                    #If TRUE, adds density plots and histograms    
             ellipses = TRUE,                                                   #If TRUE, draws ellipses    
             method = "spearman",                                               #Correlation method (also "pearson" or "kendall")    
             pch = 21,                                                          #pch symbol    
             lm = FALSE,                                                        #If TRUE, plots linear fit rather than the LOESS (smoothed) fit    
             cor = TRUE,                                                        #If TRUE, reports correlations    
             jiggle = FALSE,                                                    #If TRUE, data points are jittered    
             factor = 2,                                                        #Jittering factor    
             hist.col = 4,                                                      #Histograms color    
             stars = TRUE,                                                      #If TRUE, adds significance level with stars    
             ci = TRUE)                                                         #If TRUE, adds confidence intervals    

opar = par(no.readonly = TRUE)
par(mfrow = c(1,2))

hist(deaths_vs_diabetes_subset$total_deaths, col = "#ffbf80", 
     main = "Distribution of total_deaths" , 
     xlab = "total_deaths")                                                     #Plotting histograms to check whether variables are normally distributed
hist(deaths_vs_diabetes_subset$diabetes_prevalence, col = "#ffbf80", 
     main = "Distribution of diabetes_prevalence",
     xlab = "diabetes_prevalence")
par = opar

with (deaths_vs_diabetes_subset, {qqnorm (total_deaths,
                                          main = "Normal QQ-plot of total_deaths",
                                          xlab = "Theoritical Quantiles",
                                          ylab = "Samples Quantiles")})         #Using Quantile-quantile plot to check for normal distribution
qqline(deaths_vs_diabetes_subset$total_deaths, col = "#ffbf80")                     #Line that represents normal distribution
qqnorm(deaths_vs_diabetes_subset$new_cases)
with (deaths_vs_diabetes_subset, {qqnorm (diabetes_prevalence,
                                          main = "Normal QQ-plot of diabetes_prevalence",
                                          xlab = "Theoritical Quantiles",
                                          ylab = "Samples Quantiles")})         #Using Quantile-quantile plot to check for normal distribution
qqline(diabetes_prevalence, col = "#ffbf80s")                                   #Line that represents normal distribution

sample_covid_data<-deaths_vs_diabetes_subset[sample(1:nrow(deaths_vs_diabetes_subset), 
                                                    5000, replace = FALSE),]    #Using the shapiro-wilk normality test
normality_test <- shapiro.test(sample_covid_data$total_deaths)
normality_test$p.value                                                          #p-value is less than 0.05 meaning the total_deaths variable is not normally distributed

sample_covid_data<-deaths_vs_diabetes_subset[sample(1:nrow(deaths_vs_diabetes_subset), 
                                                    5000, replace = FALSE),]    #Using the shapiro-wilk normality test for diabetes_prevalence
normality_test <- shapiro.test(sample_covid_data$diabetes_prevalence)
normality_test$p.value                                                          #p-value is less than 0.05 meaning the diabetes_prevalence variable is not normally distributed

corr_value <- cor.test(x=deaths_vs_diabetes_subset$total_deaths,                #total_deaths : continuous, diabetes_prevalence : categorical
                       y=deaths_vs_diabetes_subset$diabetes_prevalence, 
                       method = 'spearman')                                     #Using the Spearman test since the variables are not normally distributed
corr_value                                                                      #p-value = 0.0000000000000002
detach(deaths_vs_diabetes_subset)

#It can be clearly seen tat the p-value is less than 0.05, meaning we can 
#reject the null hypothesis.
#As evident, there is a relationship between diabetic patients and the 
#number of covid deaths.