#load packages ----
library(tidyverse)
library(here)
library(readxl)
library(skimr)
library(janitor)

#read in data ----
survey <- read_csv(here("Movie_Attendance_Data.csv"))
View(survey)

#Initial Explorations of Data ----
dim(survey)
glimpse(survey)
head(survey)
tail(survey)
summary(survey)
skim(survey)

#clean variables ----
cleansurvey <- clean_names(survey)

# 1 = y, 0 = n
cleansurvey <- rename(cleansurvey, attendedMovies = q1)

# theatre vs other activities - 4 = v important
cleansurvey <- rename(cleansurvey, theatreImportance = q2)

cleansurvey <- rename(cleansurvey, numMoviesPerMonth = q3)

# 0 = $0, 1 = <$7.49, 2 = $7.50-$14.99, 3 = >$15
cleansurvey <- rename(cleansurvey, extraSpendAtMovies = q4)

# 1 = not important, 4 = very important
cleansurvey <- rename(cleansurvey, arcade = q5a)
cleansurvey <- rename(cleansurvey, foodAndDrink = q5b)
cleansurvey <- rename(cleansurvey, restrooms = q5c)
cleansurvey <- rename(cleansurvey, comfortableChairs = q5d)
cleansurvey <- rename(cleansurvey, auditoriumTypeSeating = q5e)
cleansurvey <- rename(cleansurvey, sizeOfScreen = q5f)
cleansurvey <- rename(cleansurvey, soundQual= q5g)
cleansurvey <- rename(cleansurvey, numScreens = q5h)
cleansurvey <- rename(cleansurvey, cleanRestrooms = q5i)

# 0 = 0km, 1 = 1-9km, 2 = 11(?)-24km, 3 = 25-49km, 4 = 50+
cleansurvey <- rename(cleansurvey, distanceTraveled = q6)

# 1 = not important, 4 = very important
cleansurvey <- rename(cleansurvey, infoSource_newspaper = q7a)
cleansurvey <- rename(cleansurvey, infoSource_internet = q7b)
cleansurvey <- rename(cleansurvey, infoSource_phone = q7c)
cleansurvey <- rename(cleansurvey, infoSource_tv = q7d)
cleansurvey <- rename(cleansurvey, infoSource_friends = q7e)
cleansurvey <- rename(cleansurvey, infoSource_other = q7f)

# Each response is a percent - should add to 100%
cleansurvey <- rename(cleansurvey, purchase_internet = q8a)
cleansurvey <- rename(cleansurvey, purchase_atTheatre = q8b)
cleansurvey <- rename(cleansurvey, purchase_inAdvance = q8c)
cleansurvey <- rename(cleansurvey, purchase_other = q8d)

# 1 = not active, 4 = very active
cleansurvey <- rename(cleansurvey, pActive = q9)
cleansurvey <- rename(cleansurvey, sActive = q10)

# 1 = white, 2 = aboriginal, 3 = asian, 4 = other
cleansurvey <- rename(cleansurvey, ethnicity = q11)

# 0 = male, 1 = female
cleansurvey <- rename(cleansurvey, gender = q12)

#uni year, 5 = grad
cleansurvey <- rename(cleansurvey, uniYear = q13)

# 1 = <18, 2 = 19-20, 3 = 21-23, 4 = 24-26, 5 = >26
cleansurvey <- rename(cleansurvey, age = q14)

write_csv(cleansurvey,"cleansurveydata.csv")
View(cleansurvey)

#create new variables ----

#creating a variable to determine how much revenue a person brings in on a monthly basis
#extra spending
#0 = $0, 1 = <$7.49, 2 = $7.50-$14.99, 3 = >$15
#assign the middle value to determine how much the extra spend in dollars is
cleansurvey <- cleansurvey %>% 
  mutate(extraSpendInDollar = 
           if_else(extraSpendAtMovies == 0,0,if_else(extraSpendAtMovies == 1, 3.75,if_else(extraSpendAtMovies == 2, 11.25, 18.75))))

#Expected value of extra spending from movie-goers
cleansurvey %>% summarise(sdExtraSpend = sd(extraSpendInDollar, na.rm = TRUE),
                         meanExtraSpend = mean(extraSpendInDollar, na.rm = TRUE))

#Attach $ amt to movie attended to find highest val customers
cleansurvey <- cleansurvey %>%
  mutate(revPerMonth = (extraSpendInDollar+15)*numMoviesPerMonth)

#Expected value of total spend
cleansurvey %>% summarise(sdTotalSpend = sd(revPerMonth, na.rm = TRUE),
                         meanTotalSpend = mean(revPerMonth, na.rm = TRUE),
                         medTotalSpend = median(revPerMonth, na.rm = TRUE))

#create histogram to visulize how much revenue majority of the customers bring in on a monthly basis
hist(cleansurvey$extraSpendInDollar)
hist(cleansurvey$revPerMonth,breaks =40,main = "Revenue per Customer per Month", xlab = "Revenue in $")

write_csv(cleansurvey,"cleansurveydata.csv")

#check simple statistics to see effect on how often they go to the movies, and the total amount of revenue they bring in on a monthly basis ----

#how physically active a person is effects how often they go to the movies, and total monthly revenue 
cleansurvey %>% 
  group_by(pActive) %>%
  summarise(maxMovie = max(numMoviesPerMonth, na.rm = TRUE),
            meanMovie = mean(numMoviesPerMonth, na.rm = TRUE),
            sdMovie = sd(numMoviesPerMonth, na.rm = TRUE),
            maxRevenue = max(revPerMonth, na.rm = TRUE),
            meanRevenue = mean(revPerMonth, na.rm = TRUE),
            sdRevenue = sd(revPerMonth, na.rm = TRUE))

#how socially active a person is effects how often they go to the movies, and total monthly revenue 
cleansurvey %>% 
cleansurvey %>% 
  group_by(sActive) %>%
  summarise(maxMovie = max(numMoviesPerMonth, na.rm = TRUE),
            meanMovie = mean(numMoviesPerMonth, na.rm = TRUE),
            sdMovie = sd(numMoviesPerMonth, na.rm = TRUE),
            maxRevenue = max(revPerMonth, na.rm = TRUE),
            meanRevenue = mean(revPerMonth, na.rm = TRUE),
            sdRevenue = sd(revPerMonth, na.rm = TRUE)) 

#how the ethnicity of a person effects how often they go to the movies, and total monthly revenue 
cleansurvey %>% 
cleansurvey %>% 
  group_by(ethnicity) %>%
  summarise(maxMovie = max(numMoviesPerMonth, na.rm = TRUE),
            meanMovie = mean(numMoviesPerMonth, na.rm = TRUE),
            sdMovie = sd(numMoviesPerMonth, na.rm = TRUE),
            maxRevenue = max(revPerMonth, na.rm = TRUE),
            meanRevenue = mean(revPerMonth, na.rm = TRUE),
            sdRevenue = sd(revPerMonth, na.rm = TRUE)) 

#how the gender of a person effects how often they go to the movies, and total monthly revenue 
cleansurvey %>% 
cleansurvey %>% 
  group_by(gender) %>%
  summarise(maxMovie = max(numMoviesPerMonth, na.rm = TRUE),
            meanMovie = mean(numMoviesPerMonth, na.rm = TRUE),
            sdMovie = sd(numMoviesPerMonth, na.rm = TRUE),
            maxRevenue = max(revPerMonth, na.rm = TRUE),
            meanRevenue = mean(revPerMonth, na.rm = TRUE),
            sdRevenue = sd(revPerMonth, na.rm = TRUE))  

#how the year in which a person effects how often they go to the movies, and total monthly revenue 
cleansurvey %>% 
cleansurvey %>% 
  group_by(uniYear) %>%
  summarise(maxMovie = max(numMoviesPerMonth, na.rm = TRUE),
            meanMovie = mean(numMoviesPerMonth, na.rm = TRUE),
            sdMovie = sd(numMoviesPerMonth, na.rm = TRUE),
            maxRevenue = max(revPerMonth, na.rm = TRUE),
            meanRevenue = mean(revPerMonth, na.rm = TRUE),
            sdRevenue = sd(revPerMonth, na.rm = TRUE))

#how the age of a person effects how often they go to the movies, and total monthly revenue 
cleansurvey %>% 
cleansurvey %>% 
  group_by(age) %>%
  summarise(maxMovie = max(numMoviesPerMonth, na.rm = TRUE),
            meanMovie = mean(numMoviesPerMonth, na.rm = TRUE),
            sdMovie = sd(numMoviesPerMonth, na.rm = TRUE),
            maxRevenue = max(revPerMonth, na.rm = TRUE),
            meanRevenue = mean(revPerMonth, na.rm = TRUE),
            sdRevenue = sd(revPerMonth, na.rm = TRUE))


#calculate simple statistics on questions 5 and 7 to see what are the most important preferences ----

#more explorations on the data - Display Non Movie Goers vs Goers ----

nongoers <- cleansurvey %>%
  filter(attendedMovies %in% 0) %>%
  group_by(uniYear)
goers <- cleansurvey %>%
  filter(attendedMovies %in% 1) %>%
  group_by(uniYear)

hist(nongoers$pActive,xlim = c(0,4),ylim = c(0,), main = "Physical Activity Level of Respondents Who Don't Attend Movies", xlab = "1 = Not Physically Active -> 4 = Physically Active", col = rgb(0,0,1,0.5),freq=FALSE)
hist(goers$pActive,col = rgb(1,0,0,0.5),freq = FALSE,add= T)
legend("topleft",
       c("Non-Goers","Goers"),
       fill = c(rgb(0,0,1,0.5),rgb(1,0,0,0.5)))

hist(nongoers$sActive,xlim = c(0,4),ylim = c(0,1.5), main = "Social Activity Level of Respondents Who Don't Attend Movies", xlab = "1 = Not Socially Active -> 4 = Socially Active", col = rgb(0,0,1,0.5),freq=FALSE)
hist(goers$sActive,col = rgb(1,0,0,0.5),freq = FALSE,add= T)
legend("topleft",
       c("Non-Goers","Goers"),
       fill = c(rgb(0,0,1,0.5),rgb(1,0,0,0.5)))


table(nongoers$pActive,goers$pActive)
barplot(nongoers$pActive,goers$pActive, xlim = c(0,4), )



barplot(prop.table(table(nongoers,goers)),beside=T)
#chi-squared tests ----

#socially active (convert socially active into a dummy variable called extraverted, assign true or false to the variable, >2=true)
cleansurvey <- cleansurvey %>% mutate(extroverted = sActive > 2)
chiTableExtroversion <- table(cleansurvey$attendedMovies, cleansurvey$extroverted)
view(chiTableExtroversion)
chisq.test(chiTableExtroversion)

# physicallyactive (convert physically active into a dummy variable called fit, assign true or false to the variable, >2=true)
cleansurvey <- cleansurvey %>% mutate(fit = pActive > 2)
chiTableFit <- table(cleansurvey$attendedMovies, cleansurvey$fit)
view(chiTableFit)
chisq.test(chiTableFit)

# Gender
chiTableGen <- table(cleansurvey$attendedMovies, cleansurvey$gender)
chisq.test(chiTableGen)
#correlations ----

#create a correlation matrix, will be used as part of the regression later on as well
corMatrix <- round(cor(cleansurvey, method = "pearson", use = "pairwise.complete.obs"),2)
#there is a trend where majority of the variables are correlated to one another, and so when the regression is run majority of the variables will not be used


# for specific variables names Q5 and Q7 ones, we will check indidual correlation to numMoviesPerMonth to get p values and determine if they come more often to the movies or if generate more revenue, then do they put more imporatance on different features and information sources

#Q5 and numMoviesPerMonth correlations
cor.test(cleansurvey$numMoviesPerMonth, cleansurvey$arcade)
cor.test(cleansurvey$numMoviesPerMonth, cleansurvey$foodAndDrink)
cor.test(cleansurvey$numMoviesPerMonth, cleansurvey$restrooms)
cor.test(cleansurvey$numMoviesPerMonth, cleansurvey$comfortableChairs)
cor.test(cleansurvey$numMoviesPerMonth, cleansurvey$auditoriumTypeSeating)
cor.test(cleansurvey$numMoviesPerMonth, cleansurvey$sizeOfScreen)
cor.test(cleansurvey$numMoviesPerMonth, cleansurvey$soundQual)
cor.test(cleansurvey$numMoviesPerMonth, cleansurvey$numScreens)
cor.test(cleansurvey$numMoviesPerMonth, cleansurvey$cleanRestrooms)

#Now we test the correlations between 

#Q7 and numMoviesPerMonth correlations
cor.test(cleansurvey$numMoviesPerMonth, cleansurvey$infoSource_newspaper)
cor.test(cleansurvey$numMoviesPerMonth, cleansurvey$infoSource_internet)
cor.test(cleansurvey$numMoviesPerMonth, cleansurvey$infoSource_phone)
cor.test(cleansurvey$numMoviesPerMonth, cleansurvey$infoSource_tv)
cor.test(cleansurvey$numMoviesPerMonth, cleansurvey$infoSource_friends)
cor.test(cleansurvey$numMoviesPerMonth, cleansurvey$infoSource_other)

#Q5 and revPerMonth correlations
cor.test(cleansurvey$revPerMonth, cleansurvey$arcade)
cor.test(cleansurvey$revPerMonth, cleansurvey$foodAndDrink)
cor.test(cleansurvey$revPerMonth, cleansurvey$restrooms)
cor.test(cleansurvey$revPerMonth, cleansurvey$comfortableChairs)
cor.test(cleansurvey$revPerMonth, cleansurvey$auditoriumTypeSeating)
cor.test(cleansurvey$revPerMonth, cleansurvey$sizeOfScreen)
cor.test(cleansurvey$revPerMonth, cleansurvey$soundQual)
cor.test(cleansurvey$revPerMonth, cleansurvey$numScreens)
cor.test(cleansurvey$revPerMonth, cleansurvey$cleanRestrooms)

#Q7 and revPerMonth correlations
cor.test(cleansurvey$revPerMonth, cleansurvey$infoSource_newspaper)
cor.test(cleansurvey$revPerMonth, cleansurvey$infoSource_internet)
cor.test(cleansurvey$revPerMonth, cleansurvey$infoSource_phone)
cor.test(cleansurvey$revPerMonth, cleansurvey$infoSource_tv)
cor.test(cleansurvey$revPerMonth, cleansurvey$infoSource_friends)
cor.test(cleansurvey$revPerMonth, cleansurvey$infoSource_other)



'''
intersting findings from correlations 
  - the matrix shows that people who buy their tickets at the theather generally put more importance on all preferances asked in Q5, compared to people who buy from the internet and in advance
  - revenue and monthly visists compared to options in Q5
    - as expected both variables showed similar results with the two significant options being food&drink options and restrooms
    - one difference is that the monthly vists put significane on comfortable chairs
  - revenue and monthly visists compared to options in Q7
    - once again both variables have similar results with putting importance on the phone as an inforsource, this is something interesting as it was not initially expected


'''
#regression
#from the correlation matrix we can see
#regressions ----

#as seen from the correlation analysis, majority of the variables are correlated with each other so we cannot use them, therefore we will pick the most important ones from ones that are correlated, and use variables that are not correlated to create a multiple variable regression

revReg <- lm(revPerMonth ~ sActive + ethnicity + gender + uniYear + distanceTraveled, data = cleansurvey)
summary(revReg)

attendanceReg <- lm(numMoviesPerMonth ~ pActive + sActive + ethnicity + gender + uniYear + distanceTraveled, data = cleansurvey)
summary(attendanceReg)

#from the regression we can clearly see that majority of the non-correlated are not significant in the regression, the only significant variable is university year

