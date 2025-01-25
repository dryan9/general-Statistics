# Analytics 6/25/2024 Introduction to Statistics Slides Day 1

#Ames Housing example
install.packages("AmesHousing")
library(AmesHousing)
ames <- make_ordinal_ames() #Gives it categories and determines factors vs categorical. Specific to this package
str(ames) #Looking at the data

#Calling upon ggplot2
library(ggplot2)

#Histogram from ggplot2 with the Ames dataset
ggplot(data=ames) +
  geom_histogram(mapping = aes(x=Sale_Price/1000)) +
  labs(x = "Sale Price (Thousands $)")

#Better histogram from ggplot2 with Ames dataset
ggplot(ames, aes(x=Sale_Price/1000)) + #aes can appear here or in geom_histogram, also can do data=ames or just ames
  geom_histogram(aes(y=..density..), alpha = 0.5) + #alpha = is for shading on a scale of 0-1
  geom_density(alpha = 0.2) +
  labs(x = "Sales Price (Thousands $)")

# Normal Probability Plots (QQ Plots)
ggplot(ames, aes(sample = Sale_Price/1000)) +
  stat_qq() +
  stat_qq_line()

#Graphical display for a box plot
ggplot(ames, aes(y= Sale_Price/1000,
                 x= Central_Air,
                 fill= Central_Air)) + #fill = is optional, allows for different colors for each level of the variable that is identified
  geom_boxplot() +
  labs(y = "Sales Price (Thousands $)",
       x = "Central Air") + 
  scale_fill_brewer(palette="Accent") +
  theme_classic() + #Changes background to white
  coord_flip() #Flips axises

#Breakout Session 1 Materials

#Bringing in bikes dataset
bike <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bike.csv')
str(bike)
table(bike$season) #table of Bikes DF and the column/variable season

#Histogram of Bike Rentals
ggplot(bike, aes(x=cnt)) +
  geom_histogram(fill="blue") +
  labs(x = "Bike Rental",
       y= "Frequency",
       title = "Histogram of Bike Rentals")

#Run summary to get min, Q1, med, mean, Q3 and max of cntstandard deviation
summary(bike$cnt)
sd(bike$cnt) #standard deviation
quantile(bike$cnt, probs = c(.1,.4,.8))

#Histogram of Bike Rentals with a new binwidth
ggplot(bike, aes(x=cnt)) +
  geom_histogram(fill = "red",
                 binwidth = 50) +
  labs(x = "Bike Rentals",
       y = "Frequency",
       title = "Histogram of Bike Rentals")

#Histogram of Bike Rentals with a density estimator
ggplot(bike, aes(x=cnt)) +
  geom_histogram(aes(y=after_stat(!!str2lang("density"))), alpha = 0.2) +
  geom_density() +
  labs(x = "Bike Rentals",
       title = "Histogram of Bike Rentals")

#QQ Plot for Bike Rentals (This will be right skewed)
ggplot(bike, aes(sample = cnt)) +
  stat_qq() +
  stat_qq_line()

#Creating side by side box-plots for each season of bike rentals
ggplot(bike, aes(x = factor(season),
                 y = cnt,
                 fill_factor(season))) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Winter", "Spring", "Summer", "Fall")) +
  labs(x = "Season",
       y = "Bike Rentals",
       fill = "Season")

#Lab 1 "C:\Users\seggy\Downloads\rtools40-x86_64.exe"
install.packages('UsingR')
#install.packages('Hmisc')
library(UsingR)
data(normtemp)
install.packages("ggplot2")
library(ggplot2)
#1a
str(normtemp)
temp <- normtemp$temperature
summary(temp)
sd(temp)

#1b Appears to be normal
ggplot(normtemp, aes(sample = temperature)) +
  stat_qq() +
  stat_qq_line()

#1c No, the median is closer to 98.35, 98.6 is closer to Q3
ggplot(normtemp, aes(x= temperature)) +
  geom_boxplot() +
  #  geom_hline(yintercept = 98.6) + #geom_hline for y intercept (horizontal)
  geom_vline(xintercept =  98.6) + #geom_yline for x intercept (vertical)
  labs(x = "Temperature in Fahrenheit",
       title= "Boxplot for Temperature") +
  theme_classic()

#2a
library(AmesHousing)
ames <- make_ordinal_ames() #Gives it categories and determines factors vs categorical. Specific to this package
ggplot(ames, aes(x=Sale_Price/1000)) +
  geom_histogram(aes(y=after_stat(!!str2lang("density"))), alpha = 0.2) +
  geom_density() +
  labs(x = "Sales Price in Thousands",
       title = "Ames Sales Price")

#2b
ggplot(ames, aes(sample = Sale_Price)) +
  stat_qq() +
  stat_qq_line()

ggplot(ames, aes(sample = log(Sale_Price))) +
  stat_qq() +
  stat_qq_line()

# Analytics 6/26/2024 Introduction to Statistics Slides Day 2

#Default t-test is two sided
t.test(ames$Sale_Price, mu = 178000)

#T-test for one=sided tests. In this case Ha > 178000
t.test(ames$Sale_Price, mu=178000, alternative = "greater")

#T-test for one=sided tests. In this case Ha < 178000
t.test(ames$Sale_Price, mu=178000, alternative = "less")

#Verify normality using a QQ Plot
cars <- read.csv('https://github.com/IAA-Faculty/statistical_foundations/blob/master/cars.csv')
library(ggplot2)
ggplot(data=cars, aes(sample = MPG, color= Country)) +
  stat_qq() +
  stat_qq_line()

#Shapiro-Wilk Normality Test
cars$MPG[cars$Country == "US"]

#Check Equality of Variances
var.test(MPG ~ Country, data= cars)

#Two Sample T-Test
t.test(MPG ~ Country, data = cars, var.equal = FALSE)

#Wilcoxon Rank Test when Normality Assumptions Fail
wilcox.test(Sale_Price ~ Central_Air, data =ames)


#Breakout Materials 2
coviddata <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/coviddata.csv')

#1a Checking normality for deaths per capita within each region. Both appear to be normal
ggplot(data = coviddata, aes(sample = covidDeathsPerCapita, color = region)) +
  stat_qq() +
  stat_qq_line()

#1b Checking for significant difference in deaths per capita across regions. Two sample t-test !=
var.test(covidDeathsPerCapita ~ region, data = coviddata) #Have to check equal variances first. FTR p > a, conclude equal
t.test(covidDeathsPerCapita ~ region, data= coviddata, var.equal = TRUE)
#Reject Ho because p< a and conclude that there is a significant difference between the means in regions


#1c Type out hypothesis test
#Ho: mu1 = mu2
#Ha: mu1 != mu2
#test-statistic(t) = 53.6
#p-value = < 2.2e-16
#Conclusion: Reject Ho at the alpha level of .05, there is a significant difference between the East and West regions mean covid deaths per capita.

#Lab 2
library(UsingR)
data(normtemp)

#1a One-Sample T Test for mu1 != mu2
t.test(normtemp$temperature, mu = 98.6)
#The p-value is 2.411e-07 which means that there is a less than .001% chance that we observed results at least as extreme as the sample
#Conclusion is Reject Ho and conclude that there is a significant difference

#1b 95% CI for temperature. Achieved through t.test
#(98.1220, 98.3765), we are 95% confident that the mean temperature falls between this range

#1c Restrict analysis for just females.
#Use brackets to SUBSET
t.test(normtemp$temperature[normtemp$gender == "2"], mu = 98.6,)
#Our conclusion does not change however the p-value has increased.

#1d Two Sample T-Test for Males and Females
#Check for equal variances
ggplot(data = normtemp, aes(sample = temperature, color = factor(gender))) + #factor() for turning integer into cateogorical
  stat_qq() +
  stat_qq_line()
var.test(normtemp$temperature ~ normtemp$gender, data = normtemp)
#FTR p>a for equal variances
t.test(normtemp$temperature ~ normtemp$gender, data = normtemp, var.equal = TRUE)
#Reject Ho and conclude that there is a significant difference between means for males and females.

#2
data(AirPassengers)
install.packages("tseries")
install.packages("forecast")
install.packages("tidyverse")
install.packages("dplyr")
library(tseries)
library(forecast)
library(tidyverse)
library(dplyr)
cycle(AirPassengers)
air1 <- data.frame(AirPassengers)
air2 <- air1 %>% mutate(summer=ifelse(cycle(AirPassengers) %in% 6:8,1,0))

#Check for Normality and Equal Variances
ggplot(data = air2, aes(sample = AirPassengers, color = factor(summer))) + 
  stat_qq() +
  stat_qq_line()
#right skewed, check variances
var.test(air2$AirPassengers ~ air2$summer, data = air2)
#Reject Ho, variances are unequal
#Perform Wilcoxon-Test
wilcox.test(AirPassengers ~ summer, data = air2)
#Reject Ho and conclude that air passengers differ in the summer

# Introduction to ANOVA and Regression 6/27/2024

#Train-Test Split in R
library(tidyverse)
library(AmesHousing)
set.seed(123) #Recommended for random chance variables. This is a random number generator
ames <- ames %>% mutate(id = row_number())
train <- ames %>% sample_frac(0.7) #Sendng 70% of the data
test <- anti_join(ames, train, by = "id")
dim(train)
dim(test)

#Scatterplot in R
ggplot(data = train) + #Only want to use training in this case
  geom_point(mapping = aes(x = Gr_Liv_Area, y = Sale_Price/1000)) +
  labs(y = "Sales Price (Thousands $)", x = "Greater Living Area (sqft)")

#Grouped Box-Plots
ggplot(data = train, aes(y = Sale_Price/1000,
                         x = Exter_Qual,
                         fill = Exter_Qual)) +
  geom_boxplot() +
  labs(y = "Sale Price (Thousands $)",
       x = "Exterior Quality Category") +
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 5,
               color = "red",
               fill = "red") +
  scale_fill_brewer(palette = "Blues") +
  theme_classic() +
  coord_flip()

#Overlaid Histograms
ggplot(ames, aes(x = Sale_Price/1000,
                 fill = Exter_Qual)) +
  geom_density(alpha = 0.2,
               position = "identity") +
  labs(x = "Sale Price (Thousands $)")

#Test Assumption for Normality. Residual QQ Plot
install.packages("ggpubr")
library(ggpubr)
model <- aov(MPG~Country, data = cars2) #aov is for ANOVA
ggqqplot(residuals(model))
#Shapiro-Wilks Test for Normality of Residuals
library(dplyr)
shapiro.test(model)

#Test Assumption of Equal Variances. Can be done through a test or graph (up to you)
install.packages("car")
library(car)
install.packages("stats")
library(stats)
cars2 <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/cars2.csv")
#Levene test for equal variances
leveneTest(MPG~Country, data = cars2)
#Plot Equal Variances
ggplot(model, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Residuals vs. Fitted Values Plot",
       x = "Fitted Values",
       y = "Residuals")

#ANOVA


#Welch's ANOVA for unequal variances
oneway.test(MPG~Country, data = cars2, var.equal = FALSE)

#Linear Regression
cars_lm <- lm(MPG~factor(Country), data = cars2) #factor(Country) for categorical
summary(cars_lm)

#ANOVA Predictions
cars2$predict <- predict(cars_lm, data= cars2)
cars2$resid_anova <- resid(cars_lm, data = cars2)
model_output <- cars2 %>% dplyr::select(Country, predict, resid_anova)
rbind(model_output[1:3,], model_output[255:257,], model_output[375:377,])

#Fligner-Killeen Test of Homogeneity of Variances (Not as Important) Does not depend on Normality
fligner.test(Sale_Price~Exter_Qual,data=train)

#Kruskal-Wallis Nonparametric ANOVA in R
kruskal.test(Sale_Price~Exter_Qual, data =train)

#Tukey's Test
cars_aov <- aov(MPG ~ factor(Country), data = cars2)
gh.cars <- TukeyHSD(cars_aov)
print(gh.cars)
par(mar = c(4,10,4,2)) #Breaks it into parameters
plot(gh.cars, las=1)

#Games-Howell Test
install.packages("PMCMRplus")
library(PMCMRplus)
gh.cars <- gamesHowellTest(cars_aov)
summary(gh.cars)

#Dunn's Test
install.packages("dunn.test")
library(dunn.test)
dunn.cars <- dunn.test(cars2$MPG, cars2$Country, kw = T, method = "bonferroni")

#Dunnett's Test
cars2$group <- ifelse(cars2$Country=="US", "aUS",cars2$Country)
dunnettTest(x= cars2$MPG, g = factor(cars2$group))

#Welch and Wilcoxon w Bonferroni Adjustment
summary(welchManyOneTTest(x = cars2$MPG, g = factor(cars2$Country), p.adjust.method = "bonferroni"))
summary(manyOneUTest(x = cars2$MPG, g = factor(cars2$Country), p.adjust.method = "bonferroni"))

#Breakout Session 3

#1a Plots would be qq plot for residuals and 1b below
bike <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bike.csv')
model1 <- aov(cnt~weathersit, data = bike) # y ~ x
ggqqplot(residuals(model1)) #Does not appear to be normal
#Test for variances once Normality is not ok
fligner.test(cnt~weathersit, data = bike) #p<a, Reject Ho, variances are significantly different
#Kruskal-Wallis Nonparametric ANOVA
kruskal.test(cnt~weathersit, data=bike) #p<a Reject ho and determine significant effect
#1a Plots would be qq plot for residuals

#2
model2 <- aov(cnt ~ season, data =bike)
ggqqplot(residuals(model2))

#Lab 3

#1
garlic <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/garlic.csv")

#1a Run ANOVA, check for Normality and Equal Variances
garlicanova <- aov(BulbWt ~ Fertilizer, data= garlic)
#Check for Normality with a plot and Shapiro-Wilk
ggqqplot(residuals(garlicanova)) #Looks normal
shapiro.test(residuals(garlicanova)) #p>a FTR Ho, NORMAL
#Check for unequal variances
leveneTest(BulbWt~factor(Fertilizer), data = garlic) #p>a FTR Ho, Determine equal variances
ggplot(model, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)
#Summary of the Basic ANOVA
summary(garlicanova)
#p<a Reject Ho, significant difference between bulb weight depending on fertilizer

#1b How many pairwise tests? 4 Fertilizers each being tested vs each other 6

#1c Post-Hoc Analysis in ANOVA. Experiment Error Rate .26
#Doing Tukey's test since I used basic ANOVA and normality was there and there were equal variance
garlicanova <- aov(BulbWt~factor(Fertilizer), data = garlic)
tukeygarlic <- TukeyHSD(garlicanova)
print(tukeygarlic)

#1d The significant differences are (4-1) and (4-3) ones p < a, the ones that are not different, p> a are (2-1), (3-1), (3-2), (4-2)

#2
bottle <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bottle.csv")
bottleanova <- aov(Units~factor(Line), data = bottle)
ggqqplot(residuals(bottleanova))
shapiro.test(residuals(bottleanova)) #Normal p>a
leveneTest(Units~factor(Line), data = bottle) #equal P<a
ggplot(bottleanova, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)
summary(bottleanova) #p<a Reject Ho, significant difference
bottletukey <- TukeyHSD(bottleanova)
print(bottletukey)
#Significant differences are (3-1) and (3-2). He is correct

#3
trials <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/trials.csv")
trialsanova <- aov(BPChange~factor(Treatment), data = trials)
ggqqplot(residuals(trialsanova))
shapiro.test(residuals(trialsanova)) #Normal p>a
leveneTest(BPChange~factor(Treatment), data = trials) #unequal p<a
ggplot(trialsanova, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)
oneway.test(BPChange~factor(Treatment), data=trials) #p<a Reject Ho and determine significant difference
#Games-Howell Test for Normal and Unequal Variances
trialsgh <- gamesHowellTest(trialsanova)
summary(trialsgh)
#Yes both are different

# Introduction to ANOVA and Regression 6/28/2024

#Test of correlation in R
library(AmesHousing)
ames <- make_ordinal_ames() #Gives it categories and determines factors vs categorical. Specific to this package
cor.test(train$Gr_Liv_Area, train$Sale_Price, conf.level = .1) #conf.level for alpha

#Correlation Matrix
cor(train[,c("Year_Built", "Total_Bsmt_SF", "First_Flr_SF", "Gr_Liv_Area","Sale_Price")])

#Correlation Matrix and Plot
pairs(train[,c("Year_Built", "Total_Bsmt_SF", "First_Flr_SF", "Gr_Liv_Area","Sale_Price")])

#Simple Linear Regression in R
slr <- lm(Sale_Price ~ Gr_Liv_Area, data = train)
par(mfrow = c(2,2))
plot(slr)
summary(slr)

#SLR with a categorical variable (two levels)
slr1 <- lm(Sale_Price ~ factor(Central_Air), data = train)
par(mfrow = c(2,2)) #assigning that graphs should be 2x2
plot(slr1)
summary(slr)

#Breakout Materials #4

#1
temp1a <- lm(cnt~temp, data = bike)
summary(temp1a)
atemp1a <- lm(cnt~atemp, data = bike)
summary(atemp1a)
hum1a <- lm(cnt~hum, data= bike)
summary(hum1a)
windspeed1a <- lm(cnt~windspeed, data = bike)
summary(windspeed1a)

#Lab 4

#1
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(FuelEconomy)
#1a
engdispl1 <- lm(FE~EngDispl, data = cars2010)
cor.test(cars2010$EngDispl, cars2010$FE) #x,y r = -.7874
cor.test(cars2010$NumCyl, cars2010$FE) # r= -.7402
cor.test(cars2010$ExhaustValvesPerCyl, cars2010$FE) #r = .3357
cor.test(cars2010$VarValveTiming, cars2010$FE) #r=.1250
library(ggplot2)
ggplot(data = cars2010) +
  geom_point(mapping = aes(x = NumCyl, y = FE)) #Nothing insanely concerning, seems to fit
ggplot(data = cars2010) +
  geom_point(mapping = aes(x = EngDispl, y = FE)) #Potentially some outliers
ggplot(data = cars2010) +
  geom_point(mapping = aes(x = factor(ExhaustValvesPerCyl), y = FE)) #Maybe some outliers, weird
ggplot(data = cars2010) +
  geom_point(mapping = aes(x = factor(VarValveTiming), y = FE))
#The value with the highest correlation is Engine Display
#All have p<a which would mean we reject and conclude a significant linear relationship
#1b
cor(cars2010[,c("EngDispl", "NumCyl", "ExhaustValvesPerCyl", "VarValveTiming")])
pairs(cars2010[,c("EngDispl", "NumCyl", "ExhaustValvesPerCyl", "VarValveTiming")])
#Yes, some variables have relationships close to 1 while others are close to 0

#1c
engdispl1c <- lm(FE~EngDispl, data = cars2010)
summary(engdispl1c)
#F= 1803, P = <.001. Reject Ho, conclude significant relationship.
#y = -4.5209x + 50.5632
#r^2 = .62, 62% of the variation in fe is explained by engdispl

#2
icecream <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/icecream.csv")
cor.test(icecream$Temperature, icecream$Sales)
icecreamlm <- lm(Sales~Temperature, data = icecream)
summary(icecreamlm)
#Check for Normality with a plot and Shapiro-Wilk
ggqqplot(residuals(icecreamlm)) #Looks normal
length(residuals(icecreamlm))#Check # of residuals
#Check for unequal variances (Levene only for ANOVA)
par(mfrow = c(2,2)) #assigning that graphs should be 2x2
plot(icecreamlm) #Normally distributed Residuals vs Fitted, no funnel effect
#Yes there is a relationship p<a Reject Ho
#y = 1.0901x + 119,2974

#3
minntemp <- read.delim("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/minntemp.csv", sep= " ")
minnlm <- lm(Temp~Time, data = minntemp)
summary(minnlm)
cor.test(minntemp$Time, minntemp$Temp, data = minntemp)
ggqqplot(residuals(minnlm)) #Looks normal
par(mfrow = c(2,2)) #assigning that graphs should be 2x2
plot(minnlm) #Seems to be some type of pattern
#FTR, no significant relationship at .05

#More Complex ANOVA and Regression 7/1/2024
library(tidyverse)
library(dplyr)

#Exploring the data
train %>% 
  group_by(Heating_QC, Central_Air) %>% 
  summarise(mean = mean(Sale_Price),
            sd = sd(Sale_Price),
            max = max(Sale_Price),
            min = min(Sale_Price),
            n = n())

#Two-Way ANOVA
ames_aov2 <- aov(Sale_Price ~ Heating_QC + Central_Air, data = train)
summary(ames_aov2) #Each variable has it's own unique F-Test. Both have the same null

#Tukey's on a Two-Way
tukey.ames2 <- TukeyHSD(ames_aov2)
print(tukey.ames2)
plot(tukey.ames2)

#Two-Way ANOVA with Interactions
ames_aov_int <- aov(Sale_Price ~ Heating_QC*Central_Air, data = train)
summary(ames_aov_int)

#Post-Hoc Testing for Two-Way ANOVA with Interaction
tukey.ames_int <- TukeyHSD(ames_aov_int)
plot(tukey.ames_int, las = 1)

#Sliced ANOVA to determine where the change is occurring
CA_aov <- train %>% 
  group_by(Central_Air) %>% 
  nest() %>% #analysis that follows will be nested within each group
  mutate(aov = map(data, ~ summary(aov(Sale_Price ~ Heating_QC, data = .x)))) #1st aov here is just a variable name, can be called whatever
print(CA_aov$aov) #Short hand notation way of looping through ANOVA calculations. Heating-QC is different across Y and N for Central_Air homes

#ANOVA with Random Block Design
block_aov <- aov(Num_Cart_Adds ~ Campaign_Name + Date, data = block)
summary(block_aov)

#Post-Hoc Testing for Random Block Design
tukey.block <- TukeyHSD(block_aov)
plot(tukey.block, las = 1)

#Breakout Session #5
bike <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bike.csv")

#1 Combined effect of season and workingday on cnt
breakout1_aov <- aov(cnt ~ factor(season)*factor(workingday), data = bike)
summary(breakout1_aov)
#Interaction because p<a
bike_plot <- bike %>% 
  group_by(season, workingday) %>%
  dplyr::summarise(mean = mean(cnt), 
                   sd = sd(cnt), 
                   max = max(cnt), 
                   min = min(cnt))

ggplot(data = bike_plot, aes(x = season, y = mean, fill = factor(workingday))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(y = "Change in Count", x = "Season") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()

#2 Separate registered from casual rentals for bikes
breakout2r_aov <- aov(registered ~ factor(season)*factor(workingday), data = bike)
summary(breakout2r_aov)
#FTR Ho p>a, no interaction
breakout2c_aov <- aov(casual ~ factor(season)*factor(workingday), data = bike)
summary(breakout2c_aov)
#Reject Ho p<a, there is an interaction between season and working day for casual

#3 Slice casual data by season to see which seasons have significant differences
breakout3_aov <- bike %>% 
  group_by(season) %>% 
  nest() %>% 
  mutate(aov = map(data, ~ summary(aov(casual ~ factor(workingday), data = .x)))) #1st aov here is just a variable name, can be called whatever
print(breakout3_aov$aov)
#All seasons have significant differences

#Lab 5

#1
drugdose <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/drug.csv")
#1a Side by Side bar graph, DrugDose x, BloodP y, stratify by Disease
ggplot(data = drugdose, aes(x = factor(DrugDose), y = BloodP, fill = Disease)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()
#There appears to be differences
#1b Two-Way ANOVA
drugdoseplus_aov <- aov(BloodP ~ factor(DrugDose)*factor(Disease), data = drugdose)
summary(drugdoseplus_aov) #Drug dose has a high p but there is interaction
drugdose_aov <- aov(BloodP ~ factor(DrugDose)*factor(Disease), data = drugdose)
summary(drugdose_aov)
#Reject Ho p<a, there seems to be an interactions between Drug Dose and Disease
#1c Sliced-ANOVA to investigate differences in Drug Dose across levels of Disease
slicedrugdose_aov <- drugdose %>% 
  group_by(Disease) %>% 
  nest() %>% 
  mutate(aov = map(data, ~ summary(aov(BloodP ~ factor(DrugDose), data = .x)))) #1st aov here is just a variable name, can be called whatever
print(slicedrugdose_aov$aov)
#Diseases 1 and 2/A and B have significant differences across drug dose

#2
disks = read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/disks.csv")
#2a Two-Way ANOVA with Technician and Brand x and Time y
disksplus_aov <- aov(Time ~ factor(Technician)+factor(Brand), data = disks)
summary(disksplus_aov) #Brand has a high p but must be included
disks_aov <- aov(Time ~ factor(Technician)*factor(Brand), data = disks)
summary(disks_aov)
#There is an interactions between Technician and Brand
#2b No mean effects does not make sense because there is an interaction
#2c Sliced-ANOVA for differences between Technicians for each disk drive
slicedisks_aov <- disks %>% 
  group_by(Brand) %>% 
  nest() %>% 
  mutate(aov = map(data, ~ summary(aov(Time ~ factor(Technician), data = .x)))) #1st aov here is just a variable name, can be called whatever
print(slicedisks_aov$aov)
#They are all significantly different
#Tukey HSD among Technician within each Brand
tukey.disks_int <- TukeyHSD(disks_aov)
plot(tukey.disks_int, las = 1)
print(tukey.disks_int)

# Multiple Linear Regression 7/3/2024

#MLR
ames_lm2 <- lm(Sale_Price ~ Gr_Liv_Area+ TotRms_AbvGrd, data = train)
summary(ames_lm2)

#Assumptions through Residuals
par(mfrow = c(2,2))
plot(ames_lm2)

#Breakout Session #6
bike <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bike.csv")

#1 Observations for train and test
set.seed(123)
bike <- bike %>% mutate(id = row_number())
train <- bike %>% sample_frac(0.7)
test <- anti_join(bike, train, by = "id")
count(train) #n = 12165
count(test) #n = 5214

#2 Predict cnt with temp, hum and windspeed
breakout2 <- lm(cnt ~ temp+hum+windspeed, data = train)
summary(breakout2)
#All variables are significant. Adjusted r2 is .2487

#3 Predict cnt with atemp, hum and windspeed
breakout3 <- lm(cnt ~ atemp+hum+windspeed, data = train)
summary(breakout3)
# All variables are significant. Adjusted r2 is .25

#4 Best model is the second one

#Lab 6
library(AppliedPredictiveModeling)
data(FuelEconomy)

#1a Regression predicting FE using all variables
carslab1a <- lm(FE ~ EngDispl + factor(NumCyl) + factor(Transmission) + factor(AirAspirationMethod) + factor(NumGears) + factor(TransLockup) + factor(TransCreeperGear) + factor(DriveDesc) + factor(IntakeValvePerCyl) + factor(ExhaustValvesPerCyl) + factor(CarlineClassDesc) + factor(VarValveTiming) + factor(VarValveLift), data = cars2010)
summary(carslab1a)
carslab1aa <- lm(FE ~ ., data = cars2010)
summary(carslab1aa)
#80% of the variation can be explained

#1b car::ANOVA function to eval p-values for each categorical variable
car::Anova(carslab1a)
#Variable with the highest p-value is VarValveTiming

#1c car::ANOVA without VarValveTiming
carslab1c <- lm(FE ~ EngDispl + factor(NumCyl) + factor(Transmission) + factor(AirAspirationMethod) + factor(NumGears) + factor(TransLockup) + factor(TransCreeperGear) + factor(DriveDesc) + factor(IntakeValvePerCyl) + factor(ExhaustValvesPerCyl) + factor(CarlineClassDesc) + factor(VarValveLift), data = cars2010)
summary(carslab1c)
#p-value did not increase significantly
#R-squared and Adj R-squared both increased
car::Anova(carslab1c)
#No p-values didn't change significantly

#1d
#Drop IntakeValvePerCyl
carslab1d <- lm(FE ~ EngDispl + factor(NumCyl) + factor(Transmission) + factor(AirAspirationMethod) + factor(NumGears) + factor(TransLockup) + factor(TransCreeperGear) + factor(DriveDesc) + factor(ExhaustValvesPerCyl) + factor(CarlineClassDesc) + factor(VarValveLift), data = cars2010)
summary(carslab1d)
car::Anova(carslab1d)
#Adj R2 down
#Drop TransLockup
carslab1di <- lm(FE ~ EngDispl + factor(NumCyl) + factor(Transmission) + factor(AirAspirationMethod) + factor(NumGears) + factor(TransCreeperGear) + factor(DriveDesc) + factor(ExhaustValvesPerCyl) + factor(CarlineClassDesc) + factor(VarValveLift), data = cars2010)
summary(carslab1di)
car::Anova(carslab1di)
#Both r2 going down
#Drop VarValveLift
carslab1dii <- lm(FE ~ EngDispl + factor(NumCyl) + factor(Transmission) + factor(AirAspirationMethod) + factor(NumGears) + factor(TransCreeperGear) + factor(DriveDesc) + factor(ExhaustValvesPerCyl) + factor(CarlineClassDesc), data = cars2010)
summary(carslab1dii)
car::Anova(carslab1dii)
#R2 declining
#9 variables left

#Model Selection 7/5/2024

library(tidyverse)
library(dplyr)
library(AmesHousing)
set.seed(123) #Recommended for random chance variables. This is a random number generator
ames <- ames %>% mutate(id = row_number())
train <- ames %>% sample_frac(0.7) #Sendng 70% of the data
test <- anti_join(ames, train, by = "id")

#Build selection
train_sel <- train %>% 
  dplyr::select(Sale_Price, Lot_Area, Street, Bldg_Type, House_Style, Overall_Qual, Roof_Style, Central_Air, First_Flr_SF, Second_Flr_SF, Full_Bath,
                Half_Bath, Fireplaces, Garage_Area, Gr_Liv_Area, TotRms_AbvGrd) %>% 
  mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))

#Forward Selection AIC
full.model <- lm(Sale_Price ~ ., data = train_sel) #. includes all variables
empty.model <- lm(Sale_Price ~ 1, data = train_sel) #empty model has nothing

for.model <- step(empty.model,
                  scope = list(lower = empty_model,
                               upper = full.model),
                  direction = "forward", k =2) # trace= FALSE gives less info in the console

#Forward Selection BIC
for.model <- step(empty.model,
                  scope = list(lower = empty_model,
                               upper = full.model),
                  direction = "forward", k = log(nrow(train_sel))) #Change the k = for BIC

#Forward Selection P-Value
for.model <- step(empty.model,
                  scope = list(lower = empty_model,
                               upper = full.model),
                  direction = "forward", k = qchisq(0.05, 1, lower.tail =  FALSE)) #Change k = 

#Backward Selection AIC
for.model <- step(full.model, #Change to full model instead of empty since you are starting full
                  scope = list(lower = empty_model,
                               upper = full.model),
                  direction = "backward", k =2) #Change direction = to "backward"

#Backward Selection BIC
for.model <- step(full.model, #Change to full model instead of empty since you are starting full
                  scope = list(lower = empty_model,
                               upper = full.model),
                  direction = "backward", k = log(nrow(train_sel))) #Change direction = to "backward"

#Backward Selection P-Value
for.model <- step(full.model,
                  scope = list(lower = empty_model,
                               upper = full.model),
                  direction = "backward", k = qchisq(0.05, 1, lower.tail =  FALSE)) #Change k = 
#Stepwise Selection AIC
for.model <- step(empty.model, #Change to empty model for both
                  scope = list(lower = empty_model,
                               upper = full.model),
                  direction = "both", k =2) #Change direction = to "backward"

#Stepwise Selection BIC
for.model <- step(empty.model, #Change to full model instead of empty since you are starting full
                  scope = list(lower = empty_model,
                               upper = full.model),
                  direction = "both", k = log(nrow(train_sel))) #Change direction = to "backward"

#Stepwise Selection P-Value
for.model <- step(empty.model,
                  scope = list(lower = empty_model,
                               upper = full.model),
                  direction = "both", k = qchisq(0.05, 1, lower.tail =  FALSE)) #Change k = 

#Breakout Session 7
bike <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bike.csv")

#1 Split the data and get observation counts
set.seed(123)
bike <- bike %>% mutate(id = row_number())
train <- bike %>% sample_frac(0.7)
test <- anti_join(bike, train, by = "id")
count(train) #12165
count(test) #5214

#2 Build a forward AIC model to predict cnt with specified variables
train_sel <- train %>% 
  dplyr::select(cnt, atemp, hum, season, yr, hr, holiday, workingday,weathersit, windspeed) %>% 
  mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))

full_model <- lm(cnt ~ atemp + 
                   hum + 
                   factor(season) + 
                   factor(yr) + 
                   factor(hr) + 
                   factor(holiday) +
                   factor(workingday) + 
                   factor(weathersit) + 
                   windspeed, data = train_sel)
empty_model <- lm(cnt ~ 1, data = train_sel)
for_model <- step(empty_model,
                  scope = list(lower = empty_model,
                               upper = full_model),
                  direction = "forward", k =2) # trace= FALSE gives less info in the console
#hr, atemp, yr, weathersit, season, hum, holiday, windspeed, workingday

#3 Forward BIC
for_model <- step(empty_model,
                  scope = list(lower = empty_model,
                               upper = full_model),
                  direction = "forward", k = log(nrow(train_sel))) # trace= FALSE gives less info in the console
#hr, atemp, yr, weathersit, season, hum, workingday

#Lab 7
library(AppliedPredictiveModeling)
data(FuelEconomy)

#No training needed just use cars2010
#1a Build a regression predicting FE using all Forward P
full_model <- lm (FE ~ EngDispl + factor(NumCyl) + factor(Transmission) + factor(AirAspirationMethod) + factor(NumGears) + factor(TransLockup) + factor(TransCreeperGear) + factor(DriveDesc) + factor(IntakeValvePerCyl) + factor(ExhaustValvesPerCyl) + factor(CarlineClassDesc) + factor(VarValveTiming) + factor(VarValveLift), data = cars2010)
empty_model <- lm(FE ~ 1, data = cars2010)
for_model <- step(empty_model,
                  scope = list(lower = empty_model,
                               upper = full_model),
                  direction = "forward", k = qchisq(0.1, 1, lower.tail =  FALSE))
#1aa Final Model FE~EngDispl+DriveDesc,CarLineClassDesc,NumCyl,Transmission
#IntakeValvePerCyl,VarValveLift, AirAspirationMethod,NumGears,TransLockup
#1ab First var added was EngDispl
#1ac Last var added was TransLockup

#1b Stepwise with BIC
for_model <- step(empty_model, #Change to full model instead of empty since you are starting full
                  scope = list(lower = empty_model,
                               upper = full_model),
                  direction = "both", k = log(nrow(train_sel))) #Change direction = to "backward"
#1ba 7 Look at the minus sign
#1bb Way less vars

#Diagnostics 7/8/2024

library(tidyverse)
library(dplyr)
library(ggplot2)

#Fitting a quadratic model
lm.quad <- lm(strength ~ amount)
summary(lm.quad)

ggplot(lm.quad, aes(x = amount, y = resid(lm.quad))) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Residual Plot", 
       x = "Amount",
       y = "Residuals")

amount.c <- scale(amount, scale = F)
lm.quad <- lm(strength ~ amount.c + l(amount.c^2))
summary(lm.quad)

#Transforming y with log(y)
lm.var <- lm(salary ~ years)
ggplot( lm.var, aes(x =fitted(lm.var), y = resid(lm.var))) +
  geom_point(color = "blue") +
  labs(title = "Residual Plot",
       x = "Predicted Values",
       y = "Residuals")
#Spearman Rank Correlation Test
cor.test(abs(resid(lm.var)), fitted.values(lm.var), method = "spearman", exact = T)
#Use variance-stablizing transformation
lm.var <- lm(log(salary)~years)
ggplot( lm.var, aes(x =fitted(lm.var), y = resid(lm.var))) +
  geom_point(color = "blue") +
  labs(title = "Residual Plot",
       x = "Predicted Values",
       y = "Residuals")

#Other methods to adjust standard errors
install.packages("estimatr")
library(estimatr)
#HC1
model3 <- lm_robust(y ~ x, se_type = "HC1")
summary(model3)
#HC2
model4 <- lm_robust(y~x)

#Detecting lack of normality
hist(resid(lm.var))
qqnorm(resid(lm.var))
qqline(resid(lm.var))
ad.test(resid(lm.var)) #Anderson-Darling test

#Box-Cox on the original salary dataset
install.packages("MASS")
library(MASS)
lm.var <- lm(salary ~ years)
boxcox(lm.var)

#Breakout Session #8
install.packages("car")
data(mtcars)

# . means all first order
# I(x^2) means two way interaction

#Checking the linear modeling for the values in the model
pairs(lab.dat)

#Filtering down the dataset to just have the columns cyl, wt, hp
x <- scale(mtcars[,c(2,4,6)], scale = F) #Do not use scale = T on large models
lab.dat <- data.frame(cbind(x, mtcars[,1])) #adding in MPG our y
colnames(lab.dat)[4] <- "mpg" #Naming the column
#HP and WT from data has been centered due to the pairs we ran below

#First check fitted vs residuals
library(ggplot2)
library(tidyverse)
library(dplyr)

#Fit an empty and full model
empty.model <- lm(mpg ~ 1, data = lab.dat)
full.model <- lm()

#Lab 8

#Creating the data frame
cafeteria <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/cafeteria.csv")

#1 Perform a simple linear regression (Sales ~ Dispensers)
lmlab8.1 <- lm(Sales ~ factor(Dispensers), data = cafeteria)
summary(lmlab8.1)
ggplot( lmlab8.1, aes(x =fitted(lmlab8.1), y = resid(lmlab8.1))) +
  geom_point(color = "blue") +
  labs(title = "Residual Plot",
       x = "Predicted Values",
       y = "Residuals")
#Fitted vs Residual plot shows a quadratic pattern
#I would fix this by fitting a quadratic

#2 Perform a forward selection using AIC
#NEVER COMBINE FACTOR AND SCALE
Dispensers.c <- scale(cafeteria$Dispensers, scale = F)
cafeteria <- data.frame(cbind(cafeteria,Dispensers.c))
full.model <- lm(Sales ~ Dispensers.c, data = cafeteria) #. includes all variables
empty.model <- lm(Sales ~ 1, data = cafeteria) #empty model has nothing
#CANNOT do for.model since we have to include all lower ordered terms
AIC(full.model)
#Dispensers only AIC = 126.8844
fullsq.model <- lm(Sales ~ Dispensers.c + I(Dispensers.c^2), data = cafeteria)
AIC(fullsq.model)
#Dispensers sq AIC = 101.9835
fullcube.model <- lm(Sales ~ Dispensers.c + I(Dispensers.c^2)+ I(Dispensers.c^3), data = cafeteria)
AIC(fullcube.model)
#Dispensers cube AIC = 102.6002
fullquad.model <- lm(Sales ~ Dispensers.c + I(Dispensers.c^2)+ I(Dispensers.c^3)+ I(Dispensers.c^4), data = cafeteria)
AIC(fullquad.model)
#Dispensers sq AIC = 104.5498 because dont want quad
#Best model is Sales ~ Dispensers + I(Dispensers^2)
#If the best model were to be cubed, you have to include squared and lowest order

#3 Residuals vs Predicted Plot
ggplot(fullsq.model, aes(x =fitted(fullsq.model), y = resid(fullsq.model))) +
  geom_point(color = "blue") +
  labs(title = "Residual Plot",
       x = "Predicted Values",
       y = "Residuals")
#Looks random

#Diagnostics 9/9/2024

install.packages("lmtest")
library(lmtest)
#Google stock data for Non-Independence/Auto-Correlation
data(google)
x <- seq(1, length(google))
lm.model <- lm(google ~ x)
#Testing for POSITIVE autocorrelation
dwtest(lm.model, alternative = "greater") #greater for pos, less for neg

#Scottish Hill Races
url <- 'http://www.statsci.org/data/general/hills.txt'
races.table <- read.table(url, header=TRUE, sep = "\t")
n.index <- seq(1, nrow(races.table))
races.table <- cbind(races.table, n.index)
lm.model <- lm(Time ~ Distance + Climb, data = races.table)

#Visualizing Studentized Residuals
ggplot(lm.model, aes(x = n.index, y = rstudent(lm.model))) +
  geom_point(color = "orange") +
  geom_line(y = 3) +
  labs(title = "External Studentized Residuals",
       x = "Observation",
       y = "Residuals")

#Cook's D DONT FORGET TO CHANGE THE FORMULA BELOW BASED OFF OF PARAMETERS AND VARIABLES K
D.cut <- 4/(nrow(races.table)-3)
ggplot(lm.model, aes(x = n.index, y = cooks.distance(lm.model))) +
  geom_point(color = "orange") +
  geom_line(y = D.cut) +
  labs(title = "Cook's D",
       x = "Observation",
       y = "Cook's Distance")

#DFFITS
df.cut <- 2*(sqrt((3/nrow(races.table))))
ggplot(lm.model, aes(x = n.index, y = dffits(lm.model))) +
  geom_point(color = "orange") +
  geom_line(y = df.cut) +
  labs(title = "DFFITS",
       x = "Observation",
       y = "DFFITS")

#Hat Values
hat.cut <- 2*(3)/nrow(races.table)
ggplot(lm.model, aes(x = n.index, y = hatvalues(lm.model))) +
  geom_point(color = "orange") +
  geom_line(y = hat.cut) +
  labs(title = "Hat Values",
       x = "Observation",
       y = "Hat Values")

#DFBETA
db.cut <- 2/sqrt(nrow(races.table))
ggplot(lm.model, aes(x = n.index, y = dfbetas(lm.model)[,"Climb"])) +
  geom_point(color = "orange") +
  geom_line(y = db.cut) +
  geom_line(y = -db.cut) +
  labs(title = "DFBETA",
       x = "Observation",
       y = "DFBETA")
ggplot(lm.model, aes(x = n.index, y = dfbetas(lm.model)[,"Distance"])) +
  geom_point(color = "orange") +
  geom_line(y = db.cut) +
  geom_line(y = -db.cut) +
  labs(title = "DFBETAS",
       x = "Observation",
       y = "DFBETAS")

#VIF
library(car)
cor(mtcars)
lm.model <- lm(mpg ~ ., data = mtcars)
vif(lm.model) #If there is one categorical variable then look at the right

#Lab 9

#1 Run a regression
lab9.1 <- lm(FE ~ EngDispl + Transmission + AirAspirationMethod + TransLockup + TransCreeperGear + DriveDesc + IntakeValvePerCyl + CarlineClassDesc + VarValveLift, data = cars2010)
#Visualizing to identify potential influential observations
ggplot(lab9.1, aes(x = fitted(lab9.1), y = resid(lab9.1))) +
  geom_point(color = "orange") +
  labs(title = "Standardized Residuals",
       x = "Observation",
       y = "Residuals")
#Cooks's D, has a lot of influential points
n.indexlab <- seq(1, nrow(cars2010))
cars2010lab <- cbind(cars2010, n.indexlab)
D.cutlab <- 4/(nrow(cars2010lab)-9-1)
ggplot(lab9.1, aes(x = n.indexlab, y = cooks.distance(lab9.1))) +
  geom_point(color = "orange") +
  geom_line(y = D.cutlab) +
  labs(title = "Cook's D",
       x = "Observation",
       y = "Cook's Distance")
#DFFITS, yes there are a lot
df.cutlab <- 2*(sqrt((10/nrow(cars2010lab))))
ggplot(lab9.1, aes(x = n.indexlab, y = dffits(lab9.1))) +
  geom_point(color = "orange") +
  geom_line(y = df.cutlab) +
  labs(title = "DFFITS",
       x = "Observation",
       y = "DFFITS")
#Hat values THROW OUT ITS BAD
hat.cutlab <- (2*10)/nrow(cars2010lab)
ggplot(lab9.1, aes(x = n.indexlab, y = hatvalues(lab9.1))) +
  geom_point(color = "orange") +
  geom_line(y = hat.cutlab) +
  labs(title = "Hat Values",
       x = "Observation",
       y = "Hat Values")
#Add Column for DFFITS and STUDENT
cars2010lab$dffits <- dffits(lab9.1)
cars2010lab$student <- rstudent(lab9.1)

#Filter by DFFITS > 1 and Student > 3
cars2010influential <- cars2010lab %>% 
  filter(student < -3 | student >3)

#Find largest residual
cars2010lab$resid <- resid(lab9.1)
max(cars2010lab$resid)
cars2010lab$cooks <- cooks.distance(lab9.1)
max(cars2010lab$cooks)

#Model Building and Scoring for Prediction 7/11/2024

library(AmesHousing)
library(stats)
set.seed(123)
library(dplyr)
library(car)
library(tidyr)
ames <- make_ordinal_ames()
ames <- ames %>% mutate(id = row_number())
train <- ames %>% sample_frac(0.7) #Sendng 70% of the data
test <- anti_join(ames, train, by = "id")

#Ridge Regression
train_reg <- train %>% 
  dplyr::select(Sale_Price, Lot_Area, Street, Bldg_Type, House_Style, Overall_Qual, Roof_Style, Central_Air, First_Flr_SF, Second_Flr_SF, Full_Bath,
                Half_Bath, Fireplaces, Garage_Area, Gr_Liv_Area, TotRms_AbvGrd) %>% 
  mutate_if(is.numeric, ~ replace_na(., mean(., na.rm = TRUE)))
train_x <- model.matrix(Sale_Price ~ ., data = train_reg)[,-1] #This function will take a dataset and convert it into a modelable form. Dummy encodes the categoricals
train_y <- train_reg$Sale_Price #Saving off the y variable. Not necessary
#Creates a matrix of only x variables
#Extra piece on the end is [,-1] which removes the first column because it creates an intercept
test_reg <- test %>% 
  dplyr::select(Sale_Price, Lot_Area, Street, Bldg_Type, House_Style, Overall_Qual, Roof_Style, Central_Air, First_Flr_SF, Second_Flr_SF, Full_Bath,
                Half_Bath, Fireplaces, Garage_Area, Gr_Liv_Area, TotRms_AbvGrd) %>% 
  mutate_if(is.numeric, ~ replace_na(., mean(., na.rm = TRUE)))
test_x <- model.matrix(Sale_Price ~ ., data = test_reg)[,-1] #This function will take a dataset and convert it into a modelable form. Dummy encodes the categoricals
test_y <- test_reg$Sale_Price

#Load function and perform RIDGE
install.packages("glmnet")
library(glmnet)
ames_ridge <- glmnet(x = train_x, y = train_y, alpha = 0) #Alpha = 0 builds RIDGE regression
plot(ames_ridge, xvar = "lambda")

#Perform LASSO
ames_lasso <- glmnet(x = train_x, y = train_y, alpha = 1) #Alpha = 1 builds LASSO
plot(ames_lasso, xvar = "lambda")

#Perform ELASTIC NET
ames_em <- glmnet(x = train_x, y = train_y, alpha = 0.5)#Alpha = (0,1) build EN

#Perform LASSO CV
ames_lasso_cv <- cv.glmnet(x = train_x, y = train_y, alpha = 1)
plot(ames_lasso_cv)
#cv.glmnet is the best starting place, don't do it without cv. until the final model
plot(ames_lasso, xvar = "lambda")
abline(v = log(ames_lasso_cv$lambda.1se), col = "red", lty = "dashed")
abline(v = log(ames_lasso_cv$lambda.min), col = "black", lty = "dashed")

#finding our variables
coef(ames_lasso, s = c(ames_lasso_cv$lambda.min, ames_lasso_cv$lambda.1se))

#Predictions
test$pred_lm <- predict(ames_lm, newdata = test)
head(test$pred_lm)
test_reg$pred_lasso <- predict(ames_lasso, s= ames_lasso_cv$lambda.1se, newx = test_x)
#s = means lambda = here

#Predictions MAPE
test %>% 
  mutate(lm_APE = 100*abs((Sale_Price - pred_lm)/Sale_Price)) %>% 
  dplyr::sumarise(MAPE_lm = mean(lm_APE))

test %>% 
  mutate(lasso_APE = 100*abs((Sale_Price - pred_lasso)/Sale_Price)) %>% 
  dplyr::sumarise(MAPE_lasso = mean(lasso_APE))

#Breakout Session 11
bike <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bike.csv")
set.seed(123)
bike <- bike %>% 
  mutate(id = row_number())
train <- bike %>% sample_frac(0.7)
test <- anti_join(bike, train, by = "id")
#1 Observation count of each dataset
dim(train) #12165
dim(test) #5214

#2 Predict cnt using temp, atemp, hum and windspeed with RIDGE, CV. What is lambda
train_x <- model.matrix(cnt ~ temp + atemp + windspeed + hum, data = train)[,-1]
train_y <- train$cnt
bike_ridge <- glmnet(x = train_x, y = train_y, alpha = 0)
plot(bike_ridge, xvar = "lambda")
bike_ridge_cv <- cv.glmnet(x = train_x, y = train_y, alpha = 0)
plot(bike_ridge_cv) #lambda is about 4
bike_ridge_cv$lambda.min #minimum lambda is 7.4 first dashed line
bike_ridge_cv$lambda.1se #max lambda is 91.08475 second dashed line

#3 What are the first 5 predictions from the TEST dataset from this model
#Need to go ahead and prep the test dataset
test_x <- model.matrix(cnt ~ temp + atemp + windspeed + hum, data = test)[,-1]
test_y <- test$cnt
test$pred_ridge <- predict(bike_ridge, s = bike_ridge_cv$lambda.min, newx = test_x) #So want the full training, but lambda from cv training
head(test$pred_ridge)
head(test$cnt)
# First 5 predictions are 25, 92, 96, 117, 98
# Actual first 5 are 3, 56, 34, 39,  6

#4 Check MAPE for the model. MLR without temp was 478%, did we do better?
#This is for the MLR
bike_lm2 <- lm(cnt ~ atemp + hum + windspeed, data = train)
test$pred_lm <- predict(bike_lm2, newdata = test)
test %>%
  mutate(lm_APE = 100*abs((cnt - pred_lm)/cnt)) %>%
  dplyr::summarise(MAPE_lm = mean(lm_APE))

test %>%
  mutate(ridge_APE = 100*abs((cnt - pred_ridge)/cnt)) %>%
  dplyr::summarise(MAPE_ridge = mean(ridge_APE))
# 485% not better

#Lab 11
set.seed(123)
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(FuelEconomy)

#1a LASSO Regression to predict FE using all remaining vars
train_x <- model.matrix(FE ~ EngDispl + factor(NumCyl) + factor(Transmission) + factor(AirAspirationMethod) + factor(NumGears) + factor(TransLockup) + factor(TransCreeperGear) + factor(DriveDesc) + factor(IntakeValvePerCyl) + factor(ExhaustValvesPerCyl) + factor(CarlineClassDesc) + factor(VarValveTiming) + factor(VarValveLift), data = cars2010)[,-1] #This function will take a dataset and convert it into a modelable form. Dummy encodes the categoricals
train_y <- cars2010$FE #Saving off the y variable. Not necessary

cars2010_lasso <- glmnet(x = train_x, y = train_y, alpha = 1) #Alpha = 1 builds LASSO
plot(cars2010_lasso, xvar = "lambda")

#1b LASSO CV Regression
cars2010_lasso_cv <- cv.glmnet(x = train_x, y = train_y, alpha = 1)
plot(cars2010_lasso_cv)
cars2010_lasso_cv$lambda.min #0.003793359
cars2010_lasso_cv$lambda.1se #0.1428175
coef(cars2010_lasso, s= c(cars2010_lasso_cv$lambda.min, cars2010_lasso_cv$lambda.1se)) # 13 vars

#Categorical Data Analysis 7/15/2024

#Ames Housing Data
library(AmesHousing)
library(stats)
set.seed(123)
library(dplyr)
library(car)
library(tidyr)
library(ggplot2)
ames <- make_ordinal_ames()
ames <- ames %>% mutate(id = row_number())
train <- ames %>% sample_frac(0.7) #Sendng 70% of the data
test <- anti_join(ames, train, by = "id")

#Creating a binary variable
train <- train %>% 
  mutate(Bonus = ifelse(Sale_Price > 175000, 1, 0))

#Exploring the data
table(train$Central_Air)
ggplot(data = train) +
  geom_bar(mapping = aes(x = Central_Air))
table(train$Bonus)
ggplot(data = train) +
  geom_bar(mapping = aes( x = Bonus))

#Exploring bivariate data analysis
table(train$Central_Air, train$Bonus) #First var will be rows, second will be columns
ggplot(data = train) +
  geom_bar(mapping = aes( x = Bonus,
                          fill = Central_Air))

#More advanced data table
install.packages("gmodels")
library(gmodels)
CrossTable(train$Central_Air, train$Bonus) #N, Chi-Square, N/Row Total, N/Col Total, N/Table Total

#Pearson Chi-Square Test
chisq.test(table(train$Central_Air, train$Bonus)) #Give R a table! Don't have to worry about factor with chi-squared

#Fisher's Exact Test if we don't meet sample size assumptions. Calculates every permutation of the data
fisher.test(table(train$Central_Air, train$Bonus))

#Mantel-Haenszel Chi-Square Test for Two Ordinal Variables
install.packages("vcdExtra")
library(vcdExtra)
CMHtest(table(train$Central_Air, train$Bonus))$table[1,] #Assumes categories are in the correct order
#If there are more rows, we would want just the first row. We get this by adding $table[1,]

#Calculate an odds ratio in R . Two binary
install.packages("DescTools")
library(DescTools)
OddsRatio(table(train$Central_Air, train$Bonus)) #Input is a table, not just the variables themselves

#Cramers V Two nominal vars 
assocstats(table(train$Central_Air, train$Bonus)) #Give it a table. Not interpretable all (0,1)

#Spearman's Correlation two ordinal vars
cor.test(x = as.numeric(ordered(train$Central_Air)),
         y = as.numeric(ordered(train$Bonus)),
         method = "spearman")

#Breakout Session 12

#1 Split into test vs train
bike <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bike.csv")
set.seed(123)
bike <- bike %>% mutate(id = row_number())
train <- bike %>% sample_frac(0.7)
test <- anti_join(bike, train, by = "id")

#2 Create a variable called casual_high
train$casual_high <- train$casual >= train$registered

#3 Pearson Chi-Square Test at sig level of .001. Season is ordinal vs casual_high
chisq.test(table(train$season, train$casual_high)) #p < a Reject Ho
#Compare to Mantel-Haenszel Test
CMHtest(table(train$season, train$casual_high))$table[1,]
#p>a FTR

#4 Want to know if casual_high and holiday is related
CrossTable(train$holiday, train$casual_high)
CMHtest(table(train$holiday, train$casual_high))$table[1,]
#p < a Reject Ho and determine association
OddsRatio(table(train$holiday, train$casual_high))
#Odds Ratio is 3.4553

#Lab 12
safety <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/safety.csv")

#1a Continuous are none
# Nominal are type of car
# Ordinal are region, weight, size, and unsafe

#1b Examine the association between region and unsafe
CrossTable(safety$Region, safety$Unsafe, expected = TRUE) #Add expected row
CMHtest(table(safety$Region, safety$Unsafe))$table[1,]
#1ba % of cars in Asia unsafe 0.429
#1bb % of safe cars manufactured in NA 0.697
#1bci Ho is no association
# Ha as an association
#1bcii Alpha of .05 what is the decision
# p > a, FTR, there is no association
#1d Interpret the odds ratio in the context of the problem
OddsRatio(table(safety$Region, safety$Unsafe))
# Cars in Asia are .434 times as likely to be safe as North American cars
#compare column to column for odds interpretation

#1c Association between size and unsafe
CMHtest(table(safety$Size, safety$Unsafe))$table[1,]
# Reject p > a
# Strength of association
cor.test(x = as.numeric(ordered(safety$Size)),
         y = as.numeric(ordered(safety$Unsafe)),
         method = "spearman")

#Intro to Logistic Regression 7/16/2024
library(AmesHousing)
ames <- make_ordinal_ames()
ames <- ames %>% mutate(id = row_number())
train <- ames %>% sample_frac(0.7) #Sendng 70% of the data
test <- anti_join(ames, train, by = "id")

#Linear Probability Model
lp.model <- lm(Bonus ~ Gr_Liv_Area, data = train)
with(train, plot(x = Gr_Liv_Area, y = Bonus,
                 main = "OLS Regression",
                 xlab = "Greater Living Area",
                 ylab = "Bonus Eligibility"))
abline(lp.model)

#Binary Logistic Regression
ames_logit <- glm(Bonus ~ Gr_Liv_Area, data = train,
                  family = binomial(link = "logit"))
summary(ames_logit) #Don't want to look at Betas directly. Need to be transformed: 100*((e^B) - 1)

#Odds Ratio
100*(exp(cbind(coef(ames_logit), confint(ames_logit)))-1)

#Odds Ratio with a Categorical Variable
ames_logit2 <- glm(Bonus ~ Gr_Liv_Area + Central_Air + factor(Fireplaces),
                   data = train, family = binomial(link = "logit"))
summary(ames_logit2)
100*(exp(cbind(coef(ames_logit2), confint(ames_logit2)))-1)

#Concordance
install.packages("survival")
library(survival)
survival::concordance(object = ames_logit)

#Forwards and Backwards Selection for Logistic Regression
train_sel_log <- train %>% 
  dplyr::select(Bonus, Lot_Area, Street, Bldg_Type, House_Style, Overall_Qual, Roof_Style, Central_Air, First_Flr_SF, Second_Flr_SF, Full_Bath,
                Half_Bath, Fireplaces, Garage_Area, Gr_Liv_Area, TotRms_AbvGrd) %>% 
  mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))
full.model <- glm(Bonus ~ ., data = train_sel_log)
empty.model <- glm(Bonus ~ 1, data = train_sel_log)
for.model <- step(empty.model,
                  scope = list(lower = formula(empty.model),
                               upper = formula(full.model)),
                  direction = "forward",
                  k = log(dim(train_sel_log)[1]))
back.model <- step(full.model,
                   scope = list(lower = formula(empty.model),
                                upper = formula(full.model)),
                   direction = "backward",
                   k = log(dim(train_sel_log)[1]))

#Breakout #13
#1 Split into training and test
bike <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bike.csv")
set.seed(123)
bike <- bike %>% mutate(id = row_number())
train <- bike %>% sample_frac(0.7)
test <- anti_join(bike, train, by = "id")

#2 Create a variable
train$casual_high <- train$casual >= train$registered

#Lab 13
#Read in the data
safety <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/safety.csv")

#1a Build a logistic regression predicting Unsafe using Region, Weight and Size
unsafe_logit <- glm(Unsafe ~ factor(Region) + Weight + factor(Size), data = safety,
                    family = binomial(link = "logit"))
summary(unsafe_logit)
#1aa Vars sig at 0.05 are factor size 2 med and factor size 3 large
#1ab Concordance for this model
survival::concordance(object = unsafe_logit) #.8482

#1b Remove variables one at a time that have a p-value > 0.05
safety_sel_log <- safety %>% 
  dplyr::select(Unsafe, Type, Region, Weight, Size) %>% 
  mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))
full.model <- glm(Unsafe ~ ., data = safety_sel_log)
empty.model <- glm(Unsafe ~ 1, data = safety_sel_log)
back.model <- step(full.model,
                   scope = list(lower = formula(empty.model),
                                upper = formula(full.model)),
                   direction = "backward",
                   k = qchisq(0.05, 1, lower.tail =  FALSE))

survival::concordance(back.model)
unsafe_logit1 <- glm(Unsafe ~ factor(Size), data = safety,
                     family = binomial(link = "logit"))
summary(unsafe_logit1)
survival::concordance(unsafe_logit1)
