# show CS and IT job share by Male and Female in PG Co.
PGEEOCS <- read.csv("C:/Users/dyang129/Desktop/INFM 600 Project/PGEEOCS.csv", header = TRUE)
View(PGEEOCS)
barplot(as.matrix(PGEEOCS),main = "PG County Computer Related Job Distributed by Male and Female", cex.main = 0.9, col = c("blue", "pink"), density = 30, ylab = "Number of Job", xlab = "Job Code", cex.names = 0.5, beside = T)

# total #of jobs shared by Male and Female in PG Co.
df <- read.table("PGEEO.csv")
sharedJobs <- c(241625, 251445)
barplot(sharedJobs, main = "Number of Jobs Shared by Male and Female", names.arg = c("Male", "Female"), beside = TRUE, col = blues9, xlab = "Gender", ylab = "Number of Jobs")

# to calculate the mean for Median Household Income
mdSocioEc <- read.csv("C:/Users/dyang129/Desktop/Team Project/USED in Plot/mdSocioEc.csv", header=TRUE)
View(mdSocioEc)
mdMedianIncome <- as.numeric(mdSocioEc$MedianHouseholdIncome)
mean(mdMedianIncome)
boxplot(mdMedianIncome)
# distribution of the Median Income among Maryland Each County
plot(mdMedianIncome)

# to see relationship between HS diploma with Female
x1 <- mdSocioEc$High.School.Diploma
y1 <- mdSocioEc$Female
plot(x1, y1, col="blue", main = "Female with HS Diploma Regression", abline(lm(y1~x1)), xlab = "HS Diploma", ylab = "Felame" )

lm(formula = y1~x1)
# To see if there is the coefficient corelation between HS diploma and females, and P value
relation1 <- lm(formula = y1~x1)
summary(relation1)

# relationship between Bachlor degree and Female
xBac <- mdSocioEc$Bachelor.s.degree
y1 <- mdSocioEc$Female
plot(xBac, y1, col="blue", main = "Female with Bachelor's Degree Regression", abline(lm(y1~xBac)), xlab = "Bachelor's Degree", ylab = "Felame" )

# relationship between Bachlor degree and Gender
# H0 = Mean difference is 0 between male and female
# two-sided test
t.test(mdSocioEc$Bachelor.s.degree)

#difference between graduate degree or professional degree and people with Bachelor's Degree 
t.test(mdSocioEc$Graduate.or.Professional, mdSocioEc$Bachelor.s.degree)

#difference between jobs taken by males and females
CSjobGender <- read.csv("C:/Users/dyang129/Desktop/Team Project/USED in Plot/CSjobGender.csv", header = TRUE)
View(CSjobGender)
t.test(CSjobGender$Male, CSjobGender$Female)

# here we want to see the relationship between unemployed and family in proverty
x2 <- mdSocioEc$PercentFamiliesinPoverty
y2<- mdSocioEc$Unemployment.Rate
plot(x2, y2, col="blue", main = "Percent of Unemployed and Percent of Family in Proverty Regression", abline(lm(y2~x2)),cex.main = 0.8, xlab = "Percent Unemployed", ylab= "Percent Family in Poverty")

# To see if there is the coefficient corelation between HS diploma and females, and P value
relation2 <- lm(y2~x2)
lm(formula = y2~x2)
summary(lm(y2~x2))

# Race in PG County
PGRace <- c(173881, 570138, 38063, 3449, 269, 86885, 25008, 150268)
barplot(PGRace, main = "PG County Race", xlab = "Race", ylab = "Population", names.arg = c("White", "Black", "Asian", "Indian/Alaska", "Hawaiian", "Others", "Mix", "Hispanic"), col = "blue", density = 10, cex.names = 0.6)

# MD IT Tuition Assistance, if distribution is symmetric
MDITTuitionAssistance <- read.csv("E:/UM 2018 Classes/INFM 600/Team Project/USED in Plot/MDITTuitionAssistance.csv")
View(MDITTuitionAssistance)
CostAssistance <- as.numeric(MDITTuitionAssistance$Cost)
mean(CostAssistance)
boxplot(CostAssistance)
plot(CostAssistance, xlab = "program", ylab = "Assistance Amount")

#one sample, t-test to see if there is a significant difference between the means of two groups
# t.test(CostAssistance)

# to summary MD Tution Assistantship incluing min, max, median, and mean
summary(MDITTuitionAssistance)

# to visualize MD department offer Tuition Assistantship for CS related programs
MarylandTuitionAssistance <- read.csv("E:/UM 2018 Classes/INFM 600/Team Project/USED in Plot/MarylandTuitionAssistance.csv")
View(MarylandTuitionAssistance)
summary(MarylandTuitionAssistance)

# to visualize Median Household Income 
mdSocioEcMinSum <- read.csv("C:/Users/dyang129/Desktop/Team Project/USED in Plot/mdSocioEcMinSum.csv")
View(mdSocioEcMinSum)
summary(mdSocioEcMinSum)

# to see the education level distribution in PG Co.
pie(pgSocioEcEdu$numberOfPpl, labels = row.names(pgSocioEcEdu$eduLevel), main = "PG County Education Level")



# to see MD and PG Edu level
mdpgSocioEcEduTotal <- read.csv("C:/Users/dyang129/Desktop/Team Project/USED in Plot/mdpgSocioEcEduTotal.csv", header=TRUE)
View(mdpgSocioEcEduTotal)
barplot(as.matrix(mdpgSocioEcEduTotal), main = "Maryland and PG County Education", col = c("blue", "red"),density = 60, xlab = "Education Level", ylab = "Number of People",cex.names = 0.5,beside = T)

# Tuition Assistance Analysis
TA3 <- read.csv("C:/Users/dyang129/Desktop/Team Project/USED in Plot/TA3.csv", header = TRUE)
View(TA3)

# show program cost allocation
progCost <- as.numeric(TA3$x)
hist(progCost, main = "Histogram of Program Cost")
unique(TA3$Major)
aggregate(progCost, list(Major = TA3$Major), mean)
