#Clearing the environment 
rm(list = ls())

library(dplyr)
library(tidyr)
library(haven)

#Read data
library(readr)
MyData <- read_dta("MyData.dta")

#Select and rename columns of interest 
data<-MyData %>% select(intrvyr, f103yr, testever, hivstat2, f705, agegrpc, agefsx, aglstbir, langnat, statprovcod,  urbrural, hieduc, hieduc_p, f119, 
                        f121, f501, f206)
names(data)<-c('intrvyr', 'dob', 'hivtestever', 'hivstat', 'pregdesire', 'agegrp', 'agefsx', 'aglstbir', 'language', 'province', 'urbrural', 
               'education', 'prtnreduc', 'religion', 'relg.imp', 'mrtlstat', 'chldth')

#Create a column for 'age' variable based on year of birth and year of taking the survey
data$age<-data$intrvyr - data$dob

#Drop the year of birth and (dob) and year of taking the survey (intrvyr) variables
data$intrvyr<-NULL
data$dob<-NULL
data<-data%>%select(16,everything())

#Recoding some numeric values as missing
data$agefsx <- as.numeric(data$agefsx)
data$agefsx[data$agefsx == "88"] <- "N/A"
data$aglstbir <- as.numeric(data$aglstbir)
data$aglstbir[data$aglstbir == "88"] <- "N/A"
data$age <- as.numeric(data$age)
data$age[data$age == "-97990"] <- "N/A"
data$age <- as.numeric(data$age)
data$age[data$age == "-97989"] <- "N/A"

#Recoding a continuous variable into categorical variable
data$province <- as.character(data$province)
data$province[data$province == 1] <-"Lusaka"
data$province[data$province == 2] <-"Northen"
data$province[data$province == 3] <-"Southern"


data$hivtestever <- as.character(data$hivtestever)
data$hivtestever[data$hivtestever == 0] <-"no"
data$hivtestever[data$hivtestever == 1] <-"yes"


data$hivstat <- as.character(data$hivstat)
data$hivstat[data$hivstat == 1] <-"positive"
data$hivstat[data$hivstat == 2] <-"negative"
data$hivstat[data$hivstat == 3] <-"unknown"


data$agegrp <- as.character(data$agegrp)
data$agegrp[data$agegrp == 1] <-"15-19"
data$agegrp[data$agegrp == 2] <-"20-24"
data$agegrp[data$agegrp == 3] <-"25-29"
data$agegrp[data$agegrp == 4] <-"30-34"
data$agegrp[data$agegrp == 5] <-"35-39"
data$agegrp[data$agegrp == 6] <-"40-44"
data$agegrp[data$agegrp == 7] <-"45-49"
data$agegrp[data$agegrp == 8] <-"50+"


data$language <- as.character(data$language)
data$language[data$language == 1] <-"English"
data$language[data$language == 2] <-"Bemba"
data$language[data$language == 3] <-"Nyanja"
data$language[data$language == 4] <-"Tonga"
data$language[data$language == 7] <-"Other"


data$urbrural <- as.character(data$urbrural)
data$urbrural[data$urbrural == 1] <-"Urban"
data$urbrural[data$urbrural == 2] <-"Rural"



data$education <- as.character(data$education)
data$education[data$education == 0] <-"None"
data$education[data$education == 1] <-"Primary"
data$education[data$education == 2] <-"Secondary"
data$education[data$education == 3] <-"Higher"
data$education[data$education == 8] <-"N/A"


data$prtnreduc <- as.character(data$prtnreduc)
data$prtnreduc[data$prtnreduc == 0] <-"None"
data$prtnreduc[data$prtnreduc == 1] <-"Primary"
data$prtnreduc[data$prtnreduc == 2] <-"Secondary"
data$prtnreduc[data$prtnreduc == 3] <-"Higher"
data$prtnreduc[data$prtnreduc == 88] <-"Not in Union"


data$religion <- as.character(data$religion)
data$religion[data$religion == 1] <-"Catholic"
data$religion[data$religion == 2] <-"Protestant"
data$religion[data$religion == 3] <-"Pentecostal/Charismatic"
data$religion[data$religion == 4] <-"Other Christian"
data$religion[data$religion == 5] <-"Muslim"
data$religion[data$religion == 6] <-"Traditional Religion"
data$religion[data$religion == 7] <-"No religion"
data$religion[data$religion == 96] <-"Other"


data$relg.imp <- as.character(data$relg.imp)
data$relg.imp[data$relg.imp == 1] <-"Very important"
data$relg.imp[data$relg.imp == 2] <-"Somewhat important"
data$relg.imp[data$relg.imp == 3] <-"Not at all important"
data$relg.imp[data$relg.imp == 8] <-"N/A"


data$mrtlstat <- as.character(data$mrtlstat)
data$mrtlstat[data$mrtlstat == 1] <-"Yes, currently married"
data$mrtlstat[data$mrtlstat == 2] <-"Yes, currently living with a man"
data$mrtlstat[data$mrtlstat == 3] <-"No, not in union"


data$pregdesire <- as.character(data$pregdesire)
data$pregdesire[data$pregdesire == 1] <-"Likely"
data$pregdesire[data$pregdesire == 2] <-"Not likely"
data$pregdesire[data$pregdesire == 3] <-"Not likely"
data$pregdesire[data$pregdesire == 8] <-"N/A"

data$chldth <- as.character(data$chldth)
data$chldth[data$chldth == 1] <-"Yes"
data$chldth[data$chldth == 2] <-"No"
data$chldth[data$chldth == 88] <-"N/A"

#Replacing empty values in the dataset by NA
data[data==""]<-NA

#Saving data
write.table(data, file = "data.csv",
            sep = "\t", row.names = F)

#Checking for missing values
sum(is.na(data))

#Dropping the missing values
newdata <- na.omit(data)

#Saving new data
write.table(newdata, file = "newdata.csv",
            sep = "\t", row.names = F)

                                                      ################## ANALYSIS #######################
#Descriptive statistics for some variables
summary(newdata$chldth)

#Univariate analysis
library(pander)
newdata %>% group_by(chldth) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(percentage = 100*frequency/sum(frequency)) %>%
  pander


#Bivariate analysis (cat vs. cat)
newdata %>% group_by(agegrp, education) %>% 
  summarize(frequency = n()) %>% 
  mutate(percentage = 100*frequency/sum(frequency)) %>%
  pander
#Chi-Square
tbl=table(newdata$education, newdata$pregdesire)
chisq.test(tbl)

char_columns <- sapply(newdata, is.character) 
newdata[ , char_columns] <- as.data.frame(apply(newdata[ , char_columns], 2, as.numeric))
sapply(newdata, class) 

summary(newdata$aglstbir)

#Multivariate analysis (data or newdata?)
library(glm2)
newdata$relg.imp<-factor(newdata$relg.imp)
#Defining reference levels 
newdata$relg.imp <- relevel(newdata$relg.imp, "Somewhat important")
model.full=glm(pregdesire ~ mrtlstat, data = newdata, family = binomial(link="logit"))
model.full=glm(pregdesire ~ hivstat + agegrp + education + language + urbrural + province + relg.imp + chldth, data = newdata,
               family = "binomial")
summary(model.full)
#OR and 95% CI
exp(cbind(OR = coef(model.full), confint(model.full)))


glm.fit=glm(pregdesire ~ hivstat + agegrp + education + language + urbrural + province + relg.imp + chldth, data = newdata, 
            family = "binomial", subset = train)
glm.probs <- predict(glm.fit, newdata = newdata[!train,], type = "response")
glm.probs

glm.pred <- ifelse(glm.probs > 0.5, "Likely", "Not likely")
glm.pred

table(glm.pred,pregdesire)
                                                    ########################################
#Model Selection
attach(newdata)

model.null=glm(pregdesire ~ 1, data = newdata, family = "binomial")
step(model.null,
     scope = list(upper=model.full),
     direction="both",
     test="Chisq",
     data=newdata)

library(car)
Anova(model.full, type="II", test="Wald")

library(My.stepwise)
My.stepwise.glm(Y = "pregdesire", variable.list = c("hivstat", "agegrp", "education", "language", "urbrural", "province", "relg.imp", "chldth"), 
                in.variable = "NULL", data = newdata, sle = 0.20, sls = 0.10, myfamily = "binomial")



  


