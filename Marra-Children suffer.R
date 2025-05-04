
## Project:  SOC 302 Multivarable project
# Located:   ELSA 
# File Name: FILL THIS OUT
# Date:      FILL THIS OUT
# Who:       FILL THIS OUT


####################################################################################
############              Pre-Analysis: settings, packages, and data    ############
####################################################################################
# set wd
setwd("/courses/SOC302/marraa2")

### Settings + Packages
library(dplyr)
#install.packages("dplyr")
#install.packages("psych")
library(psych)
### Load data 
GSS <- read.csv("GSS2022.csv")

# set max print size
options(max.print=1000000)
####################################################################################
############              PHASE 1: CLEAN DATA FOR ANALYSIS              ############
####################################################################################


## Steps of cleaning variables Clear vars
# Step 1: Examine variable and coding schema: Table() / summary()
# Step 2: Recode (if necessary/warrented): mutate(), ifelse(), etc
# Step 3: Confirm: table() / summary()



############                     DEPENDENT VARIABLE                     ############
############         Preschool kids suffer with working mothers               ############

# STEP 1: Examine variable and coding schema 
table(GSS$fepresch)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, agree = ifelse(fepresch <= 2, 1, 0))
GSS <- mutate(GSS, disagree = ifelse(fepresch >= 3, 1, 0))
# STEP 3: Confirm creation (if necessary)
table(GSS$fepresch, GSS$agree)
table(GSS$fepresch, GSS$disagree)

       
############                  INDEPENDENT VARIABLE                    ############
############              Factors to preschool kids suffering                ############
       
       # STEP 1: Examine variable and coding schema 
       table(GSS$degree) 
       # STEP 2: Recode if necessary or justify if not neccesary
       GSS <- mutate(GSS, no_hs = ifelse(degree == 0, 1, 0))
       GSS <- mutate(GSS, hs = ifelse(degree == 1, 1, 0))
       GSS <- mutate(GSS, bachelors = ifelse(degree == 3, 1, 0))
       GSS <- mutate(GSS, associate = ifelse(degree == 2, 1, 0))
       GSS <- mutate(GSS, graduate = ifelse(degree == 4, 1, 0))
       
       
       # STEP 3: Confirm creation (if necessary)
       table(GSS$degree, GSS$no_hs)
       table(GSS$degree, GSS$hs)
       table(GSS$degree, GSS$bachelors)
       table(GSS$degree, GSS$associate)
       table(GSS$degree, GSS$graduate)
       
       ############                  Control VARIABLE                    ############
       ############              Woman wont get job or promotion              ############
       # STEP 1 : examine initial variable 
       table(GSS$discaffw)
       
       # STep 2: Create dummy variables for men and women
       GSS <- mutate(GSS, likely = ifelse(discaffw == 1 | discaffw == 2, 1, 0))
       GSS <- mutate(GSS, unlikely = ifelse(discaffw == 3 | discaffw == 4, 1, 0))
       
       # Step 3: confirm
       table(GSS$discaffw, GSS$likely)
       table(GSS$discaffw, GSS$unlikely)

       
       
############                  Control VARIABLE                    ############
############             Gender in childcare                      ############
       # STEP 1 : examine initial variable 
       table(GSS$sex)
       
       # STep 2: Create dummy variables for men and women
       GSS <- mutate(GSS, man = ifelse(sex == 1, 1, 0))
       GSS <- mutate(GSS, woman = ifelse(sex == 2, 1, 0))
       
       # Step 3: confirm
       table(GSS$sex, GSS$man)
       table(GSS$sex, GSS$woman)
       
############                  Control VARIABLE                    ############
############              Family Structure                        ############
      # STEP 1 : examine initial variable 
       table(GSS$marital)
       
       # STep 2: Create dummy variables for men and women
       GSS <- mutate(GSS, married = ifelse(marital == 1, 1, 0))
       GSS <- mutate(GSS, prev_married = ifelse(marital == 2 | martital == 3 | marital == 4, 1, 0)) 
       GSS <- mutate(GSS, never_married = ifelse(marital == 5, 1, 0))
       
       # Step 3: confirm
       table(GSS$marital, GSS$married)
       table(GSS$marital, GSS$widowed)
       table(GSS$marital, GSS$never_married)
       
############                  Control VARIABLE                    ############
############         Mother self-employed or work for somebody   ############
       # STEP 1 : examine initial variable 
       table(GSS$mawrkslf)
       
       # STep 2: Create dummy variables for men and women
       GSS <- mutate(GSS, ma_selfemployed = ifelse(mawrkslf == 1, 1, 0))
       GSS <- mutate(GSS, ma_someoneelse = ifelse(mawrkslf == 2, 1, 0))
    
       # Step 3: confirm
       table(GSS$mawrkslf, GSS$ma_selfemployed)
       table(GSS$mawrkslf, GSS$ma_someoneelse)

       
############                  Control VARIABLE                    ############
############         CHANGE IN EMPLOYMENT OVER COVID PANDEMIC ############
       # STEP 1 : examine initial variable 
       table(GSS$covemply)
       
       # STep 2: Create dummy variables for men and women
       GSS <- mutate(GSS, no_job_before_no_job_after = ifelse(covemply == 1 | covemply == 5, 1, 0))
       GSS <- mutate(GSS, same_job_before_pandemic = ifelse(covemply == 2, 1, 0))
       GSS <- mutate(GSS, lost_job_after_pandemic_new_job = ifelse(covemply == 3 | covemply == 4 ,1, 0))
       GSS <- mutate(GSS, change_left_not_due_pandemic = ifelse(covemply == 6, 1, 0))
       
       
       # Step 3: confirm
       table(GSS$covemply, GSS$no_job_before_no_job_after)
       table(GSS$covemply, GSS$same_job_before_pandemic)
       table(GSS$covemply, GSS$lost_job_after_pandemic_new_job)
       table(GSS$covemply, GSS$change_left_not_due_pandemic)
       
      
       ############                  Control VARIABLE                    ############
       ############                  Labor force status               ############
       # STEP 1 : examine initial variable 
       table(GSS$wrkstat)
       
       # STep 2: Create dummy variables for men and women
       GSS <- mutate(GSS, full_time = ifelse(wrkstat == 1, 1, 0))
       GSS <- mutate(GSS, part_time = ifelse(wrkstat == 2, 1, 0))
       GSS <- mutate(GSS, has_but_not_due_illness_vacation_strike = ifelse(wrkstat == 3 |wrkstat == 4, 1, 0))
       GSS <- mutate(GSS, retired = ifelse(wrkstat == 5 | wrkstat == 6 | wrkstat == 7, 1, 0))
       GSS <- mutate(GSS, other = ifelse(wrkstat == 8, 1, 0))
       
       
       # Step 3: confirm
       table(GSS$wrkstat, GSS$full_time)
       table(GSS$wrkstat, GSS$part_time)
       table(GSS$wrkstat, GSS$has_but_not_due_illness_vacation_strike)
       table(GSS$wrkstat, GSS$retired)
       table(GSS$wrkstat, GSS$other)
  
####################################################################################
############              PHASE 2: CREATE MY DATASET                    ############
####################################################################################
       
### STEP 1: Create a list of variables to keep
my_varlist <- c("fepresch", "agree", "disagree",
                "degree", "no_hs", "hs", "bachelors","associate" ,"graduate",
                "discaffw", "very_likely", "somewhat_likely", "somewhat_unlikely", "very_unlikely",
                "sex", "man", "woman",
                "marital", "married", "prev_married", "never_married",
                "mawrkslf", "ma_selfemployed", "ma_someoneelse",
                "covemply", "no_job_before_no_job_after", "same_job_before_pandemic", "lost_job_after_pandemic_new_job", "change_left_not_due_pandemic",
                "wrkstat", "full_time", "part_time", "has_but_not_due_illness_vacation_strike", "retired", "other")
       
       
### STEP 2: create a new dataset with only your variables and complete case
my_dataset <- GSS %>%
select(all_of(my_varlist)) %>%
  filter(complete.cases(.))
       
### STEP 3: Gather Summary Statistics and confirm valid dataset construction
describe(my_dataset)

       
       
####################################################################################
############              PHASE 3: Descriptive Statistics     ############
####################################################################################
# TABLE 1: DESCRIPTIVE STATISTICS HERE
       
       
####################################################################################
############              PHASE 4: Correlation matrix                ############
####################################################################################
#correlation between key IV and DV 
cor(my_dataset)

cor(my_dataset$fepresch, my_dataset$agree)


####################################################################################
############              PHASE 5: regression                ############
####################################################################################
#demographic
model1 <- glm(agree ~  no_hs  + bachelors + associate + graduate + man + widowed + divorced + seperated + never_married, data = my_dataset, family = binomial)
summary(model1)

#employment
model2 <- glm(agree ~ ma_selfemployed + no_job_before_no_job_after + lost_job_after_pandemic_couldnt_find_new + lost_job_after_pandemic_new_job + no_job_before_now_has + change_left_not_due_pandemic + part_time + has_but_not_due_illness_vacation_strike + unemployed_laidoff_looking + retired + in_school + keeping_house + other, data = my_dataset, family = binomial)
summary(model2)

#woman disparities in the workforce
model3 <- glm(agree ~ somewhat_likely + somewhat_unlikely + very_unlikely, data = my_dataset, family = binomial)
summary(model3)

