#Loading needed packages in case somebody doesn't have them installed already

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)

#Kaggle data set "The schools that create the most student debt"
#https://www.kaggle.com/datasets/thedevastator/the-schools-that-create-the-most-student-debt
#https://github.com/jenpeck/Capstone_Final_Project.git
#Per Kaggle page, this data is for the 2020-2021 academic year

options(timeout = 120)
dl <- "Student Loan Debt by School 2020-2021.csv"
if(!file.exists(dl))
  download.file("https://github.com/jenpeck/Capstone_Final_Project/blob/main/Student%20Loan%20Debt%20by%20School%202020-2021.csv", dl)

#this takes the csv and gives it an easier alias and looks at the class type and
#what the data frame looks like
debt <- read.csv("Student Loan Debt by School 2020-2021.csv")
class(debt)
str(debt)
colnames(debt)

#The original csv has special characters in some of the column names so I want
#to rename the columns for ease going forward
names(debt)[1] <- 'index'
names(debt)[2] <- 'opeid'
names(debt)[3] <- 'school'
names(debt)[4] <- 'city'
names(debt)[5] <- 'state'
names(debt)[6] <- 'zip'
names(debt)[7] <- 'school_type'
names(debt)[8] <- 'loan_type'
names(debt)[9] <- 'num_recipients'
names(debt)[10] <- 'num_loans_originated'
names(debt)[11] <- 'dollars_originated'
names(debt)[12] <- 'num_disbursements'
names(debt)[13] <- 'dollars_disbursed'

#check to make sure the columns are renamed
colnames(debt)

#get a look at the table
debt %>% as_tibble()

#look at a summary of some of the table items. 4078 schools, 7 loan types, 8
#school types. then check what are the types of schools and loans
debt %>% summarize(distinct_schools = n_distinct(school), 
                  distinct_loantype = n_distinct(loan_type),
                  distinct_schooltype = n_distinct(school_type))

unique(debt$school_type)
unique(debt$loan_type)

#what are the 7 loan types and counts of how many rows each 
debt %>% group_by(loan_type) %>% summarize(count = n()) %>% 
  arrange(desc(count))

#I am curious about what the totals are so i am going to look at all columns for 
#one of the schools
debt %>% filter(school == 'UNIVERSITY OF ALABAMA')

#I have to throw out every line where the loan type = total. this is because
#there are multiple 'total' lines for every school and i cannot figure out what 
#they are. One of them is the total of the other loan types combined. the other 
#four totals I cannot mathematically get to so I'm tossing them all
clean_debt <- debt %>% filter(loan_type != 'Total')

#also throw out the 25 lines that had no loan type listed
clean_debt <- clean_debt %>% filter(loan_type != "")

#make sure the totals and blanks no longer show up
clean_debt %>% group_by(loan_type) %>% summarize(count = n())

#make sure my rows went down - they did significantly from 42765 to 21360
nrow(clean_debt)

#also throw out rows with 0 recipients
clean_debt <- clean_debt %>% filter(num_recipients != '0')

#found this later and then decided to bring in at the beginning. Found out there
#were schools outside of america. I want to remove those since I am going to be 
#looking at some of the info by state. By running this example below I found 
#that the international schools have a blank state so that is how I will filter 
#them out
#clean_debt %>% filter(school == 'ROYAL COLLEGE OF SURGEONS IN IRELAND')
clean_debt <- clean_debt %>% filter(state != '')

#now rows went down to 13283
nrow(clean_debt)

###############################################################################

#start looking around at data in different ways

#how many schools of each school type - graphed. Need to make temp table first 
#so that I can look at only distinct schools

distinctschools <- clean_debt %>% select(school, school_type) %>%  
  distinct(school, school_type)

distinctschools %>% 
  ggplot(aes(school_type, fill = school_type)) + 
  geom_bar(color = "black") + 
  theme_gray() +
  ggtitle("Distibution of School Types") +
  xlab("School Types") +
  ylab("Count of Schools")

#how many recipients in each school type
clean_debt %>% group_by(school_type) %>% 
  summarize(number_recipients = sum(num_recipients)) %>% 
  arrange(desc(number_recipients))

#how much money originated by school type
clean_debt %>% group_by(school_type) %>% 
  summarize(dollars_orig = sum(dollars_originated)) %>% 
  arrange(desc(dollars_orig))

#graph number of loans by loan type - first have to make a temp table to sum up  
#the number of loans originated

sumlnorig <- clean_debt %>% select(loan_type, num_loans_originated) %>%  
  group_by(loan_type) %>%
  summarize(sum_ln_orig=sum(num_loans_originated))

sumlnorig %>% 
  ggplot(aes(x=loan_type, y=sum_ln_orig)) + 
  geom_bar(stat="identity", fill = "skyblue", color = "black", ) + 
  geom_text(aes(label = signif(sum_ln_orig)), nudge_y = 100000) +
  theme_gray() +
  ggtitle("Distibution of Loan Types") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Types of Loans") +
  ylab("Count of Loans")


#how many recipients have each loan type on the clean debt data set
clean_debt %>% group_by(loan_type) %>% 
  summarize(number_recipients = sum(num_recipients)) %>%
  arrange(desc(number_recipients))

#how much money originated by loan type
clean_debt %>% group_by(loan_type) %>% 
  summarize(dollars_orig = sum(dollars_originated)) %>% 
  arrange(desc(dollars_orig))

#figure out average $ originated by recipient by loan type
clean_debt %>% group_by(loan_type) %>% 
  summarize(avgorig = sum(dollars_originated)/sum(num_recipients)) %>%
  arrange(desc(avgorig))

#removing this section as I want to only look at originations rather than at
#disbursements 
#how much money disbursed by loan type
#clean_debt %>% group_by(loan_type) %>% summarize(dollars_disb = sum(dollars_disbursed))
#how much money disbursed by school type
#clean_debt %>% group_by(school_type) %>% summarize(dollars_disb = sum(dollars_disbursed))

#how many total schools per state - top 10 states
clean_debt %>% count(state) %>% top_n(10, n) %>% arrange(desc(n))

#how many schools by type per state 
clean_debt %>% count(state, school_type)

#top 10 most recipients by state, descending
clean_debt %>% group_by(state) %>% 
  summarize(number_recipients = sum(num_recipients)) %>%
  top_n(10, number_recipients) %>% arrange(desc(number_recipients))

#graph recipients by state desc
clean_debt %>% group_by(state) %>% 
  summarize(number_recipients = sum(num_recipients)) %>% 
  mutate(state = reorder(state, number_recipients)) %>%
  ggplot(aes(state, number_recipients)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 6)) +
  ggtitle("Recipients by State") +
  xlab("")

#figure out sum of $ originated by state - show top 10
clean_debt %>% group_by(state) %>% 
  summarize(dollars_orig = sum(dollars_originated)) %>%
  top_n(10, dollars_orig) %>% arrange(desc(dollars_orig))

#graph originations by state desc
clean_debt %>% group_by(state) %>% 
  summarize(dollars_orig = sum(dollars_originated)) %>% 
  mutate(state = reorder(state, dollars_orig)) %>%
  ggplot(aes(state, dollars_orig)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 6)) +
  ggtitle("Originations by State") +
  xlab("")

#figure out average $ originated by recipient by state. Interesting discovery - 
#if there is a blank state is an international school. This is where I found to 
#filter out the international schools at the beginning so I went back and added
#that at the top
clean_debt %>% group_by(state) %>% 
  summarize(avgorig = sum(dollars_originated)/sum(num_recipients))  %>%
  top_n(10, avgorig) %>% arrange(desc(avgorig))

#graph to show where the states land when you look at number of recipients by 
#average dollars originated. Need to make a temp table first for the calculations
thing <- clean_debt %>% 
  group_by(state) %>% 
  summarize(sumrec = sum(num_recipients), 
            sumorig = sum(dollars_originated),
            avgorig = sum(dollars_originated)/sum(num_recipients))              

thing %>% 
  ggplot(aes(sumrec, avgorig, label = state)) +   
  geom_point() +
  geom_text_repel() + 
  xlab("Number of Recipients") + 
  ylab("Avg Dollars Originated") +
  ggtitle("Avg Dollars Orig by Recipient, by State") 

#figure out most $ originated by school - show top 10
clean_debt %>% group_by(school) %>% 
  summarize(dollars_orig = sum(dollars_originated))  %>%
  top_n(10, dollars_orig) %>% arrange(desc(dollars_orig))

#figure out school with highest avg $ by recipient. Interesting discovery here - 
#these are all medical programs. I googled midwestern and rocky vista too and 
#they are both best known for their medical programs as well. 
clean_debt %>% group_by(school) %>% 
  summarize(avgorig = sum(dollars_originated)/sum(num_recipients))  %>%
  top_n(10, avgorig) %>% arrange(desc(avgorig))





