# Lending-Club-Project

#Loading libraries

library(ggplot2)
library(dplyr)
library(reshape)
library(ggthemes)
library(RColorBrewer)
library(maps)
library(lattice)
library(plotly)
library(readxl)
library(DescTools)
library(googleVis)
library(magrittr)
library(choroplethr)
library(choroplethrMaps)
library(ggpubr)

#******************  Importing the dataset  ******************/

lc_loandata <- read_csv("C:/Users/chaud/Desktop/Snap_lending-club-Project/loan.csv")
View(lc_loandata)

#******************  View the structure of loan data  ********/

#str(lc_loandata)
dim(lc_loandata)
colnames(lc_loandata)

#range of the sanctioned loan amount 
loanamount_range <- range(lc_loandata$loan_amnt)
loanamount_range

# Function to find columns which has 0 values

find_na = function(x)
{
  na_count <-sapply(x, function(y) sum(length(which(is.na(y)))))
  na_count <- data.frame(na_count)
  na_count <- round((na_count/nrow(x))*100,2)
  return(na_count)
}

lc_a = find_na(lc_loandata)
View(lc_a)

#lc_filtered=(Filter(function(x) mean(is.na(x)) <= 0.9, lc_loandata))

#View(lc_filtered)

#**********  Data Dictionary Info *************************************/

#loading the data dictionary
dataDictionary <- read_excel("C:/Users/chaud/Desktop/Snap_lending-club-Project/LCDataDictionary.xlsx")

# fields available in the data dictionary
dd_names <- as.character(na.omit(dataDictionary$LoanStatNew))

# fields available in the loan book
lc_loandata_names <- names(lc_loandata)
lc_loandata_names

# show the fields described in data dictionary but not in the loan book
setdiff(dd_names, lc_loandata_names)


#***************************Initial Analysis***********************/
# 1. Loan Range-output: pic 1

ggplot(lc_loandata,aes(loan_amnt,col="red"))+geom_freqpoly(binwidth=100)


#Desc(lc_loandata$loan_amnt, main = "Loan amount distribution", plotit = TRUE)


# 2.Line plot-output: pic 2

var1 = c(tapply(lc_loandata$funded_amnt, lc_loandata$grade, mean))
x1 = cbind(var1[1],var1[2],var1[3],var1[4],var1[5],var1[6],var1[7])
var2 = c(tapply(lc_loandata$annual_inc, lc_loandata$grade, mean))
x2 = cbind(var2[1],var2[2],var2[3],var2[4],var2[5],var2[6],var2[7])

df = data.frame(Grades = levels(factor(lc_loandata$grade)), 
                Funded_amount= x1[1,], 
                Annual_Income= x2[1,]
)

Line4 <-  gvisLineChart(df, "Grades", c("Funded_amount","Annual_Income"),
                        options=list(gvis.editor="Edit me!", height = 600,
                                     title=""))
plot(Line4)

# 3.Map-output: pic 3

#Converting State Abbreviation into State Name
stateFromLower <-function(x) {
    st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}

#Adding new column with the name "region"
lc_loandata$region<-stateFromLower(lc_loandata$addr_state)

#Mapping out which state has taken maximum loan
state_by_value <-
  lc_loandata %>% group_by(region) %>%
  summarise(value = sum(loan_amnt, na.rm=TRUE))

state_choropleth(state_by_value, title = "Value by State")



# 4. top resons why people were taking loan-output: pic 4
Desc(lc_loandata$purpose, main = "Loan purposes", plotit = TRUE)


# 5. Creating box plot for the various loan statuses-output: pic 5

box_status = ggplot(lc_loandata, aes(loan_status, loan_amnt))
box_status + geom_boxplot(aes(fill = loan_status)) +
  theme(axis.text.x = element_blank()) +
  labs(list(title = "Loan amount by status",x = "Status", y = "Amount"))

# 6. Group status-output: pic 6
good_status = "Fully Paid"
on_going_status = c("Current","Issued")
#Creating three segments of loan statuses
lc_loandata$status_group = ifelse(lc_loandata$loan_status %in% good_status,"Recovered/Good Status",
                           ifelse(lc_loandata$loan_status %in% on_going_status,"On Going","Bad Loan"))
lc_loandata$status_group = factor(lc_loandata$status_group)

#View(lc_loandata)
#Creating the percentage and the frequency plots
Desc(lc_loandata$status_group, main = "Status group distribution", plotit = TRUE)


# 7. Grade Distribution-output: pic 7
lc_loandata$grade = factor(lc_loandata$grade)
#Creating the percentage and the frequency plots
Desc(lc_loandata$grade, main = "Grade distribution", plotit = TRUE)



# 8. Creating graph for the value of different grades over years-output: pic 8
  
lc_loandata$issue_d = as.Date(gsub("^","01-",lc_loandata$issue_d), format="%d-%b-%Y")#Converting the date into required format
head(lc_loandata$issue_d)

#Calculating the sum of load amounts for various years and for various grades
amnt_df_grade = lc_loandata %>% select(issue_d,loan_amnt,grade) %>% group_by(issue_d, grade) %>% summarise(Amount = sum(loan_amnt))

ts_amnt_grade = ggplot(amnt_df_grade,aes(x = issue_d, y = Amount))
ts_amnt_grade + geom_area(aes(fill=grade)) + xlab("Date issued")

#9. Interest rate per grade-output- pic 9

box_plane = ggplot(lc_loandata, aes(grade,int_rate))
box_plane + geom_boxplot(aes(fill = grade)) +
  labs(title = "Interest rate by grade",
       x = "Grade",
       y = "Interest rate")

# 10. emp_length (Employment length in years) of the borrower and the loan amount - any correlation

Unival1 <- unique(lc_loandata$emp_length)
Unival1

Unival2 <- unique(lc_loandata$home_ownership)
Unival2


#outout-10.1
Desc(lc_loandata$emp_length, main = "Employment Length", plotit = TRUE)

#output-10.2-No correlation
ggscatter(lc_loandata, x = "emp_length", y = "loan_amnt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Employment Length", ylab = "Loan Amount")

#output 10.3-No correlation
ggscatter(lc_loandata, x = "home_ownership", y = "loan_amnt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "home_ownership", ylab = "Loan Amount")

#output 10.4-correlation
ggscatter(lc_loandata, x = "annual_inc", y = "loan_amnt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "annual_inc", ylab = "Loan Amount")




