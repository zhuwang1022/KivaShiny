library(dplyr)
library(ggplot2)
library(googleVis)
library(ggthemes)
library(shinydashboard)
library(DT)


#loan csv file 
kivaloans <- read.csv('kiva_loans.csv',header=T)

#group kiva file by country and summarize for selected columns. 

#kivaloans$unfunded_ammount <- kivaloans$loan_amount-kivaloans$funded_amount
#data frame for the map 

##df for map 
b<-kivaloans%>%mutate(unfunded_amount=loan_amount-funded_amount)%>%
  group_by(country)%>%
  summarise(Total.Loan.Balance=sum(loan_amount),
            Total.Funded.Loans=sum(funded_amount),
            Total.Unfunded.Amount=sum(unfunded_amount), 
            Avg.Funded.Amount=round(mean(funded_amount)), 
            Avg.Loan.Amount=round(mean(loan_amount)),
            MPI=round(unique(MPI),3),
            Unfunded.Ratio=round(sum(funded_amount)/sum(loan_amount),2),
            Lender.Count=round(mean(lender_count)),
            Avg.Term=round(mean(term_in_months))
            )
b$country<-gsub("Lao People's Democratic Republic","Laos",b$country)
choice1 <- colnames(b)[2:6]
choice4 <- colnames(b)[7:9]


c<-kivaloans%>%rename(.,Sector="sector")%>%group_by(country,Sector)%>%summarise(sector_count=n())%>%
  mutate(ratio=round(sector_count/sum(sector_count),4))%>%top_n(6,ratio)
choice2 <- c$country
#df for sector analysis 
bar1<-kivaloans%>%rename(.,Sector="sector")%>%
  mutate(unfunded_amount=loan_amount-funded_amount)%>%
  group_by(country,Sector)%>%
  summarise(sector_count=n(),
            Total.Loan.Balance=round(sum(loan_amount)),
            Total.Funded.Amount=round(sum(funded_amount)),
            Total.Unfunded.Amount=round(sum(unfunded_amount)))
choice3 <- bar1$country

#df for gender study 
g<-kivaloans%>%mutate(gender=ifelse(grepl("^male, female",borrower_genders,ignore.case=T)==T |
                                   grepl("^female, male",borrower_genders,ignore.case=T)==T, "mixed",
                                 ifelse(grepl("^female",borrower_genders,ignore.case=T)==T,
                                        "female",
                                        ifelse(grepl("^male",borrower_genders,ignore.case=T),"male","NA"))))%>%
  group_by(.,country,gender)%>%
  mutate(unfunded_amount=loan_amount-funded_amount)%>%
  filter(gender!="NA")


#partner count 
p<-kivaloans%>%filter(.,partner_id!="")%>%group_by(.,country,partner_id)%>%summarise(n())%>%summarise(Partner.Count=n())
choice5 <-p$country
#time to fund 
t<-kivaloans%>%mutate(.,Time.To.Fund=as.Date(strptime(funded_time, "%Y-%m-%d"))-
                        as.Date(strptime(posted_time, "%Y-%m-%d")))%>%
  group_by(.,country)%>%summarise(Fund.Period=mean(Time.To.Fund))
#choice5 <-t$country

