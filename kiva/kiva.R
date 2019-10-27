kivaloans <- read.csv('kiva_loans.csv',header=T)
head(kivaloans,5)
#kmpiregion <- read.csv('kiva_mpi_region_locations.csv',header=T)
#head(kmpiregion,5)
library(dplyr)
library(ggplot2)
library(googleVis)
library(ggthemes)
library(DT)
library(tidyverse)
library(re)
#test<-left_join(kivaloans,kmpiregion,by='region')
head(test,5)
rm(list=ls())
#kmpiregion%>%group_by(.,country)%>%summarise(.,mean(MPI))
kivaloans%>%mutate(unfunded_amount=loan_amount-funded_amount)

##use for map on tab 1 - FINAL
b<-kivaloans%>%group_by(.,country)%>%summarise(Total.Loan.Balance=round(sum(loan_amount)),
                                               Total.Funded.Loans=round(sum(funded_amount)),
                                               Average.Funded.Loan=round(mean(funded_amount)), 
                                               Average.Loan.Size=round(mean(loan_amount)),
                                               Average.Lender.Count=round(mean(lender_count)),
                                               Average.Term=round(mean(term_in_months)),
                                               MPI=unique(MPI),
                                               Unfunded.Ratio=round(sum(funded_amount)/sum(loan_amount),2),
                                               Time.To.Fund=as.Date(strptime(kivaloans$funded_time, "%Y-%m-%d"))-as.Date(strptime(kivaloans$posted_time, "%Y-%m-%d")))
kivaloans$disbursed_time=as.Date(kivaloans$disbursed_time, format="%Y-%m-%d")
kivaloans$posted_time=as.Date(kivaloans$posted_time,format="%Y-%m-%d")


kivaloans$Time.To.Fund=as.Date(strptime(kivaloans$funded_time, "%Y-%m-%d"))-as.Date(strptime(kivaloans$posted_time, "%Y-%m-%d"))

t<-kivaloans%>%mutate(gender=ifelse(grepl("^male, female",borrower_genders,ignore.case=T)==T |
                                      grepl("^female, male",borrower_genders,ignore.case=T)==T, "mixed",
                                    ifelse(grepl("^female",borrower_genders,ignore.case=T)==T,
                                           "female",
                                           ifelse(grepl("^male",borrower_genders,ignore.case=T),"male","NA"))))%>%
  mutate(.,Time.To.Fund=as.Date(strptime(funded_time, "%Y-%m-%d"))-
                        as.Date(strptime(posted_time, "%Y-%m-%d")))%>%
  group_by(.,country)%>%
  filter(gender!="NA")

library(wesanderson)
##PARTNER COUNT 
d<-kivaloans%>%group_by(.,country,partner_id)%>%summarise(n())%>%summarise(Partner.Count=n())

g<-kivaloans%>%group_by(.,country,gender)%>%summarise(Average.Funded.Loan=round(mean(funded_amount)), 
                                                      Average.Loan.Size=round(mean(loan_amount)),
                                                      Average.Lender.Count=round(mean(lender_count)),
                                                      Average.Term=round(mean(term_in_months)),
                                                      )

g1<-kivaloans%>%group_by(.,country,gender)%>%filter(gender!="NA")

p<-kivaloans%>%filter(.,partner_id!="")%>%group_by(.,country,partner_id)%>%summarise(n())%>%summarise(Partner.Count=n())
  
ggplot(g1,aes(x=loan_amount))+
  geom_density(aes(color=gender))



kivaloans$gender<-ifelse(grepl("^male, female",kivaloans$borrower_genders,ignore.case=T)==T |
                           grepl("^female, male",kivaloans$borrower_genders,ignore.case=T)==T, "mixed",
                         ifelse(grepl("^female",kivaloans$borrower_genders,ignore.case=T)==T,
                                "female",
                                ifelse(grepl("^male",kivaloans$borrower_genders,ignore.case=T),"male","NA")))



kivaloans%>%mutate(gender=ifelse(grepl("^male, female",borrower_genders,ignore.case=T)==T |
                                 grepl("^female, male",borrower_genders,ignore.case=T)==T, "mixed",
                               ifelse(grepl("^female",borrower_genders,ignore.case=T)==T,
                                      "female",
                                      ifelse(grepl("^male",borrower_genders,ignore.case=T),"male","NA"))))

xx<-kivaloans%>%filter(gender!="NA")
ggplot(xx,aes(x=lende_count,y=term_in_months,color=gender))+
  geom_point(alpha=0.3)


ggplot(data=xx,aes(x=lender_count,color=gender))+
  geom_histogram(aes(fill=gender),binwidth=6, alpha=0.5,position='dodge')+
  scale_color_manual(values=c("#E69F00","#56B4E9","#999999"))+
  scale_fill_manual(values=c("#E69F00","#56B4E9","#999999"))+
  theme_bw()+xlim(0,50)+
  labs(x="Number of Lenders Per Loan",y="",title="Lending Terms",fill="Gender")+
  theme(plot.title = element_text(hjust = 0.5,size=16))+
  guides(color=F, fill = guide_legend("Gender"))



y<-b%>%filter(country=="India")%>%select(.,Average.Loan.Size)[[1]]
y[[1]]

b$country<-gsub("Democratic Republic of the Congo","DRCongo",b$country)
b$country<-gsub("Lao People's Democratic Republic","Laos",b$country)

b[b$country=="Myanmar (Burma)",]
y=kivaloans[kivaloans$country=="China",]
##draw geochart - FINAL 
G<-gvisGeoChart(b,locationvar = "country",colorvar="MPI",sizevar="Average.Lender.Count",
                options=list(displayMode="Markers",width=600, height=400,title="KIVA Microfinancing Trend",titleTextStyle="{color:'red', fontName:'Courier', fontSize:50}",
                             magnifyingGlass.enable=T,magnifyingGlass.zoomFactor=5,magnifyingGlass="{enable: true, zoomFactor: 7.5}",
                             colors="['#c1e7ff','#abd2ec','#94bed9','#7faac6','#6996b3','#5383a1','#3d708f','#255e7e','#004c6d']"))


##barplot-FINAL 
bar<-kivaloans%>%rename(.,Sector="sector")%>%
  mutate(unfunded_amount=loan_amount-funded_amount)%>%
  group_by(country,Sector)%>%
  summarise(sector_count=n(),
            Total.Loan.Balance=round(sum(loan_amount)),
            Total.Funded.Amount=round(sum(funded_amount)),
            Total.Unfunded.Amount=round(sum(unfunded_amount)))%>%
  top_n(6,Total.Funded.Amount)
bar<-bar[bar$Total.Unfunded.Amount!=0,]


bar2<-kivaloans%>%rename(.,Sector="sector")%>%
  mutate(unfunded_amount=loan_amount-funded_amount)%>%
  group_by(country,Sector)%>%
  summarise(sector_count=n(),
            Total.Loan.Balance=round(sum(loan_amount)),
            Total.Funded.Amount=round(sum(funded_amount)),
            Total.Unfunded.Amount=round(sum(unfunded_amount)))%>%
  top_n(6,Total.Loan.Balance)
bar2

bar2
unique(kivaloans[kivaloans$country=='India',"MPI"])
unique(kivaloans%>%filter(country=="India")%>%select(.,MPI))

library(wesanderson)
ggplot(data=bar[bar$country=="India",],aes(x=reorder(Sector,Total.Unfunded.Amount), y=Total.Unfunded.Amount, fill=Sector))+
  geom_bar(stat="identity")+coord_flip()+scale_fill_brewer(palette = "Blues")+theme_bw()+
  labs(x="Sector", y="Total Unfunded Amount",title="Top Unfunded Sectors")+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
  

ggplot(data=bar2[bar2$country=="India",],aes(x=reorder(Sector,Total.Unfunded.Amount), y=Total.Unfunded.Amount, fill=Sector))+
  geom_bar(stat="identity")+coord_flip()+scale_fill_brewer(palette = "Blues")+theme_bw()+
  labs(x="Sector", y="Total Unfunded Amount",title="Top Unfunded Sectors")+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
##currency
formatCurrency(10000,currency = "$", interval = 3, mark = ",")
paste0("$", formatC(10000, format="f", digits=0, big.mark=","))
plot(G)







kivaloans%>%group_by(.,country)
k <-ggplot(data=kivaloans,aes(x=MPI,y=loan_amount))+geom_point(aes(color=borrower_genders, alpha=0.5))
kivaloans%>%group_by(., country=='India',sector)%>%summarise(.,count=n())
k

a<-kivaloans[kivaloans$country=='India',]%>%group_by(.,sector)%>%summarise(.,loan_theme=n())%>%
  mutate(.,loan_theme_per=loan_theme/sum(loan_theme))%>%ungroup()%>%arrange(desc(loan_theme_per))%>%
  mutate(Sectors=factor(sector, levels = as.character(sector)))
a
ggplot(data=a,aes(x="",y=loan_theme_per, fill=Sectors))+
  geom_bar(width = 0.5, stat = "identity", color = "white")+
  coord_polar(theta="y")+theme_void()+
  labs(title="Loan Breakdown",x="",y="")+
  theme(plot.title = element_text(hjust = 0.5))+geom_col()+
  geom_text(aes(label = scales::percent(round(loan_theme_per,3))), position = position_stack(vjust = 0.5))
#plot(gvisPieChart(a,labelvar = "Sectors",numvar="loan_theme_per"))


##barplot of loanbreakdown
c<-kivaloans%>%rename(.,Sector="sector")%>%group_by(country,Sector)%>%summarise(sector_count=n())%>%
  mutate(ratio=round(sector_count/sum(sector_count),4))%>%top_n(6,ratio)
c1<-filter(c,country=="India")%>%ungroup()
  
  
c2<-add_row(c1, Sector="Other",ratio=1-sum(c1$ratio))
c2

##barplot
bar<-kivaloans%>%rename(.,Sector="sector")%>%
  mutate(unfunded_amount=loan_amount-funded_amount)%>%
  group_by(country,Sector)%>%
  summarise(sector_count=n(),
            Total.Loan.Balance=round(sum(loan_amount)),
            Total.Funded.Amount=round(sum(funded_amount)),
            Total.Unfunded.Amount=round(sum(unfunded_amount)))%>%
  top_n(6,Total.Unfunded.Amount)

bar$country[[bar$country=="Albania"]]

unique(filter(bar,country!="Albania")%>%select(.,country))

ggplot(data=bar[bar$country=="India"],aes(x=Sector, y=ratio))+
  geom_bar(stat="identity")+coord_flip()+scale_fill_brewer(name="Sector",palette = "GnBu")

t<-grepl("male", select(kivaloans,borrower_genders),ignore.case=T)
tz
grepl("male",kivaloans$borrower_genders,ignore.case=T)

t
kivaloans$gender<-ifelse(grepl("^male, female",kivaloans$borrower_genders,ignore.case=T)==T |
                           grepl("^female, male",kivaloans$borrower_genders,ignore.case=T)==T, "mixed",
                         ifelse(grepl("^female",kivaloans$borrower_genders,ignore.case=T)==T,
                                "female","male"))

t<-

class(kivaloans$borrower_gender)
grepl("male","I i am here")
ggplot(data=c1,aes(x=1, y=ratio, fill = reorder(Sector,ratio)))+
  geom_bar(width = 0.4, stat = "identity", color = "grey50")+
  coord_polar(theta="y")+theme_void()+scale_fill_brewer(name="Sector",palette = "GnBu", direction = 1)+
  labs(title="Loan Sector Breakdown",x="",y="")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=scales::percent(round(ratio,2))),size=3,position = position_stack(vjust = 0.5))



  ggplot(c1,aes(x=Sector,y=ratio))+geom_bar(stat='identity')+coord_flip()+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = 'Set2') +
  theme_bw() +
  theme(legend.key=element_blank())



ggplot(c1, aes(x = 1, weight = ratio, fill = Sector)) +
  geom_bar(width = 1, colour = "grey50") +
  geom_text(x = 1.3, aes(y = ratio, label = paste0(ratio, "%"), color = Sector)) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Oranges", direction = -1, guide_legend(reverse = TRUE)) +
  scale_color_brewer(palette = "Blues", direction = 1, guide_legend(reverse = TRUE))    +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.caption = element_text(hjust = 0.5)) +
  labs(fill = "", colour = "", 
       caption = "TBD") +
  ggtitle("Loan Sector Breakdown")
##use for map on tab 1
b<-kivaloans%>%group_by(.,country)%>%summarise(Total.Loan.Balance=round(sum(loan_amount)),Total.Funded.Loans=round(sum(funded_amount)),
                                               Average.Funded.Loan=round(mean(funded_amount)), Average.Loan.Size=round(mean(loan_amount)),
                                               Average.Lender.Count=round(mean(lender_count)),Average.Term=round(mean(term_in_months)),
                                              MPI=unique(MPI))


b$country<-gsub("Democratic Republic of the Congo","DRCongo",b$country)
b$country<-gsub("Lao People's Democratic Republic","Laos",b$country)

b[b$country=="Myanmar (Burma)",]
y=kivaloans[kivaloans$country=="China",]

as.character(y$id)

ggplot(kivaloans[kivaloans$country=="China",],aes(x=loan_amount,y=term_in_months))+geom_smooth(method='lm')

Bubble <- gvisBubbleChart(kivaloans, idvar="country", 
                          xvar="lender_count", yvar="term_in_months",sizevar="loan_amount"
                          options=list(
                            hAxis='{minValue:75, maxValue:125}'))
plot(Bubble)
G<-gvisGeoChart(b,locationvar = "country",colorvar="MPI",sizevar="Average.Lender.Count",
                options=list(displayMode="Markers",width=600, height=400,title="KIVA Microfinancing Trend",titleTextStyle="{color:'red', fontName:'Courier', fontSize:50}",
                             magnifyingGlass.enable=T,magnifyingGlass.zoomFactor=5,magnifyingGlass="{enable: true, zoomFactor: 7.5}",
                             colors="['#c1e7ff','#abd2ec','#94bed9','#7faac6','#6996b3','#5383a1','#3d708f','#255e7e','#004c6d']"))

formatCurrency(10000,currency = "$", interval = 3, mark = ",")
paste0("$", formatC(10000, format="f", digits=0, big.mark=","))
plot(G)
#"#004c6d","#255e7e","#3d708f","#5383a1","#6996b3","#7faac6","#94bed9","#abd2ec","#c1e7ff"]"
#'#004c6d','#255e7e','#3d708f','#5383a1','#6996b3','#7faac6','#94bed9','#abd2ec','#c1e7ff'
#'
#'
#'
bar1<-kivaloans%>%rename(.,Sector="sector")%>%
mutate(unfunded_amount=loan_amount-funded_amount)%>%
  group_by(country,Sector)%>%
  summarise(sector_count=n(),
            Total.Loan.Balance=round(sum(loan_amount)),
            Total.Funded.Amount=round(sum(funded_amount)),
            Total.Unfunded.Amount=round(sum(unfunded_amount)))
rm(list=ls())

#DELETED
output$loanbreakdown <- renderPlot({
  c1<-filter(c,country==input$country1)%>%ungroup()
  c2<-add_row(c1, Sector="Other",ratio=1-sum(c1$ratio))
  ggplot(c2,aes(x=1, y=ratio, fill = reorder(Sector,ratio)))+
    geom_bar(width = 0.4, stat = "identity", color = "grey50")+
    coord_polar(theta="y")+theme_void()+scale_fill_brewer(name="Sector",palette = "GnBu", direction = 1)+
    labs(title="Loan Sector Breakdown",x="",y="")+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_text(aes(label=scales::percent(round(ratio,2))),size=3,position = position_stack(vjust = 0.5))
})