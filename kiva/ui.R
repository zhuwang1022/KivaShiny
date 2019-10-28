library(dplyr)
library(ggplot2)
library(googleVis)
library(ggthemes)
library(shinydashboard)
library(DT)

shinyUI(dashboardPage(skin="blue",
  dashboardHeader(title='Kiva Dashboard'),
  dashboardSidebar(
    sidebarUserPanel("Zhu Wang"),
    sidebarMenu(
      menuItem('Overview',tabName='intro',icon=icon('dot-circle')),
      menuItem('Where Kiva Works?',tabName='map',icon=icon('drafting-compass')),  #same tab name as the daboard body
      menuItem('Loan Sectors',tabName='loansectors',icon=icon('globe')),
      menuItem('Gender Analysis', tabName='gender',icon=icon('users')),
      menuItem('Data', tabName='data',icon=icon('table')),
      menuItem('Glossary',tabName='glossary',icon=icon('pencil-alt'))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = 'intro',
              fluidRow(
                box(HTML('Kiva is a non-profit crowdsourcing microfinance platform, providing banking and 
                financial services to underserved communities around the world.
                Loans are also offered to borrowers who aim to create social impact 
                in their communities. Lenders may crowdfund the loan in increments of $25 or more.<br><br> 
              This Shiny app is built based on a dataset provided by Kiva for a Kaggle competition. 
              Information included is limited to loans posted during early 2013 to mid 2017.<br><br>
              The Global Multidimensional Poverty Index (MPI) published by the United Nations Human Development Program 
              is an additional metric incorporated to the Kiva dataset. MPI uses healthy, education, and standard 
              of living indicators to determine the degree of poverty experienced by a population.<br><br>
              The purpose of this Shiny app is for users who want to learn about the landscape of microfinance through Kiva 
              and users those who would like to consider lending for the first time. This app explores various metrics such as general 
              use of loans, loan balance, funded and unfunded amount, lender counts, etc. on a country by country 
              basis, as well as on a global basis.<br><br>
              Please refer to the below data sources for this app:<br><br>
              <a href="https://www.kaggle.com/kiva/data-science-for-good-kiva-crowdfunding">(1) Kiva Dataset</a><br><br>
              <a href="http://hdr.undp.org/en/2019-MPI">(2) United Nations MPI Data</a>
            '), title = 'Overview of Kiva', solidHeader = TRUE,
                    status = 'primary', width = 12)
              )
      ),
      tabItem(tabName = "map",
              fluidRow(infoBoxOutput("maxBox"),
                       infoBoxOutput("minBox"),
                       infoBoxOutput("avgBox")),
              fluidRow(column(6,selectizeInput('selected','Select Metric to Display',
                             choice1)),
              column(6,selectizeInput('metrics','Select Metric for Color Intensity',choice4))),
              fluidRow(htmlOutput("map")
                       )),
      
      tabItem(tabName='loansectors',
              fluidRow(valueBoxOutput('mpi1Box',width=2),
                       valueBoxOutput('avgloan1Box',width=2),
                       valueBoxOutput('fund1Box',width=2),
                       valueBoxOutput('mpi2Box',width=2),
                       valueBoxOutput('avgloan2Box',width=2),
                       valueBoxOutput('fund2Box',width=2)),
              fluidRow(column(6,selectizeInput('country1','Select Country 1 to Display',
                                            choices = choice3)),
                       column(6,selectizeInput('country2','Select Country 2 to Display',
                                            choices = choice3))
              ),
              fluidRow(
                tabBox(
                  tabPanel('Loan Amt', plotOutput('loansectors3')),
                  tabPanel('Funded Amt',plotOutput('loansectors5')),
                  tabPanel('Unfunded Amt', plotOutput('loansectors1'))
                  ),
                tabBox(
                  tabPanel('Loan Amt',plotOutput('loansectors4')),
                  tabPanel('Funded Amt',plotOutput('loansectors6')),
                  tabPanel('Unfunded Amt', plotOutput('loansectors2'))

                ))
              ),
      tabItem(tabName='gender',
              fluidRow(
                tabBox(tabPanel('Loan Amt',plotOutput('gender1'))),
                tabBox(
                  tabPanel('Lending Term',plotOutput('gender2')),
                  tabPanel('Lender Count',plotOutput('gender3'))
                                ),

                fluidRow(column(4,sliderInput("slider1", label = "Choose Range", min = 0, 
                            max = 50000, value = c(2000, 10000))),
                column(4,sliderInput("slider3", label="Choose Frequency",min=0,
                            max=0.001,value=c(0,0.0002))),
                column(4,sliderInput("slider2", label="Choose Binwidth",min=0,
                            max=10000,value=1000)))
              )),
      tabItem(tabName = "data",
              fluidRow(box(DT::dataTableOutput("table"), width = 12))),
      tabItem(tabName = 'glossary',
              fluidRow(
                box(HTML('Loan Amount - The amount disbursed by the field agent to the borrower(USD)<br><br>
                Funded Amount - The amount disbursed by Kiva to the field agent(USD)<br><br>
                Unfunded Amount - The amount to be sourced via crowdfunding(USD)<br><br>
                Term in Months - The duration for which the loan was disbursed in months<br><br>
                Lender Count -The total number of lenders that contributed to this loan<br><br>
                MPI - The Multidimensional Poverty Index identifies multiple deprivations at 
                the household and individual level in health, education and standard of living. 
                A larger number indicates a higher level of poverty.
            '), title = 'Glossary of Terms', solidHeader = TRUE,
                    status = 'primary', width = 12)
              )
      )
    )
  )
))
