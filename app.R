#Sharif Amlani
#R 4.1.1
#Summer 2022

######################## Code Summary ##################

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################

######################### Library #####################
library(ggplot2)
library(dplyr)
library(shiny)
library(shinysky)
library(scales)
######################## Upload Data ##################

#Set Working Directory

#Upload Data
WW.Pure <- read.csv("https://data.ca.gov/dataset/b8c6ee3b-539d-4d62-8fa2-c7cd17c16656/resource/16bb2698-c243-4b66-a6e8-4861ee66f8bf/download/master-covid-public.csv"); WW.1 <- WW.Pure

######################## Examine Data ##################
head(WW.1)

table(WW.1$FACILITY.NAME)

######################## Data Management ##################
#Date
WW.1$sample_collect_date_2 <- as.Date(sapply(strsplit(WW.1$sample_collect_date, " "), `[`, 1), format="%m/%d/%Y")

#Key indicators
WW.1$pcr_target_avg_conc <- as.numeric(WW.1$pcr_target_avg_conc)

WW.1$hum_frac_mic_conc <- as.numeric(WW.1$hum_frac_mic_conc)

#Normalize data
WW.1$PMM_oV_norm_conc <- as.numeric(WW.1$pcr_target_avg_conc) / as.numeric(WW.1$hum_frac_mic_conc)

#Fix PCR Target data
WW.1$pcr_gene_target <- stringr::str_to_upper(WW.1$pcr_gene_target)


#################### 7 Day Average #################
WW.2 <- WW.1 %>%  dplyr::arrange(sample_collect_date_2) %>% dplyr::group_by(FACILITY.NAME, pcr_gene_target, pcr_target_units) %>% 
  dplyr::mutate(pcr_target_avg_conc_m1 = lag(pcr_target_avg_conc),
                pcr_target_avg_conc_m2 = lag(pcr_target_avg_conc, n= 2), 
                pcr_target_avg_conc_m3 = lag(pcr_target_avg_conc, n= 3), 
                pcr_target_avg_conc_m4 = lag(pcr_target_avg_conc, n= 4), 
                pcr_target_avg_conc_m5 = lag(pcr_target_avg_conc, n= 5), 
                pcr_target_avg_conc_m6 = lag(pcr_target_avg_conc, n= 6), 
                pcr_target_avg_conc_m7 = lag(pcr_target_avg_conc, n= 7), 
                pcr_target_avg_conc_m8 = lag(pcr_target_avg_conc, n= 8), 
                pcr_target_avg_conc_m9 = lag(pcr_target_avg_conc, n= 9),
                pcr_target_avg_conc_m10 = lag(pcr_target_avg_conc, n= 10), 
                pcr_target_avg_conc_m11 = lag(pcr_target_avg_conc, n= 11),
                pcr_target_avg_conc_m12 = lag(pcr_target_avg_conc, n= 12),
                pcr_target_avg_conc_m13 = lag(pcr_target_avg_conc, n= 13)) %>%
  dplyr::mutate(PMM_oV_norm_conc_m1 = lag(PMM_oV_norm_conc),
                PMM_oV_norm_conc_m2 = lag(PMM_oV_norm_conc, n= 2), 
                PMM_oV_norm_conc_m3 = lag(PMM_oV_norm_conc, n= 3), 
                PMM_oV_norm_conc_m4 = lag(PMM_oV_norm_conc, n= 4), 
                PMM_oV_norm_conc_m5 = lag(PMM_oV_norm_conc, n= 5), 
                PMM_oV_norm_conc_m6 = lag(PMM_oV_norm_conc, n= 6), 
                PMM_oV_norm_conc_m7 = lag(PMM_oV_norm_conc, n= 7), 
                PMM_oV_norm_conc_m8 = lag(PMM_oV_norm_conc, n= 8), 
                PMM_oV_norm_conc_m9 = lag(PMM_oV_norm_conc, n= 9),
                PMM_oV_norm_conc_m10 = lag(PMM_oV_norm_conc, n= 10), 
                PMM_oV_norm_conc_m11 = lag(PMM_oV_norm_conc, n= 11),
                PMM_oV_norm_conc_m12 = lag(PMM_oV_norm_conc, n= 12),
                PMM_oV_norm_conc_m13 = lag(PMM_oV_norm_conc, n= 13))

#************* One Week Average ****************
One_Week <- c("PMM_oV_norm_conc", grep("PMM_oV_norm_conc_m", colnames(WW.2), value = T)[2:7])
WW.2$PMM_oV_norm_conc_7_Day <- rowMeans(WW.2[One_Week], na.rm = F)

One_Week <- c("pcr_target_avg_conc", grep("pcr_target_avg_conc_m", colnames(WW.2), value = T)[2:7])
WW.2$pcr_target_avg_conc_7_Day <- rowMeans(WW.2[One_Week], na.rm = F)


######################## User Interface ##################

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("COVID-19 Wastewater Concentrations In California"),
  
  sidebarPanel(
  selectInput("Facility", "Select Facility", choices = sort(unique(WW.2$FACILITY.NAME))),
  
  ),
  
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot"),
    
    h3("How Endemic is COVID-19?", align = "left"),
    
    
    p("If we look at ", 
      a("The New York Times'", href = "https://www.nytimes.com/interactive/2021/us/covid-cases.html"),
    "COVID-19 Dashboard it feels like reporting COVID-19 cases does not appear to be as smooth as in the past. There are several reasons for this: (1) increase in at-home testing
    means that fewer cases are reported to county health officals and (2) pandemic fatigue as caused people to given up on testing even when/if they are feeling ill.
    These challenges means that reported cases do not fully capture how much COVID-19 is spreading in the United States."),
      
    
    h3("Wastewater Surveillance", align = "left"),
    
    p("Wastewater surveillance is extremely important for early detection of COVID-19 and is the best indicator of disease transmission. According to the",
      
    a(" CDC",  href = "https://www.cdc.gov/healthywater/surveillance/wastewater-surveillance/wastewater-surveillance.html"), 
    ", People infected with SARS-CoV-2 can shed the virus in their feces, even if they don't have symptoms.
    With the increase in at-home detection and politicization of COVID-19, case numbers reported to county public health officials may underestimate 
    the true number of cases in the community. As another ",
    
    a("article",  href = "https://www.sacbee.com/news/coronavirus/article263044553.html"), 
    
    " reports, The sewage never lies. Therefore, sewage water will be our best measure of community spread moving forward."),
    
    br(),
    
    p("Examining the concentration of COVID-19 in wastewater is a powerful tool to accuralty mreasure risk and assess how quikly COVID-19 is spreading through the community."),
    
    h3("Data and Code", align = "left"),
    
    p("This shiny app used data from the California Department of Public Health's Wastewater Surveillance Program. Please see",
      a("CDPH",  href = "https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/COVID-19/Wastewater-Surveillance.aspx"), 
      
    "for more information on the data I used to build his app. Please visit my",
    a("GitHub",  href = "http://shiny.rstudio.com"), 
    
    "for the code I used to build this app.")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$distPlot <- renderPlot({
      
      WW.3 <- subset(WW.2, FACILITY.NAME == input$Facility)
      
      ggplot(WW.3, aes(x = sample_collect_date_2, y = pcr_target_avg_conc, color = pcr_gene_target)) +
        facet_grid(pcr_target_units ~ ., scales = "free_y") +
        geom_point() +
        geom_line(aes(x = sample_collect_date_2, y = pcr_target_avg_conc_7_Day, color = pcr_gene_target, alpha = 0.7), size=1)+
        scale_x_date(date_labels = "%b-%Y", date_breaks = "3 month") +
        scale_y_continuous(labels = label_number(accuracy = .1, suffix = " M", scale = 1e-6)) + # millions
        labs(y = "Wastewater Concentration",
             title = "COVID-19 Concentrations",
             color = "COVID-19 Gene Target",
             subtitle = unique(WW.3$FACILITY.NAME),
             caption = "Source: Sewange Coronavrius Alert Network (SCAN)\nNote: line shows a seven-day average.") +
        theme_minimal() +
        guides(alpha = "none") +
        theme(axis.title.x = element_blank(),
              plot.title = element_text(hjust = 0.5),
              legend.position = "bottom",
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption= element_text(hjust = 0))
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
