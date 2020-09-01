

library(shiny)
#library(foreign)
library(plyr)
library(dplyr)
library(seplyr)
library(tidyr)
library(readxl)
library(magrittr)
library(rvest)
library(maps)
library(ggplot2)
library(reshape2)
library(ggiraph)
library(RColorBrewer)
library(leaflet)
library(plotly)
library(geojsonio)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(openxlsx)
library(rapportools)
library(DT)
library(lazyeval)
library(rlang)
library(geojsonlint)
library(purrr)
library(cowplot)
library(shinycssloaders)
library(sp)
library(spdplyr)




mycurrency <- function(x){
    return(paste("$", formatC(as.numeric(x), format="f", digits=0, big.mark=",")))
}

mycurrency2 <- function(x){
  return(paste("$", formatC(as.numeric(x), format="f", digits=2, big.mark=",")))
}

bignumber <- function(x){
  return(formatC(as.numeric(x), format="f", digits=0, big.mark=","))
}

countries_df = read.csv("input_data/countries_codes_and_coordinates.csv") %>%
  select(-population) %>% mutate(country = as.character(country))

worldcountry = geojson_read("input_data/50m.geojson", what = "sp") %>% filter(CONTINENT == "Africa") %>% dplyr::arrange(NAME)
country_geoms = read.csv("input_data/country_geoms.csv")

population_data <- read.xlsx("population_data.xlsx")%>%
  gather(year, population, 2:11) %>%
  mutate(year = as.numeric(year))%>%
  arrange(country) 

llin_data <- read.xlsx("LLIN_data.xlsx")%>%
  gather(year, llin, 2:10) %>%
  mutate(year = as.numeric(year))%>%
  arrange(country) 

budget_data <- read_xlsx("pmi_budgets.xlsx", col_names = TRUE) %>%
    mutate(Total_malaria_budget = as.numeric(Total_malaria_budget),
           ITN_budget = as.numeric(ITN_budget),
           Procurement = as.numeric(Procurement)) %>% merge(population_data, by = c("country", "year"))


combined_df <- read.xlsx("combined_df.xlsx") %>%
  merge(population_data, by = c("country", "year")) %>%
  mutate(ITN_budget_capita = round(as.numeric(ITN_budget/population), 4),
         Total_budget_capita = round(as.numeric(Total_malaria_budget/population), 4),
         year = as.numeric(year))

df_dhs <- read.xlsx("df_dhs.xlsx") %>% mutate(year = as.numeric(year), access_summary = access, hh_use = perc_sleep_itn, hh_ownership = perc_at_least_itn) %>%  
  group_by(country, year) %>%
  gather(5:22, key = "variable", value = "percentage") %>%
  arrange(country) %>%
  filter(!is.na(percentage))%>%
  mutate(
    group = case_when(variable %in% c("hh_ownership","access_summary","hh_use", "max_possible_sleep_itn") ~ "Summary",
                      variable %in% c("perc_at_least_net","perc_at_least_itn", "perc_net_2_pers", "perc_itn_2_pers") ~ "Net ownership",
                      variable == "access" ~ "Net access within household",
                      variable %in% c("perc_sleep_net","perc_sleep_itn", "perc_sleep_own_itn") ~ "Net use by households",
                      variable %in% c("child_sleep_net", "child_sleep_itn", "child_sleep_own_itn") ~ "Net use by children < 5",
                      variable %in% c("pregnant_sleep_net", "pregnant_sleep_itn", "pregnant_sleep_own_itn")~ "Net use by pregnant women"),
    description = case_when(variable == "perc_at_least_net"~"HH with at least one mosquito net (%)",  
                            variable == "perc_at_least_itn" ~"HH with at least one ITN (%)",
                            variable == "hh_use" ~ "HH use of ITNs (%)",
                            variable == "access_summary" ~ "HH access to ITNs (%)",
                            variable == "max_possible_sleep_itn" ~ "Maximum theoretical use (2 people per net)",
                            variable == "perc_net_2_pers" ~ "HH with at least one net \n for every 2 people who stayed the night (%)",
                            variable == "perc_itn_2_pers" ~ "HH with at least one ITN \n for every 2 people who stayed the night (%)",
                            variable == "access" ~ "Percentage of the population with access \n to an ITN within their own household",
                            variable == "perc_sleep_net" ~"Percentage of the HH members who slept \n under an evertreated net the night before the survey",
                            variable == "perc_sleep_itn" ~  "Percentage of the HH members who slept \n under an ITN the night before the survey",
                            variable == "perc_sleep_own_itn" ~ "Among the household population in households with at least one ITN, \n the % who slept under an ITN the night before the survey.", 
                            variable == "child_sleep_net" ~ "Percentage of children < 5 who slept \n under an evertreated net the night before the survey", 
                            variable == "child_sleep_itn" ~ "Percentage of children < 5 who slept \n under an ITN the night before the survey",
                            variable == "child_sleep_own_itn" ~ "Among children < 5 in households with at least one ITN, \n the % who slept under an ITN the night before the survey", 
                            variable == "pregnant_sleep_net" ~ "Percentage of pregnant women who slept \n under an evertreated net last night", 
                            variable == "pregnant_sleep_itn" ~ "Percentage of pregnant women who slept \n under an ITN the night before the survey",
                            variable == "pregnant_sleep_own_itn" ~ "Among pregnant women in households with at least one ITN, \n the % who slept under an ITN the night before the survey."
)
  )

cases_df <- read.xlsx("cases.xlsx")%>%
  gather(year, cases, 2:10) %>%
  arrange(country) %>% 
  mutate(cases = as.numeric(cases),
         year = as.numeric(year))%>%
  merge(population_data, by = c("country", "year")) %>% drop_na()

cases_df_large <- 
  #cases_df %>% inner_join(countries_df, by="country")%>%
  merge(cases_df, countries_df, by= "country") %>%
  mutate(cases1M = round(as.numeric(cases/1000000), 2),
         cases_percent = round(as.numeric(cases/population), 4)) %>%
  filter(country != "South Sudan")

llin_df_large <- merge(llin_data, countries_df, by = "country") %>%
  filter(continent_level == "Africa", country != "South Sudan")

budget_data_large <- 
  #budget_data %>% full_join(countries_df)%>%
  merge(budget_data, countries_df, by="country") %>%
    mutate(ITN_budget1M = round(as.numeric(ITN_budget/1000000),1),
           Total_budget1M = round(as.numeric(Total_malaria_budget/1000000), 1),
           ITN_budget_capita = round(as.numeric(ITN_budget/population), 4),
           Total_budget_capita = round(as.numeric(Total_malaria_budget/population), 4))

countries_list <- unique(budget_data_large$country)

#------------------------------------
variable <- c("hh_ownership", "access_summary","hh_use","max_possible_sleep_itn",
              "perc_at_least_net","perc_at_least_itn","perc_net_2_pers", "perc_itn_2_pers",
              "access",
              "perc_sleep_net","perc_sleep_itn", "perc_sleep_own_itn",
              "child_sleep_net", "child_sleep_itn", "child_sleep_own_itn", "pregnant_sleep_net", "pregnant_sleep_itn", "pregnant_sleep_own_itn")

label <- c("HH with at least one ITN (%)", "HH population with access to ITNs (%)", "HH population to use ITNs (%)","Maximum theoretical use (2 people per netm %)",
           "HH with at least one mosquito net (%)", "HH with at least one ITN (%)", "HH with at least one net \n for every 2 people (%) who stayed the night", "HH with at least one ITN \n for every 2 people (%) who stayed the night",
           "Percentage of the population with access \n to an ITN within their own household",
           "Percentage of the HH members who slept under \n an evertreated net the night before the survey", "Percentage of the HH members who slept \n under an ITN the night before the survey",
           "Among the household population in households with \n at least one ITN, the percentage who slept \n under an ITN the night before the survey.", 
           "Percentage of children < 5 who slept under \n an evertreated net the night before the survey", "Percentage of children < 5 who slept \n under an ITN the night before the survey",
           "Among children < 5 in households with \n at least one ITN, the percentage who slept \n under an ITN the night before the survey", 
           "Percentage of pregnant women who slept \n under an evertreated net last night", "Percentage of pregnant women who slept \n under an ITN the night before the survey",
           "Among pregnant women in households with \n at least one ITN, the percentage who slept \n under an ITN the night before the survey."
           )

group <- c("Summary", "Summary", "Summary","Summary",
           "Net ownership", "Net ownership", "Net ownership", "Net ownership",
           "Net access within household",
           "Net use by households", "Net use by households","Net use by households",
           "Net use by children < 5", "Net use by children < 5","Net use by children < 5",
           "Net use by pregnant women", "Net use by pregnant women", "Net use by pregnant women")

labels_df <- data.frame(variable, label, group)

budget <- c("ITN_budget", "Total_malaria_budget")
label_budget <- c("ITN Budget", "Total Malaria Budget")
budget_labels <- data.frame(budget, label_budget)

df_construction <- function(selected_country, selected_group){
  df_dhs %>%
        dplyr::filter(country == selected_country,
                      group == selected_group) %>%
        select(country, year, FieldworkStart, FieldworkEnd, variable, percentage, group)
}

double_plotting <- function(selected_country, selected_group, selected_budget){

    lab <- labels_df %>% filter(group == selected_group)%>% mutate(variable = as.character(variable)) %>%
      select(variable, label)
    
    lab_budget <- budget_labels %>% filter(budget == selected_budget) %>% select(label_budget)
    
    df1 <- df_construction(selected_country, selected_group) %>%
        full_join(lab, by = "variable")%>% arrange( desc(percentage)) %>% arrange(year) 
    df1$variable  <- factor(df1$variable, levels = unique(df1$variable))
    
    df2 <- combined_df %>%
        filter(country == selected_country)%>%
        mutate(lab = lab_budget[1,1]) %>%
        select(country, year, all_of(selected_budget), lab) %>%
        distinct()
    
    df3 <- cases_df_large %>%
      filter(country == selected_country) %>%
      mutate(lab = "Malaria cases % \n of population") %>%
      select(country, year, cases_percent, lab)
    
    if(selected_group == "Net ownership"){
      brewer_palette <- "OrRd"
      
    } else if(selected_group == "Net access within household"){
      brewer_palette <- "BuPu"
      
    } else if(selected_group == "Net use by households"){
      brewer_palette <- "Greens"
      
    } else if(selected_group == "Net use by children < 5"){
      brewer_palette <- "Reds"
      
    }else if(selected_group == "Summary"){
      brewer_palette <- "Blues"
      
    } else {
      brewer_palette <- "Purples"
    }
    
    fixplotly <-function(i.plotly,i.labels){
      
      nlabels <- length(i.labels)
      nlists <- length(i.plotly$x$data)
     
      # Show all labels
      for (i in 1:nlists) i.plotly$x$data[[i]]$showlegend <- T
      
      # Fix labels names
      sequ <- 1:nlists-nlabels*(floor((1:nlists-1)/nlabels))
      for (i in 1:nlists) i.plotly$x$data[[i]]$name<-i.labels[sequ[i]]
     
      return(i.plotly)
    }
    
    if( all(!is.na(df1$percentage))){
        p1 <- ggplot(data = df1, aes(x = year, y = percentage, text = paste("Percentage:", round(percentage*100, 2), "% \n", "Field work start: ", FieldworkStart, "\n","Field work end: ", FieldworkEnd), 
                                      color = variable,
                                      group = 1)) +
            geom_line(size=1) + geom_point()+ 
            scale_x_continuous(limits = c(2010, 2019), breaks = seq(2010, 2019, 1))+
            scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1))+
            facet_wrap(~group)+
            #scale_color_brewer(name = "", labels = df1$label, palette = brewer_palette)+ 
          #scale_colour_manual(values = colorRampPalette(brewer.pal(9, brewer_palette), bias = 1)(length(unique(df1$variable)))) +
          scale_colour_manual(values = brewer.pal(9,brewer_palette)[round(seq(from = 3, to = 9, length.out = length(unique(df1$variable))))] )+
          
            labs(fill = "")+ 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                  panel.background = element_blank(), axis.line = element_line(colour = "black"),
                  text = element_text(size = 10), axis.text.x = element_text(angle = 45),  
                  axis.text.y = element_text(size = 8), legend.title = element_blank(),
                  plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
                  strip.text = element_text(size = 12, face = "bold", hjust = 0.5), plot.margin = unit(c(2, 2, 2, 2), "cm"))
        
        ply1_initial <- ggplotly(p1, tooltip = c("text"), showlegend = FALSE)
        ply1 <- fixplotly(ply1_initial, unique(df1$label))
        
    } else {
        new_lab <- labels_df %>% filter(group == selected_group) %>% select(group)
        
        p1 <- ggplot(data = df2,  aes(x = year, y = !!rlang::sym(as.name(selected_budget)),
                                      group = 1)) +
            geom_blank()+ 
            scale_x_continuous(limits = c(2010, 2019), breaks = seq(2010, 2019, 1))+
            scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1))+
            facet_wrap(~new_lab[1,1])+
            labs(fill = "")+ 
            annotate("text", x = 2015, y = 0.7, label = "There is no data for this country.", size = 3, color = "gray45")+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                  panel.background = element_blank(), axis.line = element_line(colour = "black"),
                  text = element_text(size = 10), axis.text.x = element_text(angle = 45), 
                  axis.text.y = element_text(size = 8),
                  plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background = element_rect(fill="white"),
                  strip.text = element_text(size = 12, face = "bold", hjust = 0.5), plot.margin = unit(c(2, 2, 2, 2), "cm"))
        
        ply1 <- ggplotly(p1, showlegend = TRUE)
    }

    if(selected_budget %in% c("ITN_budget_capita", "Total_budget_capita")){
      p2 <- ggplot(data = df2, aes(x = year, y = !!rlang::sym(as.name(selected_budget)), 
                                   text = paste(lab_budget[1,1], "(PMI)", !!rlang::sym(as.name(selected_budget))," %","\n","Year: ", 
                                   year), group = 1, color = "Budget")) +
          geom_line( size = 1)+ geom_point()+ scale_x_continuous(limits = c(2010, 2019), breaks = seq(2010, 2019, 1))+ scale_color_manual(values = "#C2A5CF")+
          guides(fill = FALSE)+
          facet_wrap(~lab)+
          scale_y_continuous(limits = c(0, 1.3), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2), labels = mycurrency2(c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2)))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour = "black"),
                text = element_text(size = 10), axis.text.x = element_text(angle = 45),  
                axis.text.y = element_text(size = 8), legend.title = element_blank(),
                plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
                strip.text = element_text(size = 12, face = "bold", hjust = 0.5), plot.margin = unit(c(2, 2, 2, 2), "cm"))
        
        
        ply2 <- ggplotly(p2, tooltip = c("text"), showlegend = TRUE)
        
    } else {
      p2 <- ggplot(data = df2, aes(x = year, y = !!rlang::sym(as.name(selected_budget)), 
                                   text = paste(lab_budget[1,1], "(PMI)", mycurrency(!!rlang::sym(as.name(selected_budget))),"\n","Year: ", 
                                                year), group = 1, color = "Budget")) +
        geom_line( size = 1)+ geom_point()+ scale_x_continuous(limits = c(2010, 2019), breaks = seq(2010, 2019, 1))+ scale_color_manual(values = "#C2A5CF")+
        guides(fill = FALSE)+
        facet_wrap(~lab)+
        scale_y_continuous(limits = c(0, max(df2[,3])+1000000), breaks = c(0, 5000000, 10000000, 15000000, 20000000, max(df2[,3])+1000000),
                           labels = mycurrency(c(0,5000000, 10000000, 15000000, 20000000, max(df2[,3])+1000000)))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  
              axis.text.y = element_text(size = 8), legend.title = element_blank(),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5), plot.margin = unit(c(2, 2, 2, 2), "cm"))
      
      
      ply2 <- ggplotly(p2, tooltip = c("text"), showlegend = TRUE)
    }
    
    
    p3 <- ggplot(data = df3, aes(x = year, y = cases_percent, group = 1, text = paste("Malaria cases percentage: ", cases_percent*100, "%", "\n","Year: ", year),
                                  color = "Cases Percentage")) +
      geom_line(size = 1)+ geom_point()+ scale_x_continuous(limits = c(2010, 2019), breaks = seq(2010, 2019, 1))+ scale_color_manual(values = "#4A1486")+
      guides(fill = FALSE)+
      facet_wrap(~lab)+
      scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            text = element_text(size = 10), axis.text.x = element_text(angle = 45),  
            axis.text.y = element_text(size = 8), legend.title = element_blank(),
            plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
            strip.text = element_text(size = 12, face = "bold", hjust = 0.5), plot.margin = unit(c(2, 2, 2, 2), "cm"))
    
    ply3 <- ggplotly(p3, tooltip = c("text"), showlegend = TRUE)
    
    plot <- plotly::subplot(ply1, ply2, ply3, margin = 0.09, titleX = TRUE, titleY = TRUE) %>%
        layout(showlegend = TRUE, showlegend2 = TRUE, showlegend3 = TRUE, xaxis = list(title = "Year", titlefont = list(size = 12)),
               xaxis2 = list(title="Year", titlefont = list(size = 12)), xaxis3 = list(title="Year", titlefont = list(size = 12)),
               yaxis = list(title = "", titlefont = list(size = 12)), yaxis2 = list(title = ""), title = selected_country)
    
    print(plot)
    
}


# create plotting parameters for map
bins = c(0,1, 5, 10, 20, 30, 40, Inf)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% budget_data_large$alpha3, ]
#-------------------------------------------------------
# creat cv base map 
basemap <- leaflet(worldcountry, options = leafletOptions(minZoom = 3.5,
                                                      maxZoom = 10)) %>% 
    addTiles() %>%
    addProviderTiles("CartoDB.Voyager") %>%
    fitBounds(~-100,-50,~80,100) %>%
    setView(lng = 21, lat = 7, zoom = 2) 

# Define UI for application that draws a histogram
narrowSidebar <- HTML('<style>.span4 {min-width: 265px; max-width: 265px; }</style>')
#wideMainpanel <- HTML('<style>.span8 {min-width: 365px; max-width: 365px; }</style>')


### SHINY UI ###
ui <- bootstrapPage(
    tags$head(includeHTML("gtag.html")),
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               "Malaria tracker", id="nav",
               
               tabPanel("Malaria Map",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            leafletOutput("mymap", width="100%", height="100%"),
                            
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 150, left = 20, width = 250, fixed=TRUE,
                                          draggable = TRUE, height = "auto",

                                          tags$i(h6("Msc in Economics Master thesis, HEC Lausanne")),
                                          tags$i(h6("Author: Inés Guardans")),
                                         
                                          selectInput(inputId = "variable_plot",
                                                      label = "Variable of interest",
                                                      choices = c("Total Malaria Budget", "Total Malaria Budget per capita", 
                                                                  "ITN Budget", "ITN Budget per capita","Malaria Cases", "Percentage of Malaria Cases", "LLINs delivered")
                                                      ),
                                          
                                          uiOutput("country"),
                                          
                                          uiOutput("year_ui")
                  
                            )
                            
                            # absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                            #               tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
                            # 
                            # absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                            #               actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                            #                            onclick = sprintf("window.open('%s')",
                            #                                              "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
                        )
               ),
               
               tabPanel("Country Plots",
                        
                        sidebarLayout(
                            sidebarPanel(
                              tags$head(narrowSidebar), width = 3,
                              style = "position: fixed; height: 90vh; overflow-y: auto;",
                             
                              
                              pickerInput("budget_select", "Budget:",
                                          choices = label_budget,
                                          selected = c("ITN Budget"),
                                          multiple = TRUE,
                                          options = list(`selected-text-format`= "count")),
                                
                                pickerInput("country_select", "Countries:",
                                            choices = sort(unique(combined_df$country)),
                                            selected = "Angola",
                                            multiple = TRUE,
                                            options = list(`selected-text-format`= "count")),

                                pickerInput("group_select", "Variables:",
                                            choices = unique(group),
                                            selected = c("Summary"),
                                            multiple = TRUE,
                                            options = list(`selected-text-format`= "count")),

                                "Select country and variables of interest."
                            ),
                            
                            mainPanel(
                              #tags$head(wideMainpanel), 
                              width = 9,
                                h3("Country plots of use and ITN budget"),
                                br(),
                                uiOutput("plots") %>% withSpinner(color="#0dc5c1")
                            )
                        )
               ),
               

               tabPanel("Data",
                        downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                        
                        dataTableOutput("country_data")
               ),
               
               tabPanel("About this site",
                        tags$div(
                            tags$h4("Project description"), 
                            
                            "This platform is presented as a visual support to my master thesis to quantify and visualize the main parameters of bet net use, access and ownership analysed in malaria control surveys. To do so, I use 
                            data from Demographic and Health Surveys for the last ten years and for a wide set of Sub-Saharan African countries. It also includes some contextual variables like malaria funding or malaria incidence.",tags$br(),
                            
                            tags$br(),tags$h4("Background"), 
                            "I am a student in the MsC Economics master at the University of Lausanne." ,tags$br(),tags$br(),
                            
                            "This thesis has been written in the context of my internship at Vestergaard Sàrl. Vestergaard is a leading player in the malaria community. 
                            It contributes to the malaria eradication efforts by selling insecticide treated bed nets.
                            In addition, as part of their business development, I have been involved in the ", tags$a(href="https://www.ivizard.org/live-permanet/", "SmartNet Initiative."), 
                            "The project consists in using mobile technology and real-time data to have more accurate information to tackle the disease.",tags$br(),
                            "One of our goals is to understand how current field surveys assess the main variables of interest regarding net ownership, 
                            access and use, so that we can gain some insights and eventually improve the net distribution efficiency and usage through digital surveys.",tags$br(),
                            
                            tags$br(),tags$br(),tags$h4("Code"),
                            "Code and input data used to generate this Shiny mapping tool will be available on ",tags$a(href="https://github.com/inesguardans/Malaria_Tracker", "Github."),tags$br(),
                            "I have followed the formulas stipulated in the", tags$a(href = "https://dhsprogram.com/data/Guide-to-DHS-Statistics/", "Guide to DHS Statistics."),
                            
                            tags$br(),tags$br(),tags$h4("Sources"),
                            tags$b("WHO: "), tags$a(href="https://www.who.int/publications-detail/world-malaria-report-2019", "World malaria report 2019"),tags$br(),
                            tags$b("Funding: "), tags$a(href="https://www.pmi.gov/resource-library/mops", "PMI Malaria Operational Plans"),tags$br(),
                            tags$b("Cases: "),  tags$a(href="https://www.who.int/publications-detail/world-malaria-report-2019", "World malaria report 2019 Annex H"),tags$br(),
                            tags$b("Malaria control data: "),  tags$a(href="https://dhsprogram.com/data/available-datasets.cfm", "Demographic and Health Surveys"),tags$br(),
                            
                            
                            tags$br(),tags$br(),tags$h4("Author"),
                            "Inés Guardans, HEC Lausanne",tags$br(),

                            tags$br(),tags$br(),tags$h4("Contact"),
                            "ines.guardansgonzalez@unil.ch",tags$br(),tags$br(),
                            tags$img(src = "logo_unil.png", width = "160px", height = "75px"),tags$br(), tags$br(),
                            tags$img(src = "vestergaard_logo.png", width = "180px", height = "75px")
                            
                        )
               )
               
    )          
)


### SHINY SERVER ###

server = function(input, output, session) {
    
  
  country_list <- reactive({
    if(input$variable_plot %in% c("ITN Budget", "Total Malaria Budget", "Total Malaria Budget per capita", "ITN Budget per capita")){
      list <- sort(unique(combined_df$country))
    }
    
    else if(input$variable_plot %in%  c("Malaria Cases", "Percentage of Malaria Cases")){
      list <- unique(cases_df_large$country)
    }
    
    else if(input$variable_plot == "LLINs delivered") {
      list <- unique(llin_df_large$country)
    }
    return(list)
  })
  
  
  output$country <- renderUI(
    multiInput(
      inputId = "country",
      label = "Countries",
      choices = NULL,
      #selected = "Select all",
      choiceNames = country_list(),
      choiceValues = country_list(),
      options = list(
        enable_search = TRUE,
        non_selected_header = "Choose countries:",
        selected_header = "You have selected:"
      ),
      width = "110%")
  )
  
  reactive_year <- reactive({
    if(input$variable_plot %in% c("ITN Budget", "Total Malaria Budget", "Total Malaria Budget per capita", "ITN Budget per capita")){
      year_vector <- seq(2010, 2019, 1)
    }
    
    else if(input$variable_plot %in%  c("Malaria Cases", "Percentage of Malaria Cases")){
      year_vector <- seq(2010, 2018, 1)
    }
    
    else if(input$variable_plot == "LLINs delivered"){
      year_vector <- seq(2011, 2019, 1)
    }
    return(year_vector)
  })
  
  output$year_ui <- renderUI({
    selectInput("year", "Select a year", choices = reactive_year(), selected = max(reactive_year()))
  })
  
    # Data
    reactive_data <- reactive({

      if(input$variable_plot %in% c("ITN Budget", "Total Malaria Budget", "Total Malaria Budget per capita", "ITN Budget per capita")){
        
        # Country
        if (!is.null(input$country)){
          chosen_country <- input$country}
        
        else {
          chosen_country <- unique(budget_data_large$country)
        }
        
        #Year
        if (!is.null(input$year)){
          chosen_year <- input$year
        }
        
        else {
          chosen_year <- "2019"
        }
        
        filtered_data <- budget_data_large %>%
          dplyr::filter(country %in% chosen_country,
                        year == chosen_year)
      }
      
      else if(input$variable_plot %in%  c("Malaria Cases", "Percentage of Malaria Cases")){
        # Country
        if (!is.null(input$country)){
          chosen_country <- input$country}
        
        else {
          chosen_country <- unique(cases_df_large$country)
        }
        
        # Year
        
        if (!is.null(input$year)){
          chosen_year <- input$year
        }
        
        else {
          chosen_year <- "2018"
        }
        
        filtered_data <- cases_df_large %>%
          dplyr::filter(country %in% chosen_country,
                        year == chosen_year)%>% arrange(country)
      }
      
      else if(input$variable_plot ==  "LLINs delivered"){
        # Country
        if (!is.null(input$country)){
          chosen_country <- input$country}
        
        else {
          chosen_country <- unique(llin_df_large$country)
        }
        
        #Year
      
        
        if (!is.null(input$year)){
          chosen_year <- input$year
        }
        
        else {
          chosen_year <- "2019"
        }
        
        filtered_data <- llin_df_large %>%
          dplyr::filter(country %in% chosen_country,
                        year == chosen_year)
      }
        
        return(filtered_data)
    })

    # Data with countries
    reactive_data_large <- reactive({
        large_countries = reactive_data() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
        worldcountry_subset = worldcountry[worldcountry$ADM0_A3 %in% large_countries$alpha3, ]
        large_countries = large_countries[match(worldcountry_subset$ADM0_A3, large_countries$alpha3),]
        large_countries
    })

    reactive_polygons = reactive({
        worldcountry[worldcountry$ADM0_A3 %in% reactive_data_large()$alpha3, ] %>% arrange(NAME)
    })

    # Colors
    cv_pal <- reactive({
      if(input$variable_plot == "ITN Budget"){
        cv_pal <- colorBin("Oranges", domain = reactive_data_large()$ITN_budget1M, bins = bins)
        

      }
      else if(input$variable_plot == "ITN Budget per capita"){
        cv_pal <- colorBin(palette = "Oranges", domain = reactive_data_large()$ITN_budget_capita, bins = c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 1, Inf))

      }

      else if(input$variable_plot == "Total Malaria Budget"){
        cv_pal <- colorBin(palette = "Purples", domain = reactive_data_large()$Total_budget1M, bins = bins)
      }

      else if(input$variable_plot == "Total Malaria Budget per capita"){
        cv_pal <- colorBin("Purples", domain = reactive_data_large()$Total_budget_capita, bins = c(0, 0.2, 0.4, 0.6, 0.8, 1, 3, 5))
      }

      else if(input$variable_plot == "Malaria Cases"){
        cv_pal <- colorBin("Blues", domain = reactive_data()$cases1M, bins = c(0, 0.1, 0.5, 1, 2, 5, 10, 15, Inf))
      }

      else if(input$variable_plot == "Percentage of Malaria Cases"){
        cv_pal <- colorBin("Blues", domain = reactive_data()$cases_percent, bins = c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 1))

      }

      else if(input$variable_plot == "LLINs delivered"){
        cv_pal <- colorBin("Greens", domain = reactive_data()$llin, bins = c(0,1000, 50000, 100000, 500000, 1000000, 5000000, 10000000, Inf))

      }
      return(cv_pal)
    })

    
    # Variable to plot
    variable_plot <- reactive({
      if(input$variable_plot == "ITN Budget"){
        choice <- reactive_data_large()$ITN_budget1M
      }
      else if(input$variable_plot == "ITN Budget per capita"){
        choice <- reactive_data_large()$ITN_budget_capita
      }
      
      else if(input$variable_plot == "Total Malaria Budget"){
        choice <- reactive_data_large()$Total_budget1M
      }
      
      else if(input$variable_plot == "Total Malaria Budget per capita"){
        choice <- reactive_data_large()$Total_budget_capita
      }
      
      else if(input$variable_plot == "Malaria Cases"){
        choice <- reactive_data()$cases1M
      }
      
      else if(input$variable_plot == "Percentage of Malaria Cases"){
        choice <- reactive_data()$cases_percent
      }
      
      else if(input$variable_plot == "LLINs delivered"){
        choice <- reactive_data()$llin
      }
      
      return(choice)
    })
    
    
    #Legend title
    legend_title <- reactive({
      if(input$variable_plot %in% c("ITN Budget", "ITN Budget per capita")){
        title <- "PMI ITN Budget"
      }
      
      else if(input$variable_plot %in% c("Total Malaria Budget", "Total Malaria Budget per capita")){
        title <- "PMI Total Malaria Budget"
      }
      
      else if(input$variable_plot == "Malaria Cases"){
        title <- "Malaria Cases (in Millions)"
      }
      
      else if(input$variable_plot == "Percentage of Malaria Cases"){
        title <- "Percentage of Malaria Cases"
      }
      
      else if(input$variable_plot == "LLINs delivered"){
        title <- "Number of LLINs delivered"
      }
      
      return(title)
    })

    output$mymap <- renderLeaflet({

        basemap %>% addLegend("bottomright", pal = cv_pal(), values = ~variable_plot(),
                              title = paste0("<small>", legend_title()), opacity = 0.7)
      
      #basemap

    })

    popup <- reactive({
      if(input$variable_plot %in% c("ITN Budget", "Total Malaria Budget")){
        popup_text <-  paste0("<strong>Country: </strong>",
                         reactive_data()$country,
                         "<br><strong> Year: </strong>",
                         reactive_data()$year,
                         "<br><strong> ITN budget: </strong>",
                         mycurrency(reactive_data()$ITN_budget),
                         "<br><strong>Total malaria budget: </strong>",
                         mycurrency(reactive_data()$Total_malaria_budget))
      }
      
      else if(input$variable_plot %in% c("ITN Budget per capita", "Total Malaria Budget per capita")){
        popup_text <-  paste0("<strong>Country: </strong>",
                              reactive_data()$country,
                              "<br><strong> Year: </strong>",
                              reactive_data()$year,
                              "<br><strong> ITN budget per capita: </strong>",
                              mycurrency(reactive_data()$ITN_budget_capita),
                              "<br><strong>Total malaria budget per capita: </strong>",
                              mycurrency(reactive_data()$Total_budget_capita))
      }
      
      else if(input$variable_plot == "Malaria Cases"){
        popup_text <-  paste0("<strong>Country: </strong>",
                         reactive_data()$country,
                         "<br><strong> Year: </strong>",
                         reactive_data()$year,
                         "<br><strong> Presumed and confirmed cases: </strong>",
                         bignumber(reactive_data()$cases))
      }
      
      else if(input$variable_plot == "Percentage of Malaria Cases"){
        popup_text <-  paste0("<strong>Country: </strong>",
                              reactive_data()$country,
                              "<br><strong> Year: </strong>",
                              reactive_data()$year,
                              "<br><strong> Presumed and confirmed cases: </strong>",
                              mycurrency(reactive_data()$cases),
                              "<br><strong> Percentage of cases: </strong>",
                              paste0(reactive_data()$cases_percent*100, "%"))
      }
      
      else if(input$variable_plot == "LLINs delivered"){
        popup_text <-  paste0("<strong>Country: </strong>",
                              reactive_data()$country,
                              "<br><strong> Year: </strong>",
                              reactive_data()$year,
                              "<br><strong> Number of LLINs delivered: </strong>",
                              bignumber(reactive_data()$llin))
      }
    
      return(popup_text)
    })
    
    text_col <- reactive({

      if(input$variable_plot %in% c("ITN Budget", "ITN Budget per capita")){
        color = "#FD8D3C"
      }

      else if(input$variable_plot %in% c("Total Malaria Budget", "Total Malaria Budget per capita")){
        color = "#4A1486"
      }

      else if(input$variable_plot == "Malaria Cases"){
        color <- "#3690C0"
      }

      else if(input$variable_plot == "Percentage of Malaria Cases"){
        color <- "#3690C0"
      }
      
      else if(input$variable_plot == "LLINs delivered"){
        color <- "#41AB5D"
      }

      return(color)

    })

    observe({
        
        leafletProxy("mymap") %>%
            clearMarkers() %>%
            clearShapes() %>%
            addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.7, fillColor = ~cv_pal()(variable_plot()),
                        label = popup() %>% lapply(htmltools::HTML), labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px", "color" = text_col()),
                                textsize = "15px", direction = "auto"))
      # %>% 
      #     addLegend("bottomright", pal = pal, values = choice, title = paste0("<small>", title))

    })
    
    #************************************************************************************
    # Correlation plots tab
    
    # Use variable
    selected_group <- reactive({

        filtered <- labels_df %>% filter(group %in% input$group_select)
        
        selected_group <- as.character(unique(filtered$group))
        return(selected_group)
        })
    
    # Budget variable
    selected_budget <- reactive({
      
      filtered <- budget_labels %>% filter(label_budget %in% input$budget_select)
      
      budget <- as.character(filtered[,1])
      return(budget)
    })
    
    double_plots_list <- reactive({
        list <- list()
        
        for(i in input$country_select){
            for (j in selected_group()){
              for(h in selected_budget()){
                plot <- double_plotting(i, j, h)
                pltName <- paste("a",i,j,h, sep="")
                
                list[[pltName]] <-  plot
              }
                
            }
        }
       return(list)
    })

   output$plots <- renderUI({
       req(double_plots_list())

       plots_list <- purrr::imap(double_plots_list(), ~{
           tagList(
               plotlyOutput(
                   outputId = paste0("plot_", .y)
               ),
               br()
           )
       })
       
       tagList(plots_list)

   })
   
   observe({
       if(!is.null(input$country_select)){
           purrr::iwalk(double_plots_list(), ~{
               output_name <- paste0("plot_", .y)
               output[[output_name]] <- renderPlotly(.x)
           })   
       }
   })

   

    
    output$country_data <- 
        DT::renderDataTable({
        df_dhs <- df_dhs %>%
          mutate(percentage = round(percentage, 4)) %>%
          select(country, year, FieldworkStart, FieldworkEnd, group, description, percentage)
        
        datatable(df_dhs, 
                  rownames = FALSE,
                  options = list(
                      autoWidth = TRUE,
                      #columnDefs = list(list(width = "250px", targets = c(1,2,3)), list(className = "dt-center", targets = c(2,3))),
                      pageLength = 25, info = TRUE, lengthMenu = 30,
                      paging = TRUE,
                      searching = TRUE,
                      lengthChange = TRUE,
                      ordering = TRUE,
                      scrollX = TRUE
                  ))
        
        })
    
    # dhs_reactive <- reactive({
    #   
    # })
    # # output to download data
    output$downloadCsv <- downloadHandler(
      filename = function() {
        paste("Country_data", ".csv", sep="")
      },
      content = function(file) {
        write.csv(df_dhs , file)
      }
    )
}

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")