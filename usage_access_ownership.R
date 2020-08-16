library(foreign)
library(dplyr)
library(seplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(lazyeval)
library(rlang)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(zoo)
library(rdhs)
library(stargazer)


mycurrency <- function(x){
  return(paste("$", formatC(as.numeric(x), format="f", digits=0, big.mark=",")))
}


df_HR <- read_xlsx("df_HR.xlsx")
df_PR <- read_xlsx("df_PR.xlsx")

budget_data <- read_xlsx("pmi_budgets.xlsx", col_names = TRUE) %>%
  mutate(Total_malaria_budget = as.numeric(Total_malaria_budget),
         ITN_budget = as.numeric(ITN_budget),
         Procurement = as.numeric(Procurement),
         year = as.character(year))

# countries_df = read.csv("input_data/countries_codes_and_coordinates.csv")
# 
# countries_df <- countries_df %>%
#   select(country, alpha3)
# 
# 
# budget_data_large <- merge(budget_data, countries_df, by="country")%>%
#   mutate(ITN_budget100k = round(as.numeric(ITN_budget/100000)),0)

countries <- c("Angola", "Angola", 
               "Benin","Benin", 
               "Burkina Faso", "Burkina Faso", "Burkina Faso",
               "Ghana","Ghana", "Ghana", 
               "Guinea","Guinea", 
               "Kenya", "Kenya",
               "Liberia", "Liberia","Liberia",
               "Madagascar","Madagascar","Madagascar",
               "Malawi", "Malawi","Malawi","Malawi","Malawi",
               "Mali", "Mali","Mali","Mali", 
               "Mozambique","Mozambique", "Mozambique", 
               "Nigeria", "Nigeria","Nigeria","Nigeria","Rwanda", "Rwanda","Rwanda","Rwanda",
               "Senegal","Senegal","Senegal", "Senegal","Senegal", "Senegal","Senegal",
               "Tanzania", "Tanzania","Tanzania","Tanzania",
               "Uganda", "Uganda","Uganda", "Uganda",
               "Zambia", "Zambia","Zimbabwe", "Zimbabwe")

year <- c(2011, 2015, #angola
          2012, 2017, #benin
          2010, 2014, 2017, #Burkina faso
          2014, 2016, 2019, #ghana
          2012, 2018, #guinea
          2014, 2015, #kenya
          2011, 2013, 2016, #liberia
          2011, 2013, 2016, #madagascar
          2010, 2012, 2014, 2015, 2017,  #malawi
          2010, 2012, 2015, 2018, #mali
          2011, 2015, 2018, #mozambique
          2010, 2013, 2015, 2018, #nigeria
          2010, 2013, 2015, 2017, #Rwanda
          2010, 2012, 2014, 2015, 2016, 2017, 2018, #Senegal
          2010, 2012, 2015, 2017, #tanzania
          2011, 2014, 2016, 2018,  #Uganda
          2013, 2018, #zambia
          2010, 2015)#zimbabwe


paths <- c("Data_DHS/Angola/AOPR62FL.dta", "Data_DHS/Angola/AOPR71FL.dta", 
           "Data_DHS/Benin/BJPR61FL.dta","Data_DHS/Benin/BJPR71FL.dta", 
           "Data_DHS/Burkina_Faso/BFPR62FL.dta", "Data_DHS/Burkina_Faso/BFPR71FL.dta", "Data_DHS/Burkina_Faso/BFPR7AFL.dta", 
           "Data_DHS/Ghana/GHPR72FL.dta","Data_DHS/Ghana/GHPR7BFL.dta", "Data_DHS/Ghana/GHPR82FL.dta",
           "Data_DHS/Guinea/GNPR62FL.dta","Data_DHS/Guinea/GNPR71FL.dta",
           "Data_DHS/Kenya/KEPR72FL.dta", "Data_DHS/Kenya/KEPR7AFL.dta",
           "Data_DHS/Liberia/LBPR71FL.dta","Data_DHS/Liberia/LBPR6AFL.dta", "Data_DHS/Liberia/LBPR61FL.dta", 
           "Data_DHS/Madagascar/MDPR61FL.dta","Data_DHS/Madagascar/MDPR6AFL.dta", "Data_DHS/Madagascar/MDPR71FL.dta",
           "Data_DHS/Malawi/MWPR61FL.dta",
           "Data_DHS/Malawi/MWPR6AFL.dta","Data_DHS/Malawi/MWPR71FL.dta","Data_DHS/Malawi/MWPR7AFL.dta","Data_DHS/Malawi/MWPR7IFL.dta",
           "Data_DHS/Mali/MLPR61FL.dta","Data_DHS/Mali/MLPR6AFL.dta","Data_DHS/Mali/MLPR72FL.dta","Data_DHS/Mali/MLPR7AFL.dta",
           "Data_DHS/Mozambique/MZPR62FL.dta","Data_DHS/Mozambique/MZPR71FL.dta","Data_DHS/Mozambique/MZPR7AFL.dta",
           "Data_DHS/Nigeria/NGPR61FL.dta","Data_DHS/Nigeria/NGPR6AFL.dta","Data_DHS/Nigeria/NGPR71FL.dta",
           "Data_DHS/Nigeria/NGPR7AFL.dta", 
           "Data_DHS/Rwanda/RWPR61FL.dta","Data_DHS/Rwanda/RWPR6IFL.dta","Data_DHS/Rwanda/RWPR70FL.dta","Data_DHS/Rwanda/RWPR7AFL.dta",
           "Data_DHS/Senegal/SNPR61FL.dta","Data_DHS/Senegal/SNPR6DFL.dta",
           "Data_DHS/Senegal/SNPR70FL.dta","Data_DHS/Senegal/SNPR7HFL.dta","Data_DHS/Senegal/SNPR7IFL.dta","Data_DHS/Senegal/SNPR7ZFL.dta","Data_DHS/Senegal/SNPR80FL.dta",
           "Data_DHS/Tanzania/TZPR63FL.dta","Data_DHS/Tanzania/TZPR6AFL.dta", "Data_DHS/Tanzania/TZPR7BFL.dta","Data_DHS/Tanzania/TZPR7IFL.dta",
           "Data_DHS/Uganda/UGPR61FL.dta","Data_DHS/Uganda/UGPR72FL.dta", "Data_DHS/Uganda/UGPR7BFL.dta", "Data_DHS/Uganda/UGPR7IFL.dta",
           "Data_DHS/Zambia/ZMPR61FL.dta","Data_DHS/Zambia/ZMPR71FL.dta",
           "Data_DHS/Zimbabwe/ZWPR62FL.dta","Data_DHS/Zimbabwe/ZWPR72FL.dta")

countries_HR <- c("Angola", "Angola", 
               "Benin","Benin", 
               "Burkina Faso", "Burkina Faso", "Burkina Faso",
               "Ghana","Ghana", "Ghana", 
               "Guinea","Guinea", 
               "Kenya", "Kenya",
               "Liberia", "Liberia","Liberia",
               "Madagascar","Madagascar","Madagascar",
               "Malawi", "Malawi","Malawi","Malawi","Malawi",
               "Mali", "Mali","Mali","Mali", 
               "Mozambique","Mozambique", "Mozambique", 
               "Nigeria", "Nigeria","Nigeria","Nigeria","Rwanda", "Rwanda","Rwanda","Rwanda",
               "Tanzania", "Tanzania","Tanzania","Tanzania",
               "Uganda", "Uganda","Uganda", "Uganda",
               "Zambia", "Zambia","Zimbabwe", "Zimbabwe")

year_HR <- c(2011, 2015, #angola
          2012, 2017, #benin
          2010, 2014, 2017, #Burkina faso
          2014, 2016, 2019, #ghana
          2012, 2018, #guinea
          2014, 2015, #kenya
          2011, 2013, 2016, #liberia
          2011, 2013, 2016, #madagascar
          2010, 2012, 2014, 2015, 2017,  #malawi
          2010, 2012, 2015, 2018, #mali
          2011, 2015, 2018, #mozambique
          2010, 2013, 2015, 2018, #nigeria
          2010, 2013, 2015, 2017, #Rwanda
          2010, 2012, 2015, 2017, #tanzania
          2011, 2014, 2016, 2018,  #Uganda
          2013, 2018, #zambia
          2010, 2015)#zimbabwe

paths_HR <- c("Data_DHS/Angola/AOHR62FL.dta", "Data_DHS/Angola/AOHR71FL.dta", 
              "Data_DHS/Benin/BJHR61FL.dta","Data_DHS/Benin/BJHR71FL.dta", 
              "Data_DHS/Burkina_Faso/BFHR62FL.dta", "Data_DHS/Burkina_Faso/BFHR71FL.dta", "Data_DHS/Burkina_Faso/BFHR7AFL.dta", 
              "Data_DHS/Ghana/GHHR72FL.dta","Data_DHS/Ghana/GHHR7BFL.dta","Data_DHS/Ghana/GHHR82FL.dta",
              "Data_DHS/Guinea/GNHR62FL.dta","Data_DHS/Guinea/GNHR71FL.dta",
              "Data_DHS/Kenya/KEHR72FL.dta","Data_DHS/Kenya/KEHR7AFL.dta",
              "Data_DHS/Liberia/LBHR71FL.dta","Data_DHS/Liberia/LBHR6AFL.dta", "Data_DHS/Liberia/LBHR61FL.dta", 
              "Data_DHS/Madagascar/MDHR61FL.dta","Data_DHS/Madagascar/MDHR6AFL.dta", "Data_DHS/Madagascar/MDHR71FL.dta",
              "Data_DHS/Malawi/MWHR61FL.dta",
              "Data_DHS/Malawi/MWHR6AFL.dta","Data_DHS/Malawi/MWHR71FL.dta","Data_DHS/Malawi/MWHR7AFL.dta","Data_DHS/Malawi/MWHR7IFL.dta",
              "Data_DHS/Mali/MLHR61FL.dta","Data_DHS/Mali/MLHR6AFL.dta","Data_DHS/Mali/MLHR72FL.dta","Data_DHS/Mali/MLHR7AFL.dta",
              "Data_DHS/Mozambique/MZHR62FL.dta","Data_DHS/Mozambique/MZHR71FL.dta","Data_DHS/Mozambique/MZHR7AFL.dta",
              "Data_DHS/Nigeria/NGHR61FL.dta","Data_DHS/Nigeria/NGHR6AFL.dta","Data_DHS/Nigeria/NGHR71FL.dta",
              "Data_DHS/Nigeria/NGHR7AFL.dta", 
              "Data_DHS/Rwanda/RWHR61FL.dta","Data_DHS/Rwanda/RWHR6IFL.dta","Data_DHS/Rwanda/RWHR70FL.dta","Data_DHS/Rwanda/RWHR7AFL.dta",
              
              "Data_DHS/Tanzania/TZHR63FL.dta","Data_DHS/Tanzania/TZHR6AFL.dta", "Data_DHS/Tanzania/TZHR7BFL.dta","Data_DHS/Tanzania/TZHR7IFL.dta",
              "Data_DHS/Uganda/UGHR61FL.dta","Data_DHS/Uganda/UGHR72FL.dta", "Data_DHS/Uganda/UGHR7BFL.dta", "Data_DHS/Uganda/UGHR7IFL.dta",
              "Data_DHS/Zambia/ZMHR61FL.dta","Data_DHS/Zambia/ZMHR71FL.dta",
              "Data_DHS/Zimbabwe/ZWHR62FL.dta","Data_DHS/Zimbabwe/ZWHR72FL.dta")

senegal_country <- c("Senegal", "Senegal", "Senegal", "Senegal", "Senegal", "Senegal", "Senegal")

paths_senegal <- c("Data_DHS/Senegal/SNHR61FL.dta","Data_DHS/Senegal/SNHR6DFL.dta",
                   "Data_DHS/Senegal/SNHR70FL.dta","Data_DHS/Senegal/SNHR7HFL.dta",
                   "Data_DHS/Senegal/SNHR7IFL.dta","Data_DHS/Senegal/SNHR7ZFL.dta","Data_DHS/Senegal/SNHR80FL.dta")

years_senegal <- c(2010, 2012, 2014, 2015, 2016, 2017, 2018)

## set up your credentials
set_rdhs_config(email = "ines.guardansgonzalez@unil.ch",
                project = "Master Thesis Malaria country overview",
                config_path = "~/.rdhs.json",
                global = TRUE)


###### PR
cc <- c("AO", "BJ", "BF", "GH", "GN", "KE", "LB", "MD", "MW", "ML", "MZ", "NG", "RW", "SN",
        "TZ", "UG", "ZM", "ZW")# lets find all the surveys that fit our search criteria

survs <- dhs_surveys(countryIds = cc,
                     surveyType = c("DHS", "MIS"),
                     surveyYearStart = 2010) %>% select(SurveyId, CountryName, SurveyYear, FieldworkStart, FieldworkEnd, NumberofHouseholds) %>%
  rename(country = CountryName,
         year = SurveyYear)

create_df <- function(path, Country, Year){
  
  data <- read.dta(path) 
  data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  
  selection <- function(df){
    df %>%
      dplyr::select(hhid, hml1, hv103, hv104, hv005, hml12, hv013, hml16, hml18)
  }
  
  data <- data %>%
    selection() %>%
    mutate(country = Country, 
           year = Year)
  
}

list_PR <- mapply(create_df, paths, countries, year, SIMPLIFY = FALSE)

df_PR <- Reduce(f = "full_join", x = list_PR)

df_PR <- df_PR %>% mutate(year = as.character(year)) %>% full_join(survs, by = c("country", "year")) 

df_PR <- df_PR %>%
  mutate(hv103 = case_when(hv103 == "yes" ~ 1,
                           hv103 == "Yes" ~ 1,
                           hv103 == "no" ~ 0,
                           hv103 == "No" ~ 0,
                           hv103 == 9 ~ NA_real_,
                           TRUE ~ as.numeric(hv103)),
         hv104 = case_when(hv104 == "female" ~ 2,
                           hv104 == "Female" ~ 2,
                           hv104 == "male" ~ 1,
                           hv104 == "Male" ~1,
                           hv104 == 9 ~ NA_real_,
                           TRUE ~ as.numeric(hv104)),
         hml18 = case_when(hml18 == "not pregnant, don't know" ~ 0,
                           hml18 == "Not pregnant, don't know" ~ 0,
                           hml18 == "pregnant" ~ 1,
                           hml18 == "Pregnant" ~ 1,
                           hml18 == "woman not interviewd" ~ 9, 
                           TRUE ~ as.numeric(hml18)),
         hml12 = case_when(hml12 == "both treated (itn) and untreated bednets" ~ 2,
                           hml12 == "both treated (itn) and untreated nets" ~2,
                           hml12 == "Both treated (ITN) and untreated bednets" ~2,
                           hml12 == "Both treated (ITN) and untreated nets" ~2,
                           hml12 == "did not sleep under a net" ~ 0,
                           hml12 == "Did not sleep under a net" ~ 0,
                           hml12 == "no bednet" ~ 0,
                           hml12 == "No bednet" ~ 0,
                           hml12 == "only treated (itn) nets" ~ 1,
                           hml12 == "Only treated (ITN) nets" ~1, 
                           hml12 == "only treated bednets (itn)" ~1,
                           hml12 == "Only treated bednets (ITN)" ~1, 
                           hml12 == "only untreated bednets" ~3,
                           hml12 == "Only untreated bednets" ~3,
                           hml12 == "only untreated nets" ~3,
                           hml12 == "Only untreated nets" ~3,
                           TRUE ~ as.numeric(hml12)),
         hml1 = as.numeric(hml1),
         hv005 = as.numeric(hv005),
         hv013 = as.numeric(hv013),
         hml16 = as.numeric(hml16),
         hhid = trimws(hhid, which = c("both"))
  )
write.xlsx(df_PR, "df_PR.xlsx")

#------------------------------------------------------------
##### return HR
## Function to cbind data frames of different lengths
cbindPad <- function(...){
  args <- list(...)
  n <- sapply(args,nrow)
  mx <- max(n)
  pad <- function(x, mx){
    if (nrow(x) < mx){
      nms <- colnames(x)
      padTemp <- matrix(NA, mx - nrow(x), ncol(x))
      colnames(padTemp) <- nms
      if (ncol(x)==0) {
        return(padTemp)
      } else {
        return(rbind(x,padTemp))
      }
    }
    else{
      return(x)
    }
  }
  rs <- lapply(args,pad,mx)
  return(do.call(cbind,rs))
}


create_df_HR <- function(path, Country, Year){
  data <- read.dta(path) 
  data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  selection1 <- function(df){
    df %>%
      dplyr::select(hhid, hml1, hv013, hv005, hv227)
  }
  
  df1 <- data %>%
    selection1()
  
  selection2 <- function(df){
    mylist <- list()
    
    
    for(i in seq_along(1:7)){
      if(!is.null(df[[paste0("hml10_", i)]])){
        
        mylist[[i]] <- df %>% select(!!rlang::sym(as.name(paste0("hml10_", i))))
      }
    }
    
    mydf <- data.frame(mylist)
    return(mydf)
  }
  df2 <-  selection2(data)
  
  
  data_filtered <- cbindPad(df1, df2) %>%
    mutate(country = Country, 
           year = Year) 
  
  return(data_filtered)
}

create_df_senegal <- function(path, Country, Year){
  data <- read.dta(path) 
  data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  selection1 <- function(df){
    df %>%
      dplyr::select(hhid, hml1, hv013, hv005, hv227)
  }
  
  df1 <- data %>%
    selection1()
  
  selection2 <- function(df){
    mylist <- list()
    
    for(j in 1:9){
      if(!is.null(df[[paste0("hml10_0", j)]])){
        
        mylist[[j]] <- df %>% select(!!rlang::sym(as.name(paste0("hml10_0", j))))
      }
    }
    
    for(i in 1:30){
      if(!is.null(df[[paste0("hml10_", i)]])){
        
        mylist[[i]] <- df %>% select(!!rlang::sym(as.name(paste0("hml10_", i))))
      }
     
    }
    
    mydf <- data.frame(mylist)
    return(mydf)
  }
  df2 <-  selection2(data)
  
  
  data_filtered <- cbindPad(df1, df2) %>%
    mutate(country = Country, 
           year = Year) 
  
  return(data_filtered)
}

list_HR <- mapply(create_df_HR, paths_HR, countries_HR, year_HR, SIMPLIFY = FALSE)
list_senegal <- mapply(create_df_senegal, paths_senegal, senegal_country, years_senegal)

df_senegal <- Reduce(f = "full_join", x = list_senegal) 
df_senegal <- df_senegal %>% mutate(year = as.character(year)) %>% full_join(survs, by = c("country", "year")) 

df_HR <- Reduce(f = "full_join", x = list_HR) 
df_HR <- df_HR %>% mutate(year = as.character(year)) %>% full_join(survs, by = c("country", "year")) 

df_HR <- df_HR %>%
  mutate(hml10_1 = case_when(
                        hml10_1 == "No, don't know" ~ 0,
                        hml10_1 == "Yes" ~ 1,
                        hml10_1 == "no, don't know" ~ 0,
                        hml10_1 == "yes" ~ 1,
                        TRUE ~ as.numeric(hml10_1)),
    hml10_2 = case_when(
                        hml10_2 == "No, don't know" ~ 0,
                        hml10_2 == "Yes" ~ 1,
                        hml10_2 == "no, don't know" ~ 0,
                        hml10_2 == "yes" ~ 1,
                        TRUE ~ as.numeric(hml10_2)),
    hml10_3 = case_when(
                        hml10_3 == "No, don't know" ~ 0,
                        hml10_3 == "Yes" ~ 1,
                        hml10_3 == "no, don't know" ~ 0,
                        hml10_3 == "yes" ~ 1,
                        TRUE ~ as.numeric(hml10_3)),
    hml10_4 = case_when(
                        hml10_4 == "No, don't know" ~ 0,
                        hml10_4 == "Yes" ~ 1,
                        hml10_4 == "no, don't know" ~ 0,
                        hml10_4 == "yes" ~ 1,
                        TRUE ~ as.numeric(hml10_4)),
    hml10_5 = case_when(
                        hml10_5 == "No, don't know" ~ 0,
                        hml10_5 == "Yes" ~ 1,
                        hml10_5 == "no, don't know" ~ 0,
                        hml10_5 == "yes" ~ 1,
                        hml10_5 == "TRUE" ~1,
                        hml10_5 == "FALSE" ~0,
                        TRUE ~ as.numeric(hml10_5)),
    hml10_6 = case_when(
                        hml10_6 == "No, don't know" ~ 0,
                        hml10_6 == "Yes" ~ 1,
                        hml10_6 == "no, don't know" ~ 0,
                        hml10_6 == "yes" ~ 1,
                        hml10_6 == "TRUE" ~1,
                        hml10_6 == "FALSE" ~0,
                        TRUE ~ as.numeric(hml10_6)),
    hml10_7 = case_when(
                        hml10_7 == "No, don't know" ~ 0,
                        hml10_7 == "Yes" ~ 1,
                        hml10_7 == "no, don't know" ~ 0,
                        hml10_7 == "yes" ~ 1,
                        hml10_7 == "TRUE" ~1,
                        hml10_7 == "FALSE" ~0,
                        TRUE ~ as.numeric(hml10_7)),
    hv227 = case_when(hv227 == "Yes" ~ 1,
                      hv227 == "No" ~ 0,
                      hv227 == "yes" ~ 1,
                      hv227 == "no" ~ 0,
                      hv227 == 9 ~ as.numeric(NA),
                      TRUE ~ as.numeric(hv227)),
    hml1 = as.numeric(hml1),
    hv013 = as.numeric(hv013),
    hv005 = as.numeric(hv005),
    hhid = trimws(hhid, which = c("both"))) %>%
  rowwise()%>%
  mutate(itn_total = sum(hml10_1, hml10_2, hml10_3, hml10_4, hml10_5, hml10_6, hml10_7, na.rm = TRUE),
         one_itn = ifelse(itn_total > 0, 1, 0),
         itn_total_double = itn_total*2,
         potential_users = min(hv013,itn_total_double),
         NumberofHouseholds = case_when(country == "Mali" & year == "2010" ~ 1617,
                                        country == "Mozambique" & year == "2015" ~ 7169,
                                        country == "Tanzania" & year == "2012" ~ 100040,
                                        TRUE ~ as.numeric(NumberofHouseholds))) %>% 
  select(country, year, hhid, NumberofHouseholds, FieldworkStart, FieldworkEnd, hv227, hml1, hv013, hv005, itn_total, one_itn, itn_total_double, potential_users)


df_senegal <- df_senegal %>%
  mutate(hml10_1 = case_when(hml10_1 == "No, don't know" ~ 0,
                        hml10_1 == "Yes" ~ 1,
                        hml10_1 == "no, don't know" ~ 0,
                        hml10_1 == "yes" ~ 1,
                        TRUE ~ as.numeric(hml10_1)),
    hml10_2 = case_when(hml10_2 == "No, don't know" ~ 0,
                        hml10_2 == "Yes" ~ 1,
                        hml10_2 == "no, don't know" ~ 0,
                        hml10_2 == "yes" ~ 1,
                        TRUE ~ as.numeric(hml10_2)),
    hml10_3 = case_when(hml10_3 == "No, don't know" ~ 0,
                        hml10_3 == "Yes" ~ 1,
                        hml10_3 == "no, don't know" ~ 0,
                        hml10_3 == "yes" ~ 1,
                        TRUE ~ as.numeric(hml10_3)),
    hml10_4 = case_when(hml10_4 == "No, don't know" ~ 0,
                        hml10_4 == "Yes" ~ 1,
                        hml10_4 == "no, don't know" ~ 0,
                        hml10_4 == "yes" ~ 1,
                        TRUE ~ as.numeric(hml10_4)),
    hml10_5 = case_when(hml10_5 == "No, don't know" ~ 0,
                        hml10_5 == "Yes" ~ 1,
                        hml10_5 == "no, don't know" ~ 0,
                        hml10_5 == "yes" ~ 1,
                        hml10_5 == "TRUE" ~1,
                        hml10_5 == "FALSE" ~0,
                        TRUE ~ as.numeric(hml10_5)),
    hml10_6 = case_when(hml10_6 == "No, don't know" ~ 0,
                        hml10_6 == "Yes" ~ 1,
                        hml10_6 == "no, don't know" ~ 0,
                        hml10_6 == "yes" ~ 1,
                        hml10_6 == "TRUE" ~1,
                        hml10_6 == "FALSE" ~0,
                        TRUE ~ as.numeric(hml10_6)),
    hml10_7 = case_when(hml10_7 == "No, don't know" ~ 0,
                        hml10_7 == "Yes" ~ 1,
                        hml10_7 == "no, don't know" ~ 0,
                        hml10_7 == "yes" ~ 1,
                        hml10_7 == "TRUE" ~1,
                        hml10_7 == "FALSE" ~0,
                        TRUE ~ as.numeric(hml10_7)),
    hml10_01 = case_when(hml10_01 == "No, don't know" ~ 0,
                        hml10_01 == "Yes" ~ 1,
                        hml10_01 == "no, don't know" ~ 0,
                        hml10_01 == "yes" ~ 1,
                        TRUE ~ as.numeric(hml10_01)),
    hml10_02 = case_when(hml10_02 == "No, don't know" ~ 0,
                        hml10_02 == "Yes" ~ 1,
                        hml10_02 == "no, don't know" ~ 0,
                        hml10_02 == "yes" ~ 1,
                        TRUE ~ as.numeric(hml10_02)),
    hml10_03 = case_when(hml10_03 == "No, don't know" ~ 0,
                        hml10_03 == "Yes" ~ 1,
                        hml10_03 == "no, don't know" ~ 0,
                        hml10_03 == "yes" ~ 1,
                        TRUE ~ as.numeric(hml10_03)),
    hml10_04 = case_when(hml10_04 == "No, don't know" ~ 0,
                        hml10_04 == "Yes" ~ 1,
                        hml10_04 == "no, don't know" ~ 0,
                        hml10_04 == "yes" ~ 1,
                        TRUE ~ as.numeric(hml10_04)),
    hml10_05 = case_when(hml10_05 == "No, don't know" ~ 0,
                        hml10_05 == "Yes" ~ 1,
                        hml10_05 == "no, don't know" ~ 0,
                        hml10_05 == "yes" ~ 1,
                        hml10_05 == "TRUE" ~1,
                        hml10_05 == "FALSE" ~0,
                        TRUE ~ as.numeric(hml10_05)),
    hml10_06 = case_when(hml10_06 == "No, don't know" ~ 0,
                        hml10_06 == "Yes" ~ 1,
                        hml10_06 == "no, don't know" ~ 0,
                        hml10_06 == "yes" ~ 1,
                        hml10_06 == "TRUE" ~1,
                        hml10_06 == "FALSE" ~0,
                        TRUE ~ as.numeric(hml10_06)),
    hml10_07 = case_when(hml10_07 == "No, don't know" ~ 0,
                        hml10_07 == "Yes" ~ 1,
                        hml10_07 == "no, don't know" ~ 0,
                        hml10_07 == "yes" ~ 1,
                        hml10_07 == "TRUE" ~1,
                        hml10_07 == "FALSE" ~0,
                        TRUE ~ as.numeric(hml10_07)),
    hml10_08 = case_when(hml10_08 == "No, don't know" ~ 0,
                         hml10_08 == "Yes" ~ 1,
                         hml10_08 == "no, don't know" ~ 0,
                         hml10_08 == "yes" ~ 1,
                         hml10_08 == "TRUE" ~1,
                         hml10_08 == "FALSE" ~0,
                         TRUE ~ as.numeric(hml10_08)),
    hml10_09 = case_when(hml10_09 == "No, don't know" ~ 0,
                         hml10_09 == "Yes" ~ 1,
                         hml10_09 == "no, don't know" ~ 0,
                         hml10_09 == "yes" ~ 1,
                         hml10_09 == "TRUE" ~1,
                         hml10_09 == "FALSE" ~0,
                         TRUE ~ as.numeric(hml10_09)),
    hml10_10 = case_when(hml10_10 == "No, don't know" ~ 0,
                         hml10_10 == "Yes" ~ 1,
                         hml10_10 == "no, don't know" ~ 0,
                         hml10_10 == "yes" ~ 1,
                         hml10_10 == "TRUE" ~1,
                         hml10_10 == "FALSE" ~0,
                         TRUE ~ as.numeric(hml10_07)),
    hml10_11 = case_when(hml10_11 == "No, don't know" ~ 0,
                         hml10_11 == "Yes" ~ 1,
                         hml10_11 == "no, don't know" ~ 0,
                         hml10_11 == "yes" ~ 1,
                         TRUE ~ as.numeric(hml10_11)),
    hml10_12 = case_when(hml10_12 == "No, don't know" ~ 0,
                         hml10_12 == "Yes" ~ 1,
                         hml10_12 == "no, don't know" ~ 0,
                         hml10_12 == "yes" ~ 1,
                         TRUE ~ as.numeric(hml10_12)),
    hml10_13 = case_when(hml10_13 == "No, don't know" ~ 0,
                         hml10_13 == "Yes" ~ 1,
                         hml10_13 == "no, don't know" ~ 0,
                         hml10_13 == "yes" ~ 1,
                         TRUE ~ as.numeric(hml10_13)),
    hml10_14 = case_when(hml10_14 == "No, don't know" ~ 0,
                         hml10_14 == "Yes" ~ 1,
                         hml10_14 == "no, don't know" ~ 0,
                         hml10_14 == "yes" ~ 1,
                         TRUE ~ as.numeric(hml10_14)),
    hml10_15 = case_when(hml10_15 == "No, don't know" ~ 0,
                         hml10_15 == "Yes" ~ 1,
                         hml10_15 == "no, don't know" ~ 0,
                         hml10_15 == "yes" ~ 1,
                         hml10_15 == "TRUE" ~1,
                         hml10_15 == "FALSE" ~0,
                         TRUE ~ as.numeric(hml10_15)),
    hml10_16 = case_when(hml10_16 == "No, don't know" ~ 0,
                         hml10_16 == "Yes" ~ 1,
                         hml10_16 == "no, don't know" ~ 0,
                         hml10_16 == "yes" ~ 1,
                         hml10_16 == "TRUE" ~1,
                         hml10_16 == "FALSE" ~0,
                         TRUE ~ as.numeric(hml10_16)),
    hml10_17 = case_when(hml10_17 == "No, don't know" ~ 0,
                         hml10_17 == "Yes" ~ 1,
                         hml10_17 == "no, don't know" ~ 0,
                         hml10_17 == "yes" ~ 1,
                         hml10_17 == "TRUE" ~1,
                         hml10_17 == "FALSE" ~0,
                         TRUE ~ as.numeric(hml10_17)),
    hml10_18 = case_when(hml10_18 == "No, don't know" ~ 0,
                         hml10_18 == "Yes" ~ 1,
                         hml10_18 == "no, don't know" ~ 0,
                         hml10_18 == "yes" ~ 1,
                         hml10_18 == "TRUE" ~1,
                         hml10_18 == "FALSE" ~0,
                         TRUE ~ as.numeric(hml10_18)),
    hml10_19 = case_when(hml10_19 == "No, don't know" ~ 0,
                         hml10_19 == "Yes" ~ 1,
                         hml10_19 == "no, don't know" ~ 0,
                         hml10_19 == "yes" ~ 1,
                         hml10_19 == "TRUE" ~1,
                         hml10_19 == "FALSE" ~0,
                         TRUE ~ as.numeric(hml10_19)),
    hml10_20 = case_when(hml10_20 == "No, don't know" ~ 0,
                         hml10_20 == "Yes" ~ 1,
                         hml10_20 == "no, don't know" ~ 0,
                         hml10_20 == "yes" ~ 1,
                         hml10_20 == "TRUE" ~1,
                         hml10_20 == "FALSE" ~0,
                         TRUE ~ as.numeric(hml10_20)),
    hml10_21 = case_when(hml10_21 == "No, don't know" ~ 0,
                         hml10_21 == "Yes" ~ 1,
                         hml10_21 == "no, don't know" ~ 0,
                         hml10_21 == "yes" ~ 1,
                         TRUE ~ as.numeric(hml10_21)),
    hml10_22 = case_when(hml10_22 == "No, don't know" ~ 0,
                         hml10_22 == "Yes" ~ 1,
                         hml10_22 == "no, don't know" ~ 0,
                         hml10_22 == "yes" ~ 1,
                         TRUE ~ as.numeric(hml10_22)),
    hml10_23 = case_when(hml10_23 == "No, don't know" ~ 0,
                         hml10_23 == "Yes" ~ 1,
                         hml10_23 == "no, don't know" ~ 0,
                         hml10_23 == "yes" ~ 1,
                         TRUE ~ as.numeric(hml10_23)),
    hml10_24 = case_when(hml10_24 == "No, don't know" ~ 0,
                         hml10_24 == "Yes" ~ 1,
                         hml10_24 == "no, don't know" ~ 0,
                         hml10_24 == "yes" ~ 1,
                         TRUE ~ as.numeric(hml10_24)),
    hml10_25 = case_when(hml10_25 == "No, don't know" ~ 0,
                         hml10_25 == "Yes" ~ 1,
                         hml10_25 == "no, don't know" ~ 0,
                         hml10_25 == "yes" ~ 1,
                         hml10_25 == "TRUE" ~1,
                         hml10_25 == "FALSE" ~0,
                         TRUE ~ as.numeric(hml10_25)),
    hml10_26 = case_when(hml10_26 == "No, don't know" ~ 0,
                         hml10_26 == "Yes" ~ 1,
                         hml10_26 == "no, don't know" ~ 0,
                         hml10_26 == "yes" ~ 1,
                         hml10_26 == "TRUE" ~1,
                         hml10_26 == "FALSE" ~0,
                         TRUE ~ as.numeric(hml10_26)),
    hml10_27 = case_when(hml10_27 == "No, don't know" ~ 0,
                         hml10_27 == "Yes" ~ 1,
                         hml10_27 == "no, don't know" ~ 0,
                         hml10_27 == "yes" ~ 1,
                         hml10_27 == "TRUE" ~1,
                         hml10_27 == "FALSE" ~0,
                         TRUE ~ as.numeric(hml10_27)),
    hml10_28 = case_when(hml10_28 == "No, don't know" ~ 0,
                         hml10_28 == "Yes" ~ 1,
                         hml10_28 == "no, don't know" ~ 0,
                         hml10_28 == "yes" ~ 1,
                         hml10_28 == "TRUE" ~1,
                         hml10_28 == "FALSE" ~0,
                         TRUE ~ as.numeric(hml10_28)),
    hml10_29 = case_when(hml10_29 == "No, don't know" ~ 0,
                         hml10_29 == "Yes" ~ 1,
                         hml10_29 == "no, don't know" ~ 0,
                         hml10_29 == "yes" ~ 1,
                         hml10_29 == "TRUE" ~1,
                         hml10_29 == "FALSE" ~0,
                         TRUE ~ as.numeric(hml10_29)),
    hml10_30 = case_when(hml10_30 == "No, don't know" ~ 0,
                         hml10_30 == "Yes" ~ 1,
                         hml10_30 == "no, don't know" ~ 0,
                         hml10_30 == "yes" ~ 1,
                         hml10_30 == "TRUE" ~1,
                         hml10_30 == "FALSE" ~0,
                         TRUE ~ as.numeric(hml10_30)),
    hv227 = case_when(hv227 == "Yes" ~ 1,
                      hv227 == "No" ~ 0,
                      hv227 == "yes" ~ 1,
                      hv227 == "no" ~ 0,
                      hv227 == 9 ~ as.numeric(NA),
                      TRUE ~ as.numeric(hv227)),
    hml1 = as.numeric(hml1),
    hv013 = as.numeric(hv013),
    hv005 = as.numeric(hv005),
    hhid = trimws(hhid, which = c("both"))) %>%
  rowwise()%>%
  mutate(itn_total = sum(hml10_1, hml10_2, hml10_3, hml10_4, hml10_5, hml10_6, hml10_7,  
                         hml10_01, hml10_02, hml10_03, hml10_04, hml10_05, hml10_06, hml10_07, hml10_08, hml10_09, hml10_10, 
                         hml10_11, hml10_12, hml10_13, hml10_14, hml10_15, hml10_16, hml10_17, hml10_18, hml10_19, hml10_20, hml10_21,
                         hml10_22, hml10_23, hml10_24, hml10_25, hml10_26, hml10_27, hml10_28, hml10_29, hml10_30, na.rm = TRUE),
         one_itn = ifelse(itn_total > 0, 1, 0),
         itn_total_double = itn_total*2,
         potential_users = min(hv013,itn_total_double)) %>%
  select(country, year, hhid, NumberofHouseholds, FieldworkStart, FieldworkEnd, hv227, hml1, hv013, hv005, itn_total, one_itn, itn_total_double, potential_users)



df_HR <- full_join(df_HR, df_senegal)

write.xlsx(df_HR, "df_HR.xlsx")

#----------------------------------------------------------------
### HH posession of nets
df_posession <- df_HR %>%
  group_by(country, year) %>%
  summarise(at_least_net = sum(hv227 ==1, na.rm = TRUE), # num 1 all nets
            at_least_itn = sum(one_itn, na.rm =TRUE), #num 2 itn nets
            hh_1_stayed = sum(hv013 > 0, na.rm = TRUE),
            net_2_pers = sum(hv013 >0 & hml1/hv013 >= 0.5, na.rm = TRUE), #num 5
            itn_2_pers = sum(hv013 > 0 & itn_total/hv013 >= 0.5, na.rm = TRUE), #num 6
  ) %>% #denum 5 i 6
  inner_join(df_HR %>% select(country, year, NumberofHouseholds), by = c("country", "year")) %>% 
  mutate(perc_at_least_net = at_least_net/NumberofHouseholds, # 1
         perc_at_least_itn = at_least_itn/NumberofHouseholds, #2 
         perc_net_2_pers = net_2_pers/hh_1_stayed,
         perc_itn_2_pers = itn_2_pers/hh_1_stayed
  ) %>% #5
  select(country, year, perc_at_least_net, perc_at_least_itn, perc_net_2_pers, perc_itn_2_pers) %>%
  distinct()



## Access
df_access <- df_PR %>% select(country, year, hhid, hv103)%>%
  group_by(country, year, hhid) %>%
  summarise(number_pers = sum(hv103 ==1, na.rm = TRUE)#denominator
  ) %>%
  full_join(df_HR %>% select(country, year, hhid, itn_total_double, potential_users), 
            by = c("country", "year", "hhid"), na_matches="never")%>% 
  mutate(access_hhid = potential_users/number_pers) %>% 
  group_by(country, year) %>%
  summarise(access = mean(access_hhid, na.rm = TRUE))%>%
  select(country, year, access)

#-----------------------------------------------------------------

## USAGE
df_usage <- df_PR %>% select(country, year, hhid, hml12, hv103, hv104, hml16, hml18) %>%
  dplyr::group_by(country, year, hhid) %>%
  dplyr::summarise(sleep_under_net = sum(hv103==1 & hml12 %in% 1:3, na.rm = TRUE),
                   child_sleep_under_net = sum(hv103==1 & hml16 %in% 0:4 & hml12 %in% 1:3, na.rm = TRUE),
                   pregnant_sleep_under_net = sum(hv103==1 & hv104 ==2 & hml16 %in% 15:49 & hml18 == 1 & hml12 %in% 1:3, na.rm = TRUE),
                   sleep_itn = sum(hv103==1 & hml12 ==1, na.rm=TRUE),
                   child_sleep_under_itn = sum(hv103==1 & hml16 %in% 0:4 & hml12 ==1, na.rm=TRUE),
                   pregnant_sleep_under_itn = sum(hv103==1 & hv104 ==2 & hml16 %in% 15:49 & hml18 == 1 & hml12 == 1, na.rm = TRUE),
                   number_pers = sum(hv103 ==1, na.rm = TRUE),
                   number_children = sum(hv103 ==1 & hml16 %in% 0:4, na.rm = TRUE),
                   number_pregnant = sum(hv103 ==1 & hml16 %in% 15:49 & hv104 ==2 & hml18 ==1, na.rm = TRUE)) %>%
  inner_join(df_HR %>% select(hhid, country, year, potential_users), by = c("country", "year", "hhid")) %>% 
  mutate(perc_sleep_net_hh = sleep_under_net/number_pers,
         perc_sleep_itn_hh = sleep_itn/number_pers,
         child_sleep_net_hh = child_sleep_under_net/number_children,
         child_sleep_itn_hh = child_sleep_under_itn/number_children,
         pregnant_sleep_net_hh = pregnant_sleep_under_net/number_pregnant,
         pregnant_sleep_itn_hh = pregnant_sleep_under_itn/number_pregnant,
         max_possible_usage = pmin(potential_users, sleep_itn),
         max_possible_sleep_itn_hh = max_possible_usage/number_pers
  ) %>% 
  dplyr::group_by(country, year) %>%
  dplyr::summarise(perc_sleep_net = mean(perc_sleep_net_hh, na.rm = TRUE),
                   perc_sleep_itn = mean(perc_sleep_itn_hh, na.rm = TRUE),
                   max_possible_sleep_itn = mean(max_possible_sleep_itn_hh, na.rm = TRUE),
                   child_sleep_net = mean(child_sleep_net_hh, na.rm = TRUE),
                   child_sleep_itn = mean(child_sleep_itn_hh, na.rm = TRUE),
                   pregnant_sleep_net = mean(pregnant_sleep_net_hh, na.rm = TRUE),
                   pregnant_sleep_itn = mean(pregnant_sleep_itn_hh, na.rm = TRUE))%>%
  inner_join(df_HR %>% select(country, year, FieldworkStart, FieldworkEnd), by = c("country", "year")) %>%
  select(country, year, FieldworkStart, FieldworkEnd,
         perc_sleep_net, perc_sleep_itn, max_possible_sleep_itn, 
         child_sleep_net, child_sleep_itn, 
         pregnant_sleep_net, pregnant_sleep_itn) %>% distinct()



df_usage2 <- df_PR %>% select(country, year, hhid, hml12, hv103, hv104, hml16, hml18) %>%
  group_by(country, year, hhid) %>%
  summarise(sleep_itn_own_itn = sum(hv103==1 & hml12 == 1, na.rm=TRUE),
            child_itn_own_itn = sum(hv103==1 & hml16 %in% 0:4 & hml12 == 1, na.rm=TRUE),
            pregnant_itn_own_itn = sum(hv103==1 & hv104 ==2 & hml16 %in% 15:49 & hml18 == 1 & hml12 == 1, na.rm = TRUE),
            number_pers_own_itn = sum(hv103 ==1, na.rm = TRUE),
            number_children_own_itn = sum(hv103 ==1 & hml16 %in% 0:4, na.rm = TRUE),
            number_pregnant_own_itn = sum(hv103 ==1 & hml16 %in% 15:49 & hv104 ==2 & hml18 ==1, na.rm = TRUE)) %>%
  inner_join(df_HR %>% select(hhid, country, year, itn_total, potential_users, FieldworkStart, FieldworkEnd), by = c("country", "year", "hhid")) %>% filter(itn_total >0) %>%
  mutate(perc_sleep_own_itn_hh = sleep_itn_own_itn/number_pers_own_itn,
         child_sleep_own_itn_hh = child_itn_own_itn/number_children_own_itn,
         pregnant_sleep_own_itn_hh = pregnant_itn_own_itn/number_pregnant_own_itn) %>%
  group_by(country, year) %>%
  summarise(perc_sleep_own_itn = mean(perc_sleep_own_itn_hh, na.rm = TRUE),
            child_sleep_own_itn = mean(child_sleep_own_itn_hh, na.rm = TRUE),
            pregnant_sleep_own_itn = mean(pregnant_sleep_own_itn_hh, na.rm = TRUE))%>%
  inner_join(df_HR %>% select(country, year, FieldworkStart, FieldworkEnd), by = c("country", "year")) %>%
  select(country, year, FieldworkStart, FieldworkEnd,
         perc_sleep_own_itn, 
         child_sleep_own_itn, 
         pregnant_sleep_own_itn) %>% distinct()



df_dhs <- merge(df_usage, df_usage2,  by=c("country", "year", "FieldworkStart", "FieldworkEnd"), all = TRUE)%>% full_join(df_posession) %>% full_join(df_access) 
df_combined <- merge(df_dhs, budget_data, by=c("country", "year"), all = TRUE)

write.xlsx(df_combined, "combined_df.xlsx")
write.xlsx(df_dhs, "df_dhs.xlsx")

df_dhs <- read_excel("df_dhs.xlsx")
df_dhs_malawi <- df_dhs %>% ungroup() %>% mutate(year = as.numeric(year), access_summary = access, hh_usage = perc_sleep_itn, hh_ownership = perc_at_least_itn)%>%
  group_by(country, year) %>%
  gather(5:18, key = "variable", value = "percentage") %>%
  arrange(country) %>% mutate(
    group = case_when(variable %in% c("hh_ownership","access_summary","hh_usage", "max_possible_sleep_itn") ~ "Summary",
                      variable %in% c("perc_at_least_net","perc_at_least_itn", "perc_net_2_pers", "perc_itn_2_pers") ~ "Net ownership",
                      variable == "access" ~ "Net access within household",
                      variable %in% c("perc_sleep_net","perc_sleep_itn", "perc_sleep_own_itn") ~ "Net usage by households",
                      variable %in% c("child_sleep_net", "child_sleep_itn", "child_sleep_own_itn") ~ "Net usage by children < 5",
                      variable %in% c("pregnant_sleep_net", "pregnant_sleep_itn", "pregnant_sleep_own_itn")~ "Net usage by pregnant women"),
    description = case_when(variable == "perc_at_least_net"~"HH with at least one mosquito net (%)",  
                            variable == "perc_at_least_itn" ~"HH with at least one ITN (%)",
                            variable == "hh_usage" ~ "HH usage of ITNs (%)",
                            variable == "access_summary" ~ "HH access to ITN's (%)",
                            variable == "max_possible_sleep_itn" ~ "Maximum theoretical use (2 people per net)",
                            variable == "perc_net_2_pers" ~ "HH with at least one net \n for every 2 people (%)",
                            variable == "perc_itn_2_pers" ~ "HH with at least one ITN \n for every 2 people (%)",
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
    ),
    percentage = round(percentage, 4))%>% ungroup() %>%
  filter(country == "Malawi") %>% select(country, year, variable, percentage, description)

df_dhs_uganda <- df_dhs %>% mutate(year = as.numeric(year)) %>%
  group_by(country, year) %>%
  gather(5:18, key = "variable", value = "percentage") %>%
  arrange(country) %>% mutate(
    group = case_when(variable %in% c("hh_ownership","access_summary","hh_usage", "max_possible_sleep_itn") ~ "Summary",
                      variable %in% c("perc_at_least_net","perc_at_least_itn", "perc_net_2_pers", "perc_itn_2_pers") ~ "Net ownership",
                      variable == "access" ~ "Net access within household",
                      variable %in% c("perc_sleep_net","perc_sleep_itn", "perc_sleep_own_itn") ~ "Net usage by households",
                      variable %in% c("child_sleep_net", "child_sleep_itn", "child_sleep_own_itn") ~ "Net usage by children < 5",
                      variable %in% c("pregnant_sleep_net", "pregnant_sleep_itn", "pregnant_sleep_own_itn")~ "Net usage by pregnant women"),
    description = case_when(variable == "perc_at_least_net"~"HH with at least one mosquito net (%)",  
                            variable == "perc_at_least_itn" ~"HH with at least one ITN (%)",
                            variable == "hh_usage" ~ "HH usage of ITNs (%)",
                            variable == "access_summary" ~ "HH access to ITN's (%)",
                            variable == "max_possible_sleep_itn" ~ "Maximum theoretical use (2 people per net)",
                            variable == "perc_net_2_pers" ~ "HH with at least one net \n for every 2 people (%)",
                            variable == "perc_itn_2_pers" ~ "HH with at least one ITN \n for every 2 people (%)",
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
    ),
    percentage = round(percentage, 4))%>% ungroup() %>%
  filter(country == "Uganda") %>% select(country, year, variable, percentage, description)


stargazer(df_dhs_malawi, 
          align=TRUE, type = "latex", summary = FALSE, no.space = TRUE, rownames = FALSE,
          title = "Variables_malawi", out = "malawi.tex")


stargazer(df_dhs_uganda, 
          align=TRUE, type = "latex", summary = FALSE, no.space = TRUE, rownames = FALSE,
          title = "Uganda Variables ", out = "uganda.tex")


df_dhs_summary <- read.xlsx("df_dhs.xlsx") %>% mutate(year = as.numeric(year)) %>%
  group_by(country, year) %>%
  gather(5:17, key = "variable", value = "percentage") %>%
  arrange(country) %>% mutate(
    use = round(percentage, 2),
    not_use = 1-use,
    group = case_when(variable %in% c("perc_at_least_net","perc_at_least_itn", "perc_net_2_pers", "perc_itn_2_pers") ~ "Net ownership",
                      variable %in% c("perc_sleep_net","perc_sleep_itn", "perc_sleep_own_itn") ~ "Net usage by households",
                      variable %in% c("child_sleep_net", "child_sleep_itn", "child_sleep_own_itn") ~ "Net usage by children < 5",
                      variable %in% c("pregnant_sleep_net", "pregnant_sleep_itn", "pregnant_sleep_own_itn")~ "Net usage by pregnant women"),
    description = case_when(variable == "perc_at_least_net"~"HH with at least one mosquito net (%)",  
                            variable == "perc_at_least_itn" ~"HH with at least one ITN (%)",
                            variable == "perc_net_2_pers" ~ "HH with at least one net for every 2 people (%)",
                            variable == "perc_itn_2_pers" ~ "HH with at least one ITN for every 2 people (%)",
                            variable == "perc_sleep_net" ~"Percentage of the HH members who slept under an evertreated net the night before the survey",
                            variable == "perc_sleep_itn" ~  "Percentage of the HH members who slept under an ITN the night before the survey",
                            variable == "perc_sleep_own_itn" ~ "Among the household population in households with at least one ITN, the % who slept under an ITN the night before the survey", 
                            variable == "child_sleep_net" ~ "Percentage of children < 5 who slept under an evertreated net the night before the survey", 
                            variable == "child_sleep_itn" ~ "Percentage of children < 5 who slept under an ITN the night before the survey",
                            variable == "child_sleep_own_itn" ~ "Among children < 5 in households with at least one ITN, the % who slept under an ITN the night before the survey", 
                            variable == "pregnant_sleep_net" ~ "Percentage of pregnant women who slept under an evertreated net last night", 
                            variable == "pregnant_sleep_itn" ~ "Percentage of pregnant women who slept under an ITN the night before the survey",
                            variable == "pregnant_sleep_own_itn" ~ "Among pregnant women in households with at least one ITN, the % who slept under an ITN the night before the survey"
    ))%>% ungroup() %>%
  filter(year == 2018,
         description %in% c("Among the household population in households with at least one ITN, the % who slept under an ITN the night before the survey",
                            "Among children < 5 in households with at least one ITN, the % who slept under an ITN the night before the survey",
                            "Among pregnant women in households with at least one ITN, the % who slept under an ITN the night before the survey"
         )) %>% select(country, year, variable, use, not_use, description)

write.xlsx(df_dhs_summary, "dhs_averages_use.xlsx")

df_dhs_ownership <- read.xlsx("df_dhs.xlsx") %>% mutate(year = as.numeric(year)) %>%
  group_by(country, year) %>%
  gather(5:17, key = "variable", value = "percentage") %>%
  arrange(country) %>% mutate(
    percentage = round(percentage, 2),
    group = case_when(variable %in% c("perc_at_least_net","perc_at_least_itn", "perc_net_2_pers", "perc_itn_2_pers") ~ "Net ownership",
                      variable %in% c("perc_sleep_net","perc_sleep_itn", "perc_sleep_own_itn") ~ "Net usage by households",
                      variable %in% c("child_sleep_net", "child_sleep_itn", "child_sleep_own_itn") ~ "Net usage by children < 5",
                      variable %in% c("pregnant_sleep_net", "pregnant_sleep_itn", "pregnant_sleep_own_itn")~ "Net usage by pregnant women"),
    description = case_when(variable == "perc_at_least_net"~"HH with at least one mosquito net (%)",  
                            variable == "perc_at_least_itn" ~"HH with at least one ITN (%)",
                            variable == "perc_net_2_pers" ~ "HH with at least one net for every 2 people (%)",
                            variable == "perc_itn_2_pers" ~ "HH with at least one ITN for every 2 people (%)",
                            variable == "perc_sleep_net" ~"Percentage of the HH members who slept under an evertreated net the night before the survey",
                            variable == "perc_sleep_itn" ~  "Percentage of the HH members who slept under an ITN the night before the survey",
                            variable == "perc_sleep_own_itn" ~ "Among the household population in households with at least one ITN, the % who slept under an ITN the night before the survey", 
                            variable == "child_sleep_net" ~ "Percentage of children < 5 who slept under an evertreated net the night before the survey", 
                            variable == "child_sleep_itn" ~ "Percentage of children < 5 who slept under an ITN the night before the survey",
                            variable == "child_sleep_own_itn" ~ "Among children < 5 in households with at least one ITN, the % who slept under an ITN the night before the survey", 
                            variable == "pregnant_sleep_net" ~ "Percentage of pregnant women who slept under an evertreated net last night", 
                            variable == "pregnant_sleep_itn" ~ "Percentage of pregnant women who slept under an ITN the night before the survey",
                            variable == "pregnant_sleep_own_itn" ~ "Among pregnant women in households with at least one ITN, the % who slept under an ITN the night before the survey"
    ))%>% ungroup() %>%
  filter(year == 2018,
         description == "HH with at least one ITN (%)") %>% select(country, year, variable, percentage, description)

write.xlsx(df_dhs_ownership, "dhs_averages_ownership.xlsx")
