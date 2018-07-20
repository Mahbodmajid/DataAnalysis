library(ggplot2)
library(ggthemes)
library(tidyr)
library(stringr)
library(WDI)
library(readr)
library(plyr)
library(ggbiplot)
library(dplyr)
library(highcharter)
#setwd("Desktop/96-97-2/Data Analysis/HW/HW10/")
read_csv("../WDI_csv/WDISeries.csv") -> WDISeries
read_csv("../WDI_csv/WDIData.csv") -> WDIData

get_indicator_gathered <- function(indicator, years = 1960:2017, Country_Code = NULL){
  WDIData %>%
    filter(`Indicator Code` == indicator) %>% 
    select(-X63) -> original
  
  if(!is.null(Country_Code)){
    original %>% 
      filter(`Country Code` %in%  Country_Code) -> original
  }
  
  original %>%
    gather("year", "value", 5:62) %>%
    select(Country = `Country Name`, Country_Code = `Country Code`,Indicator = `Indicator Code`, year, value) %>% 
    mutate(value = as.numeric(value)) %>% 
    filter(year %in% years) -> gathered
  return(list(original = original,
              gathered = gathered,
              na_count = gathered %>% 
                group_by(year) %>%
                summarize(na_count = sum(is.na(value))) %>% 
                ungroup() %>% 
                arrange(desc(year)),
              most_recent = gathered %>% 
                filter(!is.na(value)) %>%
                group_by(Country, Country_Code) %>% 
                top_n(wt = year, n = 1) %>% 
                ungroup()))
}

get_multiple_indicator_gathered <- function(indicators, years = 1960:2017, Country_Code = NULL){
  WDIData %>%
    filter(`Indicator Code` %in%  indicators) %>% 
    select(-X63) -> original
  
  if(!is.null(Country_Code)){
    original %>% 
      filter(`Country Code` %in%  Country_Code) -> original
  }
  
  original %>%
    gather("year", "value", 5:62) %>%
    select(Country = `Country Name`, Country_Code = `Country Code`,Indicator = `Indicator Code`, year, value) %>% 
    mutate(value = as.numeric(value)) %>% 
    filter(year %in% years) -> gathered
  return(list(original = original,
              gathered = gathered,
              na_count = gathered %>% 
                group_by(year, Indicator) %>%
                summarize(na_count = sum(is.na(value))) %>% 
                ungroup() %>% 
                arrange(desc(year)),
              most_recent = gathered %>% 
                filter(!is.na(value)) %>%
                group_by(Country, Country_Code, Indicator) %>% 
                top_n(wt = year, n = 1) %>% 
                ungroup()))
}

cor_tester <- function(indicator1, indicator2, Xpre = 2, Ypre = 2, years = NULL){
  WDISeries %>%
    filter(`Series Code` == indicator1) %>% 
    select(`Indicator Name`) %>% as.data.frame() %>% .[1,1]-> full_name_1
  
  str_sub(full_name_1, end = str_locate(full_name_1, "\\(") -2) %>% .[1]-> name_1
  
  WDISeries %>%
    filter(`Series Code` == indicator2) %>% 
    select(`Indicator Name`) %>% as.data.frame() %>% .[1,1] -> full_name_2
  
  str_sub(full_name_2, end = str_locate(full_name_2, "\\(")- 2) %>% .[1] -> name_2
  if(is.null(years)){
    get_indicator_gathered(indicator1)$most_recent %>% select(-Country) %>%
      right_join(get_indicator_gathered(indicator2)$most_recent,
                 by = c("Country_Code")) %>%
      mutate(Xyear= year.x,
             Xval = round(value.x,Xpre),
             Yyear = year.y,
             Yval = round(value.y,Ypre)) %>%
      drop_na() -> Y_X
  } else {
    get_indicator_gathered(indicator1)$gathered %>% select(-Country) %>%
      inner_join(get_indicator_gathered(indicator2)$gathered,
                 by = c("Country_Code", "year")) %>%
      mutate(Xyear= year,
             Xval = round(value.x,Xpre),
             Yyear = year,
             Yval = round(value.y,Ypre)) %>%
      drop_na() -> Y_X
  }
  
  return(list(full_name_1 = full_name_1,
              full_name_2 = full_name_2,
              name_1 = name_1,
              name_2 = name_2,
              cor_test = cor.test(Y_X$Yval, Y_X$Xval, method = "spearman"),
              plot = Y_X %>% 
                hchart(
                type = "scatter",
                hcaes(x = Xval, y = Yval, color = Country),
                name = paste0(name_2, " / ",name_1),
                tooltip = list(pointFormat =paste0("<b> {point.Country} <b/> <br/>",
                               full_name_1 , " ({point.Xyear}): {point.Xval} <br/>",
                               full_name_2 , " ({point.Yyear}): {point.Yval} <br/>"))
                ) %>%
                hc_title(text = paste0(name_2, " / ",name_1)) %>%
                hc_xAxis(title = list(text = name_1)) %>%
                hc_yAxis(title = list(text = name_2))  %>%
                hc_tooltip(crosshairs = T) %>%
                hc_add_theme(hc_theme_sandsignika())
                ))
}

