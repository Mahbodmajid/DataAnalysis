mypath <- "../class_data"
read_csv(paste0(mypath,"/constituents.csv")) -> constituents
read_csv("../class_data/indexes.csv") -> indexes
constituents$Symbol[constituents$Symbol == "BRK.B"] <- "BRK-B"
constituents$Symbol[constituents$Symbol == "BF.B"] <- "BF-B"
list.files(paste0(mypath, "/stock_dfs/")) -> all_files
files_path <- paste0(mypath, "/stock_dfs/")

library(stringr)
files_list = list()

for(file in all_files){
  index = str_locate(string = file, pattern = ".csv")[1]
  company_name = substr(x = file,
                        start = 1,
                        stop = index - 1)
  read_csv(file = paste0(files_path, file)) %>%
    mutate(Company = company_name ) -> new_file
  
  files_list[[company_name]] <- new_file
}

rbinded <- bind_rows(files_list)
rbinded %>%
  mutate(month = as.integer(format(Date,"%m")) +
           12 * (as.integer(format(Date,"%Y")) - 2000)) -> rbinded_full

add_name_sector <- function(mydf){
  cs <- constituents %>% rename(Company = Symbol)
  return(left_join(mydf, cs , by = c("Company")) %>%
           mutate(Name = ifelse(is.na(Name), Company, Name)))
}

get_top_company_duration <- function(duration, how_many){
  return(
    list(
      by_growth = 
      rbinded_full %>%
        group_by(month, Company) %>%
        arrange(Date) %>% 
        summarize(value = first(`Adj Close`)) %>%
        ungroup() %>% 
        group_by(Company) %>% 
        mutate(growth = value - lag(value, duration)) %>% 
        ungroup() %>%
        na.omit() %>%
        top_n(wt = growth, n = how_many) %>% 
        arrange(desc(growth))
      ,
      by_growth_percent = 
      rbinded_full %>%
        group_by(month, Company) %>%
        arrange(Date) %>% 
        summarize(value = first(`Adj Close`)) %>%
        ungroup() %>% 
        group_by(Company) %>% 
        mutate(growth = value / lag(value, duration)) %>% 
        ungroup() %>%
        na.omit() %>%
        filter(is.finite(growth)) %>% 
        top_n(wt = growth, n = how_many) %>% 
        arrange(desc(growth))
  ))
}

get_top_company_duration(12, 10)[["by_growth_percent"]] %>%
  add_name_sector() %>%
  arrange(desc(growth)) -> mydata

ggplot(mydata, aes(x = reorder(paste0(Name,
                                      " ",
                                      month %% 12 + 1,
                                      "/",
                                      2000 + floor((month - 1)/12)),
                                      growth),
                   y = growth)) +
  geom_bar(stat = "identity", fill = 4)+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_flip()+
  xlab("Company, Date")+
  ylab("Growth Percent")+
  ggtitle("Company 12 Months")

get_top_company_duration(12, 10)[["by_growth"]] %>%
  add_name_sector() %>%
  arrange(desc(growth)) -> mydata

ggplot(mydata, aes(x = reorder(paste0(Name,
                                    " ",
                                    month %% 12 + 1,
                                    "/",
                                    2000 + floor((month - 1)/12)),
                             growth),
                 y = growth)) +
  geom_bar(stat = "identity", fill = 4)+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_flip()+
  xlab("Company, Date")+
  ylab("Solid Growth")+
  ggtitle("Company 12 Months")

get_top_company_duration(24, 10)[["by_growth_percent"]] %>%
  add_name_sector() %>%
  arrange(desc(growth)) -> mydata

ggplot(mydata, aes(x = reorder(paste0(Name,
                                    " ",
                                    month %% 12 + 1,
                                    "/",
                                    2000 + floor((month - 1)/12)),
                             growth),
                 y = growth)) +
  geom_bar(stat = "identity", fill = 4)+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_flip()+
  xlab("Company, Date")+
  ylab("Growth Percent")+
  ggtitle("Company 24 Months")

get_top_company_duration(24, 10)[["by_growth"]] %>%
  add_name_sector() %>%
  arrange(desc(growth)) -> mydata

ggplot(mydata, aes(x = reorder(paste0(Name,
                                    " ",
                                    month %% 12 + 1,
                                    "/",
                                    2000 + floor((month - 1)/12)),
                             growth),
                 y = growth)) +
  geom_bar(stat = "identity", fill = 4)+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_flip()+
  xlab("Company, Date")+
  ylab("Solid Growth")+
  ggtitle("Company 24 Months")

get_top_company_duration(60, 10)[["by_growth_percent"]] %>%
  add_name_sector() %>%
  arrange(desc(growth)) -> mydata

ggplot(mydata, aes(x = reorder(paste0(Name,
                                    " ",
                                    month %% 12 + 1,
                                    "/",
                                    2000 + floor((month - 1)/12)),
                             growth),
                 y = growth)) +
  geom_bar(stat = "identity", fill = 4)+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_flip()+
  xlab("Company, Date")+
  ylab("Growth Percent")+
  ggtitle("Company 60 Months")

get_top_company_duration(60, 10)[["by_growth"]] %>%
  add_name_sector() %>%
  arrange(desc(growth)) -> mydata

ggplot(mydata, aes(x = reorder(paste0(Name,
                                    " ",
                                    month %% 12 + 1,
                                    "/",
                                    2000 + floor((month - 1)/12)),
                             growth),
                 y = growth)) +
  geom_bar(stat = "identity", fill = 4)+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_flip()+
  xlab("Company, Date")+
  ylab("Solid Growth")+
  ggtitle("Company 60 Months")

get_top_sector_duration <- function(duration, how_many){
  return(list(
    by_growth = 
    rbinded_full %>%
      add_name_sector() %>% 
      group_by(month, Sector, Date) %>%
      summarize(value = sum(`Adj Close`)) %>%
      group_by(month, Sector) %>% 
      arrange(Date) %>% 
      summarize(value = first(value)) %>%
      group_by(Sector) %>% 
      mutate(growth = value - lag(value, duration)) %>% 
      ungroup() %>%
      na.omit() %>%
      top_n(wt = growth, n = how_many) %>% 
      arrange(desc(growth)),
    by_growth_percent = 
      rbinded_full %>%
      add_name_sector() %>% 
      group_by(month, Sector, Date) %>%
      summarize(value = sum(`Adj Close`)) %>%
      group_by(month, Sector) %>% 
      arrange(Date) %>% 
      summarize(value = first(value)) %>%
      group_by(Sector) %>% 
      mutate(growth = value / lag(value, duration)) %>% 
      ungroup() %>%
      na.omit() %>%
      filter(is.finite(growth)) %>% 
      top_n(wt = growth, n = how_many) %>% 
      arrange(desc(growth))
  ))
}

get_top_sector_duration(12, 10)[["by_growth_percent"]] %>% 
  arrange(desc(growth)) -> mydata

ggplot(mydata, aes(x = reorder(paste0(Sector,
                                      " ",
                                      month %% 12 + 1,
                                      "/",
                                      2000 + floor((month - 1)/12)),
                               growth),
                   y = growth)) +
  geom_bar(stat = "identity", fill = 4)+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_flip()+
  xlab("Sector, Date")+
  ylab("Growth Percent")+
  ggtitle("Sector 12 Months")

get_top_sector_duration(12, 10)[["by_growth"]] %>% 
  arrange(desc(growth)) -> mydata

ggplot(mydata, aes(x = reorder(paste0(Sector,
                                      " ",
                                      month %% 12 + 1,
                                      "/",
                                      2000 + floor((month - 1)/12)),
                               growth),
                   y = growth)) +
  geom_bar(stat = "identity", fill = 4)+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_flip()+
  xlab("Sector, Date")+
  ylab("Solid Growth")+
  ggtitle("Sector 12 Months")

get_top_sector_duration(24, 10)[["by_growth_percent"]] %>% 
  arrange(desc(growth)) -> mydata

ggplot(mydata, aes(x = reorder(paste0(Sector,
                                      " ",
                                      month %% 12 + 1,
                                      "/",
                                      2000 + floor((month - 1)/12)),
                               growth),
                   y = growth)) +
  geom_bar(stat = "identity", fill = 4)+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_flip()+
  xlab("Sector, Date")+
  ylab("Growth Percent")+
  ggtitle("Sector 24 Months")

get_top_sector_duration(24, 10)[["by_growth"]] %>% 
  arrange(desc(growth)) -> mydata

ggplot(mydata, aes(x = reorder(paste0(Sector,
                                      " ",
                                      month %% 12 + 1,
                                      "/",
                                      2000 + floor((month - 1)/12)),
                               growth),
                   y = growth)) +
  geom_bar(stat = "identity", fill = 4)+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_flip()+
  xlab("Sector, Date")+
  ylab("Solid Growth")+
  ggtitle("Sector 24 Months")

get_top_sector_duration(60, 10)[["by_growth_percent"]] %>% 
  arrange(desc(growth)) -> mydata

ggplot(mydata, aes(x = reorder(paste0(Sector,
                                      " ",
                                      month %% 12 + 1,
                                      "/",
                                      2000 + floor((month - 1)/12)),
                               growth),
                   y = growth)) +
  geom_bar(stat = "identity", fill = 4)+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_flip()+
  xlab("Sector, Date")+
  ylab("Growth Percent")+
  ggtitle("Sector 60 Months")

get_top_sector_duration(60, 10)[["by_growth"]] %>% 
  arrange(desc(growth)) -> mydata

ggplot(mydata, aes(x = reorder(paste0(Sector,
                                      " ",
                                      month %% 12 + 1,
                                      "/",
                                      2000 + floor((month - 1)/12)),
                               growth),
                   y = growth)) +
  geom_bar(stat = "identity", fill = 4)+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_flip()+
  xlab("Sector, Date")+
  ylab("Solid Growth")+
  ggtitle("Sector 60 Months")