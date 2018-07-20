library(knitr)
library(kableExtra)
library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(ggplot2)
library(highcharter)
library(formattable)


setwd("/Users/macbook/Desktop/96-97-2/Data\ Analysis/timss15_grade_8")

read_xlsx("T15_G8_ItemInformation.xlsx", sheet = "MAT") -> info_m
read_xlsx("T15_G8_ItemInformation.xlsx", sheet = "SCI") -> info_s
info <- bind_rows(info_m, info_s)

read_xlsx("T15_G8_Codebook.xlsx", sheet = "BSA") -> CB_BSA

read_rds("data/bsa.rds") -> BSA
read_rds("data/bst.rds") -> BST
read_rds("data/bts.rds") -> BTS
read_rds("data/btm.rds") -> BTM
read_rds("data/bcg.rds") -> BCG
read_rds("data/bsg.rds") -> BSG


BSA$itsex <- replace(BSA$itsex,BSA$itsex == 1, "F")
BSA$itsex <- replace(BSA$itsex,BSA$itsex == 2, "M")
BSA$itsex <- as.factor(BSA$itsex)


left_join(x = info,
          y =  CB_BSA %>% rename(`Item ID` = Variable),
          by = "Item ID") -> questions_info

questions_info_f <- questions_info
questions_info_f$`Value Scheme Detailed` <-
  as.factor(paste(
    questions_info$`Value Scheme Detailed`,
    "  /  ",
    questions_info$`Maximum Points`
  ))
questions_info_f$`Item ID` <- tolower(questions_info_f$`Item ID`)



questions_info_f %>%
  select(`Value Scheme Detailed`) %>%
  unique() %>%
  arrange(`Value Scheme Detailed`) %>%
  mutate(level = as.numeric(`Value Scheme Detailed`)) -> marking_scheme

questions_scheme_type <-
  questions_info_f %>% select(Question = `Item ID`, `Value Scheme Detailed`)

#write_csv(marking_scheme, "../HW/HW4/Problems/hw_04/Solutions/marking_scheme.csv")

hed <- function(data) {
  View(head(data, 10))
}

getMark <- function(questions, answers) {
  mark <- rep(NA, length(answers))
  questions_type <-
    as.numeric(
      left_join(questions,
                questions_scheme_type,
                by = "Question")$`Value Scheme Detailed`
    )
  full_marks <- rep(0, length(answers))
  result <- mark %>%
    replace(questions_type == 1 & answers == 4, 1) %>%
    replace(questions_type > 1 &
              questions_type <= 3 & answers == 3, 1) %>%
    replace(questions_type > 3 &
              questions_type <= 6 & answers == 2, 1) %>%
    replace(questions_type > 6 &
              questions_type <= 10 & answers == 1, 1) %>%
    
    replace(questions_type == 1 & answers != 4, 0) %>%
    replace(questions_type > 1 &
              questions_type <= 3 & answers != 3, 0) %>%
    replace(questions_type > 3 &
              questions_type <= 6 & answers != 2, 0) %>%
    replace(questions_type > 6 &
              questions_type <= 10 & answers != 1, 0) %>%
    
    replace(questions_type > 10 &
              questions_type <= 23 &
              answers <= 20 & answers >= 10,
            1) %>%
    replace(questions_type > 10 &
              questions_type <= 23 &
              answers <= 80 & answers >= 70,
            0) %>%
    
    replace(questions_type > 23 &
              questions_type <= 27 &
              answers < 20 & answers >= 10,
            0.5) %>%
    replace(questions_type > 23 &
              questions_type <= 27 &
              answers <= 22 & answers >= 20,
            1) %>%
    replace(questions_type > 23 &
              questions_type <= 27 &
              answers <= 80 & answers >= 70,
            0) %>%
    
    replace(questions_type == 28 &
              answers == 10,
            0.5) %>%
    replace(questions_type == 28 &
              answers == 20,
            1) %>%
    replace(questions_type == 28 &
              answers == 79,
            0) %>%
    
    replace(questions_type == 29 &
              answers == 10,
            0.5) %>%
    replace(questions_type == 29 &
              answers == 20,
            1) %>%
    replace(questions_type == 29 &
              answers == 79,
            0)
  weights <- left_join(questions,
                            questions_info_f %>% select(Question = `Item ID`, weight = `Maximum Points`),
                            by = "Question")$weight
  ret <- data.frame(cbind(score = result, weight = weights))
  ret$weight <- as.numeric(ret$weight)
  ret$score <- as.numeric(ret$score)
  return(ret)
}
