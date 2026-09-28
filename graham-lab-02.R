
  #### Set-up ####
library(tidyverse)
library(gt)
library(psych)
install.packages("readxl")
install.packages("janitor")
library(readxl)
library(dplyr)
library(janitor)

readxl::read_excel("Attachment_Anxiety_Data.xlsx", sheet = 1)

#### Mutating Data ####
attachment_anxiety.df <- my_data |> 
  mutate(Gender = case_when(Gender == 1 ~ "Male", 
                                 Gender == 2 ~ "Female",
                                TRUE ~ NA_character_),
         `Age group` = case_when(`Age group` == 1 ~ "18-24",
                                 `Age group` == 2 ~ "24-34",
                                 `Age group` == 3 ~ "35-44",
                                 `Age group` == 4 ~ "45-54",
                                 `Age group` == 5 ~ "55-64",
                                 TRUE ~ NA_character_),
         SA_1 = as.integer(SA_1),
         SA_2 = as.integer(SA_2),
         SA_3 = as.integer(SA_3),
         SA_4 = as.integer(SA_4),
         SA_5 = as.integer(SA_5),
         SA_6 = as.integer(SA_6),
         SA_7 = as.integer(SA_7),
         SA_8 = as.integer(SA_8),
         SA_9 = as.integer(SA_9),
         SA_10 = as.integer(SA_10),
         SA_11 = as.integer(SA_11),
         SA_12 = as.integer(SA_12),
         SA_13 = as.integer(SA_13),
         SA_14 = as.integer(SA_14),
         SA_15 = as.integer(SA_15),
         SA_16 = as.integer(SA_16),
         SA_17 = as.integer(SA_17),
         SA_18 = as.integer(SA_18),
         SA_19 = as.integer(SA_19),
         SA_20 = as.integer(SA_20),
         AA_1 = as.integer(AA_1),
         AA_2 = as.integer(AA_2),
         AA_3 = as.integer(AA_3),
         AA_4 = as.integer(AA_4),
         AA_5 = as.integer(AA_4),
         AA_6 = as.integer(AA_6),
         AA_7 = as.integer(AA_7),
         AA_8 = as.integer(AA_8),
         AA_9 = as.integer(AA_9),
         SEst_1 = as.integer(SEst_1),
         SEst_2 = as.integer(SEst_2),
         SEst_3 = as.integer(SEst_3),
         SEst_4 = as.integer(SEst_4),
         SEst_5 = as.integer(SEst_5),
         SEst_6 = as.integer(SEst_6),
         SEst_7 = as.integer(SEst_7),
         SEst_8 = as.integer(SEst_8),
         SEst_9 = as.integer(SEst_9),
         SEst_10 = as.integer(SEst_10))

#### Scoring Scales ####
attachment_anxiety_clean.df <- attachment_anxiety.df
v_keys <- list(
  Social_Interaction_Anxiety = c("SA_1", "SA_2", "SA_3", "SA_4", "-SA_5", "SA_6", "SA_7", "SA_8", "-SA_9", "SA_10", "-SA_11", "SA_12", "SA_13", "SA_14", "SA_15", "SA_16", "SA_17", "SA_18", "SA_19", "SA_20"),
  Close_Relationships_Anxiety = c("AA_1", "AA_2", "AA_3", "AA_4", "AA_5", "AA_6", "AA_7", "AA_8", "AA_9"),
  Self_Esteem = c("SEst_1", "-SEst_2", "SEst_3", "SEst_4", "-SEst_5", "-SEst_6", "SEst_7", "-SEst_8","-SEst_9", "SEst_10")) 

  SA_scores <- scoreItems(
    v_keys["Social_Interaction_Anxiety"],
    attachment_anxiety_clean.df,
    totals = F,
    min = 0,
    max = 4
  )

AA_scores <- scoreItems(
  v_keys["Close_Relationships_Anxiety"],
  attachment_anxiety_clean.df,
  totals = F,
  min = 1,
  max = 7)

SE_scores <- scoreItems(
  v_keys["Self_Esteem"],
  attachment_anxiety_clean.df,
  totals = F,
  min = 1,
  max = 4)
  
SA_scores.df <- as.data.frame(SA_scores$scores)
AA_scores.df <- as.data.frame(AA_scores$scores)
SE_scores.df <- as.data.frame(SE_scores$scores)
allscores.df <- cbind(SA_scores.df, AA_scores.df, SE_scores.df)

#### Combining Data Frames ####
demographics_and_scores.df <- cbind(allscores.df, attachment_anxiety_clean.df) |> 
select(Social_Interaction_Anxiety, Close_Relationships_Anxiety, Self_Esteem, Gender, `Age group`, Relationship, Ethnicity) |> 
  mutate(Social_Interaction_Anxiety = as.integer(Social_Interaction_Anxiety), Close_Relationships_Anxiety = as.integer(Close_Relationships_Anxiety), Self_Esteem = as.integer(Self_Esteem))

#### Demographics Table ####
demographics.table <- bind_rows(demographics_and_scores.df |> 
    tabyl(Gender) |> 
    transmute(
      Variable = "Gender",
      Category = Gender,
      n,
      Percent = percent),
  
  demographics_and_scores.df |> 
    tabyl(`Age group`) |> 
    transmute(
      Variable = "Age Group",
      Category = `Age group`,
      n,
      Percent = percent)) |> 
  
  gt() |> 
  fmt_percent(
    columns = Percent,
    decimals = 1) |> 
  tab_header(
    title = "Participant Demographics")

#### Score Table ####
Summary <- demographics_and_scores.df |> 
  select(Social_Interaction_Anxiety,
         Close_Relationships_Anxiety,
         Self_Esteem) |> 
  describe()

  N <- nrow(demographics_and_scores.df)
  cat("The extracted sample size is N =", N)

  Summary <- Summary |> 
    mutate(Scales = c(
      "Social Interaction Anxiety",
      "Close Relationships Anxiety",
      "Self-Esteem")) |> 
    mutate(alphas = round(c(SA_scores$alpha,
                      AA_scores$alpha,
                      SE_scores$alpha), 2)) |> 
    select(Scales, mean, median, sd, range, alphas) 
    
   Score_Table <- Summary |> 
    gt() |> 
     fmt_number(
       columns = c(mean, median, sd, range, alphas),
       decimals = 2) |> 
     cols_align(
       align = "right",
       columns = c(mean, median, sd, range, alphas)) |> 
     tab_header(title = "Attachment Anxiety Descriptive Statistics")

#### Score Table ####
   female.df <- demographics_and_scores.df |> 
     filter(Gender == "Female")
   
   Filtered_Summary <- female.df |> 
     select(Social_Interaction_Anxiety,
            Close_Relationships_Anxiety,
            Self_Esteem) |> 
     describe()
   
   Filtered_N <- nrow(female.df)
   cat("The extracted sample size is N =", N)
   
   Filtered_Summary <- Filtered_Summary |> 
     mutate(Scales = c(
       "Social Interaction Anxiety",
       "Close Relationships Anxiety",
       "Self-Esteem")) |> 
     mutate(alphas = round(c(SA_scores$alpha,
                             AA_scores$alpha,
                             SE_scores$alpha), 2)) |> 
     select(Scales, mean, median, sd, range, alphas) 
   
   Filtered_Score_Table <- Filtered_Summary |> 
     gt() |> 
     fmt_number(
       columns = c(mean, median, sd, range, alphas),
       decimals = 2) |> 
     cols_align(
       align = "right",
       columns = c(mean, median, sd, range, alphas)) |> 
     tab_header(title = "Female Attachment Anxiety Descriptive Statistics")
   