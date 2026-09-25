---
  title: "9041 Assignment 2"
author: Alana Graham
date: September 25, 2026
format: 
  html:
  toc: true
code-fold: true
code-summary: "Show code"
df-print: paged
execute:
  echo: false
warning: false
---
  ```{r, Getting Started} ```
  #### Getting started ####
library(tidyverse)
library(gt)
library(psych)
install.packages(“readxl”)
install.packages(“janitor”)
install.packages("readxl")
library(readxl)
library(dplyr)

readxl::read_excel("Attachment_Anxiety_Data.xlsx", sheet = 1)
attachment_anxiety.df <- my_data

```{r, Cleaning Data} ```

attachment_anxiety.df |> 
  mutate(Gender_char = case_when(Gender == 1 ~ "Male", 
                                 Gender == 2 ~ "Female"))
