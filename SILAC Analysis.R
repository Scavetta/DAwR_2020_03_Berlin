# SILAC Analysis
# Rick Scavetta
# 04.03.2020
# Protein profiles during myocardial cell differentiation

# Clear workspace
rm(list = ls())

# Load packages
library(tidyverse)
library(glue)

# Part 0: Import data ----
protein.df <- read.delim("Protein.txt", stringsAsFactors = FALSE)

# Examine the data:
glimpse(protein.df)
head(protein.df)
tail(protein.df)
summary(protein.df)

# Quantify the contaminants
protein.df %>% 
  count(Contaminant)
summary(protein.df$Contaminant)
table(protein.df$Contaminant)

protein.df %>% 
  filter(Contaminant == "+") %>% 
  nrow()

sum(protein.df$Contaminant == "+")

# Proportion or percentage
sum(protein.df$Contaminant == "+")/nrow(protein.df)*100

# Remove contaminants
protein.df <- protein.df %>% 
  as_tibble() %>% 
  filter(Contaminant != "+") 


# Part 1: Transformations & cleaning data ----

# log 10 transformations of the intensities
protein.df$Intensity.H <- log10(protein.df$Intensity.H)
protein.df$Intensity.M <- log10(protein.df$Intensity.M)
protein.df$Intensity.L <- log10(protein.df$Intensity.L)

# Add the intensities
protein.df$Intensity.H.M <- protein.df$Intensity.H + protein.df$Intensity.M
protein.df$Intensity.M.L <- protein.df$Intensity.M + protein.df$Intensity.L

# log2 transformations of the ratios
protein.df$Ratio.H.M <- log2(protein.df$Ratio.H.M)
protein.df$Ratio.M.L <- log2(protein.df$Ratio.M.L)

# Part 2: Query data using filter() ----
# Exercise 9.2 (Find protein values)
query <- c("GOGA7", "PSA6", "S10AB")

# Add _MOUSE to each value in the query
# New method, nice :)
query1 <- glue("{query}_MOUSE")

# old school, it also works fine
query2 <- paste0(query, "_MOUSE")

# Some further examples of vector recycling:
query3 <- paste0(query, c("_MOUSE", "_HORSE", "_DONKEY"))
query4 <- paste0(query, rep(c("_MOUSE", "_HORSE", "_DONKEY"), each = 3))

protein.df %>% 
  filter(Uniprot %in% query1) %>% 
  select(Uniprot, Ratio.H.M, Ratio.M.L)

# Exercise 9.3 (Find significant hits) and 10.2
# For the H/M ratio column, create a new data 
# frame containing only proteins that have 
# a p-value less than 0.05
protein.df %>% 
  filter(Ratio.H.M.Sig < 0.05) %>% 
  select(Uniprot, Ratio.H.M, Ratio.H.M.Sig) -> protein.H.M.Sig

protein.H.M.Sig_2 <- protein.df[protein.df$Ratio.H.M.Sig < 0.05 & 
                                  !is.na(protein.df$Ratio.H.M.Sig), 
                                c("Uniprot", "Ratio.H.M", "Ratio.H.M.Sig")]

# Exercise 10.4 (Find top 20 values) 


# Exercise 10.5 (Find intersections)

