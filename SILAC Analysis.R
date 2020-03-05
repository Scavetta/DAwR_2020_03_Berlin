# SILAC Analysis
# Rick Scavetta
# 04.03.2020
# Protein profiles during myocardial cell differentiation

# Clear workspace
rm(list = ls())

# Load packages
library(tidyverse)

# Part 0: Import data ----
protein.df <- read.delim("Protein.txt")

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