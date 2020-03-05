# Intro to R
# Rick Scavetta
# 04.03.2020
# IMPRS @ MPI MolGen workshop

# Clear workspace
rm(list = ls())

# Load some packages
# (install them first! but only once)
library(tidyverse)

# Basic R syntax
n <- log2(8) # the log2 of 8
n # shortcut to print(n)

# PlantGrowth Case Study ----

# A built-in data set
data(PlantGrowth)

# Descriptive stats
# The "global mean", i.e. ANOVA Null hypothesis
mean(PlantGrowth$weight)

# group-wise stats (with dplyr)
# using tidyverse notation
# %>% "the pipe operator"
# say: "... and then ..."
PlantGrowth %>% 
  group_by(group) %>% 
  summarise(avg = mean(weight),
            stdev = sd(weight))

# Data vis (with ggplot2)
# 3 essential components
# 1 - The data
# 2 - Aesthetics - mapping variables onto scales
# scales: x, y, color, size, shape, linetype
# 3 - Geometry - how the plot will look

# box plot
ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_boxplot()

# "dot plot"
ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_jitter(width = 0.25, alpha = 0.5)

# Q-Q plot:
ggplot(PlantGrowth, aes(sample = weight)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  facet_wrap(~ group, scales = "free_y")

# quick & dirty, but not separated
# qqnorm(PlantGrowth$weight)
# qqline(PlantGrowth$weight, col = "red")

# Inferential stats
# first step: define a linear model
# ~ means "described by"
plant_lm <- lm(weight ~ group, data = PlantGrowth)
plant_lm

# T-tests
# Typically, use t.test(), but here, we can use:
summary(plant_lm) # p-values are labelled Pr(>|t|)

# 1-way ANOVA
anova(plant_lm) # p-value 0.01591

# For all pair-wise comparisons: use 
# plant_aov <- aov(weight ~ group, data = PlantGrowth)
# TukeyHSD(plant_aov)

# Element 2: Functions (verbs) ----
# Everything that happens, is because of a function

# Arithmetic operators
34 + 6

# BEDMAS - order of operations
# brackets, expon, div, mult, add, sub
2 - 3/4 # 1.25
(2 - 3)/4 # -0.25

# Use objects in place of numbers
n <- 34
p <- 6
# so...
n + p

# Generic functions have a form:
# fun_name(fun_arg = ...)

# Call args by name or position

# i.e.
log(x = 8, base = 2) # long form, naming
log(8, 2) # long form, positional matching
log(8, base = 2) # common: mix naming and position
log2(8) # short form, positional matching
log(base = 2, 8) # works, but confusing!

# Funs can have 0 to many un-named args
ls()
# Args can be named or un-named

# e.g. combine
xx <- c(3, 8, 9, 23)
xx

myNames <- c("healthy", "tissue", "quantity")
myNames

# How is + a function?
p + n
# this is actually...
`+`(p, n)

# A regular sequence of numbers
seq(from = 1, to = 100, by = 7)
# typically:
foo1 <- seq(1, 100, 7)
foo1

# use objects in functions:
foo2 <- seq(1, n, p)
foo2

# regular sequence of 1 interval
seq(1, 10, 1)
# Use the colon operator instead:
1:10

# Two major types of math functions:
# Aggregration functions
# 1 output value (typically)
# mean, sd, n, var, median, max, min, sum, prod

# Transformation functions
# same number of output as input
# log, [0,1], z-scores, sqrt, exponents
# subtract background
# +, -, /, ...

# Exercise: Are these transformation or aggregation?
foo2 + 100 # trans
foo2 + foo2 # trans
# foo2 %o% foo2 # trans
sum(foo2) + foo2 # agg, followed by trans
1:3 + foo2 # trans

# FUNDAMENTAL CONCEPT: VECTOR RECYCLING

1:4 + foo2

# 3 types of messages:
# Information: Neutral
# Warning: Possible problem
# Error: Full stop

# Exercise: Calculate y = 1.12x − 0.4 for xx
# i.e. y=mx+b
y <- (1.12 * xx) - 0.4

# Element 3: Objects (nouns) ----
# Anything that exists is an object

# Vectors - 1-dimensional, homogenous
# Everything in the values section
foo1
myNames

# 4 most common 
# "user-defined atomic vector types"
# Logical - TRUE/FALSE, T/F, 1/0 (Boolean)
# Integer - whole numbers
# Double - real numbers (float)
# Character - All values (string)

# Numeric - Generic reference to int or dbl

# check
typeof(foo1)
typeof(myNames)

foo3 <- c("Liver", "Brain", "Testes", "Muscle",
          "Intestine", "Heart")
typeof(foo3)

foo4 <- c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)
typeof(foo4)

# Homogenous types:
test <- c(1:10, "bob")
test
typeof(test)

# We can't do math:
mean(test)

# R has a type hierarchy

# Solution: Coercion to another type
# use an as.*() function
test <- as.numeric(test)
test

# Now we can do math: but deal with the NA
mean(test, na.rm = TRUE)

# Lists - 1-dimensional, heterogenous
typeof(plant_lm)

# how many elements:
length(plant_lm)
length(foo1) # also works for vectors

# attributes (meta data)
attributes(plant_lm)
# 13 named elements

# use common "accessor" functions for attributes
names(plant_lm)

# Anything that's named can be called with $
plant_lm$coefficients # a 3-element named dbl (numeric) vector
plant_lm$residuals # a 30-element dbl vector
plant_lm$model # dataframe

# Add comment as metadata:
comment(plant_lm) <- "I love R so much!"
attributes(plant_lm)

# Add comment as an actual list item:
plant_lm$myComment <- "But python not so much :/"
plant_lm$myComment

# What is class?
# An attribute to an object
attributes(plant_lm)
# can also access with "accessor" function:
class(plant_lm)
# class tells R functions what to do with this object
# e.g.
summary(plant_lm) # get t-test and ANOVA summary from an "lm"
summary(PlantGrowth) # summarise each column in a "dataframe"

# Dataframes - 2-dimensional, heterogenous
class(PlantGrowth)
# A special class of type list...
typeof(PlantGrowth)
# ...where each element is a vector of the SAME length!
# Rows = observations
# Columns = variables

# Make a data frame from scratch:
foo_df <- data.frame(foo4, foo3, foo2)
foo_df

# update metadata (names)
names(foo_df) <- myNames
foo_df

# Call each variable by name:
foo_df$quantity # as a vector

# Basic functions:
str(foo_df) # structure
glimpse(foo_df)
summary(foo_df)

# also...
dim(foo_df)
nrow(foo_df)
ncol(foo_df)