# =====   Data Saints  =====

# ______
# '-._   ```"""---.._
#            ,-----.:___           `\  ,;;;,
#             '-.._     ```"""--.._  |,%%%%%%              _
#             ,    '.              `\;;;;  -\      _    _.'/\
#           .' `-.__ \            ,;;;;" .__{=====/_)==:_  ||
#   ,===/        ```";,,,,,,,;;;;;'`-./.____,'/ /     '.\/
#     '---/              ';;;;;;;;'      `--.._.' /
#     ,===/                          '-.        `\/
#   '---/                            ,'`.        |
#      ;                        __.-'    \     ,'
#      \______,,.....------'''``          `---`
#     

#  GROUP COMPONENTS

# Fynn Daniel - 
# Ettari Adriano - 
# Formicola Roberto - trarez@live.it


# ===== PROBLEM  1 =====

# Pre-Processing & Data Analysis

setwd("C:/Users/TraRe/OneDrive/Desktop/Magistrale/Econometrics/ProblemSet1")
library(tidyverse)

file <- "ps1.dat"
dat <- read_table(file)


## Problem 1: Part 1.1

# Number of NA per variable

a <- list()
count <- 0

for (sample in dat) {
  countNA <- sum(is.na(sample))
  count <- count + 1
  a[count] <- countNA
} 

a

# The list A contains number of NAs per each variable


# Number of NAs per sample (row)
{
dat$SampleNAPercentages <- rowSums(is.na(dat))/length(dat)*100

dat2 <- subset(dat, dat$SampleNAPercentages < 3)
}

# Exclude all variables whose name begins with J or j
#A ssign the filtered data to a new data.frame called dat2

dat2 <- dat2[, !startsWith(names(dat2), c('\"J', '\"j'))]



## Problem 1: Part 1.2

# Set the random seed
set.seed(42) 

#create the subsample and name it "dat3"
dat3 <- sample(dat2, 500, replace = FALSE)

#export the newly created dataframe
write.csv(dat3, "dataset-3.csv")


## Problem 1: Part 1.3

saveRDS(dat2, "datset-2.RData")

# Produce frequency tables

GR <- dat2['"GR"']
unique(GR)
GR_table <- table(GR)
barplot(GR_table, ylab = "Level Frequency")


FU01 <- dat2['"FU01"']
FU01_table <- table(FU01)
barplot(FU01_table, ylab = "Level Frequency")


# FU02 is an ordinal variable with the 
# following ordering over the levels: Q2 < Q1.

FU02 <- dat2['"FU02"']
FU02_table <- table(FU02)
barplot(FU02_table, ylab = "Level Frequency")


# As already shown, the variables format is chr (characters)


## Problem 1: Part 1.4

# Consider the variables whose name begins with UT
UT_df <- dat2[,  startsWith(names(dat2), '"UT')]
UT_df <- as.data.frame(UT_df)

# IDK because the professor does not specify, but we need to substitute the NAs (I think)
UT_df[is.na(UT_df)] <- 0

# Compute their correlation matrix
cor(UT_df)

# Show pairwise scatterplots conditional on GR
UT_df$GR <- GR



## Problem 1: Part 1.5

# Compute...
#          Median Absolute Deviation
apply(UT_df, MARGIN = 2, FUN = mad)
#          Arithmetic Mean
apply(UT_df, MARGIN = 2, FUN = mean)
#          Median
apply(UT_df, MARGIN = 2, FUN = median)
#          Standard Deviation
apply(UT_df, MARGIN = 2, FUN = sd)

## Problem 1: Part 1.6


# Use the functions tapply() and by()
# and use them to estimate the median 
# of the UT* variables conditional on GR

?tapply

# ===== PROBLEM  2 =====
