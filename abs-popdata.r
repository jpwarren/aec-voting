library(readxl)
library(readr)
library(reshape2)

url <- "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&32180ds0003_2006-16.xls&3218.0&Data%20Cubes&53380EA63C189F99CA25816A00175EBD&0&2016&28.07.2017&Latest"
destfile <- "abs-pop-division.xls"
download.file(url, destfile)

# The data we want is in Table 1
popdata <- read_excel(destfile, sheet='Table 1', skip=8, range="A10:M159",
                      col_names = c("CED_Code", "CED_Name",
                                    "2006",
                                    "2007",
                                    "2008",
                                    "2009",
                                    "2010",
                                    "2011",
                                    "2012pr",
                                    "2013pr",
                                    "2014pr",
                                    "2015pr",
                                    "2016pr"))
#View(popdata)

# Load some data created with TableBuilder

age5p <- read_csv("2016-age5p-by-ced.csv", skip = 1,
                  col_names = c("What", "Citizenship","CED","Age5P","Count"), col_types="cccci")
age15 <- read_csv("2016-agep-15-30-by-ced.csv", skip = 1,
                  col_names = c("What", "Citizenship","CED","Age","Count"), col_types="cccci")

# pivot to CED by Age5P layout
df_age5p <- recast(age5p, CED ~ Age5P)
df_age <- recast(age15, CED ~ Age)

# Build the youth counts
df_age$`16-17` <- df_age$`16` + df_age$`17`
df_age$`18-19` <- df_age$`18` + df_age$`19`

df_youth <- subset(df_age, select = c("CED", "16-17", "18-19"))
df_youth$`20-24` <- df_age5p$`20-24 years`
df_youth$`25-29` <- df_age5p$`25-29 years`

# Join the two frames on CED
df_merge <- merge(df_youth, divisionelectorcount, by.x = "CED", by.y = "X1")

# Now subtract the enrolled per CED from the estimated population
df_merge$`16-17.missing` <- df_merge$`16-17.x` - df_merge$`16-17.y`
df_merge$`18-19.missing` <- df_merge$`18-19.x` - df_merge$`18-19.y`
df_merge$`20-24.missing` <- df_merge$`20-24.x` - df_merge$`20-24.y`
df_merge$`25-29.missing` <- df_merge$`25-29.x` - df_merge$`25-29.y`

# Add some totals for each CED
df_merge$youth.total <- apply(subset(df_merge, select=c("16-17.x", "18-19.x", "20-24.x")), 1, sum)
df_merge$youth.missing <- apply(subset(df_merge, select=c("16-17.missing", "18-19.missing", "20-24.missing")), 1, sum)

# Calculate some rates
df_merge$`16-17.rate` <- df_merge$`16-17.missing` / df_merge$`16-17.x`
df_merge$`18-19.rate` <- df_merge$`18-19.missing` / df_merge$`18-19.x`
df_merge$`20-24.rate` <- df_merge$`20-24.missing` / df_merge$`20-24.x`
df_merge$`25-29.rate` <- df_merge$`25-29.missing` / df_merge$`25-29.x`
df_merge$youth.rate <- df_merge$youth.missing / df_merge$youth.total

# Plonk this data in a separate frame
df_missing = subset(df_merge, select = c("CED",
                                         "16-17.missing", "16-17.rate",
                                         "18-19.missing", "18-19.rate",
                                         "20-24.missing", "20-24.rate",
                                         "25-29.missing", "25-29.rate",
                                         "youth.missing", "youth.rate"
                                         ))

# where are the most unenrolled young people?
df_missing_sorted <- df_missing[order(-df_missing$`18-19.missing`),]
View(df_missing_sorted)

# Testing ERP population estimates from data at: https://docs.google.com/spreadsheets/d/1XtR6AJJdZQRQhb3bGeF7ftiLPwqOeVIJ-DcyTxZ4Guo/edit#gid=1344531317 
# code from: https://pastebin.com/k8pzEdhE
#erpced <- read_csv("ERP-by-CED-data.csv") #, skip = 1,
                  #col_names = c("", "Citizenship","CED","Age5P","Count"), col_types="cccci")
                  
#df_erpced = recast(subset(erpced, select=c("CED", "AGEP", "Citizens_ERP")), CED ~ AGEP)
#df_erpced <- df_erpced[order(df_erpced$CED),]
