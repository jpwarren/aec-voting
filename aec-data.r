library(readr)
library(plyr)
elector_count_6_2017 <- read_csv("http://www.aec.gov.au/Enrolling_to_vote/Enrolment_stats/elector_count/2017/elector-count-6-2017.csv",
                                 skip = 9)
elector_count_6_2017<-elector_count_6_2017[,1:15]
elector_count_6_2017<-elector_count_6_2017[!is.na(elector_count_6_2017$X1),]
View(elector_count_6_2017)

#
elector_count_9_2016 <- read_csv("http://www.aec.gov.au/Enrolling_to_vote/Enrolment_stats/elector_count/2016/elector-count-9-2016.csv",
                                 skip = 9)
elector_count_9_2016<-elector_count_9_2016[,1:15]
elector_count_9_2016<-elector_count_9_2016[!is.na(elector_count_9_2016$X1),]

division_enrol_rate_3_2017 <- read_csv("http://www.aec.gov.au/Enrolling_to_vote/Enrolment_stats/rate-div/files/2017-03-div-enrol-rate.csv",
                                       skip = 1)
division_enrol_rate_3_2017 <- head(division_enrol_rate_3_2017, -2)
division_enrol_rate_3_2017$rate_perc <- substr(division_enrol_rate_3_2017$Enrolment_Rate,0,2)
division_enrol_rate_3_2017$rate_perc <- as.numeric(substr(division_enrol_rate_3_2017$Enrolment_Rate,0,2)) /100
#division_enrol_rate_3_2017$Enrolment_Rate <- as.factor(division_enrol_rate_3_2017$Enrolment_Rate)
View(division_enrol_rate_3_2017)

division_enrol_lookup <- division_enrol_rate_3_2017$Division

state<-c('NSW','VIC','QLD','SA','WA','TAS','NT','ACT')
stateelectors<-elector_count_6_2017[elector_count_6_2017$X1 %in% state,]
View(stateelectors)

#divisionelectorcount<-elector_count_6_2017[!(elector_count_6_2017$X1 %in% state),]
divisionelectorcount<-elector_count_9_2016[!(elector_count_9_2016$X1 %in% state),]

divisionelectorcount<-divisionelectorcount[divisionelectorcount$X1 != "Male",]
divisionelectorcount<-divisionelectorcount[divisionelectorcount$X1 != "Female",]
divisionelectorcount<-divisionelectorcount[divisionelectorcount$X1 != "Indeterminate/Unknown",]
indx = !(colnames(divisionelectorcount) %in% 'X1')
# Numbers have ',' as thousands separator, which we need to get rid of
divisionelectorcount[indx] <- lapply(divisionelectorcount[indx], function(x) as.numeric(gsub(",","",x)))
# Get rid of totals row
divisionelectorcount <- head(divisionelectorcount, -1)
# Sort by CED
divisionelectorcount <- divisionelectorcount[order(divisionelectorcount$X1),]

View(divisionelectorcount)

# Estimate missing electors by age range
# Assumes unenrolment is constant for all age cohorts in a division, which is wrong,
# but we don't know the per-age cohort enrolment rates (that's what we're trying to work out)
divrates <- division_enrol_rate_3_2017$rate_perc[match(divisionelectorcount$X1, division_enrol_rate_3_2017$Division)]

unenrolled = divisionelectorcount
# columns that aren't the division name
indx = !(colnames(unenrolled) %in% 'X1')
# Numbers have ',' as thousands separator, which we need to get rid of
unenrolled[indx] <- lapply(unenrolled[indx], function(x) as.numeric(gsub(",","",x)))
# Given the enrol rate: r and the total enrolled: X, missing = X(1/r - 1)
unenrolled[indx] <- lapply(unenrolled[indx], function(x) x * (1/divrates - 1) )
# per-cohort Totals
#unenrolled <- head(unenrolled, -1)
#rbind(unenrolled, c('Grand Total'), lapply(unenrolled[indx], function(x) sum(x, na.rm=TRUE)))

# Show the top 20 electorates by unenrolled 16-24 year olds
head(unenrolled[order(-unenrolled$`16-24`),]$X1, 20)
