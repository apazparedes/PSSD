library(lmtest)
library(sandwich)
library(multiwayvcov)
library(foreign)
library(plm)
library(philentropy)
library(clubSandwich)
library(KernSmooth)
library(dplyr)
library(ggplot2)
library(xtable)
library(causalweight)

data <- read.csv("~/data_final.txt", sep=";")

signif.num <- function(x) {
  symnum(x, corr = FALSE, na = FALSE, legend = FALSE,
         cutpoints = c(0, 0.01, 0.05, 0.1, 1), 
         symbols = c("***", "**", "*", " "))
}

data$page_count = as.numeric(data$page_count)
data$word_count = as.numeric(data$word_count)
data$sexual  = as.numeric(data$sexual)     
data$factordate = as.factor(data$date)

data$feb2015 = 0
data$mar2015 = 0
data$apr2015 = 0
data$may2015 = 0
data$jun2015 = 0
data$jul2015 = 0
data$aug2015 = 0
data$sep2015 = 0
data$oct2015 = 0
data$nov2015 = 0
data$dez2015 = 0
data$jan2015[data$date == " 2015-01"] = 1
data$feb2015[data$date == " 2015-02"] = 1
data$mar2015[data$date == " 2015-03"] = 1
data$apr2015[data$date == " 2015-04"] = 1
data$may2015[data$date == " 2015-05"] = 1
data$jun2015[data$date == " 2015-06"] = 1
data$jul2015[data$date == " 2015-07"] = 1
data$aug2015[data$date == " 2015-08"] = 1
data$sep2015[data$date == " 2015-09"] = 1
data$oct2015[data$date == " 2015-10"] = 1
data$nov2015[data$date == " 2015-11"] = 1
data$dez2015[data$date == " 2015-12"] = 1

data$jan2016 = 0
data$feb2016 = 0
data$mar2016 = 0
data$apr2016 = 0
data$may2016 = 0
data$jun2016 = 0
data$jul2016 = 0
data$aug2016 = 0
data$sep2016 = 0
data$oct2016 = 0
data$nov2016 = 0
data$dez2016 = 0
data$jan2016[data$date == " 2016-01"] = 1
data$feb2016[data$date == " 2016-02"] = 1
data$mar2016[data$date == " 2016-03"] = 1
data$apr2016[data$date == " 2016-04"] = 1
data$may2016[data$date == " 2016-05"] = 1
data$jun2016[data$date == " 2016-06"] = 1
data$jul2016[data$date == " 2016-07"] = 1
data$aug2016[data$date == " 2016-08"] = 1
data$sep2016[data$date == " 2016-09"] = 1
data$oct2016[data$date == " 2016-10"] = 1
data$nov2016[data$date == " 2016-11"] = 1
data$dez2016[data$date == " 2016-12"] = 1

data$jan2017 = 0
data$feb2017 = 0
data$mar2017 = 0
data$apr2017 = 0
data$may2017 = 0
data$jun2017 = 0
data$jul2017 = 0
data$aug2017 = 0
data$sep2017 = 0
data$oct2017 = 0
data$nov2017 = 0
data$dez2017 = 0
data$jan2017[data$date == " 2017-01"] = 1
data$feb2017[data$date == " 2017-02"] = 1
data$mar2017[data$date == " 2017-03"] = 1
data$apr2017[data$date == " 2017-04"] = 1
data$may2017[data$date == " 2017-05"] = 1
data$jun2017[data$date == " 2017-06"] = 1
data$jul2017[data$date == " 2017-07"] = 1
data$aug2017[data$date == " 2017-08"] = 1
data$sep2017[data$date == " 2017-09"] = 1
data$oct2017[data$date == " 2017-10"] = 1
data$nov2017[data$date == " 2017-11"] = 1
data$dez2017[data$date == " 2017-12"] = 1

data$jan2018 = 0
data$feb2018 = 0
data$mar2018 = 0
data$apr2018 = 0
data$may2018 = 0
data$jun2018 = 0
data$jul2018 = 0
data$aug2018 = 0
data$sep2018 = 0
data$oct2018 = 0
data$nov2018 = 0
data$dez2018 = 0
data$jan2018[data$date == " 2018-01"] = 1
data$feb2018[data$date == " 2018-02"] = 1
data$mar2018[data$date == " 2018-03"] = 1
data$apr2018[data$date == " 2018-04"] = 1
data$may2018[data$date == " 2018-05"] = 1
data$jun2018[data$date == " 2018-06"] = 1
data$jul2018[data$date == " 2018-07"] = 1
data$aug2018[data$date == " 2018-08"] = 1
data$sep2018[data$date == " 2018-09"] = 1
data$oct2018[data$date == " 2018-10"] = 1
data$nov2018[data$date == " 2018-11"] = 1
data$dez2018[data$date == " 2018-12"] = 1

data$jan2019 = 0
data$feb2019 = 0
data$mar2019 = 0
data$apr2019 = 0
data$may2019 = 0
data$jun2019 = 0
data$jul2019 = 0
data$aug2019 = 0
data$sep2019 = 0
data$oct2019 = 0
data$nov2019 = 0
data$dez2019 = 0
data$jan2019[data$date == " 2019-01"] = 1
data$feb2019[data$date == " 2019-02"] = 1
data$mar2019[data$date == " 2019-03"] = 1
data$apr2019[data$date == " 2019-04"] = 1
data$may2019[data$date == " 2019-05"] = 1
data$jun2019[data$date == " 2019-06"] = 1
data$jul2019[data$date == " 2019-07"] = 1
data$aug2019[data$date == " 2019-08"] = 1
data$sep2019[data$date == " 2019-09"] = 1
data$oct2019[data$date == " 2019-10"] = 1
data$nov2019[data$date == " 2019-11"] = 1
data$dez2019[data$date == " 2019-12"] = 1

data$jan2020 = 0
data$feb2020 = 0
data$mar2020 = 0
data$apr2020 = 0
data$may2020 = 0
data$jun2020 = 0
data$jul2020 = 0
data$aug2020 = 0
data$sep2020 = 0
data$oct2020 = 0
data$nov2020 = 0
data$dez2020 = 0
data$jan2020[data$date == " 2020-01"] = 1
data$feb2020[data$date == " 2020-02"] = 1
data$mar2020[data$date == " 2020-03"] = 1
data$apr2020[data$date == " 2020-04"] = 1
data$may2020[data$date == " 2020-05"] = 1
data$jun2020[data$date == " 2020-06"] = 1
data$jul2020[data$date == " 2020-07"] = 1
data$aug2020[data$date == " 2020-08"] = 1
data$sep2020[data$date == " 2020-09"] = 1
data$oct2020[data$date == " 2020-10"] = 1
data$nov2020[data$date == " 2020-11"] = 1
data$dez2020[data$date == " 2020-12"] = 1

data$date[data$date == " 2015-01"] = 1
data$date[data$date == " 2015-02"] = 2
data$date[data$date == " 2015-03"] = 3
data$date[data$date == " 2015-04"] = 4
data$date[data$date == " 2015-05"] = 5
data$date[data$date == " 2015-06"] = 6
data$date[data$date == " 2015-07"] = 7
data$date[data$date == " 2015-08"] = 8
data$date[data$date == " 2015-09"] = 9
data$date[data$date == " 2015-10"] = 10
data$date[data$date == " 2015-11"] = 11
data$date[data$date == " 2015-12"] = 12
data$date[data$date == " 2016-01"] = 13
data$date[data$date == " 2016-02"] = 14
data$date[data$date == " 2016-03"] = 15
data$date[data$date == " 2016-04"] = 16
data$date[data$date == " 2016-05"] = 17
data$date[data$date == " 2016-06"] = 18
data$date[data$date == " 2016-07"] = 19
data$date[data$date == " 2016-08"] = 20
data$date[data$date == " 2016-09"] = 21
data$date[data$date == " 2016-10"] = 22
data$date[data$date == " 2016-11"] = 23
data$date[data$date == " 2016-12"] = 24
data$date[data$date == " 2017-01"] = 25
data$date[data$date == " 2017-02"] = 26
data$date[data$date == " 2017-03"] = 27
data$date[data$date == " 2017-04"] = 28
data$date[data$date == " 2017-05"] = 29
data$date[data$date == " 2017-06"] = 30
data$date[data$date == " 2017-07"] = 31
data$date[data$date == " 2017-08"] = 32
data$date[data$date == " 2017-09"] = 33
data$date[data$date == " 2017-10"] = 34
data$date[data$date == " 2017-11"] = 35
data$date[data$date == " 2017-12"] = 36
data$date[data$date == " 2018-01"] = 37
data$date[data$date == " 2018-02"] = 38
data$date[data$date == " 2018-03"] = 39
data$date[data$date == " 2018-04"] = 40
data$date[data$date == " 2018-05"] = 41
data$date[data$date == " 2018-06"] = 42
data$date[data$date == " 2018-07"] = 43
data$date[data$date == " 2018-08"] = 44
data$date[data$date == " 2018-09"] = 45
data$date[data$date == " 2018-10"] = 46
data$date[data$date == " 2018-11"] = 47
data$date[data$date == " 2018-12"] = 48
data$date[data$date == " 2019-01"] = 49
data$date[data$date == " 2019-02"] = 50
data$date[data$date == " 2019-03"] = 51
data$date[data$date == " 2019-04"] = 52
data$date[data$date == " 2019-05"] = 53
data$date[data$date == " 2019-06"] = 54
data$date[data$date == " 2019-07"] = 55
data$date[data$date == " 2019-08"] = 56
data$date[data$date == " 2019-09"] = 57
data$date[data$date == " 2019-10"] = 58
data$date[data$date == " 2019-11"] = 59
data$date[data$date == " 2019-12"] = 60
data$date[data$date == " 2020-01"] = 61
data$date[data$date == " 2020-02"] = 62
data$date[data$date == " 2020-03"] = 63
data$date[data$date == " 2020-04"] = 64
data$date[data$date == " 2020-05"] = 65
data$date[data$date == " 2020-06"] = 66
data$date[data$date == " 2020-07"] = 67
data$date[data$date == " 2020-08"] = 68
data$date[data$date == " 2020-09"] = 69
data$date[data$date == " 2020-10"] = 70
data$date[data$date == " 2020-11"] = 71

data$date = as.numeric(data$date)
data$q12015 = 0
data$q12015[data$date >=  2 & data$date <=4] = 1
data$q22015 = 0
data$q22015[data$date >=  5 & data$date <=7] = 1
data$q32015 = 0
data$q32015[data$date >=  8 & data$date <=10] = 1
data$q42015 = 0
data$q42015[data$date >=  11 & data$date <=13] = 1
data$q12016 = 0
data$q12016[data$date >=  14 & data$date <=16] = 1
data$q22016 = 0
data$q22016[data$date >=  17 & data$date <=19] = 1
data$q32016 = 0
data$q32016[data$date >=  20 & data$date <=22] = 1
data$q42016 = 0
data$q42016[data$date >=  23 & data$date <=25] = 1
data$q12017 = 0
data$q12017[data$date >=  26 & data$date <=28] = 1
data$q22017 = 0
data$q22017[data$date >=  29 & data$date <=31] = 1
data$q32017 = 0
data$q32017[data$date >=  32 & data$date <=34] = 1
data$q42017 = 0
data$q42017[data$date >=  35 & data$date <=37] = 1
data$q12018 = 0
data$q12018[data$date >=  38 & data$date <=40] = 1
data$q22018 = 0
data$q22018[data$date >=  41 & data$date <= 43] = 1
data$q32018 = 0
data$q32018[data$date >=  44 & data$date <=46] = 1
data$q42018 = 0
data$q42018[data$date >=  47 & data$date <=49] = 1
data$q12019 = 0
data$q12019[data$date >=  50 & data$date <=52] = 1
data$q22019 = 0
data$q22019[data$date >=  53 & data$date <=55] = 1
data$q32019 = 0
data$q32019[data$date >=  56 & data$date <=58] = 1
data$q42019 = 0
data$q42019[data$date >=  59 & data$date <=61] = 1
data$q12020 = 0
data$q12020[data$date >=  62 & data$date <=64] = 1
data$q22020 = 0
data$q22020[data$date >=  65 & data$date <=67] = 1
data$q32020 = 0
data$q32020[data$date >=  68 & data$date <=71] = 1


data$h22015 = 0
data$h22015[data$date >=  5 & data$date <=10] = 1
data$h12016 = 0
data$h12016[data$date >=  11 & data$date <=16] = 1
data$h22016 = 0
data$h22016[data$date >=  17 & data$date <=22] = 1
data$h12017 = 0
data$h12017[data$date >=  23 & data$date <=28] = 1
data$h22017 = 0
data$h22017[data$date >=  29 & data$date <=34] = 1
data$h12018 = 0
data$h12018[data$date >=  35 & data$date <=40] = 1
data$h22018 = 0
data$h22018[data$date >=  41 & data$date <=46] = 1
data$h12019 = 0
data$h12019[data$date >=  47 & data$date <=52] = 1
data$h22019 = 0
data$h22019[data$date >=  53 & data$date <=58] = 1
data$h12020 = 0
data$h12020[data$date >=  59 & data$date <=64] = 1
data$h22020 = 0
data$h22020[data$date >=  65 & data$date <=71] = 1


data$y2015 = 0
data$y2015[data$date >=  1 & data$date <=10] = 1
data$y2016 = 0
data$y2016[data$date >=  11 & data$date <=22] = 1
data$y2017 = 0
data$y2017[data$date >=  23 & data$date <=34] = 1
data$y2018 = 0
data$y2018[data$date >=  35 & data$date <=46] = 1
data$y2019 = 0
data$y2019[data$date >=  47 & data$date <=58] = 1
data$y2020 = 0
data$y2020[data$date >=  59 & data$date <=71] = 1

data$date = as.numeric(data$date)
data$after_treatment = 0
data$after_treatment[data$date > 34] = 1

mins <-aggregate(data[ ,"date"]  ,list(data$court) , min)
courtswithobsinh12015 <- as.vector(mins$Group.1[mins$x<7])
data = data[data$court %in% courtswithobsinh12015,]


helpdatabow_w = data[data$date <7,] %>%
  group_by(court) %>% 
  summarise(weighted_bow1 = weighted.mean(bow1, word_count),
            weighted_bow2 = weighted.mean(bow2, word_count),
            weighted_bow3 = weighted.mean(bow3, word_count),
            weighted_bow4 = weighted.mean(bow4, word_count),
            weighted_bow5 = weighted.mean(bow5, word_count),
            weighted_bow6 = weighted.mean(bow6, word_count),
            weighted_bow7 = weighted.mean(bow7, word_count),
            weighted_bow8 = weighted.mean(bow8, word_count),
            weighted_bow9 = weighted.mean(bow9, word_count),
            weighted_bow10 = weighted.mean(bow10, word_count),
            weighted_bow11 = weighted.mean(bow11, word_count),
            weighted_bow12 = weighted.mean(bow12, word_count),
            weighted_bow13 = weighted.mean(bow13, word_count),
            weighted_bow14 = weighted.mean(bow14, word_count),
            weighted_bow15 = weighted.mean(bow15, word_count),
            weighted_bow16 = weighted.mean(bow16, word_count),
            weighted_bow17 = weighted.mean(bow17, word_count),
            weighted_bow18 = weighted.mean(bow18, word_count),
            weighted_bow19 = weighted.mean(bow19, word_count),
            weighted_bow20 = weighted.mean(bow20, word_count),
            weighted_bow21 = weighted.mean(bow21, word_count),
            weighted_bow22 = weighted.mean(bow22, word_count),
            weighted_bow23 = weighted.mean(bow23, word_count),
            weighted_bow24 = weighted.mean(bow24, word_count),
            weighted_bow25 = weighted.mean(bow25, word_count),
            weighted_bow26 = weighted.mean(bow26, word_count),
            weighted_bow27 = weighted.mean(bow27, word_count),
            weighted_bow28 = weighted.mean(bow28, word_count),
            weighted_bow29 = weighted.mean(bow29, word_count),
            weighted_bow30 = weighted.mean(bow30, word_count),
            weighted_bow31 = weighted.mean(bow31, word_count),
            weighted_bow32 = weighted.mean(bow32, word_count),
            weighted_bow33 = weighted.mean(bow33, word_count),
            weighted_bow34 = weighted.mean(bow34, word_count),
            weighted_bow35 = weighted.mean(bow35, word_count),
            weighted_bow36 = weighted.mean(bow36, word_count),
            weighted_bow37 = weighted.mean(bow37, word_count),
            weighted_bow38 = weighted.mean(bow38, word_count),
            weighted_bow39 = weighted.mean(bow39, word_count),
            weighted_bow40 = weighted.mean(bow40, word_count),
            weighted_bow41 = weighted.mean(bow41, word_count),
            weighted_bow42 = weighted.mean(bow42, word_count),
            weighted_bow43 = weighted.mean(bow43, word_count),
            weighted_bow44 = weighted.mean(bow44, word_count),
            weighted_bow45 = weighted.mean(bow45, word_count),
            weighted_bow46 = weighted.mean(bow46, word_count),
            weighted_bow47 = weighted.mean(bow47, word_count),
            weighted_bow48 = weighted.mean(bow48, word_count),
            weighted_bow49 = weighted.mean(bow49, word_count),
            weighted_bow50 = weighted.mean(bow50, word_count),
            weighted_bow51 = weighted.mean(bow51, word_count),
            weighted_bow52 = weighted.mean(bow52, word_count),
            weighted_bow53 = weighted.mean(bow53, word_count),
            weighted_bow54 = weighted.mean(bow54, word_count),
            weighted_bow55 = weighted.mean(bow55, word_count),
            weighted_bow56 = weighted.mean(bow56, word_count),
            weighted_bow57 = weighted.mean(bow57, word_count),
            weighted_bow58 = weighted.mean(bow58, word_count),
            weighted_bow59 = weighted.mean(bow59, word_count),
            weighted_bow60 = weighted.mean(bow60, word_count),
            weighted_bow61 = weighted.mean(bow61, word_count),
            weighted_bow62 = weighted.mean(bow62, word_count),
            weighted_bow63 = weighted.mean(bow63, word_count),
            weighted_bow64 = weighted.mean(bow64, word_count))

helpdatabow_2_w = data[data$date <7,] %>%
  group_by(court) %>% 
  summarise(weighted_bow_2_1 = weighted.mean(bow_2_1, word_count),
            weighted_bow_2_2 = weighted.mean(bow_2_2, word_count),
            weighted_bow_2_3 = weighted.mean(bow_2_3, word_count),
            weighted_bow_2_4 = weighted.mean(bow_2_4, word_count),
            weighted_bow_2_5 = weighted.mean(bow_2_5, word_count),
            weighted_bow_2_6 = weighted.mean(bow_2_6, word_count),
            weighted_bow_2_7 = weighted.mean(bow_2_7, word_count),
            weighted_bow_2_8 = weighted.mean(bow_2_8, word_count),
            weighted_bow_2_9 = weighted.mean(bow_2_9, word_count),
            weighted_bow_2_10 = weighted.mean(bow_2_10, word_count),
            weighted_bow_2_11 = weighted.mean(bow_2_11, word_count),
            weighted_bow_2_12 = weighted.mean(bow_2_12, word_count),
            weighted_bow_2_13 = weighted.mean(bow_2_13, word_count),
            weighted_bow_2_14 = weighted.mean(bow_2_14, word_count),
            weighted_bow_2_15 = weighted.mean(bow_2_15, word_count),
            weighted_bow_2_16 = weighted.mean(bow_2_16, word_count),
            weighted_bow_2_17 = weighted.mean(bow_2_17, word_count),
            weighted_bow_2_18 = weighted.mean(bow_2_18, word_count),
            weighted_bow_2_19 = weighted.mean(bow_2_19, word_count),
            weighted_bow_2_20 = weighted.mean(bow_2_20, word_count),
            weighted_bow_2_21 = weighted.mean(bow_2_21, word_count),
            weighted_bow_2_22 = weighted.mean(bow_2_22, word_count),
            weighted_bow_2_23 = weighted.mean(bow_2_23, word_count),
            weighted_bow_2_24 = weighted.mean(bow_2_24, word_count),
            weighted_bow_2_25 = weighted.mean(bow_2_25, word_count),
            weighted_bow_2_26 = weighted.mean(bow_2_26, word_count),
            weighted_bow_2_27 = weighted.mean(bow_2_27, word_count),
            weighted_bow_2_28 = weighted.mean(bow_2_28, word_count),
            weighted_bow_2_29 = weighted.mean(bow_2_29, word_count),
            weighted_bow_2_30 = weighted.mean(bow_2_30, word_count),
            weighted_bow_2_31 = weighted.mean(bow_2_31, word_count),
            weighted_bow_2_32 = weighted.mean(bow_2_32, word_count),
            weighted_bow_2_33 = weighted.mean(bow_2_33, word_count),
            weighted_bow_2_34 = weighted.mean(bow_2_34, word_count),
            weighted_bow_2_35 = weighted.mean(bow_2_35, word_count),
            weighted_bow_2_36 = weighted.mean(bow_2_36, word_count),
            weighted_bow_2_37 = weighted.mean(bow_2_37, word_count),
            weighted_bow_2_38 = weighted.mean(bow_2_38, word_count),
            weighted_bow_2_39 = weighted.mean(bow_2_39, word_count),
            weighted_bow_2_40 = weighted.mean(bow_2_40, word_count),
            weighted_bow_2_41 = weighted.mean(bow_2_41, word_count),
            weighted_bow_2_42 = weighted.mean(bow_2_42, word_count),
            weighted_bow_2_43 = weighted.mean(bow_2_43, word_count),
            weighted_bow_2_44 = weighted.mean(bow_2_44, word_count),
            weighted_bow_2_45 = weighted.mean(bow_2_45, word_count),
            weighted_bow_2_46 = weighted.mean(bow_2_46, word_count),
            weighted_bow_2_47 = weighted.mean(bow_2_47, word_count),
            weighted_bow_2_48 = weighted.mean(bow_2_48, word_count),
            weighted_bow_2_49 = weighted.mean(bow_2_49, word_count),
            weighted_bow_2_50 = weighted.mean(bow_2_50, word_count),
            weighted_bow_2_51 = weighted.mean(bow_2_51, word_count),
            weighted_bow_2_52 = weighted.mean(bow_2_52, word_count),
            weighted_bow_2_53 = weighted.mean(bow_2_53, word_count),
            weighted_bow_2_54 = weighted.mean(bow_2_54, word_count),
            weighted_bow_2_55 = weighted.mean(bow_2_55, word_count),
            weighted_bow_2_56 = weighted.mean(bow_2_56, word_count),
            weighted_bow_2_57 = weighted.mean(bow_2_57, word_count),
            weighted_bow_2_58 = weighted.mean(bow_2_58, word_count),
            weighted_bow_2_59 = weighted.mean(bow_2_59, word_count),
            weighted_bow_2_60 = weighted.mean(bow_2_60, word_count),
            weighted_bow_2_61 = weighted.mean(bow_2_61, word_count),
            weighted_bow_2_62 = weighted.mean(bow_2_62, word_count),
            weighted_bow_2_63 = weighted.mean(bow_2_63, word_count),
            weighted_bow_2_64 = weighted.mean(bow_2_64, word_count),
            weighted_bow_2_65 = weighted.mean(bow_2_65, word_count))

helpdatatfidf_w = data[data$date < 7,] %>%
  group_by(court) %>% 
  summarise(weighted_tfidf1 = weighted.mean(tfidf1, word_count),
            weighted_tfidf2 = weighted.mean(tfidf2, word_count),
            weighted_tfidf3 = weighted.mean(tfidf3, word_count),
            weighted_tfidf4 = weighted.mean(tfidf4, word_count),
            weighted_tfidf5 = weighted.mean(tfidf5, word_count),
            weighted_tfidf6 = weighted.mean(tfidf6, word_count),
            weighted_tfidf7 = weighted.mean(tfidf7, word_count),
            weighted_tfidf8 = weighted.mean(tfidf8, word_count),
            weighted_tfidf9 = weighted.mean(tfidf9, word_count),
            weighted_tfidf10 = weighted.mean(tfidf10, word_count),
            weighted_tfidf11 = weighted.mean(tfidf11, word_count),
            weighted_tfidf12 = weighted.mean(tfidf12, word_count),
            weighted_tfidf13 = weighted.mean(tfidf13, word_count),
            weighted_tfidf14 = weighted.mean(tfidf14, word_count),
            weighted_tfidf15 = weighted.mean(tfidf15, word_count),
            weighted_tfidf16 = weighted.mean(tfidf16, word_count),
            weighted_tfidf17 = weighted.mean(tfidf17, word_count),
            weighted_tfidf18 = weighted.mean(tfidf18, word_count),
            weighted_tfidf19 = weighted.mean(tfidf19, word_count),
            weighted_tfidf20 = weighted.mean(tfidf20, word_count),
            weighted_tfidf21 = weighted.mean(tfidf21, word_count),
            weighted_tfidf22 = weighted.mean(tfidf22, word_count),
            weighted_tfidf23 = weighted.mean(tfidf23, word_count),
            weighted_tfidf24 = weighted.mean(tfidf24, word_count),
            weighted_tfidf25 = weighted.mean(tfidf25, word_count),
            weighted_tfidf26 = weighted.mean(tfidf26, word_count),
            weighted_tfidf27 = weighted.mean(tfidf27, word_count),
            weighted_tfidf28 = weighted.mean(tfidf28, word_count),
            weighted_tfidf29 = weighted.mean(tfidf29, word_count),
            weighted_tfidf30 = weighted.mean(tfidf30, word_count),
            weighted_tfidf31 = weighted.mean(tfidf31, word_count),
            weighted_tfidf32 = weighted.mean(tfidf32, word_count),
            weighted_tfidf33 = weighted.mean(tfidf33, word_count),
            weighted_tfidf34 = weighted.mean(tfidf34, word_count),
            weighted_tfidf35 = weighted.mean(tfidf35, word_count),
            weighted_tfidf36 = weighted.mean(tfidf36, word_count))


data = merge(data, helpdatabow_w, by = "court", all.x = TRUE)
data = merge(data, helpdatabow_2_w, by = "court", all.x = TRUE)
data = merge(data, helpdatatfidf_w, by = "court", all.x = TRUE)

# for robustness check
# euclidean <- function(a, b) sqrt(sum((a - b)^2))
manhattan_dist <- function(a1, a2){
  distance <- abs(a1-a2)
  distance <- sum(distance)
  return(distance)
}

data$dist_bow = 0
for (i in c(1:nrow(data))){
  a = c(t(data[i,c(570:633)]))
  b = c(t(data[i,c(12:75)]))
  data$dist_bow[i] = manhattan_dist(a,b)
}

data$dist_bow_2 = 0
for (i in c(1:nrow(data))){
  a = c(t(data[i,c(634:698)]))
  b = c(t(data[i,c(76:140)]))
  data$dist_bow_2[i] = manhattan_dist(a,b)
}

data$dist_tfidf = 0
for (i in c(1:nrow(data))){
  a = c(t(data[i,c(699:734)]))
  b = c(t(data[i,c(141:176)]))
  data$dist_tfidf[i] = manhattan_dist(a,b)
}

# distance to 2015 average relative to median distance (in percent)
median_bow = median(data[data$date > 7,]$dist_bow, na.rm = TRUE)
data$dist_bow_rel = 100*data$dist_bow/median_bow - 100
median_bow_2 = median(data[data$date > 7,]$dist_bow_2, na.rm = TRUE)
data$dist_bow_2_rel = 100*data$dist_bow_2/median_bow_2 - 100
median_tfidf = median(data[data$date > 7,]$dist_tfidf, na.rm = TRUE)
data$dist_tfidf_rel = 100*data$dist_tfidf/median_tfidf - 100

# make everything in percent (ranging from 1 to 100)
data$victimshare = data$victimshare*100
data$perpetratorshare = data$perpetratorshare*100
data$negativity = data$negativity*100
#####################################################################
data$democratic_judge = 0
data$democratic_judge[data$party == " d"] = 1
data$democratic_state = 0
data$democratic_state[data$court == " Appellate Court of Illinois" | data$court == " California Court of Appeal" | data$court == " Colorado Court of Appeals" | data$court == " Connecticut Appellate Court" | data$court == " Court of Appeals of Washington" | data$court == " Court of Chancery of Delaware" | data$court == " District Court, District of Columbia" | data$court == " District of Columbia Court of Appeals" | data$court == " Hawaii Intermediate Court of Appeals" | data$court == " Massachusetts Appeals Court" | data$court == " Michigan Court of Appeals" | data$court == " New Jersey Superior Court" | data$court == " New Mexico Court of Appeals" | data$court == " New York Attorney General Reports" | data$court == " New York Court of Appeals" | data$court == " Superior Court of Delaware" | data$court == " Vermont Superior Court"] = 1

data$republican_judge = 0
data$republican_judge[data$party == " r"] = 1
data$republican_state = 0
data$republican_state[data$court == " Court of Appeals of Alaska" | data$court == " Court of Appeals of Arizona" | data$court == " Court of Appeals of Arkansas" | data$court == " Court of Appeals of Georgia" | data$court == " Court of Appeals of Kansas" | data$court == " Court of Appeals of Mississippi" | data$court == " Court of Appeals of North Carolina" | data$court == " Court of Appeals of South Carolina" | data$court == " Court of Appeals of Tennessee" | data$court == " Court of Appeals of Texas" | data$court == " Court of Criminal Appeals of Tennessee" | data$court == " Court of Criminal Appeals of Texas" | data$court == " Idaho Court of Appeals" | data$court == " Indiana Court of Appeals" | data$court == " Missouri Court of Appeals" | data$court == " Nebraska Court of Appeals"] = 1 


attach(data)
data$sexualharassment  = (AA_sexualharassment + AA_importuning > 0)
data$sexualassault  = (data$AA_sexualmisconduct + data$AA_criminalsexualconduct + data$AA_sexualassault + 
                         data$AA_sexualbattery + data$AA_rape + data$AA_rapeofaspouse  + data$AA_sexualassaultinspousalrelationship + 
                         data$AA_sexualassaultincohabitatingrelationship + data$AA_assaultwithintenttocommitsexualabuse + data$AA_forciblerape + 
                         data$AA_sbattery  + data$AA_forciblesexualoffense + data$AA_objectsexualpenetration + data$AA_criminalsexualpenetration  + data$AA_criminalsexualact + 
                         data$AA_spousalsexualbattery + data$AA_forcibleactsofsexualpenetration + data$AA_assaultwithintenttoravish +
                          data$AA_sexualabuse + data$AA_abusivesexualcontact + data$AA_institutionalsexualassault + data$AA_sexualassaultonaninmate + 
                         data$AA_sexualassaultonaparolee + data$AA_sexualbatterywithastudent> 0)
data$sodomy  = ( AA_criminalsodomy  + AA_involuntarydeviatesexualintercourse + AA_forciblesodomy  + AA_sodomy + AA_oralcopulation  + 
                   AA_unnaturalintercourse + AA_crimeagainstnature>0)
data$sexualextortion  = (AA_sexualextortion + AA_sexualexploitationofaminor + AA_sexualexploitationofaninmate + AA_sexualexploitationofavulnerableadult  > 0)
data$fondling = (AA_childmolesting  + AA_indecencychild  + AA_forcibletouching + AA_lasciviousactswithachild + AA_lewdconductwithachild + 
                   AA_indecentassaultonachild + AA_indecentlibertieswithachild + AA_indecentroposalsastoachild + 
                   AA_lewdastoachild + AA_actsastoachild + AA_indecentassault + AA_impropersexualcontact + AA_sexualimposition + 
                   AA_criminalsexualcontact > 0)
data$sexualassaultonachild  = (AA_sexualassaultofaminor +  AA_sexualassaultofaminor + AA_sexualassaultonachild + 
                                 AA_sexualabuseofachild + AA_sexualbatteryofachild + AA_sexualmisconductwithachild + 
                                 AA_sexualexploitationofachild  + AA_sexualassaultofachild  + AA_rapeofachild + 
                                 AA_sexualmisconductinvolvingachild + AA_batteryonachild + AA_forciblerapeofachild + 
                                 AA_sexualassaultagainstachild   > 0)
data$statutorysexualassault = (AA_statutorysexualassault + AA_unlawfulsexualcontact+ AA_statutoryrape + AA_statutorysodomy + 
                                 AA_sexualcontactwithachild + AA_criminalsexualcontactofaminor + AA_statutorysexualoffense + 
                                 AA_statutoryrapeofapersonunder15 + AA_sexualconductagainstachild + AA_childseduction + 
                                 AA_sexualactwith + AA_unlawfulsexualacts +  AA_unlawfulsexualintercourse + AA_unlawfulsexualconduct + 
                                 AA_unlawfulsexualrelations + AA_unlawfulsexualconductwithaminor + AA_sexualcontactwithaprisoner + 
                                 AA_carnalknowledgeofaninmate + AA_carnalknowledgeofaparolee + AA_carnalknowledgeofaprobationer + 
                                 AA_sexualcontactwithastudent  + AA_statutorysexualoffensewithachild + AA_carnalknowledgeofachild 
                               + AA_sexualactivitywithastudent + AA_sexualactivitybyasubstituteparent + AA_statutoryrapeofachild > 0)

data$year = 2015
data$year[data$date>10 & data$date <23] = 2016
data$year[data$date>22 & data$date <35] = 2017
data$year[data$date>34 & data$date <47] = 2018
data$year[data$date>46 & data$date <59] = 2019
data$year[data$date>58 & data$date <71] = 2020


total = table(data$sexual, data$year)
general = summary(lm(sexual~y2016+y2017+y2018+y2019+y2020, data = data))
sexualharassment = summary(lm(sexualharassment~y2016+y2017+y2018+y2019+y2020 , data=data[data$sexual==1,]))
sexualassault = summary(lm(sexualassault~y2016+y2017+y2018+y2019+y2020 , data=data[data$sexual==1,]))
fondling = summary(lm(fondling~y2016+y2017+y2018+y2019+y2020 , data=data[data$sexual==1,]))
sodomy = summary(lm(sodomy~y2016+y2017+y2018+y2019+y2020 , data=data[data$sexual==1,]))
sexualextortion = summary(lm(sexualextortion~y2016+y2017+y2018+y2019+y2020 , data=data[data$sexual==1,]))
sexualassaultonachild = summary(lm(sexualassaultonachild~y2016+y2017+y2018+y2019+y2020 , data=data[data$sexual==1,]))
statutorysexualassault = summary(lm(statutorysexualassault~y2016+y2017+y2018+y2019+y2020 , data=data[data$sexual==1,]))


general2 = summary(lm(sexual~y2016+y2017+y2018+y2019+y2020+factor(court), data = data))
sexualharassment2 = summary(lm(sexualharassment~y2016+y2017+y2018+y2019+y2020 +factor(court), data=data[data$sexual==1,]))
sexualassault2 = summary(lm(sexualassault~y2016+y2017+y2018+y2019+y2020 +factor(court), data=data[data$sexual==1,]))
fondling2 = summary(lm(fondling~y2016+y2017+y2018+y2019+y2020 +factor(court), data=data[data$sexual==1,]))
sodomy2 = summary(lm(sodomy~y2016+y2017+y2018+y2019+y2020 +factor(court), data=data[data$sexual==1,]))
sexualextortion2 = summary(lm(sexualextortion~y2016+y2017+y2018+y2019+y2020 +factor(court), data=data[data$sexual==1,]))
sexualassaultonachild2 = summary(lm(sexualassaultonachild~y2016+y2017+y2018+y2019+y2020 +factor(court), data=data[data$sexual==1,]))
statutorysexualassault2 = summary(lm(statutorysexualassault~y2016+y2017+y2018+y2019+y2020 +factor(court), data=data[data$sexual==1,]))

v <- c("Absolute Frequencies:","Total","Sexual Offenses",
       "Relative Frequencies","Sexual Offenses", "p-Value", 
       "Composition of Sexual Offenses:",
       "Sexual Assault:",  "p-Value",
       "Sexual Assault on a Minor/Child:","p-Value",
       "Statutory Sexual Assault:", "p-Value",
       "Sodomy:", "p-Value",
       "Fondling:", "p-Value",
       "Sexual Harassment:", "p-Value")
des=c()
des=rbind(des,c(as.character(v[1]),"","","","","",""))
des=rbind(des, c(as.character(v[2]), total[1,1]+total[2,1], total[1,2]+total[2,2], total[1,3]+total[2,3], total[1,4]+total[2,4], total[1,5]+total[2,5], total[1,6]+total[2,6]))
des=rbind(des, c(as.character(v[3]), total[2,1], total[2,2], total[2,3], total[2,4], total[2,5], total[2,6]))
des=rbind(des,c(as.character(v[4]),"","","","","",""))
des=rbind(des, c(as.character(v[5]), round(general$coefficients[1,1],3), round(general$coefficients[1,1]+general$coefficients[2,1],3), round(general$coefficients[1,1]+general$coefficients[3,1],3), round(general$coefficients[1,1]+general$coefficients[4,1],3), round(general$coefficients[1,1]+general$coefficients[5,1],3), round(general$coefficients[1,1]+general$coefficients[6,1],3)))
des=rbind(des,c(as.character(v[6]), "",  round(general2$coefficients[2,4],3),  round(general2$coefficients[3,4],3),  round(general2$coefficients[4,4],3),  round(general2$coefficients[5,4],3),  round(general2$coefficients[6,4],3)))
des=rbind(des,c(as.character(v[7]),"","","","","",""))
des=rbind(des, c(as.character(v[8]), round(sexualassault$coefficients[1,1],3), round(sexualassault$coefficients[1,1]+sexualassault$coefficients[2,1],3), round(sexualassault$coefficients[1,1]+sexualassault$coefficients[3,1],3), round(sexualassault$coefficients[1,1]+sexualassault$coefficients[4,1],3), round(sexualassault$coefficients[1,1]+sexualassault$coefficients[5,1],3), round(sexualassault$coefficients[1,1]+sexualassault$coefficients[6,1],3)))
des=rbind(des,c(as.character(v[9]), "",  round(sexualassault2$coefficients[2,4],3),  round(sexualassault2$coefficients[3,4],3),  round(sexualassault2$coefficients[4,4],3),  round(sexualassault2$coefficients[5,4],3),  round(sexualassault2$coefficients[6,4],3)))
des=rbind(des, c(as.character(v[10]), round(sexualassaultonachild$coefficients[1,1],3), round(sexualassaultonachild$coefficients[1,1]+sexualassaultonachild$coefficients[2,1],3), round(sexualassaultonachild$coefficients[1,1]+sexualassaultonachild$coefficients[3,1],3), round(sexualassaultonachild$coefficients[1,1]+sexualassaultonachild$coefficients[4,1],3), round(sexualassaultonachild$coefficients[1,1]+sexualassaultonachild$coefficients[5,1],3), round(sexualassaultonachild$coefficients[1,1]+sexualassaultonachild$coefficients[6,1],3)))
des=rbind(des,c(as.character(v[11]), "",  round(sexualassaultonachild2$coefficients[2,4],3),  round(sexualassaultonachild2$coefficients[3,4],3),  round(sexualassaultonachild2$coefficients[4,4],3),  round(sexualassaultonachild2$coefficients[5,4],3),  round(sexualassaultonachild2$coefficients[6,4],3)))
des=rbind(des, c(as.character(v[12]), round(statutorysexualassault$coefficients[1,1],3), round(statutorysexualassault$coefficients[1,1]+statutorysexualassault$coefficients[2,1],3), round(statutorysexualassault$coefficients[1,1]+statutorysexualassault$coefficients[3,1],3), round(statutorysexualassault$coefficients[1,1]+statutorysexualassault$coefficients[4,1],3), round(statutorysexualassault$coefficients[1,1]+statutorysexualassault$coefficients[5,1],3), round(statutorysexualassault$coefficients[1,1]+statutorysexualassault$coefficients[6,1],3)))
des=rbind(des,c(as.character(v[13]), "",  round(statutorysexualassault2$coefficients[2,4],3),  round(statutorysexualassault2$coefficients[3,4],3),  round(statutorysexualassault2$coefficients[4,4],3),  round(statutorysexualassault2$coefficients[5,4],3),  round(statutorysexualassault2$coefficients[6,4],3)))
des=rbind(des, c(as.character(v[14]), round(sodomy$coefficients[1,1],3), round(sodomy$coefficients[1,1]+sodomy$coefficients[2,1],3), round(sodomy$coefficients[1,1]+sodomy$coefficients[3,1],3), round(sodomy$coefficients[1,1]+sodomy$coefficients[4,1],3), round(sodomy$coefficients[1,1]+sodomy$coefficients[5,1],3), round(sodomy$coefficients[1,1]+sodomy$coefficients[6,1],3)))
des=rbind(des,c(as.character(v[15]), "",  round(sodomy2$coefficients[2,4],3),  round(sodomy2$coefficients[3,4],3),  round(sodomy2$coefficients[4,4],3),  round(sodomy2$coefficients[5,4],3),  round(sodomy2$coefficients[6,4],3)))
des=rbind(des, c(as.character(v[16]), round(fondling$coefficients[1,1],3), round(fondling$coefficients[1,1]+fondling$coefficients[2,1],3), round(fondling$coefficients[1,1]+fondling$coefficients[3,1],3), round(fondling$coefficients[1,1]+fondling$coefficients[4,1],3), round(fondling$coefficients[1,1]+fondling$coefficients[5,1],3), round(fondling$coefficients[1,1]+fondling$coefficients[6,1],3)))
des=rbind(des,c(as.character(v[17]), "",  round(fondling2$coefficients[2,4],3),  round(fondling2$coefficients[3,4],3),  round(fondling2$coefficients[4,4],3),  round(fondling2$coefficients[5,4],3),  round(fondling2$coefficients[6,4],3)))
des=rbind(des, c(as.character(v[18]), round(sexualharassment$coefficients[1,1],3), round(sexualharassment$coefficients[1,1]+sexualharassment$coefficients[2,1],3), round(sexualharassment$coefficients[1,1]+sexualharassment$coefficients[3,1],3), round(sexualharassment$coefficients[1,1]+sexualharassment$coefficients[4,1],3), round(sexualharassment$coefficients[1,1]+sexualharassment$coefficients[5,1],3), round(sexualharassment$coefficients[1,1]+sexualharassment$coefficients[6,1],3)))
des=rbind(des,c(as.character(v[19]), "",  round(sexualharassment2$coefficients[2,4],3),  round(sexualharassment2$coefficients[3,4],3),  round(sexualharassment2$coefficients[4,4],3),  round(sexualharassment2$coefficients[5,4],3),  round(sexualharassment2$coefficients[6,4],3)))

print(xtable(des),include.rownames=FALSE)

###################### Descriptives #################
################# Smoothed ##################
x.control = data[data$sexual == 0 & data$date >=7 & data$date !=71,]$date
y.control = data[data$sexual == 0 & data$date >=7 & data$date !=71,]$dist_bow_rel
x.treat = data[data$sexual == 1  & data$date >=7 & data$date !=71,]$date
y.treat = data[data$sexual == 1 & data$date >=7 & data$date !=71,]$dist_bow_rel
BoW_treat.smoothed <- sm.regression(x.treat,y.treat, display = "rgl")
BoW_control.smoothed <- sm.regression(x.control,y.control, display = "rgl")
plot(BoW_treat.smoothed$eval.points,BoW_treat.smoothed$estimate,type = "l",xlab="Time",ylab = "BoW: Distance to h1-2015",xaxt="n")
axis(side = 1, at = c(13,25,37,49,61), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
lines(BoW_control.smoothed$eval.points,BoW_control.smoothed$estimate,lty = 3)
rect(xleft=34, ybottom=-10, xright=46, ytop=10, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(v=34)
mtext("#MeToo", side=3, at=34)


x.control = data[data$sexual == 0 & data$date >=7 & data$date !=71,]$date
y.control = data[data$sexual == 0 & data$date >=7 & data$date !=71,]$dist_bow_2_rel
x.treat = data[data$sexual == 1  & data$date >=7 & data$date !=71,]$date
y.treat = data[data$sexual == 1 & data$date >=7 & data$date !=71,]$dist_bow_2_rel
BoW2_treat.smoothed <- sm.regression(x.treat,y.treat, display = "rgl")
BoW2_control.smoothed <- sm.regression(x.control,y.control, display = "rgl")
plot(BoW2_treat.smoothed$eval.points,BoW2_treat.smoothed$estimate,type = "l",xlab="Time",ylab = "reduced sample BoW: Distance to h1-2015",xaxt="n")
axis(side = 1, at = c(13,25,37,49,61), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
lines(BoW2_control.smoothed$eval.points,BoW2_control.smoothed$estimate,lty = 3)
rect(xleft=34, ybottom=-10, xright=46, ytop=10, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(v=34)
mtext("#MeToo", side=3, at=34)

x.control = data[data$sexual == 0 & data$date >=7 & data$date !=71,]$date
y.control = data[data$sexual == 0 & data$date >=7 & data$date !=71,]$dist_tfidf_rel
x.treat = data[data$sexual == 1  & data$date >=7 & data$date !=71,]$date
y.treat = data[data$sexual == 1 & data$date >=7 & data$date !=71,]$dist_tfidf_rel
tfidf_treat.smoothed <- sm.regression(x.treat,y.treat, display = "rgl")
tfidf_control.smoothed <- sm.regression(x.control,y.control, display = "rgl")
plot(tfidf_treat.smoothed$eval.points,tfidf_treat.smoothed$estimate,type = "l",xlab="Time",ylab = "Tf-idf: Distance to h1-2015",xaxt="n",ylim= c(4,12))
axis(side = 1, at = c(13,25,37,49,61), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
lines(tfidf_control.smoothed$eval.points,tfidf_control.smoothed$estimate,lty = 3)
rect(xleft=34, ybottom=-10, xright=46, ytop=20, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(v=34)
mtext("#MeToo", side=3, at=34)

x.control = data[data$sexual == 0  & data$perpetratorshare_NA ==0 & data$date !=71,]$date
y.control = data[data$sexual == 0  & data$perpetratorshare_NA ==0 & data$date !=71,]$negativity
x.treat = data[data$sexual == 1  & data$perpetratorshare_NA ==0 & data$date !=71,]$date
y.treat = data[data$sexual == 1  & data$perpetratorshare_NA ==0 & data$date !=71,]$negativity
negativity_treat.smoothed <- sm.regression(x.treat,y.treat, display = "rgl")
negativity_control.smoothed <- sm.regression(x.control,y.control, display = "rgl")
plot(negativity_treat.smoothed$eval.points,negativity_treat.smoothed$estimate,type = "l",xlab="Time",ylab = "Negativity in perpetrator context",xaxt="n",ylim = c(2,2.6))
axis(side = 1, at = c(13,25,37,49,61), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
lines(negativity_control.smoothed$eval.points,negativity_control.smoothed$estimate,lty = 3)
rect(xleft=34, ybottom=-10, xright=46, ytop=20, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(v=34)
mtext("#MeToo", side=3, at=34)

x.control = data[data$sexual == 0 & data$victimshare_NA ==0 & data$date !=71,]$date
y.control = data[data$sexual == 0 & data$victimshare_NA ==0 & data$date !=71,]$victimshare
x.treat = data[data$sexual == 1 & data$victimshare_NA ==0 & data$date !=71,]$date
y.treat = data[data$sexual == 1 & data$victimshare_NA ==0 & data$date !=71,]$victimshare
Victimshare_treat.smoothed <- sm.regression(x.treat,y.treat, display = "rgl")
Victimshare_control.smoothed <- sm.regression(x.control,y.control, display = "rgl")
plot(Victimshare_treat.smoothed$eval.points,Victimshare_treat.smoothed$estimate,type = "l",xlab="Time",ylab = "Victim mentions as subject",xaxt="n",ylim = c(27,33))
axis(side = 1, at = c(13,25,37,49,61), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
lines(Victimshare_control.smoothed$eval.points,Victimshare_control.smoothed$estimate,lty = 3)
rect(xleft=34, ybottom=-10, xright=46, ytop=36, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(v=34)
mtext("#MeToo", side=3, at=34)

######################## 1-period DiD ######################

lmDiDBoW = lm(dist_bow_rel ~ sexual + sexual * after_treatment + after_treatment + word_count + factor(court) , data = data[data$date >= 7 ,])
vcov_lmDiDBoW = vcovCR(lmDiDBoW, cluster = data[data$date >= 7 ,]$court, type = "CR0")
testBoW = coeftest(lmDiDBoW, vcov=vcov_lmDiDBoW)
vcov_lmDiDBoW = NULL

lmDiDBoW_red = lm(dist_bow_rel ~ sexual + sexual * after_treatment + after_treatment + word_count + factor(court) , data = data[data$date >= 7 & data$y2018 == 0,])
vcov_lmDiDBoW_red = vcovCR(lmDiDBoW_red, cluster = data[data$date >= 7 & data$y2018 == 0,]$court, type = "CR0")
testBoW_red = coeftest(lmDiDBoW_red, vcov=vcov_lmDiDBoW_red)
vcov_lmDiDBoW_red = NULL

lmDiDBoW_2 = lm(dist_bow_2_rel ~ sexual + sexual * after_treatment + after_treatment + word_count + factor(court) , data = data[data$date >= 7 ,])
vcov_lmDiDBoW_2 = vcovCR(lmDiDBoW_2, cluster = data[data$date >= 7 ,]$court, type = "CR0")
testBoW_2 = coeftest(lmDiDBoW_2, vcov=vcov_lmDiDBoW_2)
vcov_lmDiDBoW_2 = NULL

lmDiDBoW_2_red = lm(dist_bow_2_rel ~ sexual + sexual * after_treatment + after_treatment + word_count + factor(court) , data = data[data$date >= 7 & data$y2018 == 0,])
vcov_lmDiDBoW_2_red = vcovCR(lmDiDBoW_2_red, cluster = data[data$date >= 7 & data$y2018 == 0,]$court, type = "CR0")
testBoW_2_red = coeftest(lmDiDBoW_2_red, vcov=vcov_lmDiDBoW_2_red)
vcov_lmDiDBoW_2_red = NULL

lmDiDtfidf = lm(dist_tfidf_rel ~ sexual + sexual * after_treatment + after_treatment + word_count + factor(court) , data = data[data$date >= 7 ,])
vcov_lmDiDtfidf = vcovCR(lmDiDtfidf, cluster = data[data$date >= 7 ,]$court, type = "CR0")
testtfidf = coeftest(lmDiDtfidf, vcov=vcov_lmDiDtfidf)
vcov_lmDiDtfidf  = NULL

lmDiDtfidf_red = lm(dist_tfidf_rel ~ sexual + sexual * after_treatment + after_treatment + word_count + factor(court) , data = data[data$date >= 7 & data$y2018 == 0,])
vcov_lmDiDtfidf_red = vcovCR(lmDiDtfidf_red, cluster = data[data$date >= 7 & data$y2018 == 0,]$court, type = "CR0")
testtfidf_red = coeftest(lmDiDtfidf_red, vcov=vcov_lmDiDtfidf_red)
vcov_lmDiDtfidf_red = NULL

lmDiDvictimsubject = lm(victimshare ~ sexual + sexual * after_treatment + after_treatment + word_count + factor(court) , data = data[data$victimshare_NA == 0 ,])
vcov_lmDiDvictimsubject = vcovCR(lmDiDvictimsubject, cluster = data[data$victimshare_NA ==0,]$court, type = "CR0")
testvictimsubject = coeftest(lmDiDvictimsubject, vcov=vcov_lmDiDvictimsubject)
vcov_lmDiDvictimsubject = NULL

lmDiDvictimsubject_red = lm(victimshare ~ sexual + sexual * after_treatment + after_treatment + word_count + factor(court) , data = data[data$victimshare_NA == 0 & data$y2018 == 0,])
vcov_lmDiDvictimsubject_red = vcovCR(lmDiDvictimsubject_red, cluster = data[data$victimshare_NA ==0 & data$y2018 == 0,]$court, type = "CR0")
testvictimsubject_red = coeftest(lmDiDvictimsubject_red, vcov=vcov_lmDiDvictimsubject_red)
vcov_lmDiDvictimsubject_red = NULL

lmDiDnegativity = lm(negativity_victim ~ sexual + sexual * after_treatment + after_treatment + word_count + factor(court) , data = data[data$perpetratorshare_NA == 0 ,])
vcov_lmDiDnegativity = vcovCR(lmDiDnegativity, cluster = data[data$perpetratorshare_NA ==0,]$court, type = "CR0")
testnegativity = coeftest(lmDiDnegativity, vcov=vcov_lmDiDnegativity)
vcov_lmDiDnegativity = NULL

lmDiDnegativity_red = lm(negativity ~ sexual + sexual * after_treatment + after_treatment + word_count + factor(court) , data = data[data$perpetratorshare_NA == 0 & data$y2018 == 0,])
vcov_lmDiDnegativity_red = vcovCR(lmDiDnegativity_red, cluster = data[data$perpetratorshare_NA ==0 & data$y2018 == 0,]$court, type = "CR0")
testnegativity_red = coeftest(lmDiDnegativity_red, vcov=vcov_lmDiDnegativity_red)
vcov_lmDiDnegativity_red = NULL


###################################### 1-year periods ##############################

lmDiDyBoW = lm(dist_bow_rel ~ sexual + sexual * (y2019 + y2020) + y2016 + y2017 + y2018 + word_count + factor(court) , data = data[data$date >= 7,])
vcov_lmDiDyBoW = vcovCR(lmDiDyBoW, cluster = data[data$date >= 7,]$court, type = "CR0")
testyBoW = coeftest(lmDiDyBoW, vcov=vcov_lmDiDyBoW)
vcov_lmDiDyBoW = NULL

lmDiDyBoW_2 = lm(dist_bow_2_rel ~ sexual + sexual  * (y2019 + y2020)  +  y2016 + y2017 + y2018 + word_count + factor(court) , data = data[data$date >= 7,])
vcov_lmDiDyBoW_2 = vcovCR(lmDiDyBoW_2, cluster = data[data$date >= 7,]$court, type = "CR0")
testyBoW_2 = coeftest(lmDiDyBoW_2, vcov=vcov_lmDiDyBoW_2)
vcov_lmDiDyBoW_2 = NULL

lmDiDytfidf = lm(dist_tfidf_rel ~ sexual + sexual  * (y2019 + y2020)  +  y2016 + y2017 + y2018 + word_count + factor(court) , data = data[data$date >= 7,])
vcov_lmDiDytfidf = vcovCR(lmDiDytfidf, cluster = data[data$date >= 7,]$court, type = "CR0")
testytfidf = coeftest(lmDiDytfidf, vcov=vcov_lmDiDytfidf)
vcov_lmDiDytfidf = NULL

lmDiDyvictimsubject = lm(victimshare ~ sexual + sexual  * (y2019 + y2020)  + y2016 + y2017 + y2018 + word_count + factor(court) , data = data[data$victimshare_NA == 0 ,])
vcov_lmDiDyvictimsubject = vcovCR(lmDiDyvictimsubject, cluster = data[data$victimshare_NA ==0,]$court, type = "CR0")
testyvictimsubject = coeftest(lmDiDyvictimsubject, vcov=vcov_lmDiDyvictimsubject)
vcov_lmDiDyvictimsubject = NULL

lmDiDynegativity = lm(negativity ~ sexual + sexual  * (y2019 + y2020) +  y2016 + y2017 + y2018 + word_count + factor(court) , data = data[data$perpetratorshare_NA == 0,])
vcov_lmDiDynegativity = vcovCR(lmDiDynegativity, cluster = data[data$perpetratorshare_NA ==0 ,]$court, type = "CR0")
testynegativity = coeftest(lmDiDynegativity, vcov=vcov_lmDiDynegativity)
vcov_lmDiDynegativity = NULL

###################################### half-year periods ##############################

lmDiDhBoW = lm(dist_bow_rel ~ sexual + sexual * (h12019 + h22019 + h12020 + h22020) + h12016 + h22016 + h12017 + h22017 + h12018 + h22018 + word_count + factor(court) , data = data[data$date >= 7,])
vcov_lmDiDhBoW = vcovCR(lmDiDhBoW, cluster = data[data$date >= 7,]$court, type = "CR0")
testhBoW = coeftest(lmDiDhBoW, vcov=vcov_lmDiDhBoW)
vcov_lmDiDhBoW = NULL

lmDiDhBoW_2 = lm(dist_bow_2_rel ~ sexual + sexual  * (h12019 + h22019 + h12020 + h22020) + h12016 + h22016 + h12017 + h22017 + h12018 + h22018 + word_count + factor(court) , data = data[data$date >= 7,])
vcov_lmDiDhBoW_2 = vcovCR(lmDiDhBoW_2, cluster = data[data$date >= 7,]$court, type = "CR0")
testhBoW_2 = coeftest(lmDiDhBoW_2, vcov=vcov_lmDiDhBoW_2)
vcov_lmDiDhBoW_2 = NULL

lmDiDhtfidf = lm(dist_tfidf_rel ~ sexual + sexual  * (h12019 + h22019 + h12020 + h22020) + h12016 + h22016 + h12017 + h22017 + h12018 + h22018 + word_count + factor(court) , data = data[data$date >= 7,])
vcov_lmDiDhtfidf = vcovCR(lmDiDhtfidf, cluster = data[data$date >= 7,]$court, type = "CR0")
testhtfidf = coeftest(lmDiDhtfidf, vcov=vcov_lmDiDhtfidf)
vcov_lmDiDhtfidf = NULL

lmDiDhvictimsubject = lm(victimshare ~ sexual + sexual  * (h12019 + h22019 + h12020 + h22020)  + h22015 + h12016 + h22016 + h12017 + h22017 + h12018 + h22018 + word_count + factor(court) , data = data[data$victimshare_NA == 0 ,])
vcov_lmDiDhvictimsubject = vcovCR(lmDiDhvictimsubject, cluster = data[data$victimshare_NA ==0,]$court, type = "CR0")
testhvictimsubject = coeftest(lmDiDhvictimsubject, vcov=vcov_lmDiDhvictimsubject)
vcov_lmDiDhvictimsubject = NULL

lmDiDhnegativity = lm(negativity ~ sexual + sexual  * (h12019 + h22019 + h12020 + h22020)  + h22015 + h12016 + h22016 + h12017 + h22017 + h12018 + h22018 + word_count + factor(court) , data = data[data$perpetratorshare_NA == 0,])
vcov_lmDiDhnegativity = vcovCR(lmDiDhnegativity, cluster = data[data$perpetratorshare_NA ==0 ,]$court, type = "CR0")
testhnegativity = coeftest(lmDiDhnegativity, vcov=vcov_lmDiDhnegativity)
vcov_lmDiDhnegativity = NULL


v <- c("sex cr. x post", "sex cr. x '19", "sex cr. x '20",
       "sex cr. x H1-'19", "sex cr. x H2-'19",
       "sex cr. x H1-'20", "sex cr. x H2-'20",   
        "post", "year FE","half-year FE","court FE","# words")
des=c()
des=rbind(des, c(as.character(v[1]), paste(format(round(testBoW_red[nrow(testBoW_red),1],3), nsmall = 3),signif.num(testBoW_red[nrow(testBoW_red),4]),sep=""),"","",
                 paste(format(round(testBoW_2_red[nrow(testBoW_2_red),1],3), nsmall = 3),signif.num(testBoW_2_red[nrow(testBoW_2_red),4]),sep=""),"","",
                 paste(format(round(testtfidf_red[nrow(testtfidf_red),1],3), nsmall = 3),signif.num(testtfidf_red[nrow(testtfidf_red),4]),sep=""),"",""))
                 
des=rbind(des, c("", paste("(",format(round(testBoW_red[nrow(testBoW_red),2],3), nsmall = 3),")", sep = ""),"","",
                 paste("(",format(round(testBoW_2_red[nrow(testBoW_2_red),2],3), nsmall = 3),")", sep = ""), "","",
                 paste("(",format(round(testtfidf_red[nrow(testtfidf_red),2],3), nsmall = 3),")", sep = ""), "",""))
                 
                 
des=rbind(des, c(as.character(v[2]),"",paste(format(round(testyBoW[nrow(testyBoW)-1,1],3), nsmall = 3), signif.num(testyBoW[nrow(testyBoW)-1,4]),sep=""),"",
                 "",paste(format(round(testyBoW_2[nrow(testyBoW_2)-1,1],3), nsmall = 3), signif.num(testyBoW_2[nrow(testyBoW_2)-1,4]), sep =""),"",
                 "",paste(format(round(testytfidf[nrow(testytfidf)-1,1],3), nsmall = 3), signif.num(testytfidf[nrow(testytfidf)-1,4]), sep =""),""))
                 
                 
des=rbind(des, c("","", paste("(",format(round(testyBoW[nrow(testyBoW)-1,2],3), nsmall = 3),")", sep = ""), "",
                 "", paste("(",format(round(testyBoW_2[nrow(testyBoW_2)-1,2],3), nsmall = 3),")", sep = ""),"", 
                 "", paste("(",format(round(testytfidf[nrow(testytfidf)-1,2],3), nsmall = 3),")", sep = ""),""))
                 
                
des=rbind(des, c(as.character(v[3]),"",paste(format(round(testyBoW[nrow(testyBoW),1],3), nsmall = 3), signif.num(testyBoW[nrow(testyBoW),4]), sep =""),"", 
                 "",paste(format(round(testyBoW_2[nrow(testyBoW_2),1],3), nsmall = 3), signif.num(testyBoW_2[nrow(testyBoW_2),4]), sep =""),"",
                 "",paste(format(round(testytfidf[nrow(testytfidf),1],3), nsmall = 3), signif.num(testytfidf[nrow(testytfidf),4]), sep =""),""))
                 
                 
des=rbind(des, c("", "",paste("(",format(round(testyBoW[nrow(testyBoW),2],3), nsmall = 3),")", sep = ""),"", 
                 "",paste("(",format(round(testyBoW_2[nrow(testyBoW_2),2],3), nsmall = 3),")", sep = ""),"", 
                 "",paste("(",format(round(testytfidf[nrow(testytfidf),2],3), nsmall = 3),")", sep = ""),""))
                 
          
des=rbind(des, c(as.character(v[4]), "","",paste(format(round(testhBoW[nrow(testhBoW)-3,1],3), nsmall = 3), signif.num(testhBoW[nrow(testhBoW)-3,4]),sep=""),
                 "","",paste(format(round(testhBoW_2[nrow(testhBoW_2)-3,1],3), nsmall = 3), signif.num(testhBoW_2[nrow(testhBoW_2)-3,4]),sep=""),
                 "","",paste(format(round(testhtfidf[nrow(testhtfidf)-3,1],3), nsmall = 3), signif.num(testhtfidf[nrow(testhtfidf)-3,4]),sep="")))
                 

des=rbind(des, c("","","",paste("(",format(round(testhBoW[nrow(testhBoW)-3,2],3), nsmall = 3),")", sep = ""),
                 "","",paste("(",format(round(testhBoW_2[nrow(testhBoW_2)-3,2],3), nsmall = 3),")", sep = ""),
                 "","",paste("(",format(round(testhtfidf[nrow(testhtfidf)-3,2],3), nsmall = 3),")", sep = "")))
                 

des=rbind(des, c(as.character(v[5]), "","",paste(format(round(testhBoW[nrow(testhBoW)-2,1],3), nsmall = 3), signif.num(testhBoW[nrow(testhBoW)-2,4]),sep=""),
                 "","",paste(format(round(testhBoW_2[nrow(testhBoW_2)-2,1],3), nsmall = 3), signif.num(testhBoW_2[nrow(testhBoW_2)-2,4]),sep=""),
                 "","",paste(format(round(testhtfidf[nrow(testhtfidf)-2,1],3), nsmall = 3), signif.num(testhtfidf[nrow(testhtfidf)-2,4]),sep="")))
                 
                
des=rbind(des, c("", "","",paste("(",format(round(testhBoW[nrow(testhBoW)-2,2],3), nsmall = 3),")", sep = ""),
                 "","",paste("(",format(round(testhBoW_2[nrow(testhBoW_2)-2,2],3), nsmall = 3),")", sep = ""),
                 "","",paste("(",format(round(testhtfidf[nrow(testhtfidf)-2,2],3), nsmall = 3),")", sep = "")))
                 
                 
des=rbind(des, c(as.character(v[6]), "","",paste(format(round(testhBoW[nrow(testhBoW)-1,1],3), nsmall = 3), signif.num(testhBoW[nrow(testhBoW)-1,4]),sep=""),
                 "","",paste(format(round(testhBoW_2[nrow(testhBoW_2)-1,1],3), nsmall = 3), signif.num(testhBoW_2[nrow(testhBoW_2)-1,4]),sep=""),
                 "","",paste(format(round(testhtfidf[nrow(testhtfidf)-1,1],3), nsmall = 3), signif.num(testhtfidf[nrow(testhtfidf)-1,4]),sep="")))
                 
                
des=rbind(des, c("", "","",paste("(",format(round(testhBoW[nrow(testhBoW)-1,2],3), nsmall = 3),")", sep = ""),
                 "","",paste("(",format(round(testhBoW_2[nrow(testhBoW_2)-1,2],3), nsmall = 3),")", sep = ""),
                 "","",paste("(",format(round(testhtfidf[nrow(testhtfidf)-1,2],3), nsmall = 3),")", sep = "")))
                 
              
des=rbind(des, c(as.character(v[7]), "","", paste(format(round(testhBoW[nrow(testhBoW),1],3), nsmall = 3), signif.num(testhBoW[nrow(testhBoW),4]),sep=""),
                 "","", paste(format(round(testhBoW_2[nrow(testhBoW_2),1],3), nsmall = 3), signif.num(testhBoW_2[nrow(testhBoW_2),4]),sep=""),
                 "","", paste(format(round(testhtfidf[nrow(testhtfidf),1],3), nsmall = 3), signif.num(testhtfidf[nrow(testhtfidf),4]),sep="")))
                 
                 

des=rbind(des, c("", "","",paste("(",format(round(testhBoW[nrow(testhBoW),2],3), nsmall = 3),")", sep = ""),
                 "","",paste("(",format(round(testhBoW_2[nrow(testhBoW_2),2],3), nsmall = 3),")", sep = ""),
                 "","",paste("(",format(round(testhtfidf[nrow(testhtfidf),2],3), nsmall = 3),")", sep = "")))
                 
                 
des=rbind(des, c(as.character(v[8]), "X", "", "", "X", "", "", "X", "", ""))
des=rbind(des, c(as.character(v[9]), "", "X", "","", "X", "","", "X", ""))
des=rbind(des, c(as.character(v[10]), "","","X", "","", "X", "","", "X"))
des=rbind(des, c(as.character(v[11]), "X",  "X",  "X",  "X", "X", "X", "X", "X", "X"))
des=rbind(des, c(as.character(v[12]), "X", "X",  "X",  "X", "X", "X", "X", "X", "X" ))


print(xtable(des),include.rownames=FALSE, booktabs = TRUE)


des = c()
des=rbind(des, c(as.character(v[1]), paste(format(round(testvictimsubject_red[nrow(testvictimsubject_red),1],3), nsmall = 3), signif.num(testvictimsubject_red[nrow(testvictimsubject_red),4]), sep = ""),"","",
                paste(format(round(testnegativity_red[nrow(testnegativity_red),1],3), nsmall = 3), signif.num(testnegativity_red[nrow(testnegativity_red),4]), sep = ""),"",""))

des=rbind(des, c("", paste("(",format(round(testvictimsubject_red[nrow(testvictimsubject_red),2],3), nsmall = 3),")", sep = ""),"","",
                 paste("(",format(round(testnegativity_red[nrow(testnegativity_red),2],3), nsmall = 3),")", sep = ""),"",""))

des=rbind(des, c(as.character(v[2]),"",paste(format(round(testyvictimsubject[nrow(testyvictimsubject)-1,1],3), nsmall = 3), signif.num(testyvictimsubject[nrow(testyvictimsubject)-1,4]), sep = ""),"",
                 "", paste(format(round(testynegativity[nrow(testynegativity)-1,1],3), nsmall = 3), signif.num(testynegativity[nrow(testynegativity)-1,4]), sep = ""),""))

des=rbind(des, c("", "", paste("(",format(round(testyvictimsubject[nrow(testyvictimsubject)-1,2],3), nsmall = 3),")", sep = ""),"",
                 "", paste("(",format(round(testynegativity[nrow(testynegativity)-1,2],3), nsmall = 3),")", sep = ""),""))

des=rbind(des, c(as.character(v[3]), "", paste(format(round(testyvictimsubject[nrow(testyvictimsubject),1],3), nsmall = 3), signif.num(testyvictimsubject[nrow(testyvictimsubject),4]), sep = ""),"",
                 "", paste(format(round(testynegativity[nrow(testynegativity),1],3), nsmall = 3), signif.num(testynegativity[nrow(testynegativity),4]), sep = ""),""))

des=rbind(des, c("", "", paste("(",format(round(testyvictimsubject[nrow(testyvictimsubject),2],3), nsmall = 3),")", sep = ""),"",
                 "",paste("(",format(round(testynegativity[nrow(testynegativity),2],3), nsmall = 3),")", sep = ""),""))

des=rbind(des, c(as.character(v[4]),"","", paste(format(round(testhvictimsubject[nrow(testhvictimsubject)-3,1],3), nsmall = 3), signif.num(testhvictimsubject[nrow(testhvictimsubject)-3,4]), sep = ""),
                 "","", paste(format(round(testhnegativity[nrow(testhnegativity)-3,1],3), nsmall = 3), signif.num(testhnegativity[nrow(testhnegativity)-3,4]), sep = "")))

des=rbind(des, c("", "","",paste("(",format(round(testhvictimsubject[nrow(testhvictimsubject)-3,2],3), nsmall = 3),")", sep = ""),
                 "","",paste("(",format(round(testhnegativity[nrow(testhnegativity)-3,2],3), nsmall = 3),")", sep = "")))

des=rbind(des, c(as.character(v[5]), "","",paste(format(round(testhvictimsubject[nrow(testhvictimsubject)-2,1],3), nsmall = 3), signif.num(testhvictimsubject[nrow(testhvictimsubject)-2,4]), sep =""),
                 "","",paste(format(round(testhnegativity[nrow(testhnegativity)-2,1],3), nsmall = 3), signif.num(testhnegativity[nrow(testhnegativity)-2,4]), sep ="")))

des=rbind(des, c("", "","",paste("(",format(round(testhvictimsubject[nrow(testhvictimsubject)-2,2],3), nsmall = 3),")", sep = ""),
                 "","",paste("(",format(round(testhnegativity[nrow(testhnegativity)-2,2],3), nsmall = 3),")", sep = "")))

des=rbind(des, c(as.character(v[6]), "","",paste(format(round(testhvictimsubject[nrow(testhvictimsubject)-1,1],3), nsmall = 3), signif.num(testhvictimsubject[nrow(testhvictimsubject)-1,4]),sep=""),
                 "","",paste(format(round(testhnegativity[nrow(testhnegativity)-1,1],3), nsmall = 3), signif.num(testhnegativity[nrow(testhnegativity)-1,4]),sep="")))

des=rbind(des, c("", "","", paste("(",format(round(testhvictimsubject[nrow(testhvictimsubject)-1,2],3), nsmall = 3),")", sep = ""),
                 "","", paste("(",format(round(testhnegativity[nrow(testhnegativity)-1,2],3), nsmall = 3),")", sep = "")))

des=rbind(des, c(as.character(v[7]), "","", paste(format(round(testhvictimsubject[nrow(testhvictimsubject),1],3), nsmall = 3), signif.num(testhvictimsubject[nrow(testhvictimsubject),4]),sep=""),
                 "","", paste(format(round(testhnegativity[nrow(testhnegativity),1],3), nsmall = 3), signif.num(testhnegativity[nrow(testhnegativity),4]),sep="")))

des=rbind(des, c("", "","",paste("(",format(round(testhvictimsubject[nrow(testhvictimsubject),2],3), nsmall = 3),")", sep = ""),
                 "","",paste("(",format(round(testhnegativity[nrow(testhnegativity),2],3), nsmall = 3),")", sep = "")))

des=rbind(des, c(as.character(v[8]), "X", "", "", "X", "", ""))
des=rbind(des, c(as.character(v[9]), "", "X", "","", "X", ""))
des=rbind(des, c(as.character(v[10]), "","","X", "","", "X"))
des=rbind(des, c(as.character(v[11]), "X",  "X",  "X",  "X", "X", "X"))
des=rbind(des, c(as.character(v[12]), "X", "X",  "X",  "X", "X", "X"))


print(xtable(des),include.rownames=FALSE, booktabs = TRUE)



################################## Robustness Check ###################
data$after_treatment_test = 0
data$after_treatment_test[data$date>20] = 1

lmDiDBoW_rob = lm(dist_bow_rel ~ sexual + sexual * after_treatment_test + after_treatment_test + word_count + factor(court) , data = data[data$date >= 7 & data$date <= 34 ,])
vcov_lmDiDBoW_rob = vcovCR(lmDiDBoW_rob, cluster = data[data$date >= 7 & data$date <= 34,]$court, type = "CR0")
testBoW_rob = coeftest(lmDiDBoW_rob, vcov=vcov_lmDiDBoW_rob)

lmDiDBoW_2_rob = lm(dist_bow_2_rel ~ sexual + sexual * after_treatment_test + after_treatment_test + word_count + factor(court) , data = data[data$date >= 7 & data$date <= 34,])
vcov_lmDiDBoW_2_rob = vcovCR(lmDiDBoW_2_rob, cluster = data[data$date >= 7 & data$date <= 34,]$court, type = "CR0")
testBoW_2_rob = coeftest(lmDiDBoW_2_rob, vcov=vcov_lmDiDBoW_2_rob)

lmDiDtfidf_rob = lm(dist_tfidf_rel ~ sexual + sexual * after_treatment_test + after_treatment_test + word_count + factor(court) , data = data[data$date >= 7 & data$date <= 34,])
vcov_lmDiDtfidf_rob = vcovCR(lmDiDtfidf_rob, cluster = data[data$date >= 7 & data$date <= 34,]$court, type = "CR0")
testtfidf_rob = coeftest(lmDiDtfidf_rob, vcov=vcov_lmDiDtfidf_rob)

data$after_treatment_test = 0
data$after_treatment_test[data$date>17] = 1

lmDiDvictimsubject_rob = lm(victimshare ~ sexual + sexual * after_treatment_test + after_treatment_test + word_count + factor(court) , data = data[data$victimshare_NA == 0 & data$date <= 34,])
vcov_lmDiDvictimsubject_rob = vcovCR(lmDiDvictimsubject_rob, cluster = data[data$victimshare_NA ==0 & data$date <= 34,]$court, type = "CR0")
testvictimsubject_rob = coeftest(lmDiDvictimsubject_rob, vcov=vcov_lmDiDvictimsubject_rob)


lmDiDnegativity_rob = lm(negativity ~ sexual + sexual * after_treatment_test + after_treatment_test + word_count + factor(court) , data = data[data$perpetratorshare_NA == 0 & data$date <= 34 ,])
vcov_lmDiDnegativity_rob = vcovCR(lmDiDnegativity_rob, cluster = data[data$perpetratorshare_NA ==0  & data$date <= 34,]$court, type = "CR0")
testnegativity_rob = coeftest(lmDiDnegativity_rob, vcov=vcov_lmDiDnegativity_rob)


#################################
# IPW
weightBoW = didweight(data[data$date >= 7 ,]$dist_bow_rel, data[data$date >= 7 ,]$sexual, data[data$date >= 7 ,]$after_treatment, x = factor(data[data$date >= 7 ,]$court), boot = 999, trim = 0.05, cluster = NULL)
weightBoW_2 = didweight(data[data$date >= 7 ,]$dist_bow_2_rel, data[data$date >= 7 ,]$sexual, data[data$date >= 7 ,]$after_treatment, x = factor(data[data$date >= 7 ,]$court), boot = 999, trim = 0.05, cluster = NULL)
weighttfidf = didweight(data[data$date >= 7 ,]$dist_tfidf_rel, data[data$date >= 7 ,]$sexual, data[data$date >= 7 ,]$after_treatment, x = factor(data[data$date >= 7 ,]$court), boot = 999, trim = 0.05, cluster = NULL)
weightvictimshare = didweight(data[data$victimshare_NA == 0,]$victimshare, data[data$victimshare_NA == 0,]$sexual, data[data$victimshare_NA == 0,]$after_treatment, x = factor(data[data$victimshare_NA == 0 ,]$court), boot = 999, trim = 0.05, cluster = NULL)
weightnegativity = didweight(data[data$perpetratorshare_NA == 0,]$negativity, data[data$perpetratorshare_NA == 0,]$sexual, data[data$perpetratorshare_NA == 0,]$after_treatment, x = factor(data[data$perpetratorshare_NA == 0,]$court), boot = 999, trim = 0.05, cluster = NULL)

################################
# only those sexual crimes with no increase in reports
lmDiDBoW_rob2 = lm(dist_bow_rel ~ sexual + sexual * after_treatment + after_treatment + word_count + factor(court) , data = data[data$date >= 7 & (data$sexual == 0 | (data$sodomy== 1 | data$statutorysexualassault==1)),])
vcov_lmDiDBoW_rob2 = vcovCR(lmDiDBoW_rob2, cluster = data[data$date >= 7 & (data$sexual == 0 | (data$sodomy== 1 | data$statutorysexualassault==1)),]$court, type = "CR0")
testBoW_rob2 = coeftest(lmDiDBoW_rob2, vcov=vcov_lmDiDBoW_rob2)

lmDiDBoW_2_rob2 = lm(dist_bow_2_rel ~ sexual + sexual * after_treatment + after_treatment + word_count + factor(court) , data = data[data$date >= 7 & (data$sexual == 0 | (data$sodomy== 1 | data$statutorysexualassault==1)),])
vcov_lmDiDBoW_2_rob2 = vcovCR(lmDiDBoW_2_rob2, cluster = data[data$date >= 7  & (data$sexual == 0 | (data$sodomy== 1 | data$statutorysexualassault==1)),]$court, type = "CR0")
testBoW_2_rob2 = coeftest(lmDiDBoW_2_rob2, vcov=vcov_lmDiDBoW_2_rob2)

lmDiDtfidf_rob2 = lm(dist_tfidf_rel ~ sexual + sexual * after_treatment + after_treatment + word_count + factor(court) , data = data[data$date >= 7 & (data$sexual == 0 | (data$sodomy== 1 | data$statutorysexualassault==1)),])
vcov_lmDiDtfidf_rob2 = vcovCR(lmDiDtfidf_rob2, cluster = data[data$date >= 7  & (data$sexual == 0 | (data$sodomy== 1 | data$statutorysexualassault==1)),]$court, type = "CR0")
testtfidf_rob2 = coeftest(lmDiDtfidf_rob2, vcov=vcov_lmDiDtfidf_rob2)

lmDiDvictimsubject_rob2 = lm(victimshare ~ sexual + sexual * after_treatment + after_treatment + word_count + factor(court) , data = data[data$victimshare_NA == 0 & (data$sexual == 0 | (data$sodomy== 1 | data$statutorysexualassault==1)),])
vcov_lmDiDvictimsubject_rob2 = vcovCR(lmDiDvictimsubject_rob2, cluster = data[data$victimshare_NA ==0  & (data$sexual == 0 | (data$sodomy== 1 | data$statutorysexualassault==1)),]$court, type = "CR0")
testvictimsubject_rob2 = coeftest(lmDiDvictimsubject_rob2, vcov=vcov_lmDiDvictimsubject_rob2)
vcov_lmDiDvictimsubject_rob2 = NULL


lmDiDnegativity_rob2 = lm(negativity ~ sexual + sexual * after_treatment + after_treatment + word_count + factor(court) , data = data[data$perpetratorshare_NA == 0 & (data$sexual == 0 | (data$sodomy== 1 | data$statutorysexualassault==1)),])
vcov_lmDiDnegativity_rob2 = vcovCR(lmDiDnegativity_rob2, cluster = data[data$perpetratorshare_NA ==0  & (data$sexual == 0 | (data$sodomy== 1 | data$statutorysexualassault==1)),]$court, type = "CR0")
testnegativity_rob2 = coeftest(lmDiDnegativity_rob2, vcov=vcov_lmDiDnegativity_rob2)
vcov_lmDiDnegativity_rob2 = NULL

############ Tables
v <- c("sex cr. x placebo", "sex cr. x post", "sex cr. x post",   
       "post", "court FE","# words")
des=c()
des=rbind(des, c(as.character(v[1]), paste(format(round(testBoW_rob[nrow(testBoW_rob),1],3), nsmall = 3),signif.num(testBoW_rob[nrow(testBoW_rob),4]),sep=""),"","",
                 paste(format(round(testBoW_2_rob[nrow(testBoW_2_rob),1],3), nsmall = 3),signif.num(testBoW_2_rob[nrow(testBoW_2_rob),4]),sep=""),"","",
                 paste(format(round(testtfidf_rob[nrow(testtfidf_rob),1],3), nsmall = 3),signif.num(testtfidf_rob[nrow(testtfidf_rob),4]),sep=""),"",""))

des=rbind(des, c("", paste("(",format(round(testBoW_rob[nrow(testBoW_rob),2],3), nsmall = 3),")", sep = ""),"","",
                 paste("(",format(round(testBoW_2_rob[nrow(testBoW_2_rob),2],3), nsmall = 3),")", sep = ""), "","",
                 paste("(",format(round(testtfidf_rob[nrow(testtfidf_rob),2],3), nsmall = 3),")", sep = ""), "",""))

des=rbind(des, c(as.character(v[2]), "", paste(format(round(weightBoW$effect,3), nsmall = 3),signif.num(weightBoW$pvalue),sep=""),"",
                 "", paste(format(round(weightBoW_2$effect,3), nsmall = 3),signif.num(weightBoW_2$pvalue),sep=""),"",
                 "", paste(format(round(weighttfidf$effect,3), nsmall = 3),signif.num(weighttfidf$pvalue),sep=""),""))

des=rbind(des, c("", "", paste("(",format(round(weightBoW$se,3), nsmall = 3),")", sep = ""),"",
                 "", paste("(",format(round(weightBoW_2$se,3), nsmall = 3),")", sep = ""), "",
                 "", paste("(",format(round(weighttfidf$se,3), nsmall = 3),")", sep = ""), ""))


des=rbind(des, c(as.character(v[3]), "","", paste(format(round(testBoW_rob2[nrow(testBoW_rob2),1],3), nsmall = 3),signif.num(testBoW_rob2[nrow(testBoW_rob2),4]),sep=""),
                 "","",paste(format(round(testBoW_2_rob2[nrow(testBoW_2_rob2),1],3), nsmall = 3),signif.num(testBoW_2_rob2[nrow(testBoW_2_rob2),4]),sep=""),
                 "","",paste(format(round(testtfidf_rob2[nrow(testtfidf_rob2),1],3), nsmall = 3),signif.num(testtfidf_rob2[nrow(testtfidf_rob2),4]),sep="")))

des=rbind(des, c("", "","",paste("(",format(round(testBoW_rob2[nrow(testBoW_rob2),2],3), nsmall = 3),")", sep = ""),
                 "","",paste("(",format(round(testBoW_2_rob2[nrow(testBoW_2_rob2),2],3), nsmall = 3),")", sep = ""), 
                 "","",paste("(",format(round(testtfidf_rob2[nrow(testtfidf_rob2),2],3), nsmall = 3),")", sep = "")))


des=rbind(des, c(as.character(v[4]), "X",  "X",  "X",  "X", "X", "X", "X", "X", "X"))
des=rbind(des, c(as.character(v[5]), "X", "X",  "X",  "X", "X", "X", "X", "X", "X" ))
des=rbind(des, c(as.character(v[6]), "X",  "X",  "X",  "X", "X", "X", "X", "X", "X"))

print(xtable(des),include.rownames=FALSE, booktabs = TRUE)

des=c()
des=rbind(des, c(as.character(v[1]), paste(format(round(testvictimsubject_rob[nrow(testvictimsubject_rob),1],3), nsmall = 3),signif.num(testvictimsubject_rob[nrow(testvictimsubject_rob),4]),sep=""),"","",
                 paste(format(round(testnegativity_rob[nrow(testnegativity_rob),1],3), nsmall = 3),signif.num(testnegativity_rob[nrow(testnegativity_rob),4]),sep=""),"",""))

des=rbind(des, c("", paste("(",format(round(testvictimsubject_rob[nrow(testvictimsubject_rob),2],3), nsmall = 3),")", sep = ""),"","",
                 paste("(",format(round(testnegativity_rob[nrow(testnegativity_rob),2],3), nsmall = 3),")", sep = ""), "",""))

des=rbind(des, c(as.character(v[2]), "", paste(format(round(weightvictimshare$effect,3), nsmall = 3),signif.num(weightvictimshare$pvalue),sep=""),"",
                  "", paste(format(round(weightnegativity$effect,3), nsmall = 3),signif.num(weightnegativity$pvalue),sep=""),""))

des=rbind(des, c("", "", paste("(",format(round(weightvictimshare$se,3), nsmall = 3),")", sep = ""),"",
                 "", paste("(",format(round(weightnegativity$se,3), nsmall = 3),")", sep = ""), ""))

des=rbind(des, c(as.character(v[3]), "","", paste(format(round(testvictimsubject_rob2[nrow(testvictimsubject_rob2),1],3), nsmall = 3),signif.num(testvictimsubject_rob2[nrow(testvictimsubject_rob2),4]),sep=""),
                 "","",paste(format(round(testnegativity_rob2[nrow(testnegativity_rob2),1],3), nsmall = 3),signif.num(testnegativity_rob2[nrow(testnegativity_rob2),4]),sep="")))

des=rbind(des, c("", "","",paste("(",format(round(testvictimsubject_rob2[nrow(testvictimsubject_rob2),2],3), nsmall = 3),")", sep = ""),
                 "","",paste("(",format(round(testnegativity_rob2[nrow(testnegativity_rob2),2],3), nsmall = 3),")", sep = "")))


des=rbind(des, c(as.character(v[4]), "X",  "X",  "X",  "X", "X", "X"))
des=rbind(des, c(as.character(v[5]), "X", "X",  "X",  "X", "X", "X"))
des=rbind(des, c(as.character(v[6]), "X",  "X",  "X",  "X", "X", "X"))

print(xtable(des),include.rownames=FALSE, booktabs = TRUE)


###################################### Heterogeneity #########################################
data$female = 0
data$female[data$gender==" f"] = 1

lmDiDBoW_fem = lm(dist_bow_rel ~ sexual * female * after_treatment + word_count + factor(court) , data = data[data$date >= 7 & data$judgename_available==1 & data$gender != " x" ,])
vcov_lmDiDBoW_fem = vcovCR(lmDiDBoW_fem, cluster = data[data$date >= 7 & data$judgename_available==1 & data$gender != " x",]$court, type = "CR0")
testBoW_fem = coeftest(lmDiDBoW_fem, vcov=vcov_lmDiDBoW_fem)
vcov_lmDiDBoW_fem = NULL

lmDiDBoW_2_fem = lm(dist_bow_2_rel ~ sexual * female * after_treatment  + word_count + factor(court) , data = data[data$date >= 7 & data$judgename_available==1 & data$gender != " x",])
vcov_lmDiDBoW_2_fem = vcovCR(lmDiDBoW_2_fem, cluster = data[data$date >= 7 & data$judgename_available==1 & data$gender != " x",]$court, type = "CR0")
testBoW_2_fem = coeftest(lmDiDBoW_2_fem, vcov=vcov_lmDiDBoW_2_fem)
vcov_lmDiDBoW_2_fem = NULL

lmDiDtfidf_fem = lm(dist_tfidf_rel ~ sexual * female * after_treatment + word_count + factor(court) , data = data[data$date >= 7 & data$judgename_available==1 & data$gender != " x",])
vcov_lmDiDtfidf_fem = vcovCR(lmDiDtfidf_fem, cluster = data[data$date >= 7 & data$judgename_available==1 & data$gender != " x",]$court, type = "CR0")
testtfidf_fem = coeftest(lmDiDtfidf_fem, vcov=vcov_lmDiDtfidf_fem)
vcov_lmDiDtfidf_fem = NULL

lmDiDvictimsubject_fem = lm(victimshare ~ sexual * female * after_treatment  + word_count + factor(court) , data = data[data$victimshare_NA == 0 & data$judgename_available==1 & data$gender != " x",])
vcov_lmDiDvictimsubject_fem = vcovCR(lmDiDvictimsubject_fem, cluster = data[data$victimshare_NA ==0 & data$judgename_available==1 & data$gender != " x",]$court, type = "CR0")
testvictimsubject_fem = coeftest(lmDiDvictimsubject_fem, vcov=vcov_lmDiDvictimsubject_fem)
vcov_lmDiDvictimsubject_fem = NULL

lmDiDnegativity_fem = lm(negativity ~ sexual * female * after_treatment + word_count + factor(court) , data = data[data$perpetratorshare_NA == 0 & data$judgename_available==1 & data$gender != " x",])
vcov_lmDiDnegativity_fem = vcovCR(lmDiDnegativity_fem, cluster = data[data$perpetratorshare_NA ==0 & data$judgename_available==1 & data$gender != " x",]$court, type = "CR0")
testnegativity_fem = coeftest(lmDiDnegativity_fem, vcov=vcov_lmDiDnegativity_fem)
vcov_lmDiDnegativity_fem = NULL

################

lmDiDBoW_demj = lm(dist_bow_rel ~ sexual * democratic_judge * after_treatment + word_count + factor(court) , data = data[data$date >= 7 & data$judgename_available==1 & (data$democratic_judge==1  | data$republican_judge==1),])
vcov_lmDiDBoW_demj = vcovCR(lmDiDBoW_demj, cluster = data[data$date >= 7 & data$judgename_available==1 & (data$democratic_judge==1  | data$republican_judge==1),]$court, type = "CR0")
testBoW_demj = coeftest(lmDiDBoW_demj, vcov=vcov_lmDiDBoW_demj)
vcov_lmDiDBoW_demj = NULL

lmDiDBoW_2_demj = lm(dist_bow_2_rel ~ sexual * democratic_judge * after_treatment  + word_count + factor(court) , data = data[data$date >= 7 & data$judgename_available==1 & (data$democratic_judge==1  | data$republican_judge==1),])
vcov_lmDiDBoW_2_demj = vcovCR(lmDiDBoW_2_demj, cluster = data[data$date >= 7 & data$judgename_available==1 & (data$democratic_judge==1  | data$republican_judge==1),]$court, type = "CR0")
testBoW_2_demj = coeftest(lmDiDBoW_2_demj, vcov=vcov_lmDiDBoW_2_demj)
vcov_lmDiDBoW_2_demj = NULL

lmDiDtfidf_demj = lm(dist_tfidf_rel ~ sexual * democratic_judge * after_treatment + word_count + factor(court) , data = data[data$date >= 7 & data$judgename_available==1 & (data$democratic_judge==1  | data$republican_judge==1),])
vcov_lmDiDtfidf_demj = vcovCR(lmDiDtfidf_demj, cluster = data[data$date >= 7 & data$judgename_available==1 & (data$democratic_judge==1  | data$republican_judge==1),]$court, type = "CR0")
testtfidf_demj = coeftest(lmDiDtfidf_demj, vcov=vcov_lmDiDtfidf_demj)
vcov_lmDiDtfidf_demj = NULL

lmDiDvictimsubject_demj = lm(victimshare ~ sexual * democratic_judge * after_treatment  + word_count + factor(court) , data = data[data$victimshare_NA == 0 & data$judgename_available==1 & (data$democratic_judge==1  | data$republican_judge==1),])
vcov_lmDiDvictimsubject_demj = vcovCR(lmDiDvictimsubject_demj, cluster = data[data$victimshare_NA ==0 & data$judgename_available==1 & (data$democratic_judge==1  | data$republican_judge==1),]$court, type = "CR0")
testvictimsubject_demj = coeftest(lmDiDvictimsubject_demj, vcov=vcov_lmDiDvictimsubject_demj)
vcov_lmDiDvictimsubject_demj = NULL

lmDiDnegativity_demj = lm(negativity ~ sexual * democratic_judge * after_treatment + word_count + factor(court) , data = data[data$perpetratorshare_NA == 0 & data$judgename_available==1 & (data$democratic_judge==1  | data$republican_judge==1),])
vcov_lmDiDnegativity_demj = vcovCR(lmDiDnegativity_demj, cluster = data[data$perpetratorshare_NA ==0 & data$judgename_available==1 & (data$democratic_judge==1  | data$republican_judge==1),]$court, type = "CR0")
testnegativity_demj = coeftest(lmDiDnegativity_demj, vcov=vcov_lmDiDnegativity_demj)
vcov_lmDiDnegativity_demj = NULL

################

lmDiDBoW_dems = lm(dist_bow_rel ~ sexual * democratic_state * after_treatment + word_count + factor(court) , data = data[data$date >= 7 &  (data$democratic_state ==1  | data$republican_state==1),])
vcov_lmDiDBoW_dems = vcovCR(lmDiDBoW_dems, cluster = data[data$date >= 7 & (data$democratic_state==1  | data$republican_state==1),]$court, type = "CR0")
testBoW_dems = coeftest(lmDiDBoW_dems, vcov=vcov_lmDiDBoW_dems)
vcov_lmDiDBoW_dems = NULL

lmDiDBoW_2_dems = lm(dist_bow_2_rel ~ sexual * democratic_state * after_treatment  + word_count + factor(court) , data = data[data$date >= 7 &  (data$democratic_state==1  | data$republican_state==1),])
vcov_lmDiDBoW_2_dems = vcovCR(lmDiDBoW_2_dems, cluster = data[data$date >= 7 &  (data$democratic_state==1  | data$republican_state==1),]$court, type = "CR0")
testBoW_2_dems = coeftest(lmDiDBoW_2_dems, vcov=vcov_lmDiDBoW_2_dems)
vcov_lmDiDBoW_2_dems = NULL

lmDiDtfidf_dems = lm(dist_tfidf_rel ~ sexual * democratic_state * after_treatment + word_count + factor(court) , data = data[data$date >= 7 &  (data$democratic_state==1  | data$republican_state==1),])
vcov_lmDiDtfidf_dems = vcovCR(lmDiDtfidf_dems, cluster = data[data$date >= 7 & (data$democratic_state==1  | data$republican_state==1),]$court, type = "CR0")
testtfidf_dems = coeftest(lmDiDtfidf_dems, vcov=vcov_lmDiDtfidf_dems)
vcov_lmDiDtfidf_dems = NULL

lmDiDvictimsubject_dems = lm(victimshare ~ sexual * democratic_state * after_treatment  + word_count + factor(court) , data = data[data$victimshare_NA == 0 &  (data$democratic_state==1  | data$republican_state==1),])
vcov_lmDiDvictimsubject_dems = vcovCR(lmDiDvictimsubject_dems, cluster = data[data$victimshare_NA ==0 & (data$democratic_state==1  | data$republican_state==1),]$court, type = "CR0")
testvictimsubject_dems = coeftest(lmDiDvictimsubject_dems, vcov=vcov_lmDiDvictimsubject_dems)
vcov_lmDiDvictimsubject_dems = NULL

lmDiDnegativity_dems = lm(negativity ~ sexual * democratic_state * after_treatment + word_count + factor(court) , data = data[data$perpetratorshare_NA == 0 &  (data$democratic_state==1  | data$republican_state==1),])
vcov_lmDiDnegativity_dems = vcovCR(lmDiDnegativity_dems, cluster = data[data$perpetratorshare_NA ==0 & (data$democratic_state==1  | data$republican_state==1),]$court, type = "CR0")
testnegativity_dems = coeftest(lmDiDnegativity_dems, vcov=vcov_lmDiDnegativity_dems)
vcov_lmDiDnegativity_dems = NULL


################# Tables 

v <- c("female x sexual x post", "dem. judge x sexual x post", 
       "dem. state x sexual x post",   
       "post", "court FE","# words")
des=c()
des=rbind(des, c(as.character(v[1]), paste(format(round(testBoW_fem[nrow(testBoW_fem),1],3), nsmall = 3),signif.num(testBoW_fem[nrow(testBoW_fem),4]),sep=""),"","",
                 paste(format(round(testBoW_2_fem[nrow(testBoW_2_fem),1],3), nsmall = 3),signif.num(testBoW_2_fem[nrow(testBoW_2_fem),4]),sep=""),"","",
                 paste(format(round(testtfidf_fem[nrow(testtfidf_fem),1],3), nsmall = 3),signif.num(testtfidf_fem[nrow(testtfidf_fem),4]),sep=""),"",""))

des=rbind(des, c("", paste("(",format(round(testBoW_fem[nrow(testBoW_fem),2],3), nsmall = 3),")", sep = ""),"","",
                 paste("(",format(round(testBoW_2_fem[nrow(testBoW_2_fem),2],3), nsmall = 3),")", sep = ""), "","",
                 paste("(",format(round(testtfidf_fem[nrow(testtfidf_fem),2],3), nsmall = 3),")", sep = ""), "",""))

des=rbind(des, c(as.character(v[2]),"", paste(format(round(testBoW_demj[nrow(testBoW_demj),1],3), nsmall = 3),signif.num(testBoW_demj[nrow(testBoW_demj),4]),sep=""),"",
                 "", paste(format(round(testBoW_2_demj[nrow(testBoW_2_demj),1],3), nsmall = 3),signif.num(testBoW_2_demj[nrow(testBoW_2_demj),4]),sep=""),"",
                 "", paste(format(round(testtfidf_demj[nrow(testtfidf_demj),1],3), nsmall = 3),signif.num(testtfidf_demj[nrow(testtfidf_demj),4]),sep=""),""))

des=rbind(des, c("", "", paste("(",format(round(testBoW_demj[nrow(testBoW_demj),2],3), nsmall = 3),")", sep = ""),"",
                 "", paste("(",format(round(testBoW_2_demj[nrow(testBoW_2_demj),2],3), nsmall = 3),")", sep = ""), "",
                 "", paste("(",format(round(testtfidf_demj[nrow(testtfidf_demj),2],3), nsmall = 3),")", sep = ""), ""))


des=rbind(des, c(as.character(v[3]), "","", paste(format(round(testBoW_dems[nrow(testBoW_dems),1],3), nsmall = 3),signif.num(testBoW_dems[nrow(testBoW_dems),4]),sep=""),
                 "","",paste(format(round(testBoW_2_dems[nrow(testBoW_2_dems),1],3), nsmall = 3),signif.num(testBoW_2_dems[nrow(testBoW_2_dems),4]),sep=""),
                 "","",paste(format(round(testtfidf_dems[nrow(testtfidf_dems),1],3), nsmall = 3),signif.num(testtfidf_dems[nrow(testtfidf_dems),4]),sep="")))

des=rbind(des, c("", "","",paste("(",format(round(testBoW_dems[nrow(testBoW_dems),2],3), nsmall = 3),")", sep = ""),
                 "","",paste("(",format(round(testBoW_2_dems[nrow(testBoW_2_dems),2],3), nsmall = 3),")", sep = ""), 
                 "","",paste("(",format(round(testtfidf_dems[nrow(testtfidf_dems),2],3), nsmall = 3),")", sep = "")))


des=rbind(des, c(as.character(v[4]), "X",  "X",  "X",  "X", "X", "X", "X", "X", "X"))
des=rbind(des, c(as.character(v[5]), "X", "X",  "X",  "X", "X", "X", "X", "X", "X" ))
des=rbind(des, c(as.character(v[6]), "X",  "X",  "X",  "X", "X", "X", "X", "X", "X"))

print(xtable(des),include.rownames=FALSE, booktabs = TRUE)

des=c()
des=rbind(des, c(as.character(v[1]), paste(format(round(testvictimsubject_fem[nrow(testvictimsubject_fem),1],3), nsmall = 3),signif.num(testvictimsubject_fem[nrow(testvictimsubject_fem),4]),sep=""),"","",
                 paste(format(round(testnegativity_fem[nrow(testnegativity_fem),1],3), nsmall = 3),signif.num(testnegativity_fem[nrow(testnegativity_fem),4]),sep=""),"",""))

des=rbind(des, c("", paste("(",format(round(testvictimsubject_fem[nrow(testvictimsubject_fem),2],3), nsmall = 3),")", sep = ""),"","",
                 paste("(",format(round(testnegativity_fem[nrow(testnegativity_fem),2],3), nsmall = 3),")", sep = ""), "",""))

des=rbind(des, c(as.character(v[3]), "", paste(format(round(testvictimsubject_demj[nrow(testvictimsubject_demj),1],3), nsmall = 3),signif.num(testvictimsubject_demj[nrow(testvictimsubject_demj),4]),sep=""),"",
                 "",paste(format(round(testnegativity_demj[nrow(testnegativity_demj),1],3), nsmall = 3),signif.num(testvictimsubject_demj[nrow(testvictimsubject_demj),4]),sep=""),""))

des=rbind(des, c("", "",paste("(",format(round(testvictimsubject_demj[nrow(testvictimsubject_demj),2],3), nsmall = 3),")", sep = ""),"",
                 "",paste("(",format(round(testnegativity_demj[nrow(testnegativity_demj),2],3), nsmall = 3),")", sep = ""),""))

des=rbind(des, c(as.character(v[3]), "","", paste(format(round(testvictimsubject_dems[nrow(testvictimsubject_dems),1],3), nsmall = 3),signif.num(testvictimsubject_dems[nrow(testvictimsubject_dems),4]),sep=""),
                 "","",paste(format(round(testnegativity_dems[nrow(testnegativity_dems),1],3), nsmall = 3),signif.num(testvictimsubject_dems[nrow(testvictimsubject_dems),4]),sep="")))

des=rbind(des, c("", "","",paste("(",format(round(testvictimsubject_dems[nrow(testvictimsubject_dems),2],3), nsmall = 3),")", sep = ""),
                 "","",paste("(",format(round(testnegativity_dems[nrow(testnegativity_dems),2],3), nsmall = 3),")", sep = "")))


des=rbind(des, c(as.character(v[4]), "X",  "X",  "X",  "X", "X", "X"))
des=rbind(des, c(as.character(v[5]), "X", "X",  "X",  "X", "X", "X"))
des=rbind(des, c(as.character(v[6]), "X",  "X",  "X",  "X", "X", "X"))

print(xtable(des),include.rownames=FALSE, booktabs = TRUE)

######################### Word2Vec ################
word2vec <- read_delim("~/word2vec_final.txt", delim = ";", escape_double = FALSE, trim_ws = TRUE)

word2vec$dist_q1_2015 = 0
for (i in c(2:(nrow(word2vec)/2))){
  a = c(t(word2vec[1,c(3:ncol(word2vec))]))
  b = c(t(word2vec[i,c(3:ncol(word2vec))]))
  word2vec$dist_q1_2015[i] = manhattan_dist(a,b)
}
for (i in c((nrow(word2vec)/2+2):nrow(word2vec))){
  a = c(t(word2vec[nrow(word2vec)/2+1,c(3:ncol(word2vec))]))
  b = c(t(word2vec[i,c(3:ncol(word2vec))]))
  word2vec$dist_q1_2015[i] = manhattan_dist(a,b)
}

x = c(1:22)
y.control = word2vec[word2vec$sexual == 0,]$dist_q1_2015[-1]
y.treat = word2vec[word2vec$sexual == 1,]$dist_q1_2015[-1]
plot(x,y.treat,type = "l",xlab="",ylab = "Word2Vec: Distance to q1-2015",xaxt="n",ylim=c(20,33))
axis(side = 1, at = c(4,8,12,16,20), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
lines(x,y.control,lty = 3)
rect(xleft=11, ybottom=-10, xright=15, ytop=100, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(v=11)
mtext("#MeToo", side=3, at=11)


########################## Event Study #############
panel_data = data[data$sexual == 1 & data$judge_name != " UNKNOWN" & data$judge_name != " PerCuriam",]

panel_data$judge_name = paste(panel_data$judge_name,panel_data$court)

judges_with_enough_obs = panel_data %>% 
  group_by(judge_name) %>% 
  summarise(remain = as.numeric(sum(date<34)>0 & sum(date>45)>0))

panel_data = merge(panel_data, judges_with_enough_obs, by = "judge_name", all.x = TRUE)
panel_data = panel_data[panel_data$remain == 1,]


panel_data$halfyear = 0
panel_data$halfyear[panel_data$date >=  7 & panel_data$date <=10] = 1
panel_data$halfyear[panel_data$date >=  11 & panel_data$date <=16] = 2
panel_data$halfyear[panel_data$date >=  17 & panel_data$date <=22] = 3
panel_data$halfyear[panel_data$date >=  23 & panel_data$date <=28] = 4
panel_data$halfyear[panel_data$date >=  29 & panel_data$date <=34] = 5
panel_data$halfyear[panel_data$date >=  35 & panel_data$date <=40] = 6
panel_data$halfyear[panel_data$date >=  41 & panel_data$date <=46] = 7
panel_data$halfyear[panel_data$date >=  47 & panel_data$date <=52] = 8
panel_data$halfyear[panel_data$date >=  53 & panel_data$date <=58] = 9
panel_data$halfyear[panel_data$date >=  59 & panel_data$date <=64] = 10
panel_data$halfyear[panel_data$date >=  65 & panel_data$date <=71] = 11
panel_data$court = as.factor(panel_data$court)
panel_data$judge_name = as.factor(panel_data$judge_name)
panel_data$halfyear = as.factor(panel_data$halfyear)
halfyearaggregated_data = aggregate(.~ halfyear + judge_name-1, data = panel_data[panel_data$date>=7,c(1,738:740,756)], FUN = mean, na.rm=TRUE, na.action=NULL)
halfyearaggregated_data$halfyear = as.numeric(halfyearaggregated_data$halfyear)-1
halfyearaggregated_data$h22015 = 0
halfyearaggregated_data$h22015[halfyearaggregated_data$halfyear == 1] = 1
halfyearaggregated_data$h12016 = 0
halfyearaggregated_data$h12016[halfyearaggregated_data$halfyear == 2] = 1
halfyearaggregated_data$h22016 = 0
halfyearaggregated_data$h22016[halfyearaggregated_data$halfyear == 3] = 1
halfyearaggregated_data$h12017 = 0
halfyearaggregated_data$h12017[halfyearaggregated_data$halfyear == 4] = 1
halfyearaggregated_data$h22017 = 0
halfyearaggregated_data$h22017[halfyearaggregated_data$halfyear == 5] = 1
halfyearaggregated_data$h12018 = 0
halfyearaggregated_data$h12018[halfyearaggregated_data$halfyear == 6] = 1
halfyearaggregated_data$h22018 = 0
halfyearaggregated_data$h22018[halfyearaggregated_data$halfyear == 7] = 1
halfyearaggregated_data$h12019 = 0
halfyearaggregated_data$h12019[halfyearaggregated_data$halfyear == 8] = 1
halfyearaggregated_data$h22019 = 0
halfyearaggregated_data$h22019[halfyearaggregated_data$halfyear == 9] = 1
halfyearaggregated_data$h12020 = 0
halfyearaggregated_data$h12020[halfyearaggregated_data$halfyear == 10] = 1
halfyearaggregated_data$h22020 = 0
halfyearaggregated_data$h22020[halfyearaggregated_data$halfyear == 11] = 1

FE_bow = plm(dist_bow_rel ~ h22015 + h12016 + h22016 + h12017 + h22017  + h22018 + h12019 + h22019 + h12020 + h22020 , index=c("judge_name", "halfyear"), data = halfyearaggregated_data, model=c("within"))
test_FE_bow = coeftest(FE_bow, vcov.=vcovHC(FE_bow,type="HC1"))

FE_bow2 = plm(dist_bow_2_rel ~ h22015 +h12016 + h22016 + h12017 + h22017  + h12018 + h12019 + h22019 + h12020 + h22020 , index=c("judge_name", "halfyear"), data = halfyearaggregated_data, model=c("within"))
test_FE_bow2 = coeftest(FE_bow2, vcov.=vcovHC(FE_bow2,type="HC1"))

FE_tfidf = plm(dist_tfidf_rel ~ h22015 + h12016 + h22016 + h12017 + h22017  + h12018 + h12019 + h22019 + h12020 + h22020, index=c("judge_name", "halfyear"), data = halfyearaggregated_data, model=c("within"))
test_FE_tfidf = coeftest(FE_tfidf, vcov.=vcovHC(FE_tfidf,type="HC1"))


plot(c(-5:5),c(test_FE_bow[1:5,1],0,test_FE_bow[6:10,1]),"l",xlab="",ylab = "BoW: Distance to H1-2015",xaxt="n",ylim=c(-5,5))
lines(c(-5:5),c(test_FE_bow[1:5,1] + 1.68*test_FE_bow[1:5,2],0,test_FE_bow[6:10,1] + 1.68*test_FE_bow[6:10,2]),col ="grey")
lines(c(-5:5),c(test_FE_bow[1:5,1] - 1.68*test_FE_bow[1:5,2],0,test_FE_bow[6:10,1] - 1.68*test_FE_bow[6:10,2]),col ="grey")
axis(side = 1, at = c(-3.7,-1.7,0.3,2.3,4.3), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
rect(xleft=0, ybottom=-10, xright=2, ytop=100, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(h=0,lty=3)
mtext("#MeToo", side=3, at=0)

plot(c(-5:5),c(test_FE_bow2[1:5,1],0,test_FE_bow2[6:10,1]),"l",xlab="",ylab = "red. sample BoW: Distance to H1-2015",xaxt="n",ylim=c(-5,5))
lines(c(-5:5),c(test_FE_bow2[1:5,1] + 1.68*test_FE_bow2[1:5,2],0,test_FE_bow2[6:10,1] + 1.68*test_FE_bow2[6:10,2]),col ="grey")
lines(c(-5:5),c(test_FE_bow2[1:5,1] - 1.68*test_FE_bow2[1:5,2],0,test_FE_bow2[6:10,1] - 1.68*test_FE_bow2[6:10,2]),col ="grey")
axis(side = 1, at = c(-3.7,-1.7,0.3,2.3,4.3), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
rect(xleft=0, ybottom=-10, xright=2, ytop=100, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(h=0,lty=3)
mtext("#MeToo", side=3, at=0)

plot(c(-5:5),c(test_FE_tfidf[1:5,1],0,test_FE_tfidf[6:10,1]),"l",xlab="",ylab = "tfidf: Distance to H1-2015",xaxt="n",ylim=c(-8,5))
lines(c(-5:5),c(test_FE_tfidf[1:5,1] + 1.68*test_FE_tfidf[1:5,2],0,test_FE_tfidf[6:10,1] + 1.68*test_FE_tfidf[6:10,2]),col ="grey")
lines(c(-5:5),c(test_FE_tfidf[1:5,1] - 1.68*test_FE_tfidf[1:5,2],0,test_FE_tfidf[6:10,1] - 1.68*test_FE_tfidf[6:10,2]),col ="grey")
axis(side = 1, at = c(-3.7,-1.7,0.3,2.3,4.3), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
rect(xleft=0, ybottom=-10, xright=2, ytop=100, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(h=0,lty=3)
mtext("#MeToo", side=3, at=0)


halfyearaggregated_data = merge(halfyearaggregated_data,unique(panel_data[,c(1,741:744,754)]), by = "judge_name")

FE_bow_fem = plm(dist_bow_rel ~ female*(h12016 + h22016 + h12017 + h22017 + h12018  + h22018 + h12019 + h22019 + h12020 + h22020), index=c("judge_name", "halfyear"), data = halfyearaggregated_data, model=c("within"))
test_FE_bow_fem = coeftest(FE_bow_fem, vcov.=vcovHC(FE_bow_fem,type="HC1"))

FE_bow2_fem = plm(dist_bow_2_rel ~ female * (h12016 + h22016 + h12017 + h22017 + h12018 + h22018 + h12019 + h22019 + h12020 + h22020) , index=c("judge_name", "halfyear"), data = halfyearaggregated_data, model=c("within"))
test_FE_bow2_fem = coeftest(FE_bow2_fem, vcov.=vcovHC(FE_bow2_fem,type="HC1"))

FE_tfidf_fem = plm(dist_tfidf_rel ~ female * (h12016 + h22016 + h12017 + h22017 + h12018 + h22018 + h12019 + h22019 + h12020 + h22020), index=c("judge_name", "halfyear"), data = halfyearaggregated_data, model=c("within"))
test_FE_tfidf_fem = coeftest(FE_tfidf_fem, vcov.=vcovHC(FE_tfidf_fem,type="HC1"))


plot(c(-4:5),c(test_FE_bow_fem[11:20,1]),"l",xlab="",ylab = "BoW: Difference of female to male judges",xaxt="n",ylim=c(-9,7))
lines(c(-4:5),c(test_FE_bow_fem[11:20,1] + 1.68*test_FE_bow_fem[11:20,2]),col ="grey")
lines(c(-4:5),c(test_FE_bow_fem[11:20,1] - 1.68*test_FE_bow_fem[11:20,2]),col ="grey")
axis(side = 1, at = c(-3.7,-1.7,0.3,2.3,4.3), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
rect(xleft=0, ybottom=-10, xright=2, ytop=100, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(h=0,lty=3)
mtext("#MeToo", side=3, at=0)

plot(c(-4:5),c(test_FE_bow2_fem[11:20,1]),"l",xlab="",ylab = "red. sample BoW: Diff. of fem. to male judges",xaxt="n",ylim=c(-8,8))
lines(c(-4:5),c(test_FE_bow2_fem[11:20,1] + 1.68*test_FE_bow2_fem[11:20,2]),col ="grey")
lines(c(-4:5),c(test_FE_bow2_fem[11:20,1] - 1.68*test_FE_bow2_fem[11:20,2]),col ="grey")
axis(side = 1, at = c(-3.7,-1.7,0.3,2.3,4.3), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
rect(xleft=0, ybottom=-10, xright=2, ytop=100, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(h=0,lty=3)
mtext("#MeToo", side=3, at=0)

plot(c(-4:5),c(test_FE_tfidf_fem[11:20,1]),"l",xlab="",ylab = "tfidf: Difference of female to male judges",xaxt="n",ylim=c(-16,12))
lines(c(-4:5),c(test_FE_tfidf_fem[11:20,1] + 1.68*test_FE_tfidf_fem[11:20,2]),col ="grey")
lines(c(-4:5),c(test_FE_tfidf_fem[11:20,1] - 1.68*test_FE_tfidf_fem[11:20,2]),col ="grey")
axis(side = 1, at = c(-3.7,-1.7,0.3,2.3,4.3), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
rect(xleft=0, ybottom=-100, xright=2, ytop=100, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(h=0,lty=3)
mtext("#MeToo", side=3, at=0)



FE_bow_dem = plm(dist_bow_rel ~ democratic_judge*(h12016 + h22016 + h12017 + h22017  + h12018 + h22018 + h12019 + h22019 + h12020 + h22020), index=c("judge_name", "halfyear"), data = halfyearaggregated_data[halfyearaggregated_data$democratic_judge==1 |halfyearaggregated_data$republican_judge==1,], model=c("within"))
test_FE_bow_dem = coeftest(FE_bow_dem, vcov.=vcovHC(FE_bow_dem,type="HC1"))

FE_bow2_dem = plm(dist_bow_2_rel ~ democratic_judge * (h12016 + h22016 + h12017 + h22017  + h12018 + h22018 + h12019 + h22019 + h12020 + h22020) , index=c("judge_name", "halfyear"), data = halfyearaggregated_data[halfyearaggregated_data$democratic_judge==1 |halfyearaggregated_data$republican_judge==1,], model=c("within"))
test_FE_bow2_dem = coeftest(FE_bow2_dem, vcov.=vcovHC(FE_bow2_dem,type="HC1"))

FE_tfidf_dem = plm(dist_tfidf_rel ~ democratic_judge * (h12016 + h22016 + h12017 + h22017  + h12018 + h22018 + h12019 + h22019 + h12020 + h22020), index=c("judge_name", "halfyear"), data = halfyearaggregated_data[halfyearaggregated_data$democratic_judge==1 |halfyearaggregated_data$republican_judge==1,], model=c("within"))
test_FE_tfidf_dem = coeftest(FE_tfidf_dem, vcov.=vcovHC(FE_tfidf_dem,type="HC1"))


plot(c(-4:5),c(test_FE_bow_dem[11:20,1]),"l",xlab="",ylab = "BoW: Difference of dem. to rep. judges",xaxt="n",ylim=c(-7,8))
lines(c(-4:5),c(test_FE_bow_dem[11:20,1] + 1.68*test_FE_bow_dem[11:20,2]),col ="grey")
lines(c(-4:5),c(test_FE_bow_dem[11:20,1] - 1.68*test_FE_bow_dem[11:20,2]),col ="grey")
axis(side = 1, at = c(-3.7,-1.7,0.3,2.3,4.3), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
rect(xleft=0, ybottom=-10, xright=2, ytop=100, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(h=0,lty=3)
mtext("#MeToo", side=3, at=0)

plot(c(-4:5),c(test_FE_bow2_dem[11:20,1]),"l",xlab="",ylab = "red. sample BoW: Diff. of dem. to rep. judges",xaxt="n",ylim=c(-8,10))
lines(c(-4:5),c(test_FE_bow2_dem[11:20,1] + 1.68*test_FE_bow2_dem[11:20,2]),col ="grey")
lines(c(-4:5),c(test_FE_bow2_dem[11:20,1] - 1.68*test_FE_bow2_dem[11:20,2]),col ="grey")
axis(side = 1, at = c(-3.7,-1.7,0.3,2.3,4.3), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
rect(xleft=0, ybottom=-100, xright=2, ytop=100, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(h=0,lty=3)
mtext("#MeToo", side=3, at=0)

plot(c(-4:5),c(test_FE_tfidf_dem[11:20,1]),"l",xlab="",ylab = "tfidf: Difference of dem. to rep. judges",xaxt="n",ylim=c(-18,15))
lines(c(-4:5),c(test_FE_tfidf_dem[11:20,1] + 1.68*test_FE_tfidf_dem[11:20,2]),col ="grey")
lines(c(-4:5),c(test_FE_tfidf_dem[11:20,1] - 1.68*test_FE_tfidf_dem[11:20,2]),col ="grey")
axis(side = 1, at = c(-3.7,-1.7,0.3,2.3,4.3), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
rect(xleft=0, ybottom=-100, xright=2, ytop=100, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(h=0,lty=3)
mtext("#MeToo", side=3, at=0)

#######################
panel_data_victim = panel_data[panel_data$victimshare_NA==0,-755]
judges_with_enough_obs = panel_data_victim %>% 
  group_by(judge_name) %>% 
  summarise(remain = as.numeric(sum(date<34)>0 & sum(date>45)>0))

panel_data_victim = merge(panel_data_victim, judges_with_enough_obs, by = "judge_name", all.x = TRUE)
panel_data_victim = panel_data_victim[panel_data_victim$remain == 1,]

panel_data_victim$halfyear = as.factor(panel_data_victim$halfyear)
halfyearaggregated_data = aggregate(.~ halfyear + judge_name, data = panel_data_victim[,c(1,452,453,755)], FUN = sum)
halfyearaggregated_data$victimshare = (halfyearaggregated_data$victim_subj)/(halfyearaggregated_data$victim_subj + halfyearaggregated_data$victim_obj)*100
halfyearaggregated_data$halfyear = as.numeric(halfyearaggregated_data$halfyear)-1

halfyearaggregated_data$h12015 = 0
halfyearaggregated_data$h12015[halfyearaggregated_data$halfyear == 0] = 1
halfyearaggregated_data$h22015 = 0
halfyearaggregated_data$h22015[halfyearaggregated_data$halfyear == 1] = 1
halfyearaggregated_data$h12016 = 0
halfyearaggregated_data$h12016[halfyearaggregated_data$halfyear == 2] = 1
halfyearaggregated_data$h22016 = 0
halfyearaggregated_data$h22016[halfyearaggregated_data$halfyear == 3] = 1
halfyearaggregated_data$h12017 = 0
halfyearaggregated_data$h12017[halfyearaggregated_data$halfyear == 4] = 1
halfyearaggregated_data$h22017 = 0
halfyearaggregated_data$h22017[halfyearaggregated_data$halfyear == 5] = 1
halfyearaggregated_data$h12018 = 0
halfyearaggregated_data$h12018[halfyearaggregated_data$halfyear == 6] = 1
halfyearaggregated_data$h22018 = 0
halfyearaggregated_data$h22018[halfyearaggregated_data$halfyear == 7] = 1
halfyearaggregated_data$h12019 = 0
halfyearaggregated_data$h12019[halfyearaggregated_data$halfyear == 8] = 1
halfyearaggregated_data$h22019 = 0
halfyearaggregated_data$h22019[halfyearaggregated_data$halfyear == 9] = 1
halfyearaggregated_data$h12020 = 0
halfyearaggregated_data$h12020[halfyearaggregated_data$halfyear == 10] = 1
halfyearaggregated_data$h22020 = 0
halfyearaggregated_data$h22020[halfyearaggregated_data$halfyear == 11] = 1

FE_victimsubj = plm(victimshare ~ h12015 + h22015 + h12016 + h22016 + h12017 + h22017  + h22018 + h12019 + h22019 + h12020 + h22020 , index=c("judge_name", "halfyear"), data = halfyearaggregated_data, model=c("within"))
test_FE_victimsubj = coeftest(FE_victimsubj, vcov.=vcovHC(FE_victimsubj,type="HC1"))

plot(c(-6:5),c(test_FE_victimsubj[1:6,1],0,test_FE_victimsubj[7:11,1]),"l",xlab="",ylab = "mentions of victim as subject",xaxt="n",ylim=c(-5,7))
lines(c(-6:5),c(test_FE_victimsubj[1:6,1] + 1.68*test_FE_victimsubj[1:6,2],0,test_FE_victimsubj[7:11,1] + 1.68*test_FE_victimsubj[7:11,2]),col ="grey")
lines(c(-6:5),c(test_FE_victimsubj[1:6,1] - 1.68*test_FE_victimsubj[1:6,2],0,test_FE_victimsubj[7:11,1] - 1.68*test_FE_victimsubj[7:11,2]),col ="grey")
axis(side = 1, at = c(-5.7,-3.7,-1.7,0.3,2.3,4.3), labels = c("1-2015","1-2016","1-2017","1-2018","1-2019","1-2020"))
rect(xleft=0, ybottom=-10, xright=2, ytop=100, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(h=0,lty=3)
mtext("#MeToo", side=3, at=0)

halfyearaggregated_data = merge(halfyearaggregated_data,unique(panel_data[,c(1,741:744,754)]), by = "judge_name")

FE_victimsubj_fem = plm(victimshare ~ female*(h22015 + h12016 + h22016 + h12017 + h22017  + h12018 + h22018 + h12019 + h22019 + h12020 + h22020), index=c("judge_name", "halfyear"), data = halfyearaggregated_data, model=c("within"))
test_FE_victimsubj_fem = coeftest(FE_victimsubj_fem, vcov.=vcovHC(FE_victimsubj_fem,type="HC1"))

plot(c(-5:5),c(test_FE_victimsubj_fem[12:22,1]),"l",xlab="",ylab = "Victim as subject: Diff. of fem. to male judges",xaxt="n",ylim=c(-10,16))
lines(c(-5:5),c(test_FE_victimsubj_fem[12:22,1] + 1.68*test_FE_victimsubj_fem[12:22,2]),col ="grey")
lines(c(-5:5),c(test_FE_victimsubj_fem[12:22,1] - 1.68*test_FE_victimsubj_fem[12:22,2]),col ="grey")
axis(side = 1, at = c(-3.7,-1.7,0.3,2.3,4.3), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
rect(xleft=0, ybottom=-100, xright=2, ytop=100, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(h=0,lty=3)
mtext("#MeToo", side=3, at=0)

FE_victimsubj_dem = plm(victimshare ~ democratic_judge*(h22015 + h12016 + h22016 + h12017 + h22017 + h12018 + h22018 + h12019 + h22019 + h12020 + h22020), index=c("judge_name", "halfyear"), data = halfyearaggregated_data[halfyearaggregated_data$democratic_judge==1 |halfyearaggregated_data$republican_judge==1,], model=c("within"))
test_FE_victimsubj_dem = coeftest(FE_victimsubj_dem, vcov.=vcovHC(FE_victimsubj_dem,type="HC1"))

plot(c(-5:5),c(test_FE_victimsubj_dem[12:22,1]),"l",xlab="",ylab = "Victim as subject: Diff. of dem. to rep. judges",xaxt="n",ylim=c(-10,24))
lines(c(-5:5),c(test_FE_victimsubj_dem[12:22,1] + 1.68*test_FE_victimsubj_dem[12:22,2]),col ="grey")
lines(c(-5:5),c(test_FE_victimsubj_dem[12:22,1] - 1.68*test_FE_victimsubj_dem[12:22,2]),col ="grey")
axis(side = 1, at = c(-3.7,-1.7,0.3,2.3,4.3), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
rect(xleft=0, ybottom=-100, xright=2, ytop=100, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(h=0,lty=3)
mtext("#MeToo", side=3, at=0)



###########################
panel_data_perpetrator = panel_data[panel_data$perpetratorshare_NA==0,-755]

judges_with_enough_obs = panel_data_perpetrator %>% 
  group_by(judge_name) %>% 
  summarise(remain = as.numeric(sum(date<34)>0 & sum(date>45)>0))

panel_data_perpetrator = merge(panel_data_perpetrator, judges_with_enough_obs, by = "judge_name", all.x = TRUE)
panel_data_perpetrator = panel_data_perpetrator[panel_data_perpetrator$remain == 1,]

panel_data_perpetrator$halfyear = as.factor(panel_data_perpetrator$halfyear)
halfyearaggregated_data = aggregate(.~ halfyear + judge_name, data = panel_data_perpetrator[,c(1,447,755)], FUN = mean)
halfyearaggregated_data$halfyear = as.numeric(halfyearaggregated_data$halfyear)-1

halfyearaggregated_data$h12015 = 0
halfyearaggregated_data$h12015[halfyearaggregated_data$halfyear == 0] = 1
halfyearaggregated_data$h22015 = 0
halfyearaggregated_data$h22015[halfyearaggregated_data$halfyear == 1] = 1
halfyearaggregated_data$h12016 = 0
halfyearaggregated_data$h12016[halfyearaggregated_data$halfyear == 2] = 1
halfyearaggregated_data$h22016 = 0
halfyearaggregated_data$h22016[halfyearaggregated_data$halfyear == 3] = 1
halfyearaggregated_data$h12017 = 0
halfyearaggregated_data$h12017[halfyearaggregated_data$halfyear == 4] = 1
halfyearaggregated_data$h22017 = 0
halfyearaggregated_data$h22017[halfyearaggregated_data$halfyear == 5] = 1
halfyearaggregated_data$h12018 = 0
halfyearaggregated_data$h12018[halfyearaggregated_data$halfyear == 6] = 1
halfyearaggregated_data$h22018 = 0
halfyearaggregated_data$h22018[halfyearaggregated_data$halfyear == 7] = 1
halfyearaggregated_data$h12019 = 0
halfyearaggregated_data$h12019[halfyearaggregated_data$halfyear == 8] = 1
halfyearaggregated_data$h22019 = 0
halfyearaggregated_data$h22019[halfyearaggregated_data$halfyear == 9] = 1
halfyearaggregated_data$h12020 = 0
halfyearaggregated_data$h12020[halfyearaggregated_data$halfyear == 10] = 1
halfyearaggregated_data$h22020 = 0
halfyearaggregated_data$h22020[halfyearaggregated_data$halfyear == 11] = 1

FE_perpetrator = plm(negativity ~ h12015 + h22015 + h12016 + h22016 + h12017 + h22017  + h22018 + h12019 + h22019 + h12020 + h22020 , index=c("judge_name", "halfyear"), data = halfyearaggregated_data, model=c("within"))
test_FE_perpetrator = coeftest(FE_perpetrator, vcov.=vcovHC(FE_perpetrator,type="HC1"))

plot(c(-6:5),c(test_FE_perpetrator[1:6,1],0,test_FE_perpetrator[7:11,1]),"l",xlab="",ylab = "Negativity of perpetrator context",xaxt="n",ylim=c(-0.6,0.6))
lines(c(-6:5),c(test_FE_perpetrator[1:6,1] + 1.68*test_FE_perpetrator[1:6,2],0,test_FE_perpetrator[7:11,1] + 1.68*test_FE_perpetrator[7:11,2]),col ="grey")
lines(c(-6:5),c(test_FE_perpetrator[1:6,1] - 1.68*test_FE_perpetrator[1:6,2],0,test_FE_perpetrator[7:11,1] - 1.68*test_FE_perpetrator[7:11,2]),col ="grey")
axis(side = 1, at = c(-5.7,-3.7,-1.7,0.3,2.3,4.3), labels = c("1-2015","1-2016","1-2017","1-2018","1-2019","1-2020"))
rect(xleft=0, ybottom=-10, xright=2, ytop=100, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(h=0,lty=3)
mtext("#MeToo", side=3, at=0)

halfyearaggregated_data = merge(halfyearaggregated_data,unique(panel_data[,c(1,741:744,754)]), by = "judge_name")

FE_perpetrator_fem = plm(negativity ~ female*(h22015 + h12016 + h22016 + h12017 + h22017 + h12018 + h22018 + h12019 + h22019 + h12020 + h22020), index=c("judge_name", "halfyear"), data = halfyearaggregated_data, model=c("within"))
test_FE_perpetrator_fem = coeftest(FE_perpetrator_fem, vcov.=vcovHC(FE_perpetrator_fem,type="HC1"))

plot(c(-5:5),c(test_FE_perpetrator_fem[12:22,1]),"l",xlab="",ylab = "Context negativity: Diff. of fem. to male judges",xaxt="n",ylim=c(-1.5,1.5))
lines(c(-5:5),c(test_FE_perpetrator_fem[12:22,1] + 1.68*test_FE_perpetrator_fem[12:22,2]),col ="grey")
lines(c(-5:5),c(test_FE_perpetrator_fem[12:22,1] - 1.68*test_FE_perpetrator_fem[12:22,2]),col ="grey")
axis(side = 1, at = c(-3.7,-1.7,0.3,2.3,4.3), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
rect(xleft=0, ybottom=-100, xright=2, ytop=100, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(h=0,lty=3)
mtext("#MeToo", side=3, at=0)

FE_perpetrator_dem = plm(negativity ~ democratic_judge*(h22015 + h12016 + h22016 + h12017 + h22017 + h12018 + h22018 + h12019 + h22019 + h12020 + h22020), index=c("judge_name", "halfyear"), data = halfyearaggregated_data[halfyearaggregated_data$democratic_judge==1 |halfyearaggregated_data$republican_judge==1,], model=c("within"))
test_FE_perpetrator_dem = coeftest(FE_perpetrator_dem, vcov.=vcovHC(FE_perpetrator_dem,type="HC1"))

plot(c(-5:5),c(test_FE_perpetrator_dem[12:22,1]),"l",xlab="",ylab = "Context negativity: Diff. of dem. to rep. judges",xaxt="n",ylim=c(-1.2,1.4))
lines(c(-5:5),c(test_FE_perpetrator_dem[12:22,1] + 1.68*test_FE_perpetrator_dem[12:22,2]),col ="grey")
lines(c(-5:5),c(test_FE_perpetrator_dem[12:22,1] - 1.68*test_FE_perpetrator_dem[12:22,2]),col ="grey")
axis(side = 1, at = c(-3.7,-1.7,0.3,2.3,4.3), labels = c("1-2016","1-2017","1-2018","1-2019","1-2020"))
rect(xleft=0, ybottom=-100, xright=2, ytop=100, col= rgb(0.5,0.5,0.5,alpha=0.2),border = rgb(0.5,0.5,0.5,alpha=0.2))
abline(h=0,lty=3)
mtext("#MeToo", side=3, at=0)
