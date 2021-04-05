library(readr)
cryo_observer <- read_csv("cryo_observer_scale.csv")
library(ggpubr)
boxplot(cryo_test$OBS_Thickness_preop, cryo_test$OBS_Thickness_postop,range = 0)
shapiro.test(cryo_test$OBS_Thickness_preop)
shapiro.test(cryo_test$Total_Observer_Score_preop)
shapiro.test(rnorm(100, mean = 5, sd = 3))
shapiro.test(runif(100, min = 2, max = 4))
runif(100, min = 2, max = 4)
itching_diff <- cryo_test$OBS_Thickness_preop - cryo_test$OBS_Thickness_postop
mean(itching_diff)
sd(itching_diff)
obs_thickness_diff <- itching_diff
rm(itching_diff)
pliability_diff <- cryo_test$Pliability_preop - cryo_test$Pliability_postop
mean(pliability_diff)
sd(pliability_diff)


library(dplyr)
head(cryo_observer)

#Thickness
mean(filter(cryo_observer, cryo_observer$Time == "preop") %>% select(OBS_Thickness) %>% unlist)
sd(filter(cryo_observer, cryo_observer$Time == "preop") %>% select(OBS_Thickness) %>% unlist)

mean(filter(cryo_observer, cryo_observer$Time == "postop") %>% select(OBS_Thickness) %>% unlist)
sd(filter(cryo_observer, cryo_observer$Time == "postop") %>% select(OBS_Thickness) %>% unlist)

#Pliability
mean(filter(cryo_observer, cryo_observer$Time == "preop") %>% select(Pliability) %>% unlist)
sd(filter(cryo_observer, cryo_observer$Time == "preop") %>% select(Pliability) %>% unlist)

mean(filter(cryo_observer, cryo_observer$Time == "postop") %>% select(Pliability) %>% unlist)
sd(filter(cryo_observer, cryo_observer$Time == "postop") %>% select(Pliability) %>% unlist)

t.test(filter(cryo_observer, cryo_observer$Time == "preop") %>% select(Pliability) %>% unlist,filter(cryo_observer, 
cryo_observer$Time == "postop") %>% select(Pliability) %>% unlist, paired = TRUE)

#Pigmentation
mean(filter(cryo_observer, cryo_observer$Time == "preop") %>% select(Pigmentation) %>% unlist)
sd(filter(cryo_observer, cryo_observer$Time == "preop") %>% select(Pigmentation) %>% unlist)

mean(filter(cryo_observer, cryo_observer$Time == "postop") %>% select(Pigmentation) %>% unlist)
sd(filter(cryo_observer, cryo_observer$Time == "postop") %>% select(Pigmentation) %>% unlist)

t.test(filter(cryo_observer, cryo_observer$Time == "preop") %>% select(Pigmentation) %>% unlist,filter(cryo_observer, 
 cryo_observer$Time == "postop") %>% select(Pigmentation) %>% unlist, paired = TRUE)

#Relief
mean(filter(cryo_observer, cryo_observer$Time == "preop") %>% select(Relief) %>% unlist)
sd(filter(cryo_observer, cryo_observer$Time == "preop") %>% select(Relief) %>% unlist)

mean(filter(cryo_observer, cryo_observer$Time == "postop") %>% select(Relief) %>% unlist)
sd(filter(cryo_observer, cryo_observer$Time == "postop") %>% select(Relief) %>% unlist)

t.test(filter(cryo_observer, cryo_observer$Time == "preop") %>% select(Relief) %>% unlist,filter(cryo_observer, 
                                                                                                       cryo_observer$Time == "postop") %>% select(Relief) %>% unlist, paired = TRUE)


mean(filter(cryo_observer, cryo_observer$Time == "postop") %>% select(Vascularization) %>% unlist)
sd(filter(cryo_observer, cryo_observer$Time == "postop") %>% select(Vascularization) %>% unlist)

t.test(filter(cryo_observer, cryo_observer$Time == "preop") %>% select(OBS_Thickness) %>% unlist,filter(cryo_observer, 
cryo_observer$Time == "postop") %>% select(OBS_Thickness) %>% unlist, paired = TRUE)
