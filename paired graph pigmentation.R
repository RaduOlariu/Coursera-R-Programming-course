library(readr)
cryo_test <- read_csv("cryo test.csv")
library(ggpubr)
ggpaired(cryo_test, "Pigmentation_preop", "Pigmentation_postop", color = "condition", fill= "condition", palette = "npg", width = 0.2)
