##### output html
### ------------------------------------------------------------------------
library(knitr)
library(markdown)
setwd("C:\\dev\\RepData_PeerAssessment1\\")
#knit("PA1_template.Rmd", output = NULL)
knitr::opts_chunk$set(echo = FALSE)
rmd <- file.path(getwd(), "PA1_template.r")
knit(rmd, output = "PA1_template.md", encoding = "ISO8859-1", quiet = FALSE)
rmarkdown::render("PA1_template.Rmd", output_format = NULL)
rmarkdown::render("PA1_template.Rmd", output_format = NULL)