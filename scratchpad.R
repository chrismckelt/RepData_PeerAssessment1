##### output html
### ------------------------------------------------------------------------
library(knitr)
library(markdown)
setwd("C:\\dev\\RepData_PeerAssessment1\\")
knitr::opts_chunk$set(echo = TRUE)
#knit("PA1_template.Rmd", output = NULL)

rmd <- file.path(getwd(), "PA1_template.r")
knit(rmd, output = "PA1_template.md", encoding = "ISO8859-1", quiet = FALSE)
markdownHTMLOptions(default = TRUE)

knitr::opts_chunk$set(fig.width = 8, fig.height = 6, eval = TRUE, eval = TRUE, echo = TRUE, warning = FALSE, message = FALSE)
knit2html("PA1_template.rmd", output = "PA1_template.html")

