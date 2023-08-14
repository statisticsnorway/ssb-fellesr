pakker <- c("DBI",
  "renv",
  "tidyverse",
  "PxWebApiData",
  "arrow",
  "klassR",
  "leaflet" ,
  "getPass",
  "sf",
  "sfarrow",
  "dbplyr",
  "googleCloudStorageR",
  "shiny",
  "DT",          
  "ROracle",
  "rstudioapi",
  "httr",
  "readr",
  "knitr",
  "rmarkdown",
  "fellesr",
  "RCurl",
  "here",
  "esquisse",
  "dcmodify",
  "simputation",
  "SSBtools",
  "easySdcTable",
  "SmallCountRounding",
  "GaussSuppression",
  "GISSB",
  "ReGenesees", # Github (fork)
  "struktuR", # Github
  "SSBpris", # Github
  "SdcForetakPerson", # Github
  "Kostra" # Github
)

setdiff(pakker, rownames(installed.packages()))
