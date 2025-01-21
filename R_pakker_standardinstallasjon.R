pakker <- c("DBI",
  "configr",
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
  "RCurl",
  "here",
  "esquisse",
  "dcmodify",
  "simputation",
  "openxlsx",
  "survey",
  "RJDemetra",
  "eurostat",
  "dggridR",
  "SSBtools",
  "easySdcTable",
  "SmallCountRounding",
  "GaussSuppression",
  "GISSB",
  "tidyfst",
  "fellesr", # Github          
  "pickmdl", # Github          
  "ReGenesees", # Github (fork)
  "struktuR", # Github
  "SSBpris", # Github
  "SdcForetakPerson", # Github
  "Kostra" # Github
)

setdiff(pakker, rownames(installed.packages()))
