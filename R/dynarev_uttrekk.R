# -*- coding: utf-8 -*-

#' Uttrekk fra Dynarev til R
#'
#' @param delregnr Numerisk vektor med delregisternummer
#' @param skjema Karakter vektor med skjemanavn
#' @param enhets_type Karakter vektor med enhetstype
#' @param skjema_cols TRUE = alle kolonner fra skjema. Karakter vektor for kun utvalgte kolonner
#' @param skjema_sfu_merge TRUE = skjemadata og SFU merges sammen til én data frame
#' @param dublettsjekk TRUE = dublettsjekk etter ENHETS_ID. Karakter vektor for dublettsjekk etter valgte kolonner.
#' @param sfu_cols TRUE = alle kolonner fra SFU. Karakter vektor for kun utvalgte kolonner
#' @param con_ask TRUE = spør om passord for å koble til DB1P og laster inn data. Dersom con_ask = "con" kobles det til DB1P uten å laste inn data
#' @param raadata FALSE returnerer reviderte data, TRUE returnerer rådata
#'
#' @return
#' @export
#'
#' @examples
#' # Det er mulig å bruke funksjonen uten å skrive inn noen argumenter. Man blir da bedt om å skrive inn skjemanavn, delregnr og enhets_type. Alle kolonnene fra skjemadata blir med.
#' dynarev <- dynarev_uttrekk()
#'
#' # Her får man lastet inn alle kolonnene fra valgt skjema.
#' dynarev <- dynarev_uttrekk(delregnr = 2421,
#'                           skjema = "HELSE41",
#'                           skjema_cols = T,
#'                           enhets_type = "FRTK")
#'
#' # Her får man lastet ned utvalgte kolonner fra valgt skjema. Her er AARGANG, FORETAKETS_NAVN og FORETAKETS_ORGNR valgt.
#' dynarev <- dynarev_uttrekk(delregnr = 2421,
#'                           skjema = "HELSE41",
#'                           skjema_cols = c("AARGANG", "FORETAKETS_NAVN", "FORETAKETS_ORGNR"),
#'                           enhets_type = "FRTK")
#'
#' # Her får man lastet ned alle kolonnene med SFU-data fra valgt skjema.
#' dynarev <- dynarev_uttrekk(delregnr = 2421,
#'                           skjema = "HELSE41",
#'                           skjema_cols = F,
#'                           enhets_type = "FRTK",
#'                           sfu_cols = T)
#'
#' # Her får man lastet ned valgte kolonner med SFU-data fra valgt skjema. Her er NAVN og ORGNR valgt.
#' dynarev <- dynarev_uttrekk(delregnr = 2421,
#'                           skjema = "HELSE41",
#'                           skjema_cols = F,
#'                           enhets_type = "FRTK",
#'                           dublettsjekk = F,
#'                           sfu_cols = c("NAVN", "ORGNR"))
#'
#' # Her får man lastet ned alle kolonnene med SFU-data fra alle skjema i valgt delregister. Dersom man kun ønsker utvalgte variabler fra SFU-data erstattes sfu_cols = T med sfu_cols = c("NAVN", "ORGNR").
#' dynarev <- dynarev_uttrekk(delregnr = 2421,
#'                           skjema = T,
#'                           skjema_cols = F,
#'                           enhets_type = "FRTK",
#'                           sfu_cols = T)
#'
#' # Her får man lastet ned både skjemadata (alle kolonner) og SFU-data (alle kolonner) fra valgt delregister. Objektet dynarev inneholder her altså to datasett. For å få hentet ut skjemadata skriver man dynarev_skjema <- data.frame(dynarev[1]) og for å hente ut SFU-data skriver man dynarev_sfu <- data.frame(dynarev[2]).
#' dynarev <- dynarev_uttrekk(delregnr = 2421,
#'                           skjema = "HELSE41",
#'                           skjema_cols = T,
#'                           enhets_type = "FRTK",
#'                           sfu_cols = T)
#' dynarev_skjema <- data.frame(dynarev[1])
#' dynarev_sfu <- data.frame(dynarev[2])
#'
#' # Her får man lastet ned både skjemadata (alle kolonner) og SFU-data (utvalgte kolonner, her NAVN og ORGNR) fra valgt delregister. Objektet dynarev inneholder her altså to datasett. For å få hentet ut skjemadata skriver man dynarev_skjema <- data.frame(dynarev[1]) og for å hente ut SFU-data skriver man dynarev_sfu <- data.frame(dynarev[2]).
#' dynarev <- dynarev_uttrekk(delregnr = 2421,
#'                           skjema = "HELSE41",
#'                           skjema_cols = T,
#'                           enhets_type = "FRTK",
#'                           sfu_cols = c("NAVN", "ORGNR"))
#' dynarev_skjema <- data.frame(dynarev[1])
#' dynarev_sfu <- data.frame(dynarev[2])
#'
#' # Her får man lastet ned både skjemadata (utvalgte kolonner, her AARGANG, FORETAKETS_NAVN og FORETAKETS_ORGNR) og SFU-data (utvalgte kolonner, her NAVN og ORGNR) fra valgt delregister. Objektet dynarev inneholder her altså to datasett. For å få hentet ut skjemadata skriver man dynarev_skjema <- data.frame(dynarev[1]) og for å hente ut SFU-data skriver man dynarev_sfu <- data.frame(dynarev[2]).
#' dynarev <- dynarev_uttrekk(delregnr = 2421,
#'                           skjema = "HELSE41",
#'                           skjema_cols = c("AARGANG", "FORETAKETS_NAVN", "FORETAKETS_ORGNR"),
#'                           enhets_type = "FRTK",
#'                           sfu_cols = c("NAVN", "ORGNR"))
#' dynarev_skjema <- data.frame(dynarev[1])
#' dynarev_sfu <- data.frame(dynarev[2])
#'
#' # Her får man lastet ned både skjemadata (alle kolonner) og SFU-data (alle kolonner) fra valgt delregister. SFU-dataene blir koblet på skjemadataene slik at man kun får én fil.
#' dynarev <- dynarev_uttrekk(delregnr = 2421,
#'                           skjema = "HELSE41",
#'                           skjema_cols = T,
#'                           skjema_sfu_merge = T,
#'                           enhets_type = "FRTK",
#'                           sfu_cols = T)
#'
#' # Her får man lastet ned både skjemadata (alle kolonner) og SFU-data (utvalgte kolonner, her NAVN og ORGNR) fra valgt delregister. SFU-dataene blir koblet på skjemadataene slik at man kun får én fil.
#' dynarev <- dynarev_uttrekk(delregnr = 2421,
#'                           skjema = "HELSE41",
#'                           skjema_cols = T,
#'                           skjema_sfu_merge = T,
#'                           enhets_type = "FRTK",
#'                           sfu_cols = c("NAVN", "ORGNR"))
#'
#' # Her får man lastet ned både skjemadata (utvalgte kolonner, her AARGANG, FORETAKETS_NAVN og FORETAKETS_ORGNR) og SFU-data (utvalgte kolonner, her NAVN og ORGNR) fra valgt delregister. SFU-dataene blir koblet på skjemadataene slik at man kun får én fil.
#' dynarev <- dynarev_uttrekk(delregnr = 2421,
#'                           skjema = "HELSE41",
#'                           skjema_cols = c("AARGANG", "FORETAKETS_NAVN", "FORETAKETS_ORGNR"),
#'                           skjema_sfu_merge = T,
#'                           enhets_type = "FRTK",
#'                           sfu_cols = c("NAVN", "ORGNR"))
#'
#' # Her får man testet for dubletter (etter ENHETS_ID) i skjemadata. Liste med to datasett returneres; [1] skjemadata og [2] dublettdata (dersom det finnes dubletter, hvis ikke er denne blank).
#' dynarev <- dynarev_uttrekk(delregnr = 2421,
#'                            skjema = "HELSE39",
#'                            enhets_type = "BEDR",
#'                            dublettsjekk = T)
#'
#' # Dersom man ønsker å sjekke for dubletter etter én eller flere selvvalgte variabler skrives disse i en vektor, f.eks. c("variabel1", "variabel2").
#' dynarev <- dynarev_uttrekk(delregnr = 2421,
#'                            skjema = "HELSE0X",
#'                            enhets_type = "FRTK",
#'                            dublettsjekk = c("FORETAKSNR", "ART_SEKTOR", "FUNKSJON_KAPITTEL"))
#'
#' # For å slippe å oppgi passord flere ganger når man skal laste inn flere skjema kan man skrive con_ask = FALSE. For at dette skal fungere må man logge på først øverst i scriptet: con <- dynarev_uttrekk(con_ask = "con")
#' con <- dynarev_uttrekk(con_ask = "con")
#'
#' dynarev_1 <- dynarev_uttrekk(delregnr = 2421,
#'                              skjema = "HELSE41",
#'                              skjema_cols = T,
#'                              enhets_type = "FRTK",
#'                              con_ask = FALSE)
#'
#' dynarev_2 <- dynarev_uttrekk(delregnr = 2421,
#'                              skjema = "HELSE46",
#'                              skjema_cols = T,
#'                              enhets_type = "BEDR",
#'                              con_ask = FALSE)
#'
#' # For å laste inn alle skjema fra samme delregister i samme fil skriver man skjema = T. Under enhets_type må man samtidig skrive inn alle enhetstypene som finnes i det valgte delregisteret (f.eks. BEDR og FRTK)
#' dynarev <- dynarev_uttrekk(delregnr = 2420,
#'                           skjema = T,
#'                           skjema_cols = T,
#'                           enhets_type = c("BEDR", "FRTK"),
#'                           sfu_cols = F)
#'
#' # Her lastes det inn SFU-data fra flere skjema (fra samme delregister).
#' dynarev <- dynarev_uttrekk(delregnr = 2421,
#'                           skjema = c("HELSE38P", "HELSE39", "HELSE44P"),
#'                           skjema_cols = F,
#'                           enhets_type = c("BEDR", "FRTK"),
#'                           sfu_cols = T)
#'
#' # Her lastes det inn SFU-data fra alle skjema (fra samme delregister).
#' dynarev <- dynarev_uttrekk(delregnr = 2421,
#'                           skjema = T,
#'                           skjema_cols = F,
#'                           enhets_type = c("BEDR", "FRTK"),
#'                           sfu_cols = c("NAVN", "ORGNR"))
#'                           
#' # Her lastes det inn rådata fra de aktive enhetene i valgt skjema.
#' dynarev <- dynarev_uttrekk(delregnr = delregnr, 
#'                            skjema = "HELSE0X",
#'                            skjema_cols = T,
#'                            enhets_type = c("BEDR", "FRTK"), 
#'                            raadata = T)

dynarev_uttrekk <- function(delregnr = getPass:::readline_nomask("Skriv inn delregnr: ", silent = TRUE),
                            skjema = getPass:::readline_nomask("Skriv inn skjemanavn: ", silent = TRUE),
                            enhets_type = getPass:::readline_nomask("Skriv inn enhets_type (BEDR/FRTK): ", silent = TRUE),
                            skjema_cols = T,
                            skjema_sfu_merge = F,
                            dublettsjekk = F,
                            sfu_cols = F,
                            con_ask = T) # {
  suppressWarnings({
    
    
    # Laster inn pakker
    suppressPackageStartupMessages({
      library(tidyverse)
      library(ROracle)
      library(getPass)
    })
    
    
    # Funksjoner for å sjekke/fikse encoding
    TestIfRenvironExist <- function() {
      file.exists("~/.Renviron")
    }
    TestIfNlsLangIsSet <- function() {
      system("grep 'NLS_LANG=' ~/.Renviron")
    }
    CleanUpNlsSentence <- function() {
      system("sed -i '/NLS_LANG=/d' ~/.Renviron")
    }
    CreateRenvironFile <- function() {
      fileConn<-file("~/.Renviron")
      writeLines("NLS_LANG=\"AMERICAN_AMERICA.AL32UTF8\"", fileConn)
      close(fileConn)
    }
    DeleteRenviron <- function() {
      system("rm -f ~/.Renviron")
    }
    AppendNlsToRenviron <- function() {
      write ("NLS_LANG=\"AMERICAN_AMERICA.AL32UTF8\"",
             file = "~/.Renviron",
             append = TRUE)
    }
    
    # if (con_ask == TRUE) {
    SetUpOracleConnection <- function() {
      con <- DBI::dbConnect(
        drv = ROracle::Oracle(),
        dbname = "DB1P",
        username = Sys.info()["user"],
        password = getPass::getPass("Skriv inn Oracle passord: ")
      )
    }
    
    
    if (con_ask == "con") {
      GetTheDataFromOracle <- function() {
        con <- SetUpOracleConnection()
      }} else {
        
        GetTheDataFromOracle <- function() {
          if (con_ask == TRUE) {
            con <- SetUpOracleConnection()
          }
          
          ### Henter data for alle skjema i delreg
          # Henter skjemadata
          if (skjema == TRUE) {
            data <- dplyr::tbl(con, dbplyr::in_schema("DYNAREV", "VW_SKJEMA_DATA")) %>%
              dplyr::filter(DELREG_NR == delregnr,
                            ENHETS_TYPE %in% enhets_type,
                            AKTIV == 1) %>%
              dplyr::collect()
            
            # Henter metadata
            if (class(skjema_cols) == "character"){
              
              metadata <- dplyr::tbl(con, dbplyr::in_schema("DYNAREV", "VW_SKJEMA_METADATA")) %>%
                dplyr::filter(DELREG_NR == delregnr,
                              FELT_ID %in% skjema_cols) %>%
                dplyr::collect() %>%
                dplyr::select(FELT_TYPE, FELT_ID) %>%
                dplyr::filter(FELT_ID %in% unique(data$FELT_ID))
            } else {
              metadata <- dplyr::tbl(con, dbplyr::in_schema("DYNAREV", "VW_SKJEMA_METADATA")) %>%
                dplyr::filter(DELREG_NR == delregnr) %>%
                dplyr::collect() %>%
                dplyr::select(FELT_TYPE, FELT_ID) %>%
                # Fix for kolonner som har blitt gitt forskjellige variabeltyper i metadata for ulike skjemaer (velger variabeltypen som finnes flest ganger)
                group_by(FELT_TYPE, FELT_ID) %>%
                tally() %>%
                group_by(FELT_ID) %>%
                dplyr::slice(which.max(n)) %>%
                dplyr::select(FELT_TYPE, FELT_ID) %>%
                dplyr::filter(FELT_ID %in% unique(data$FELT_ID))
            }
            
            # Skiller ut numeriske variabler
            filter_numeric <- metadata %>% dplyr::filter(FELT_TYPE %in% c("DESIMAL", "NUMBER"))
            numerisk <- data %>%
              dplyr::select(SKJEMA, DELREG_NR, ENHETS_TYPE, ENHETS_ID, LOPENR, FELT_ID, FELT_VERDI, RAD_NR) %>%
              dplyr::filter(FELT_ID %in% unique(filter_numeric$FELT_ID)) %>%
              dplyr::mutate(FELT_VERDI = gsub(",", ".", FELT_VERDI))  %>%
              tidyr::spread(FELT_ID, FELT_VERDI, fill = 0) %>%
              dplyr::mutate(dplyr::across(c(-SKJEMA, -DELREG_NR, -ENHETS_TYPE, -ENHETS_ID, -LOPENR, -RAD_NR),
                                          as.numeric)) %>%
              dplyr::group_by(SKJEMA, ENHETS_ID, ENHETS_TYPE, DELREG_NR, LOPENR, RAD_NR) %>%
              dplyr::summarise(across(.cols = everything(), sum), .groups = 'drop')
            
            # Skiller ut karaktervariabler
            filter_char <- metadata %>% dplyr::filter(!FELT_TYPE %in% c("DESIMAL", "NUMBER"))
            karakter <- data %>%
              dplyr::select(SKJEMA, DELREG_NR, ENHETS_TYPE, ENHETS_ID, LOPENR, FELT_ID, FELT_VERDI, RAD_NR) %>%
              dplyr::filter(FELT_ID %in% unique(filter_char$FELT_ID)) %>%
              tidyr::spread(FELT_ID, FELT_VERDI, fill = "") %>%
              dplyr::mutate(dplyr::across(c(-SKJEMA, -DELREG_NR, -ENHETS_TYPE, -ENHETS_ID, -LOPENR, -RAD_NR), as.character)) %>%
              dplyr::group_by(SKJEMA, ENHETS_ID, ENHETS_TYPE, DELREG_NR, LOPENR, RAD_NR) %>%
              dplyr::summarise(across(.cols = everything(), max), .groups = 'drop')
            
            # Merger karakter og numerisk
            skjema_data  <- dplyr::full_join(karakter, numerisk, by = c("SKJEMA", "ENHETS_ID", "ENHETS_TYPE", "DELREG_NR", "LOPENR", "RAD_NR"))
            
            ### Henter data for valgt skjema i delreg
            # Henter skjemadata
          } else {
            data <- dplyr::tbl(con, dbplyr::in_schema("DYNAREV", "VW_SKJEMA_DATA")) %>%
              dplyr::filter(DELREG_NR == delregnr,
                            ENHETS_TYPE %in% enhets_type,
                            SKJEMA %in% skjema,
                            AKTIV == 1) %>%
              dplyr::collect()
            
            # Henter metadata
            if (class(skjema_cols) == "character"){
              
              metadata <- dplyr::tbl(con, dbplyr::in_schema("DYNAREV", "VW_SKJEMA_METADATA")) %>%
                dplyr::filter(DELREG_NR == delregnr,
                              SKJEMA %in% skjema,
                              FELT_ID %in% skjema_cols) %>%
                dplyr::collect() %>%
                dplyr::select(FELT_TYPE, FELT_ID) %>%
                dplyr::filter(FELT_ID %in% unique(data$FELT_ID))
            } else {
              # Henter metadata
              metadata <- dplyr::tbl(con, dbplyr::in_schema("DYNAREV", "VW_SKJEMA_METADATA")) %>%
                dplyr::filter(DELREG_NR == delregnr,
                              SKJEMA %in% skjema) %>%
                dplyr::collect() %>%
                dplyr::select(FELT_TYPE, FELT_ID) %>%
                dplyr::filter(FELT_ID %in% unique(data$FELT_ID))
            }
            
            # Skiller ut numeriske variabler
            filter_numeric <- metadata %>% dplyr::filter(FELT_TYPE %in% c("DESIMAL", "NUMBER"))
            numerisk <- data %>%
              dplyr::select(SKJEMA, DELREG_NR, ENHETS_TYPE, ENHETS_ID, LOPENR, FELT_ID, FELT_VERDI, RAD_NR) %>%
              dplyr::filter(FELT_ID %in% unique(filter_numeric$FELT_ID)) %>%
              dplyr::mutate(FELT_VERDI = gsub(",", ".", FELT_VERDI))  %>%
              tidyr::spread(FELT_ID, FELT_VERDI, fill = 0) %>%
              dplyr::mutate(dplyr::across(c(-SKJEMA, -DELREG_NR, -ENHETS_TYPE, -ENHETS_ID, -LOPENR, -RAD_NR),
                                          as.numeric)) %>%
              dplyr::group_by(SKJEMA, ENHETS_ID, ENHETS_TYPE, DELREG_NR, LOPENR, RAD_NR) %>%
              dplyr::summarise(across(.cols = everything(), sum), .groups = 'drop')
            
            # Skiller ut karaktervariabler
            filter_char <- metadata %>% dplyr::filter(!FELT_TYPE %in% c("DESIMAL", "NUMBER"))
            karakter <- data %>%
              dplyr::select(SKJEMA, DELREG_NR, ENHETS_TYPE, ENHETS_ID, LOPENR, FELT_ID, FELT_VERDI, RAD_NR) %>%
              dplyr::filter(FELT_ID %in% unique(filter_char$FELT_ID)) %>%
              tidyr::spread(FELT_ID, FELT_VERDI, fill = "") %>%
              dplyr::mutate(dplyr::across(c(-SKJEMA, -DELREG_NR, -ENHETS_TYPE, -ENHETS_ID, -LOPENR, -RAD_NR), as.character)) %>%
              dplyr::group_by(SKJEMA, ENHETS_ID, ENHETS_TYPE, DELREG_NR, LOPENR, RAD_NR) %>% #
              dplyr::summarise(across(.cols = everything(), max), .groups = 'drop')
            
            # Merger karakter og numerisk
            skjema_data  <- dplyr::full_join(karakter, numerisk, by = c("SKJEMA", "ENHETS_ID", "ENHETS_TYPE", "DELREG_NR", "LOPENR", "RAD_NR"))
          }
          
          ### DUBLETTSJEKK (skjema_cols)
          if (dublettsjekk == TRUE) {
            dublett_test <- skjema_data %>%
              dplyr::group_by(ENHETS_ID) %>% # OBS?
              dplyr::tally() %>%
              dplyr::filter(n > 1)
            
            if (nrow(dublett_test)>0) {
              print(paste0("Dubletter finnes for ENHETS_ID: ", toString(unique(dublett_test$ENHETS_ID))))
            } else {
              print("Ingen dubletter")
            }
            return(list(skjema_data, dublett_test))
          }
          ### DUBLETTSJEKK MED VALGTE VARIABLER (skjema_cols)
          if (class(dublettsjekk) == "character"){
            dublettsjekk <- as.character(dublettsjekk)
            dublett_test <- skjema_data %>%
              dplyr::group_by_at(dublettsjekk) %>%
              dplyr::tally() %>%
              dplyr::filter(n > 1)
            
            if (nrow(dublett_test)>0) {
              print(paste0("Dubletter finnes for ENHETS_ID: ", toString(unique(dublett_test$ENHETS_ID))))
            } else {
              print("Ingen dubletter")
            }
            return(list(skjema_data, dublett_test))
          }
          
          
          ### Inkluder SFU-data
          # Henter inn SFU skjemadata
          if (skjema == TRUE) {
            sfu <- dplyr::tbl(con, dbplyr::in_schema("DSBBASE", "DLR_ENHET_I_DELREG")) %>%
              dplyr::filter(DELREG_NR == delregnr) %>%
              dplyr::collect() %>%
              dplyr::rename(ENHETS_ID = IDENT_NR) # Endrer navn fra IDENT_NR til ENHETS_ID for å merge
            
          } else {
            sfu_skjema <- dplyr::tbl(con, dbplyr::in_schema("DSBBASE", "DLR_ENHET_I_DELREG_SKJEMA")) %>%
              dplyr::filter(DELREG_NR == delregnr, #) %>%
                            SKJEMA_TYPE %in% skjema) %>%
              dplyr::collect()
            # Henter inn SFU data
            sfu <- dplyr::tbl(con, dbplyr::in_schema("DSBBASE", "DLR_ENHET_I_DELREG")) %>%
              dplyr::filter(DELREG_NR == delregnr) %>%
              dplyr::collect() %>%
              dplyr::filter(IDENT_NR %in% c(unique(sfu_skjema$IDENT_NR),
                                            ENHETS_TYPE %in% c(unique(sfu_skjema$ENHETS_TYPE)),
                                            SKJEMA_TYPE %in% c(unique(sfu_skjema$SKJEMA_TYPE)),
                                            is.na(PROSEDYRE))) %>%
              dplyr::rename(ENHETS_ID = IDENT_NR) # Endrer navn fra IDENT_NR til ENHETS_ID for å merge
          }
          
          # Lager SFU-subset
          if (class(sfu_cols) == "character" & (dublettsjekk == F)) {
            sfu_subset <- sfu %>%
              dplyr::select(ENHETS_ID, ENHETS_TYPE, DELREG_NR, all_of(sfu_cols))
            
          }
          
          ### Både sfu_cols og skjema_cols
          if ((sfu_cols == T) & (skjema_sfu_merge == F) & (skjema_cols == T) & (dublettsjekk == F)) {
            return(list(skjema_data, sfu))
          }
          
          ### Både sfu_cols og skjema_cols - MERGE
          if ((sfu_cols == T) & (skjema_sfu_merge == T) & (skjema_cols == T) & (dublettsjekk == F)) {
            skjema_data <- dplyr::left_join(skjema_data, sfu, by = c("ENHETS_ID", "ENHETS_TYPE", "DELREG_NR")) # OBS: inner join?
            return(skjema_data)
          }
          
          ### Kun sfu_cols
          if ((sfu_cols == T) & (skjema_cols == F) & (dublettsjekk == F)) {
            return(sfu)
          }
          
          ### Både sfu_cols (utvalgte variabler) og skjema_cols
          if ((class(sfu_cols) == "character") & (skjema_sfu_merge == F) & (skjema_cols == T) & (dublettsjekk == F)) {
            return(list(skjema_data, sfu_subset))
          }
          
          ### Både sfu_cols (utvalgte variabler) og skjema_cols - MERGE
          if ((class(sfu_cols) == "character") & (skjema_sfu_merge == T) & (skjema_cols == T) & (dublettsjekk == F)) {
            skjema_data <- dplyr::left_join(skjema_data, sfu_subset, by = c("ENHETS_ID", "ENHETS_TYPE", "DELREG_NR")) # OBS: inner join?
            return(skjema_data)
          }
          
          ### Kun sfu_cols, utvalgte variabler
          if ((class(sfu_cols) == "character") & (skjema_cols == F) & (dublettsjekk == F)) {
            return(sfu_subset)
          }
          
          ### Både sfu_cols og skjema_cols (utvalgte variabler)
          if ((class(skjema_cols) == "character") & (sfu_cols == T) & (skjema_sfu_merge == F) & (dublettsjekk == F)) {
            return(list(skjema_data, sfu))
          }
          
          ### Både sfu_cols og skjema_cols (utvalgte variabler) - MERGE
          if ((class(skjema_cols) == "character") & (sfu_cols == T) & (skjema_sfu_merge == T) & (dublettsjekk == F)) {
            skjema_data <- dplyr::left_join(skjema_data, sfu, by = c("ENHETS_ID", "ENHETS_TYPE", "DELREG_NR")) # OBS: inner join?
            return(skjema_data)
          }
          
          ### Kun skjema_cols
          if ((sfu_cols == F) & (skjema_cols == T) & (dublettsjekk == F)) {
            return(skjema_data)
          }
          
          ### Kun skjema_cols, utvalgte variabler
          if ((sfu_cols == F) & (class(skjema_cols) == "character") & (dublettsjekk == F)) {
            return(skjema_data)
          }
          
          ### Både sfu_cols (utvalgte variabler) og skjema_cols (utvalgte variabler)
          if ((class(sfu_cols) == "character") & (class(skjema_cols) == "character") & (skjema_sfu_merge == F) & (dublettsjekk == F)) {
            return(list(skjema_data, sfu_subset))
          }
          
          ### Både sfu_cols (utvalgte variabler) og skjema_cols (utvalgte variabler) - MERGE
          if ((class(sfu_cols) == "character") & (class(skjema_cols) == "character") & (skjema_sfu_merge == T) & (dublettsjekk == F)) {
            skjema_data <- dplyr::left_join(skjema_data, sfu_subset, by = c("ENHETS_ID", "ENHETS_TYPE", "DELREG_NR"))
            return(skjema_data)
          }
          
        }}
    
    
    #Her kjører vi funksjonene vi lagde over med if-logikk.
    RunQuery <- function() {
      if (TestIfRenvironExist() == TRUE) {
        if (TestIfNlsLangIsSet() == 1) {
          AppendNlsToRenviron()
          readRenviron("~/.Renviron")
          SkjemaData <- GetTheDataFromOracle()
          CleanUpNlsSentence()
          return(SkjemaData)
        }
        if (TestIfNlsLangIsSet() == 0)
          SkjemaData <- GetTheDataFromOracle()
        return(SkjemaData)
      }
      if (TestIfRenvironExist() == FALSE) {
        CreateRenvironFile()
        readRenviron("~/.Renviron")
        SkjemaData <- GetTheDataFromOracle()
        DeleteRenviron()
        return(SkjemaData)
      }
      
      
      
    }
    RunQuery()
    
    
    
    
  }
  )
