
#' Uttrekk fra Dynarev til R
#'
#' dynarev_uttrekk er en funksjon som henter inn data fra Dynarev til R så lenge man oppgir delregisternummer og skjemanavn. Det er kun enheter som er satt som aktive (AKTIV = 1) som blir lastet ned. Det er mulig å hente skjemadata (altså variablene i selve skjemaet) og SFU-data (enhetsinformasjon). Man kan også gjennomføre dublettsjekker og velge mellom reviderte data og rådata.
#'
#' @param delregnr Numerisk vektor med delregisternummer.
#' @param skjema Karaktervektor med skjemanavn.
#' @param enhets_type Karaktervektor med enhetstype (f.eks. BEDR og/eller FRTK).
#' @param skjema_cols Boolsk/karaktervektor. Hvis TRUE henter man alle variabler fra skjema. Hvis FALSE henter man ikke skjemadata, kun SFU-data (hvis sfu_cols = TRUE). Det er også mulig å lage en vektor/liste av variabelnavn, f.eks. c("variabel1", "variabel2"), dersom man kun vil ha utvalgte variabler fra skjema.
#' @param sfu_cols Boolsk/karaktervektor. Hvis TRUE blir alle variabler fra SFU for valgt delregister og skjema inkludert. For å kun velge én eller flere variabler fra SFU skrives disse i en vektor/liste, f.eks. c("variabel1", "variabel2").
#' @param skjema_sfu_merge Boolsk. Hvis TRUE blir skjemadataene og SFU-dataene merget. Dersom FALSE blir dataene hentet som to separate datasett i en liste; [1] skjemadata og [2] SFU-data.
#' @param dublettsjekk Boolsk/karaktervektor. Hvis TRUE sjekkes det for dubletter i skjemadata etter ENHETS_ID. Dersom man ønsker å sjekke for dubletter etter én eller flere selvvalgte variabler skrives disse i en vektor, f.eks. c("variabel1", "variabel2"). Liste med to datasett returneres; [1] skjemadata og [2] dublettdata (dersom det finnes dubletter, hvis ikke er denne blank).
#' @param con_ask Boolsk/karaktervektor. Hvis TRUE får man opp en boks som spør etter Oracle-passord. Hvis FALSE spørres det ikke om passord. Ved å skrive dynarev_uttrekk(con_ask = "con") får man opp en boks som spør etter Oracle-passord og kun koblingen mot Oracle blir returnert (ikke data). Denne kan brukes dersom man skal lese inn flere skjema etter hverandre for å unngå å skrive inn passordet flere ganger.
#' @param raadata Boolsk. Hvis FALSE returneres reviderte data (fra FELT_VERDI), TRUE returnerer rådata.
#'
#' @returns Objekt (data.frame) med valgt data fra Dynarev, dersom enten skjema_cols eller sfu_cols = TRUE. Om både skjema_cols og sfu_cols = TRUE returneres en liste med to datasett [1] skjema_cols og [2] sfu_cols. Om skjema_cols, sfu_cols og skjema_sfu_merge = TRUE returneres det ett objekt (data.frame).
#' @export
#'
#' @examples
#' \dontrun{
#' dynarev <- dynarev_uttrekk(delregnr = 2421,
#'                           skjema = "HELSE41",
#'                           skjema_cols = T)
#'}
#'@encoding UTF-8

dynarev_uttrekk <- function(delregnr,
                            skjema,
                            enhets_type = c("FRTK", "BEDR"),
                            skjema_cols = T,
                            sfu_cols = F,
                            skjema_sfu_merge = F,
                            dublettsjekk = F,
                            con_ask = T,
                            raadata = F) # {
  suppressWarnings({

    # Sjekker hvilken plattform koden kjøres på (Windows/Jupter etc.)
    nodename <- Sys.info()["nodename"]

    # Funksjoner for å sjekke/fikse encoding
    # Hentet fra: https://github.com/statisticsnorway/DynarevToR/blob/main/DynarevToR.R
    TestIfRenvironExist <- function() {
      file.exists("~/.Renviron")
    }
    TestIfNlsLangIsSet <- function() {
      system("grep 'NLS_LANG=' ~/.Renviron", ignore.stdout = TRUE)
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

    # Kobler til Oracle
    SetUpOracleConnection <- function() {
      if (grepl("FW-XAPROD", nodename)){ # FW-XAPROD = RStudio (Windows)
        con <-
          suppressWarnings(RODBC::odbcConnect(
            dsn = "DB1P",
            uid = Sys.info()["user"],
            pwd = getPass::getPass("DB1P passord:"),
            DBMSencoding = "UTF-8"
          ))

      } else { # Jupyter/Linux
        con <- DBI::dbConnect(
          drv = ROracle::Oracle(),
          dbname = "DB1P",
          username = Sys.info()["user"],
          password = getPass::getPass("Skriv inn Oracle passord: ")
        )
      }
    }

    if (con_ask == "con") { # Kun kobling mot Oracle returneres (ikke data)
      GetTheDataFromOracle <- function() {
        con <- SetUpOracleConnection()
      }
    } else {

      # Funksjon for å hente data fra Oracle
      GetTheDataFromOracle <- function() {
        if (con_ask == TRUE) {
          con <- SetUpOracleConnection()
        }
        # Henter skjemadata fra alle skjema i delreg
        if (skjema == TRUE & skjema_cols != FALSE) {
          # Reviderte data
          if (raadata == FALSE) {

            if (grepl("FW-XAPROD", nodename)){
              data <- RODBC::sqlQuery(
                channel = con,
                query = paste0("SELECT * FROM DYNAREV.VW_SKJEMA_DATA WHERE DELREG_NR = '", delregnr,
                               # "' AND SKJEMA = '", skjema,
                               "' AND ENHETS_TYPE IN ('", paste(enhets_type, collapse = "', '"),
                               "') AND AKTIV = '1'"),
                as.is = T)

            } else {

              data <- dplyr::tbl(con, dbplyr::in_schema("DYNAREV", "VW_SKJEMA_DATA")) %>%
                dplyr::filter(DELREG_NR == delregnr,
                              ENHETS_TYPE %in% enhets_type,
                              AKTIV == 1) %>%
                dplyr::collect()

            }
          }

          # Rådata
          if (raadata == TRUE) {
            if (grepl("FW-XAPROD", nodename)){
              data_enhet <- RODBC::sqlQuery(
                channel = con,
                query = paste0("SELECT * FROM DYNAREV.VW_SKJEMA_ENHET WHERE DELREG_NR = '", delregnr,
                               "' AND ENHETS_TYPE IN ('", paste(enhets_type, collapse = "', '"), "')"),
                as.is = T) %>%
                dplyr::select(ENHETS_ID, SKJEMA, LOPENR, AKTIV)

              raadata <- RODBC::sqlQuery(
                channel = con,
                query = paste0("SELECT * FROM DYNAREV.VW_SKJEMA_DATA_RAADATA WHERE DELREG_NR = '", delregnr,
                               "' AND ENHETS_TYPE IN ('", paste(enhets_type, collapse = "', '"), "')"),
                as.is = T) %>%
                dplyr::select(-AKTIV)

            } else {

              data_enhet <- dplyr::tbl(con, dbplyr::in_schema("DYNAREV", "VW_SKJEMA_ENHET")) %>%
                dplyr::filter(DELREG_NR == delregnr,
                              ENHETS_TYPE %in% enhets_type) %>%
                dplyr::select(ENHETS_ID, SKJEMA, LOPENR, AKTIV)

              raadata <- dplyr::tbl(con, dbplyr::in_schema("DYNAREV", "VW_SKJEMA_DATA_RAADATA")) %>%
                dplyr::filter(DELREG_NR == delregnr,
                              ENHETS_TYPE %in% enhets_type) %>%
                dplyr::select(-AKTIV)
            }

            data <- dplyr::inner_join(raadata, data_enhet, by = c("SKJEMA", "ENHETS_ID", "LOPENR")) %>%
              dplyr::filter(AKTIV == 1) %>%
              dplyr::collect()
          }


          # Henter metadata (for å angi datatyper)
          if (class(skjema_cols) == "character"){

            if (grepl("FW-XAPROD", nodename)){
              metadata <- RODBC::sqlQuery(
                channel = con,
                query = paste0("SELECT * FROM DYNAREV.VW_SKJEMA_METADATA WHERE DELREG_NR = '", delregnr,
                               "' AND FELT_ID IN ('", paste(unique(data$FELT_ID), collapse = "', '"), "')"),
                as.is = T)

            } else {
              metadata <- dplyr::tbl(con, dbplyr::in_schema("DYNAREV", "VW_SKJEMA_METADATA")) %>%
                dplyr::filter(DELREG_NR == delregnr,
                              FELT_ID %in% skjema_cols) %>%
                dplyr::collect() %>%
                dplyr::select(FELT_TYPE, FELT_ID) %>%
                dplyr::filter(FELT_ID %in% unique(data$FELT_ID))
            }

          } else {

            if (grepl("FW-XAPROD", nodename)){

              metadata <- RODBC::sqlQuery(
                channel = con,
                query = paste0("SELECT * FROM DYNAREV.VW_SKJEMA_METADATA WHERE DELREG_NR = '", delregnr, "'"),
                as.is = T) %>%
                dplyr::select(FELT_TYPE, FELT_ID) %>%
                # Fix for kolonner som har blitt gitt forskjellige variabeltyper i metadata for ulike skjemaer (velger variabeltypen som finnes flest ganger)
                group_by(FELT_TYPE, FELT_ID) %>%
                tally() %>%
                group_by(FELT_ID) %>%
                dplyr::slice(which.max(n)) %>%
                dplyr::select(FELT_TYPE, FELT_ID) %>%
                dplyr::filter(FELT_ID %in% unique(data$FELT_ID))

            } else {

              metadata <- dplyr::tbl(con, dbplyr::in_schema("DYNAREV", "VW_SKJEMA_METADATA")) %>%
                dplyr::filter(DELREG_NR == delregnr) %>%
                dplyr::collect() %>%

                dplyr::select(FELT_TYPE, FELT_ID) %>%
                # Fix for kolonner som har blitt gitt forskjellige variabeltyper i metadata for ulike skjemaer (velger variabeltypen som finnes flest ganger)
                dplyr::group_by(FELT_TYPE, FELT_ID) %>%
                tally() %>%
                group_by(FELT_ID) %>%
                dplyr::slice(which.max(n)) %>%
                dplyr::select(FELT_TYPE, FELT_ID) %>%
                dplyr::filter(FELT_ID %in% unique(data$FELT_ID))
            }

          }

          # Skiller ut numeriske variabler
          filter_numeric <- metadata %>%
            dplyr::filter(FELT_TYPE %in% c("DESIMAL", "NUMBER"))

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

        }
        # Henter skjemadata fra utvalgte skjema i delreg
        if (class(skjema) == "character") {

          if (raadata == FALSE) {

            if (grepl("FW-XAPROD", nodename)){
              data <- RODBC::sqlQuery(
                channel = con,
                query = paste0("SELECT * FROM DYNAREV.VW_SKJEMA_DATA WHERE DELREG_NR = '", delregnr,
                               "' AND SKJEMA IN ('", paste(skjema, collapse = "', '"),
                               "') AND ENHETS_TYPE IN ('", paste(enhets_type, collapse = "', '"),
                               "') AND AKTIV = '1'"),
                as.is = T)
            } else {

              data <- dplyr::tbl(con, dbplyr::in_schema("DYNAREV", "VW_SKJEMA_DATA")) %>%
                dplyr::filter(DELREG_NR == delregnr,
                              ENHETS_TYPE %in% enhets_type,
                              SKJEMA %in% skjema,
                              AKTIV == 1) %>%
                dplyr::collect()
            }
          }
          # Henter skjemadata (rådata)
          if (raadata == TRUE) {
            if (grepl("FW-XAPROD", nodename)){
              data_enhet <- RODBC::sqlQuery(
                channel = con,
                query = paste0("SELECT * FROM DYNAREV.VW_SKJEMA_ENHET WHERE DELREG_NR = '", delregnr,
                               "' AND ENHETS_TYPE IN ('", paste(enhets_type, collapse = "', '"),
                               "') AND SKJEMA IN ('", paste(skjema, collapse = "', '"), "')"),
                as.is = T) %>%
                dplyr::select(ENHETS_ID, SKJEMA, LOPENR, AKTIV)

              raadata <- RODBC::sqlQuery(
                channel = con,
                query = paste0("SELECT * FROM DYNAREV.VW_SKJEMA_DATA_RAADATA WHERE DELREG_NR = '", delregnr,
                               "' AND ENHETS_TYPE IN ('", paste(enhets_type, collapse = "', '"),
                               "') AND SKJEMA IN ('", paste(skjema, collapse = "', '"), "')"),
                as.is = T) %>%
                dplyr::select(-AKTIV)
            } else {
              data_enhet <- dplyr::tbl(con, dbplyr::in_schema("DYNAREV", "VW_SKJEMA_ENHET")) %>%
                dplyr::filter(DELREG_NR == delregnr,
                              ENHETS_TYPE %in% enhets_type,
                              SKJEMA %in% skjema) %>%
                dplyr::select(ENHETS_ID, SKJEMA, LOPENR, AKTIV)

              raadata <- dplyr::tbl(con, dbplyr::in_schema("DYNAREV", "VW_SKJEMA_DATA_RAADATA")) %>%
                dplyr::filter(DELREG_NR == delregnr,
                              ENHETS_TYPE %in% enhets_type,
                              SKJEMA %in% skjema) %>%
                dplyr::select(-AKTIV)
            }

            data <- inner_join(raadata, data_enhet, by = c("SKJEMA", "ENHETS_ID", "LOPENR")) %>%
              dplyr::filter(AKTIV == 1) %>%
              collect()
          }

          # Henter metadata
          if (class(skjema_cols) == "character"){
            if (grepl("FW-XAPROD", nodename)){
              metadata <- RODBC::sqlQuery(
                channel = con,
                query = paste0("SELECT * FROM DYNAREV.VW_SKJEMA_METADATA WHERE DELREG_NR = '", delregnr,
                               "' AND SKJEMA IN ('", paste(skjema, collapse = "', '"),
                               "') AND FELT_ID IN ('", paste(skjema_cols, collapse = "', '"), "')"),
                as.is = T) %>%
                dplyr::select(FELT_TYPE, FELT_ID) %>%
                dplyr::filter(FELT_ID %in% unique(data$FELT_ID))
            } else {
              metadata <- dplyr::tbl(con, dbplyr::in_schema("DYNAREV", "VW_SKJEMA_METADATA")) %>%
                dplyr::filter(DELREG_NR == delregnr,
                              SKJEMA %in% skjema,
                              FELT_ID %in% skjema_cols) %>%
                dplyr::collect() %>%
                dplyr::select(FELT_TYPE, FELT_ID) %>%
                dplyr::filter(FELT_ID %in% unique(data$FELT_ID))
            }


          } else {
            if (grepl("FW-XAPROD", nodename)){
              metadata <- RODBC::sqlQuery(
                channel = con,
                query = paste0("SELECT * FROM DYNAREV.VW_SKJEMA_METADATA WHERE DELREG_NR = '", delregnr, "' AND SKJEMA = '", skjema, "'"),
                as.is = T) %>%
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
        # Dublettsjekk (etter ENHETS_ID)
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
        # Dublettsjekk (med valgte variabler)
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
        # Inkluder SFU-data (alle skjema og alle kolonner)
        if (skjema == TRUE & sfu_cols == TRUE) {
          if (grepl("FW-XAPROD", nodename)){
            sfu <- RODBC::sqlQuery(
              channel = con,
              query = paste0("SELECT * FROM DSBBASE.DLR_ENHET_I_DELREG WHERE DELREG_NR = '", delregnr, "'"),
              as.is = T) %>%
              dplyr::rename(ENHETS_ID = IDENT_NR) # Endrer navn fra IDENT_NR til ENHETS_ID for å merge
          } else {
            sfu <- dplyr::tbl(con, dbplyr::in_schema("DSBBASE", "DLR_ENHET_I_DELREG")) %>%
              dplyr::filter(DELREG_NR == delregnr) %>%
              dplyr::collect() %>%
              dplyr::rename(ENHETS_ID = IDENT_NR) # Endrer navn fra IDENT_NR til ENHETS_ID for å merge

          }
        }
        # Inkluder SFU-data (utvalgte skjema)
        if (class(skjema) == "character" & sfu_cols != FALSE){
          if (grepl("FW-XAPROD", nodename)){
            sfu_skjema <-
              RODBC::sqlQuery(
                channel = con,
                query = paste0("SELECT * FROM DSBBASE.DLR_ENHET_I_DELREG_SKJEMA WHERE DELREG_NR = '", delregnr,
                               "' AND SKJEMA_TYPE IN ('", paste(skjema, collapse = "', '"), "')"),
                as.is = T)

            sfu <- RODBC::sqlQuery(
              channel = con,
              query = paste0("SELECT * FROM DSBBASE.DLR_ENHET_I_DELREG WHERE DELREG_NR = '", delregnr, "'"),
              as.is = T) %>%
              dplyr::filter(IDENT_NR %in% c(unique(sfu_skjema$IDENT_NR),
                                            ENHETS_TYPE %in% c(unique(sfu_skjema$ENHETS_TYPE)),
                                            SKJEMA_TYPE %in% c(unique(sfu_skjema$SKJEMA_TYPE)),
                                            is.na(PROSEDYRE))) %>%
              dplyr::rename(ENHETS_ID = IDENT_NR) # %>% # Endrer navn fra IDENT_NR til ENHETS_ID for å merge
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
        }
        # Lager SFU-subset
        if (class(sfu_cols) == "character" & (dublettsjekk == F)) {
          sfu_subset <- sfu %>%
            dplyr::select(ENHETS_ID, ENHETS_TYPE, DELREG_NR, all_of(sfu_cols))
        }

        ### Output:
        # Både sfu_cols og skjema_cols
        if ((sfu_cols == T) & (skjema_sfu_merge == F) & (skjema_cols == T) & (dublettsjekk == F)) {
          return(list(skjema_data, sfu))
        }
        # Både sfu_cols og skjema_cols - MERGE
        if ((sfu_cols == T) & (skjema_sfu_merge == T) & (skjema_cols == T) & (dublettsjekk == F)) {
          skjema_data <- dplyr::left_join(skjema_data, sfu, by = c("ENHETS_ID", "ENHETS_TYPE", "DELREG_NR")) # OBS: inner join?
          return(skjema_data)
        }
        # Kun sfu_cols
        if ((sfu_cols == T) & (skjema_cols == F) & (dublettsjekk == F)) {
          return(sfu)
        }
        # Både sfu_cols (utvalgte variabler) og skjema_cols
        if ((class(sfu_cols) == "character") & (skjema_sfu_merge == F) & (skjema_cols == T) & (dublettsjekk == F)) {
          return(list(skjema_data, sfu_subset))
        }
        # Både sfu_cols (utvalgte variabler) og skjema_cols - MERGE
        if ((class(sfu_cols) == "character") & (skjema_sfu_merge == T) & (skjema_cols == T) & (dublettsjekk == F)) {
          skjema_data <- dplyr::left_join(skjema_data, sfu_subset, by = c("ENHETS_ID", "ENHETS_TYPE", "DELREG_NR")) # OBS: inner join?
          return(skjema_data)
        }
        # Kun sfu_cols, utvalgte variabler
        if ((class(sfu_cols) == "character") & (skjema_cols == F) & (dublettsjekk == F)) {
          return(sfu_subset)
        }
        # Både sfu_cols og skjema_cols (utvalgte variabler)
        if ((class(skjema_cols) == "character") & (sfu_cols == T) & (skjema_sfu_merge == F) & (dublettsjekk == F)) {
          return(list(skjema_data, sfu))
        }
        # Både sfu_cols og skjema_cols (utvalgte variabler) - MERGE
        if ((class(skjema_cols) == "character") & (sfu_cols == T) & (skjema_sfu_merge == T) & (dublettsjekk == F)) {
          skjema_data <- dplyr::left_join(skjema_data, sfu, by = c("ENHETS_ID", "ENHETS_TYPE", "DELREG_NR")) # OBS: inner join?
          return(skjema_data)
        }
        # Kun skjema_cols
        if ((sfu_cols == F) & (skjema_cols == T) & (dublettsjekk == F)) {
          return(skjema_data)
        }
        # Kun skjema_cols, utvalgte variabler
        if ((sfu_cols == F) & (class(skjema_cols) == "character") & (dublettsjekk == F)) {
          return(skjema_data)
        }
        # Både sfu_cols (utvalgte variabler) og skjema_cols (utvalgte variabler)
        if ((class(sfu_cols) == "character") & (class(skjema_cols) == "character") & (skjema_sfu_merge == F) & (dublettsjekk == F)) {
          return(list(skjema_data, sfu_subset))
        }
        # Både sfu_cols (utvalgte variabler) og skjema_cols (utvalgte variabler) - MERGE
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
  })
