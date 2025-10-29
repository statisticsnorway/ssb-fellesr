#' Eksporter data frame til tekstfil og lag SAS-program for innlesning
#'
#' Funksjonen eksporterer en data frame til en tekstfil som kan leses av SAS, og genererer samtidig et SAS-program for å lese inn dataene.
#' Det genererte SAS-programmet kan justeres for å inkludere spesifikke SAS-innstillinger.
#'
#' @param data Datasettet som skal eksporteres til SAS-format. Dette kan være en data.frame eller tibble.
#' @param filsti En karakterstreng som spesifiserer banen der filene skal lagres (både datafilen og SAS-programmet).
#' @param filnavn En karakterstreng som spesifiserer navnet på filene som skal genereres. Hvis ikke angitt, brukes navnet på objektet `data`.
#' @param libname En karakterstreng som spesifiserer navnet på SAS-libnavn som skal brukes i SAS-programmet. Standard er `"wd"`.
#'
#' @details
#' Funksjonen konverterer først alle faktorer i datasettet til karakterstrenger og erstatter manglende verdier med tomme strenger.
#' Deretter lagres dataene som en `.txt`-fil og et tilsvarende SAS-program som `.sas`-fil ved hjelp av \code{\link{foreign::write.foreign}}.
#' Funksjonen leser så inn det genererte SAS-programmet og gjør justeringer, som å legge til `encoding="utf-8"` og å sette opp et SAS-libnavn.
#'
#' @return Ingen returverdi. Funksjonen lagrer en .txt-fil og et SAS-program på den spesifiserte filstien. SAS-programmet må deretter åpnes og kjøres i SAS og da blir det lagret en .sas7bdat-fil på samme område.
#'
#' @examples
#' # Eksporter et datasett til SAS-format og generer et innlesningsprogram
#' write_sas_prog(mtcars, filsti = "output/")
#'
#' # Eksporter med spesifisert filnavn og libname
#' write_sas_prog(mtcars, filsti = "output/", filnavn = "biler", libname = "mylib")
#'
#' @export
write_sas_prog <- function(data,
                           filsti,
                           filsti_innlesning = NULL,
                           filnavn = NULL,
                           libname = "wd"){

  # Funksjon for å konvertere factor til character + erstatte missing med ""
  convert_format_r2sas <- function(data){
    data <- data %>%
      dplyr::mutate_if(is.factor, as.character) %>%
      dplyr::mutate_if(is.character, tidyr::replace_na, replace = "")
    return(data)
  }

  # Sett filnavn til navnet på objektet dersom filnavn ikke er spesifisert
  if (is.null(filnavn)){
    filename <- deparse(substitute(data))
  } else {
    filename <- filnavn
  }

  # Sett filnavn til navnet på objektet dersom filnavn ikke er spesifisert
  if (is.null(filsti_innlesning)){
    filsti_innlesning <- filsti
  }

  # Lagrer .txt-fil og genererer et innlesningsprogram i SAS
  foreign::write.foreign(df = convert_format_r2sas(data),
                         datafile = paste0(filsti, filename, '.txt'),
                         codefile = paste0(filsti, filename, '.sas'),
                         dataname = paste0(libname, '.', gsub("-", "_", filename)),
                         package = 'SAS')

  # Les inn det genererte SAS-programmet og gjør nødvendige justeringer
  my_data <- read.delim(paste0(filsti, filename, ".sas"), quote = "'") %>%
    dplyr::mutate(X..Written.by.R. = dplyr::case_when(
      stringr::str_detect(X..Written.by.R., "\\bDSD\\b") ~ 'encoding="utf-8" DSD',
      tidyr::replace_na(stringr::str_detect(X..Written.by.R., "INFILE"), FALSE) ~ as.character(glue::glue('INFILE "{filsti_innlesning}{filename}.txt"')),
      TRUE ~ X..Written.by.R.))

  # Legg til libname-definisjon i SAS-programmet
  my_data <- rbind(paste0('libname ', libname, ' "', filsti_innlesning, '" ;'), my_data)

  # Lagre det oppdaterte SAS-programmet
  write.table(my_data, file = paste0(filsti, filename, ".sas"), sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)

}
