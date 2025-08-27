#' SSB-format
#'
#' @description
#' Klassen `ssb_format` gir funksjonalitet for å mappe kategoriske og intervallbaserte verdier
#' i henhold til en forhåndsdefinert kodeliste.
#'
#' @field frmt_list En navngitt liste som spesifiserer mappingen.
#' @field is_range_format Boolsk verdi som angir om formatet er intervallbasert.
#' @field na_value Verdi som representerer manglende data, utledet fra `frmt_list`.
#' @field breaks En vektor av grenseverdier for intervallbasert mapping, utledet fra `frmt_list`.
#' @field labels En vektor med labeler for intervallene, utledet fra `frmt_list`.
#' @export
ssb_format <- R6::R6Class("ssb_format",
                          public = list(
                          frmt_list = NULL,
                          is_range_format = NULL,
                          na_value = NULL,
                          breaks = NULL,
                          labels = NULL,

                          #' @description
                          #' Initialiserer SSB Format-klassen
                          #' @param frmt_list En "named list" (tilsvarende dictionary i python) som spesifiserer mappingen.
                          #' @param is_range_format Boolsk verdi som angir om formatet er intervallbasert.
                          initialize = function(frmt_list, is_range_format) {
                            self$frmt_list <- frmt_list
                            self$is_range_format <- is_range_format
                            private$validate_range_frmt_list()
                            private$update_format()
                          },

                          #' @description
                          #' Mapper numeriske verdier til kategoriske
                          #' @param vec En vektor med numeriske verdier som skal mappes.
                          #' @return En vektor med kategoriske verdier som er mappet.
                          #' @examples
                          #' \dontrun{
                          #' format <- ssb_format$new(frmt_list = list("10-20" = "A", "21-30" = "B", "other" = "C"), is_range_format = TRUE)
                          #' format$map_range(c(15, 25, 35))
                          #' }
                          map_range = function(vec){
                            if(!self$is_range_format) stop("Dette ssb-format-objektet er ikke angitt som et intervallformat. Bruk map_cat")

                            num_vec <- suppressWarnings(as.numeric(vec))

                            na_mask <- private$check_if_na(vec)
                            result <- rep(NA_character_, length(vec))

                            if (!is.null(self$na_value)) {
                              result[na_mask] <- self$na_value
                            }
                            else if (any(na_mask)) {
                              warning("Vektoren inneholder manglende verdier, mens formatet ikke spesifiserer hvordan disse skal behandles.")
                            }

                            valid_numbers <- which(!na_mask & !is.na(num_vec))
                            if (length(valid_numbers) > 0) {
                              num_vals <- num_vec[valid_numbers]
                              bin_indices <- findInterval(num_vals, self$breaks, rightmost.closed = TRUE)
                              bin_indices[bin_indices == 0] <- NA
                              result[valid_numbers] <- self$labels[bin_indices]
                            }

                            other_mask <- is.na(result)
                            if (!is.null(self$frmt_list[["other"]])) {
                              result[other_mask] <- self$frmt_list[["other"]]
                            } else {
                              uncovered_values <- vec[other_mask]
                              if (length(uncovered_values) > 0) {
                                stop(paste("Ingen 'other' spesifisert, og verdier ikke i format:", paste(unique(uncovered_values), collapse=", ")))
                              }
                            }
                            return(result)
                          },

                          #' @description
                          #' Mapper kategorier til kategorier
                          #'
                          #' @param vec En vektor med kategoriske verdier som skal mappes.
                          #'
                          #' @return En vektor med de mappede verdiene.
                          #' @examples
                          #' \dontrun{
                          #' format <- ssb_format$new(frmt_list = list("A" = "Alpha", "B" = "Beta"), is_range_format = FALSE)
                          #' format$map_cat(c("A", "B"))
                          #' }
                          map_cat = function(vec) {
                            if(self$is_range_format) stop("Dette ssb-format-objektet er angitt som et intervallformat. Bruk map_range")

                            result <- rep(NA_character_, length(vec))
                            vec_matched <- fastmatch::fmatch(vec, names(self$frmt_list))
                            result[!is.na(vec_matched)] <- unname(unlist(self$frmt_list[vec_matched]))

                            na_mask <- private$check_if_na(vec)
                            if (!is.null(self$na_value)) {
                              result[na_mask] <- self$na_value
                            }
                            else if (any(na_mask)) {
                              warning("Vektoren inneholder manglende verdier, mens formatet ikke spesifiserer hvordan disse skal behandles.")
                            }

                            unmatched_mask <- is.na(result)
                            if (any(unmatched_mask) && is.null(self$frmt_list[["other"]])) {
                              stop(paste("Ingen 'other' spesifisert, og verdier ikke i format:", paste(unique(vec[unmatched_mask]), collapse=", ")))
                            }
                            result[unmatched_mask] <- self$frmt_list[["other"]]

                            return(result)
                          }
                          ),
                          private = list(
                            # Oppdaterer formatet basert på kodelista
                            update_format = function() {
                              private$set_na_value()
                              private$set_other_as_lowercase()
                              if (self$is_range_format) {private$store_ranges()}
                            },

                            # Validerer intervallbasert kodeliste
                            # Sikrer at nøklene følger forventet format for intervallbaserte inndata.
                            validate_range_frmt_list = function() {
                              if (self$is_range_format) {
                                for(key in names(self$frmt_list)) {
                                  if (!(grepl("-", key) | tolower(key) == "other" | private$check_if_na(key))) {
                                    stop(paste(key, "er ikke støttet som en nøkkel i kodelista. Intervaller må ha format '[VERDI]-[VERDI]'"))
                                  }
                                }
                              }
                            },

                            # Advarer om at gap mellom intervaller i kodeliste kan innebære at verdiene
                            # som faller i dette gapet blir kategorisert feil. Forutsetter at intervallene
                            # er sortert i stigende rekkefølge i formatlisten.
                            trigger_gap_in_frmt_list_warning = function(bottom_value, previous_top_value) {
                              if (bottom_value - floor(previous_top_value) > 1){
                                warning(paste0("Formatet kategoriserer verdier innenfor [", previous_top_value, " > verdi > ",
                                               bottom_value, "] i intervallet [X-",previous_top_value,"]"))
                              }
                            },

                            # Lagrer intervaller fra kodelista
                            # Ekstraherer og validerer intervallgrenser.
                            store_ranges = function() {
                              top_val <- NULL

                              bottoms <- numeric()
                              tops    <- numeric()
                              labels  <- character()

                              for (key in names(self$frmt_list)) {
                                if (grepl("-", key, fixed = TRUE)) {
                                  parts  <- strsplit(key, "-", fixed = TRUE)[[1]]
                                  if (length(parts) < 2L) {
                                    stop(sprintf("Ugyldig nøkkel '%s' – forventer 'bottom-top'.", key))
                                  }
                                  bottom <- trimws(parts[1])
                                  top    <- trimws(parts[2])

                                  bottom_val <- if (tolower(bottom) == "low")  -Inf else suppressWarnings(as.numeric(bottom))
                                  top_next   <- if (tolower(top)    == "high")  Inf else suppressWarnings(as.numeric(top))

                                  if (!is.null(top_val)) private$trigger_gap_in_frmt_list_warning(bottom_val, top_val)

                                  # validate range order
                                  if (top_next < bottom_val) {
                                    stop(sprintf("Øvre verdi %s er mindre enn nedre verdi %s (nøkkel '%s').", top_next, bottom_val, key))
                                  }

                                  # store
                                  label   <- self$frmt_list[[key]]
                                  bottoms <- c(bottoms, bottom_val)
                                  tops    <- c(tops,    top_next)
                                  labels  <- c(labels,  label)

                                  # advance
                                  top_val <- top_next
                                }
                              }

                              # Sort by bottom to get consistent order
                              ord <- order(bottoms, tops)
                              bottoms <- bottoms[ord]
                              tops    <- tops[ord]
                              labels  <- labels[ord]

                              # Optional: sanity checks that help cut()/findInterval later
                              if (any(diff(bottoms) < 0)) {
                                stop("‘bottoms’ må være ikke-synkende etter sortering.")
                              }
                              if (any(bottoms[-1] < tops[-length(tops)])) {
                                warning("Overlapp mellom intervaller oppdaget.")
                              }

                              # Final outputs: breaks has one more element than labels
                              self$breaks <- c(unname(bottoms), max(tops))
                              self$labels <- labels

                              invisible(NULL)
                            },

                            # Konverterer "other"-nøkkelen til små bokstaver om den finnes
                            set_other_as_lowercase = function() {
                              for (key in names(self$frmt_list)) {
                                if (tolower(key) == "other") {
                                  value <- self$frmt_list[[key]]
                                  self$frmt_list[["other"]] <- value
                                  break
                                }
                              }
                            },

                            # Setter NA-verdi for manglende nøkler
                            set_na_value = function() {
                              self$na_value <- NULL
                              for (key in names(self$frmt_list)) {
                                if (private$check_if_na(key)) {
                                  self$na_value <- self$frmt_list[[key]]
                                  break
                                }
                              }
                            },

                            # Sjekker om verdiene i en vektor/verdi er NA
                            check_if_na = function(key) {
                              na_values <- c(".", "none", "None", "", "NA", "<NA>", "<NaN>", "nan", "NaN")
                              return(is.na(key) | !is.na(fastmatch::fmatch(key, na_values)))
                            }
                            )
)

#' Laster et SSB-format objekt fra JSON
#'
#' @param filepath Sti til JSON-filen.
#' @param is_range_format Boolsk verdi som angir om formatet er intervallbasert.
#'
#' @return Et `ssb_format`-objekt.
#' @examples
#' \dontrun{
#' format <- get_format("path/to/file.json", TRUE)
#' }
#' @export
get_format <- function(filepath, is_range_format) {
  json_data <- jsonlite::fromJSON(filepath, simplifyVector = FALSE)
  return(ssb_format$new(json_data, is_range_format))
}
