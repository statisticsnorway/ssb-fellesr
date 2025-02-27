#' SSB-format-klasse
#'
#' Denne klassen gir funksjonalitet for å kartlegge kategoriske og intervallbaserte verdier
#' i henhold til en forhåndsdefinert kodeliste.
#'
#' @field dict En navngitt liste som spesifiserer mappingen.
#' @field is_range_format Boolsk verdi som angir om formatet er intervallbasert.
#' @field na_value Verdi som representerer manglende data.
#' @field breaks En vektor av grenseverdier for intervallbasert mapping.
#' @field labels En vektor med labeller for intervallene.
#' @export
ssb_format <- R6::R6Class("ssb_format",
                      public = list(
                        dict = NULL,
                        is_range_format = NULL,
                        na_value = NULL,
                        breaks = NULL,
                        labels = NULL,

                        #' initialize
                        #'
                        #' @param dict En "named list" (tilsvarende dictionary i python) som spesifiserer mappingen.
                        #' @param is_range_format Boolsk verdi som angir om formatet er intervallbasert.
                        #'
                        initialize = function(dict, is_range_format) {
                          self$dict <- dict
                          self$is_range_format <- is_range_format
                          private$validate_range_dict()
                          private$update_format()
                        },

                        #' map_values_range
                        #'
                        #' @param vec En vektor med numeriske verdier som skal mappes.
                        #'
                        #' @return En vektor med kategoriske verdier som er mappet.
                        #' @examples
                        #' \dontrun{
                        #' format <- ssb_format$new(dict = list("10-20" = "A", "21-30" = "B", "other" = "C"), is_range_format = TRUE)
                        #' format$map_values_range(c(15, 25, 35))
                        #' }
                        map_values_range = function(vec){
                          if(!self$is_range_format) stop("Dette ssb-format-objektet er ikke angitt som et intervallformat. Bruk map_values_cat")

                          num_vec <- as.numeric(vec)

                          na_mask <- private$check_if_na(vec)
                          result <- rep(NA_character_)
                          result[na_mask] <- self$na_value

                          valid_numbers <- which(!na_mask & !is.na(num_vec))
                          if (length(valid_numbers) > 0) {
                            num_vals <- num_vec[valid_numbers]
                            bin_indices <- findInterval(num_vals, self$breaks, rightmost.closed = TRUE)
                            bin_indices[bin_indices == 0] <- NA
                            result[valid_numbers] <- self$labels[bin_indices]
                          }

                          other_mask <- is.na(result)
                          if (!is.null(self$dict[["other"]])) {
                            result[other_mask] <- self$dict[["other"]]
                          } else {
                            uncovered_values <- vec[other_mask]
                            if (length(uncovered_values) > 0) {
                              stop(paste("Ingen 'other' spesifisert, og verdier ikke i format:", paste(unique(uncovered_values), collapse=", ")))
                            }
                          }
                          return(result)
                        },

                        #' map_values_cat
                        #'
                        #' @param vec En vektor med kategoriske verdier som skal mappes.
                        #'
                        #' @return En vektor med de mappede verdiene.
                        #' @examples
                        #' \dontrun{
                        #' format <- ssb_format$new(dict = list("A" = "Alpha", "B" = "Beta"), is_range_format = FALSE)
                        #' format$map_values_cat(c("A", "B"))
                        #' }
                        map_values_cat = function(vec) {
                          if(self$is_range_format) stop("Dette ssb-format-objektet er angitt som et intervallformat. Bruk map_values_range")

                          vec_matched <- fastmatch::fmatch(vec, names(self$dict))
                          result <- unname(unlist(self$dict[vec_matched]))

                          na_mask <- private$check_if_na(vec)
                          result[na_mask] <- self$na_value

                          unmatched_mask <- is.na(result)
                          if (any(unmatched_mask) && is.null(self$dict[["other"]])) {
                            stop(paste("No 'other' specified and values not in format:", paste(unique(vec[unmatched_mask]), collapse=", ")))
                          }
                          result[unmatched_mask] <- self$dict[["other"]]

                          return(result)
                        }
                        ),
                      private = list(
                        #' Oppdaterer formatet basert på kodelista
                        #'
                        #' @keywords internal
                        update_format = function() {
                          private$set_na_value()
                          private$set_other_as_lowercase()
                          if (self$is_range_format) {private$store_ranges()}
                        },

                        #' Validerer intervallbasert kodeliste
                        #'
                        #' Sikrer at nøklene følger forventet format for intervallbaserte inndata.
                        #'
                        #' @keywords internal
                        validate_range_dict = function() {
                          if (self$is_range_format) {
                            for(key in names(self$dict)) {
                              if (!(grepl("-", key) | tolower(key) == "other" | private$check_if_na(key))) {
                                stop(paste(key, "er ikke støttet som en nøkkel i kodelista. Intervaller må ha format '[VERDI]-[VERDI]'"))
                              }
                            }
                          }
                        },

                        #' Lagrer intervaller fra kodelista
                        #'
                        #' Ekstraherer og validerer intervallgrenser.
                        #'
                        #' @keywords internal
                        store_ranges = function() {
                          breaks_labels <- c()
                          top_values <- c()

                          for (key in names(self$dict)) {
                            if (grepl("-", key)) {
                              parts <- strsplit(key, "-")[[1]]
                              bottom <- trimws(parts[1])
                              top <- trimws(parts[2])

                              bottom_val <- ifelse(tolower(bottom) == "low", -Inf, as.numeric(bottom))
                              top_val <- ifelse(tolower(top) == "high", Inf, as.numeric(top))
                              top_values <- c(top_values, top_val)

                              if (top_val < bottom_val) {
                                stop(paste("Øvre verdi", top_val, "er mindre enn nedre verdi", bottom_val))
                              }

                              if (!is.na(bottom_val)) {
                                label <- self$dict[[key]]
                                breaks_labels[label] <- bottom_val
                              }
                            }
                          }

                          breaks_labels <- sort(breaks_labels)
                          self$breaks <- c(unname(unlist(breaks_labels)), max(top_values))
                          self$labels <- names(breaks_labels)
                        },

                        #' Konverterer "other"-nøkkelen til små bokstaver om den finnes
                        #'
                        #' @keywords internal
                        set_other_as_lowercase = function() {
                          for (key in names(self$dict)) {
                            if (tolower(key) == "other") {
                              value <- self$dict[[key]]
                              self$dict[["other"]] <- value
                              break
                            }
                          }
                        },

                        #' Setter NA-verdi for manglende nøkler
                        #'
                        #' @keywords internal
                        set_na_value = function() {
                          self$na_value <- NULL
                          for (key in names(self$dict)) {
                            if (private$check_if_na(key)) {
                              self$na_value <- self$dict[[key]]
                              break
                            }
                          }
                        },

                        #' Sjekker om verdiene i en vektor/verdi er NA
                        #'
                        #' @keywords internal
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
