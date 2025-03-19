sample_list <- list(
  "low-9" = "01-09",
  "10-19" = "10-19",
  "20-29.99999" = "20-29",
  "30-34" = "30-34",
  "35-35" = "35",
  "36-37" = "36-37",
  "38-high" = "38+",
  "NA" = "missing"
)

fmt <- ssb_format$new(sample_list, is_range_format = TRUE)

test_that("Mapping av kjente verdier fungerer", {
  expect_equal(fmt$map_range(-100), "01-09")
  expect_equal(fmt$map_range(5), "01-09")
  expect_equal(fmt$map_range(29.8), "20-29")
  expect_equal(fmt$map_range(35), "35")
  expect_equal(fmt$map_range("36"), "36-37")
  expect_equal(fmt$map_range("37.5"), "36-37")
  expect_equal(fmt$map_range(40), "38+")
})

test_that("Håndtering av NA-verdier", {
  expect_equal(fmt$map_range(NA), "missing")     # NA should map to "missing"
  expect_equal(suppressWarnings(fmt$map_range("NA")), "missing")   # "NA" as a string should map to "missing"
  expect_equal(suppressWarnings(fmt$map_range("none")), "missing") # "none" should map to "missing"
})

test_that("Håndtering av ukjente verdier", {
  expect_error(suppressWarnings(fmt$map_range(c("femti", "hundre"))), "Ingen 'other' spesifisert, og verdier ikke i format") # Udefinert verdi burde returnere feil
})

test_that("Lagring av intervaller", {
  expect_equal(length(fmt$breaks), 8) # 8 valid breaks should be stored
  expect_equal(length(fmt$breaks)-1, length(fmt$labels)) # length of labels should be 1 shorter
})

test_that("map_range fungerer", {
  input_vec <- c("5", "12", "25", "32", "36", "40", "NA")
  expected_output <- c("01-09", "10-19", "20-29", "30-34", "36-37", "38+", "missing")
  expect_equal(suppressWarnings(fmt$map_range(input_vec)), expected_output)
})

sample_list2 <- list(
  "20-29.99999" = "20-29",
  "30-34" = "30-34",
  "35-37" = "35-37",
  "38-high" = "38+",
  "NA" = "missing",
  "other" = "annet"
)

# Create an instance of the class
fmt2 <- ssb_format$new(sample_list2, is_range_format = TRUE)

test_that("Mapping av ukjente verdier fungerer med 'other'", {
  expect_equal(fmt2$map_range("10"), "annet")
})

test_that("Lagring av intervaller", {
  expect_equal(length(fmt2$breaks), 5) # 5 valid breaks should be stored
  expect_equal(length(fmt2$breaks)-1, length(fmt2$labels)) # length of labels should be 1 shorter
})

sample_list3 <- list(
  "fast" = "F",
  "midlertidig" = "M",
  "other" = "A",
  "None" = "U"
)

fmt3 <- ssb_format$new(sample_list3, is_range_format = FALSE)

test_that("Mapping av kjente verdier fungerer", {
  input_vec <- c("verdi", "midlertidig", "fast", "midlertidig", "B", ".")
  expected_output <- c("A", "M", "F", "M", "A", "U")
  expect_equal(fmt3$map_cat(input_vec), expected_output)
})


sample_list4 <- list(
  "10-19" = "10-19",
  "20-29.99999" = "20-29",
  "30-34" = "30-34",
  "35-35" = "35",
  "36-37" = "36-37",
  "NA" = "ukjent"
)

# Create an instance of the class
fmt4 <- ssb_format$new(sample_list4, is_range_format = TRUE)

test_that("Mapping av kjente verdier fungerer", {
  expect_equal(fmt4$map_range(10), "10-19")
  expect_equal(fmt4$map_range(29.8), "20-29")
  expect_equal(fmt4$map_range(""), "ukjent")
})

test_that("Håndtering av ukjente verdier", {
  expect_error(suppressWarnings(fmt4$map_range("5")), "Ingen 'other' spesifisert, og verdier ikke i format") # Udefinert verdi burde returnere feil
  expect_error(suppressWarnings(fmt4$map_range(100)), "Ingen 'other' spesifisert, og verdier ikke i format") # Udefinert verdi burde returnere feil
})

sample_list5 <- list(
  "0" = "0",
  "1-9"= "1"
)

test_that("Validering av lister med intervaller", {
  # Nøkler skal alltid inneholde enten 'other', en NA-verdi (., "" osv) eller to verdier med bindestrek mellom ([VERDI]-[VERDI])
  expect_error(ssb_format$new(sample_list5, is_range_format = TRUE), "er ikke støttet som en nøkkel")
})

sample_list6 <- list(
  "0-0" = "0",
  "1-9"= "1"
)


fmt5 <- ssb_format$new(sample_list6, is_range_format = TRUE)
fmt6 <- ssb_format$new(sample_list6, is_range_format = FALSE)

test_that("Tilbakemelding om manglende verdier i vektor, men ikke i format", {
  expect_error(suppressWarnings(fmt5$map_range(c(NA, 50))), "Ingen 'other' spesifisert")
  expect_error(suppressWarnings(fmt6$map_cat(c(NA, 50))), "Ingen 'other' spesifisert")
})
