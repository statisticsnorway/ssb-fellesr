test_that("gyldige filstier gir ikke feilmelding", {

  expect_no_error(gcs_validate_file_path(
    "gs://bøtte/mappe1/mappe2/fil.endelse"
  ))

  expect_no_error(gcs_validate_file_path(
    "bøtte/mappe1/mappe2/fil.endelse"
  ))

  expect_no_error(gcs_validate_file_path(
    "gs://bøtte/fil.endelse"
  ))

  expect_no_error(gcs_validate_file_path(
    "bøtte/fil.endelse"
  ))

})

test_that("ugyldige filstier gir feilmelding", {

  expect_error(gcs_validate_file_path(
    "gs://bøtte/mappe1/mappe2/fil."
  ))

  expect_error(gcs_validate_file_path(
    "gs://bøtte/mappe1/mappe2/fil"
  ))

  expect_error(gcs_validate_file_path(
    "gs://bøtte/mappe1/mappe2"
  ))

  expect_error(gcs_validate_file_path(
    "gs://bøtte/"
  ))

  expect_error(gcs_validate_file_path(
    "bøtte/mappe1/mappe2/fil."
  ))

  expect_error(gcs_validate_file_path(
    "bøtte/mappe1/mappe2/fil"
  ))

  expect_error(gcs_validate_file_path(
    "bøtte/mappe1/mappe2"
  ))

  expect_error(gcs_validate_file_path(
    "bøtte/"
  ))

})
