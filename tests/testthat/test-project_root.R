test_that("find_project_root returns failure when target not found", {
  result <- find_project_root("unlikely_file_123456789.xyz")
  expect_false(result$success)
  expect_true(is.na(result$project.root))
})

test_that("find_project_root returns success on known file", {
  # Create temporary nested structure with target file
  root <- tempfile("testproj_")
  dir.create(root, recursive = TRUE)
  file.create(file.path(root, "renv.lock"))

  old <- getwd()
  dir.create(file.path(root, "subdir1", "subdir2"), recursive = TRUE)
  setwd(file.path(root, "subdir1", "subdir2"))

  on.exit(setwd(old), add = TRUE)

  result <- find_project_root("renv.lock", max.iter = 10)
  expect_true(result$success)
  expect_equal(result$project.root, root)
})

test_that("set_project_root returns informative result", {
  result <- suppressWarnings(set_project_root("unlikely_file_123456789.xyz"))
  expect_false(result$success)
  expect_true(is.na(result$project.root))
})
