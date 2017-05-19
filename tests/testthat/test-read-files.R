context("Read files")

test_that("test files exist", {
  expect_true(file.exists(system.file("extdata", "nanosims_data", package = "lans2r")))
})

test_that("possible to read full ion data file", {
  expect_true(file.exists(file <- system.file("extdata", "nanosims_data", "analysis1", "mat", "12C.mat", package = "lans2r")))
  
  # check return values from file (names, size, first row)
  expect_equal(lans2r:::read_full_ion_data_file(file) %>% names(), 
               c("x.px", "y.px", "frame_size.px", "x.um", "y.um", "frame_size.um", 
                 "variable", "data_type", "value", "sigma", "ROI"))
  
  expect_equal(lans2r:::read_full_ion_data_file(file) %>% { .$frame_size.px[1]^2}, 
               lans2r:::read_full_ion_data_file(file) %>% nrow())
  
  expect_equal(lans2r:::read_full_ion_data_file(file) %>% {.[1,]} %>% as.character(),
               c("1", "1", "256", "0.0391171875", "0.0391171875", "10.014", 
                 "12C", "ion_count", "1721", "41.4849370253831", "0"))
  
  
})