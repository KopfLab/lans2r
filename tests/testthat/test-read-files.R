context("Read files")

test_that("test files exist", {
  expect_true(file.exists(system.file("extdata", "nanosims_data", package = "lans2r")))
})

test_that("possible to read ROI data file", {
  expect_true(file.exists(file <- system.file("extdata", "nanosims_data", "analysis1", "dat", "12C.dac", package = "lans2r")))
  
  # check return values from file
  expect_true(is(data <- lans2r:::read_roi_ion_data_file(file), "data.frame"))
  
  expect_equal(data %>% nrow(), data$ROI %>% unique() %>% length())
  expect_equal(data %>% names(), c("plane", "ROI", "data_type", "variable", "value", "sigma", 
                                   "coord_x", "coord_y", "size", "pixels", "LW_ratio"))
  expect_equal(data %>% {.[1,]} %>% as.character(), 
               c("all", "1", "ion_count", "12C", "1895850", "1376.89868908355", 
                 "17.38", "192.93", "0.83", "353", "2.45"))
  
})


test_that("possible to read ROI z-stack data file", {
  expect_true(file.exists(file <- system.file("extdata", "nanosims_data", "analysis1", "dat", "12C-z.dat", package = "lans2r")))
  
  # check return values from file
  expect_true(is(data <- lans2r:::read_roi_ion_zstack_data_file(file), "data.frame"))
  
  expect_equal(data %>% nrow(), (data$plane %>% unique() %>% length()) * (data$ROI %>% unique() %>% length()))
  expect_equal(data %>% names(), c("plane", "data_type", "variable", "ROI", "sigma", "value"))
  expect_equal(data %>% {.[1,]} %>% as.character(), 
               c("1", "ion_count", "12C", "1", "946.255779374689", "895400"))
  
})


test_that("possible to read all roi data", {
  
  expect_error(read_roi_data("NOFOLDER"), "directory does not exist")
  
  expect_true(file.exists(folder <- system.file("extdata", "nanosims_data", "analysis1", "dat", package = "lans2r")))
  
  expect_message(read_roi_data(folder), ".*read successfully")
  expect_silent(data <- read_roi_data(folder, quiet = T))
  
  # data checks
  expect_equal(data$variable %>% unique(), c("12C", "13C", "14N12C", "15N12C"))
  expect_equal(data %>% nrow(), 444)
  expect_equal(data %>% names(), c("plane", "ROI", "data_type", "variable", "value", "sigma", 
                                   "coord_x", "coord_y", "size", "pixels", "LW_ratio"))
  expect_equal(data %>% {.[1,]} %>% as.character(), 
               c("all", "1", "ion_count", "12C", "1895850", "1376.89868908355", 
                 "17.38", "192.93", "0.83", "353", "2.45"))
})

test_that("possible to read full ion data file", {
  expect_true(file.exists(file <- system.file("extdata", "nanosims_data", "analysis1", "mat", "12C.mat", package = "lans2r")))
  
  # check return values from file (names, size, first row)
  expect_true(is(data <- lans2r:::read_full_ion_data_file(file), "data.frame"))
  
  expect_equal(data %>% names(), 
               c("x.px", "y.px", "frame_size.px", "x.um", "y.um", "frame_size.um", 
                 "variable", "data_type", "value", "sigma", "ROI"))
  
  expect_equal(data %>% { .$frame_size.px[1]^2}, data %>% nrow())
  
  expect_equal(data %>% {.[1,]} %>% as.character(),
               c("1", "1", "256", "0.0391171875", "0.0391171875", "10.014", 
                 "12C", "ion_count", "1721", "41.4849370253831", "0"))
  
})

test_that("possible to read all map data in a folder", {
  
  expect_error(read_map_data("NOFOLDER"), "directory does not exist")
  
  expect_true(file.exists(folder <- system.file("extdata", "nanosims_data", "analysis1", "mat", package = "lans2r")))
  
  expect_message(read_map_data(folder), ".*read successfully")
  expect_silent(data <- read_map_data(folder, quiet = T))
  
  # data checks
  expect_equal(data$variable %>% unique(), c("12C", "13C", "14N12C", "15N12C"))
  expect_equal(data %>% nrow(), data$frame_size.px[1]^2 * length(data$variable %>% unique()))
  expect_equal(data %>% names(), c("x.px", "y.px", "frame_size.px", "x.um", "y.um", "frame_size.um", 
                                   "variable", "data_type", "value", "sigma", "ROI"))
  expect_equal(data %>% {.[1,]} %>% as.character(), 
               c("1", "1", "256", "0.0391171875", "0.0391171875", "10.014", 
                 "12C", "ion_count", "1721", "41.4849370253831", "0"))
  
  
})