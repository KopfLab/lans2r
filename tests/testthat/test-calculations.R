context("Calculations")

# test data
a <- data_frame(variable = "A", ROI = 1:5, value = runif(5), sigma = 0.1, data_type = "temp")
b <- data_frame(variable = "B", ROI = 1:5, value = rnorm(5), sigma = 0.2, data_type = "temp")
c <- data_frame(variable = "C", ROI = 1:5, value = rnorm(5), sigma = 0.2, data_type = "temp")
d <- data_frame(variable = "D", ROI = 1:5, value = rnorm(5), sigma = 0.2, data_type = "temp")
test_data <- bind_rows(a, b, c, d)

test_that("test that calculate works properly", {
  
  # error checks
  expect_error(calculate(data_frame()), "column not in dataset: 'variable'")
  expect_error(calculate(data_frame(variable = "A")), "column not in dataset: 'value'")
  expect_error(calculate(data_frame(variable = "A", value = 5)), "column not in dataset: 'data_type'")
  
  
  # testing calculate
  my_value_fun <- function(x, y, x.err, y.err) x*y
  my_error_fun <- function(x, y, x.err, y.err) my_value_fun(x, y, x.err, y.err) * sqrt((x.err/x)^2 + (y.err/y)^2)
  my_name_fun <- function(x, y, ...) paste0(deparse(substitute(x)), "*", deparse(substitute(y)))
  
  expect_message(
    test_data %>% 
    calculate(
      data_type = "derived",
      c(D, C, `D sigma`, `C sigma`), c(B, A, `B sigma`, `A sigma`),
      value_fun = my_value_fun, error_fun = my_error_fun, name_fun = my_name_fun), 
    "10 'derived' values \\+ errors calculated")
  
  expect_silent(
    deriv_data <- test_data %>% 
      calculate(
        data_type = "derived", quiet = T,
        c(D, C, `D sigma`, `C sigma`), c(B, A, `B sigma`, `A sigma`),
        value_fun = my_value_fun, error_fun = my_error_fun, name_fun = my_name_fun))

  # data checks
  expect_equal(deriv_data %>% filter(data_type == "derived") %>% {.$variable} %>% unique(),
               c("D*C", "B*A"))
  expect_equal(
    left_join(b, a, by="ROI") %>% mutate(value = value.x*value.y) %>% {.$value},
    deriv_data %>% filter(variable == "B*A") %>% {.$value})
  expect_equal(
    left_join(d, c, by="ROI") %>% mutate(value = value.x*value.y) %>% {.$value},
    deriv_data %>% filter(variable == "D*C") %>% {.$value})
  expect_equal(
    left_join(b, a, by="ROI") %>% 
      mutate(error = my_error_fun(value.x, value.y, sigma.x, sigma.y)) %>% {.$error},
    deriv_data %>% filter(variable == "B*A") %>% {.$sigma})
  expect_equal(
    left_join(d, c, by="ROI") %>% 
      mutate(error  = my_error_fun(value.x, value.y, sigma.x, sigma.y)) %>% {.$error},
    deriv_data %>% filter(variable == "D*C") %>% {.$sigma})
  
})

test_that("test that calculate ratios works properly", {
  # implement me
})

test_that("test that calculate abundances works properly", {
  #  implement me
})

test_that("test that calculate sums works properly", {
  # implement me
})

