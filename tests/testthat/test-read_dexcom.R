test_that("parse_id returns last six digits of file name", {
  expect_equal(parse_id("testTesttestTest123456.csv"), "123456")
  expect_equal(parse_id("123456testTestTesttest567890.csv"), "567890")
})

#test_that("shift_time changes the timestamp", {
  #expect_equal(..., TRUE)
#})
