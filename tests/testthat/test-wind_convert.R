# Test suite for wind_degr_to_sect
test_that("wind_degr_to_sect works correctly", {
  expect_equal(wind_degr_to_sect(c(0, 45, 90, 180, 270, 360)), c("N", "NE", "E", "S", "W", "N"))
  expect_equal(wind_degr_to_sect(c(11.25, 33.75, 78.75, 191.25, 281.25)), c("NNE", "NE", "E", "SSW", "WNW"))
  expect_error(wind_degr_to_sect("invalid"), "must be a numeric vector")
})

# Test suite for wind_sect_to_degr
test_that("wind_sect_to_degr works correctly", {
  expect_equal(wind_sect_to_degr(c("N", "NE", "E", "S", "W", "NNW")), c(0, 45, 90, 180, 270, 337.5))
  expect_equal(wind_sect_to_degr(c("NNE", "ENE", "SSE", "WSW")), c(22.5, 67.5, 157.5, 247.5))
  expect_error(wind_sect_to_degr(123), "must be a character vector")
})

# perform some testing also for other functions
# very limited input check is right now performed on the two functions:
# "wind_uv_to_ds" and "wind_ds_to_uv"
# to be done!
