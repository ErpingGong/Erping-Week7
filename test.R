library(testthat)
# Test 1: Check if 'BuildingID' is an integer and within the range 1:100
test_that("BuildingID is an integer and within range 1:100", {
  expect_true(all(buildings$BuildingID %% 1 == 0))
  expect_true(all(buildings$BuildingID >= 1 & buildings$BuildingID <= 100))
})

# Test 2: Check if 'NumFloors' is an integer and within the range 1:60
test_that("NumFloors is an integer and within range 1:60", {
  expect_true(all(buildings$NumFloors %% 1 == 0))
  expect_true(all(buildings$NumFloors >= 1 & buildings$NumFloors <= 60))
})

# Test 3: Ensure the YearConstructed is not in the future
current_year <- as.numeric(format(Sys.Date(), "%Y"))
test_that("YearConstructed is not in the future", {
  expect_true(all(buildings$YearConstructed <= current_year))
})
# Test 4: Check if 'BuildingUse' is a character and one of the predefined categories
valid_uses <- c("Residential", "Commercial", "Mixed")
test_that("BuildingUse is a valid category", {
  expect_true(all(buildings$BuildingUse %in% valid_uses))
})

# Test 5: Check if 'LocationType' is a character and one of the predefined categories
valid_locations <- c("Central", "Peripheral")
test_that("LocationType is a valid category", {
  expect_true(all(buildings$LocationType %in% valid_locations))
})

# Test 6: Ensure there are no duplicate BuildingIDs
test_that("There are no duplicate BuildingIDs", {
  expect_true(length(unique(buildings$BuildingID)) == length(buildings$BuildingID))
})

# Test 7: Check that NumFloors is not less than 1 (no building should have 0 floors)
test_that("NumFloors is not less than 1", {
  expect_true(all(buildings$NumFloors >= 1))
})

# Test 8: Ensure that for 'Mixed' BuildingUse, NumFloors is greater than 1 (assuming mixed-use buildings have multiple floors)
test_that("Mixed-use buildings have more than one floor", {
  mixed_use_floors <- buildings$NumFloors[buildings$BuildingUse == "Mixed"]
  expect_true(all(mixed_use_floors > 1))
})

# Test 9: Check if LocationType is not empty or NA
test_that("LocationType is not empty or NA", {
  expect_true(all(!is.na(buildings$LocationType) & buildings$LocationType != ""))
})

# Test 10: Check data types for each column
test_that("Data types are consistent", {
  expect_is(buildings$BuildingID, "integer")
  expect_is(buildings$NumFloors, "integer")
  expect_is(buildings$YearConstructed, "integer")
  expect_is(buildings$BuildingUse, "character")
  expect_is(buildings$LocationType, "character")
})

test_dir("path/to/cloud/project/Quiz7 Q2 and Q7.R")