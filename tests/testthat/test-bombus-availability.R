testthat::test_that("Bombus groups partition widespread and montane species", {
  groups <- bombus_ecological_groups()
  testthat::expect_equal(groups$widespread, c("ardens", "diversus"))
  testthat::expect_equal(
    groups$montane,
    c("beaticola", "consobrinus", "honshuensis")
  )
  testthat::expect_invisible(validate_bombus_groups(groups))
})

testthat::test_that("any-species and maximum availability are calculated correctly", {
  x <- data.frame(
    ardens = c(0.2, 0, NA),
    beaticola = c(0.3, 0, 0.1),
    consobrinus = c(0.4, 0, 0.2),
    diversus = c(0.5, 0, 0.3),
    honshuensis = c(0.6, 0, 0.4)
  )
  expected_any <- 1 - prod(1 - c(0.2, 0.3, 0.4, 0.5, 0.6))
  testthat::expect_equal(any_species_availability(x)[1], expected_any)
  testthat::expect_equal(any_species_availability(x)[2], 0)
  testthat::expect_true(is.na(any_species_availability(x)[3]))
  testthat::expect_equal(maximum_species_availability(x)[1:2], c(0.6, 0))
})

testthat::test_that("availability indices retain ecological grouping", {
  x <- data.frame(
    ardens = 0.2,
    beaticola = 0.3,
    consobrinus = 0.4,
    diversus = 0.5,
    honshuensis = 0.6
  )
  result <- add_bombus_availability_indices(x)
  testthat::expect_equal(result$Bombus_suitability_sum, 2)
  testthat::expect_equal(
    result$Bombus_widespread_any_availability,
    1 - (1 - 0.2) * (1 - 0.5)
  )
  testthat::expect_equal(
    result$Bombus_montane_any_availability,
    1 - (1 - 0.3) * (1 - 0.4) * (1 - 0.6)
  )
  testthat::expect_equal(result$Bombus_widespread_max_availability, 0.5)
  testthat::expect_equal(result$Bombus_montane_max_availability, 0.6)
})
