test_that("Errors work", {
  # Errores para dtriang
  expect_error(dtriang(0.5, 2, 1, 1.5)) # Falla porque min >= max
  expect_error(dtriang(0.5, 0, 1, 2))   # Falla porque mode > max

  # Errores para ptriang
  expect_error(ptriang(0.5, 2, 1, 1.5)) # Falla porque min >= max
  expect_error(ptriang(0.5, 0, 1, 2))   # Falla porque mode > max

  # Errores para qtriang
  expect_error(qtriang(0.5, 2, 1, 1.5)) # Falla porque min >= max
  expect_error(qtriang(0.5, 0, 1, 2))   # Falla porque mode > max
  expect_error(qtriang(1.5, 0, 1, 0.5)) # Falla porque p > 1
})

test_that("Math works", {
  expect_equal(dtriang(0.5, 0, 1, 0.5), 2)
  expect_equal(ptriang(0.5, 0, 1, 0.5), 0.5)
  expect_equal(qtriang(0.5, 0, 1, 0.5), 0.5)
  expect_length(rtriang(10, 0, 1, 0.5), 10)
})
