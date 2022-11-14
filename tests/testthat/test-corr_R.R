test_that("check expected value R factor, non financial, non KMU", {
  expect_equal(round(corr_R(0.15),7), 0.1200664)
})

test_that("check expected value R factor, financial, non KMU", {
  expect_equal(round(corr_R(0.15, AVC =1.25),7), 0.150083)
})
