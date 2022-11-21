test_that("check expected value R factor, non financial, non SME", {
  expect_equal(round(corr_R(0.15),7), 0.1200664)
})

test_that("check expected value R factor, financial, non SME", {
  expect_equal(round(corr_R(0.15, AVC =1.25),7), 0.150083)
})

test_that("check expected value R factor, non financial, SME without S", {
  expect_equal(round(corr_R(0.001, SME =1),7), 0.1941475)
})

test_that("check expected value R factor, non financial, SME = 1, S=10", {
  expect_equal(round(corr_R(0.001, SME =1, S=10),7), 0.198592)
})

test_that("check expected value R factor, non financial, SME = 1, S=60", {
  expect_equal(round(corr_R(0.001, SME =1, S=60),7), 0.2341475)
})



