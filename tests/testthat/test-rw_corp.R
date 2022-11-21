test_that("check expected rw for corporate secured real estate", {
  expect_equal(round(rw_corp(PD=0.001, LGD=0.35),7), 0.2444807)
})

test_that("check expected rw for corporate unsecured", {
  expect_equal(round(rw_corp(PD=0.001, LGD=0.45),7), 0.3143323)
})

test_that("check expected rw for corporate unsecured with AVC", {
  expect_equal(round(rw_corp(PD=0.001, LGD=0.45, AVC=1.25),7), 0.4247158)
})

test_that("check expected rw for corporate secured, Basel IV with S=1", {
  expect_equal(round(rw_corp(PD=0.001, LGD=0.35, S=1),7), 0.2306422)
})

