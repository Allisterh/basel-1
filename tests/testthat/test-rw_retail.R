test_that("check expected retail (R = 0.15 default)", {
  expect_equal(round(rw_retail(PD=0.007, LGD=0.12),7), 0.1252824)
})

test_that("check expected retail (R = 0.15 default) and SK=1", {
  expect_equal(round(rw_retail(PD=0.007, LGD=0.12, SK=1),7), 0.118191)
})
