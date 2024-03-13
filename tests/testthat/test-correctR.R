
test_that("random subsampling correction works", {
  rss <- resampled_ttest(x = x, y = y, n = 30, n1 = 80, n2 = 20) # Random subsampling
  expect_equal(2, ncol(rss))
})

test_that("k-fold correction works", {
  kcv <- kfold_ttest(x = x, y = y, n = 100, k = 30) # k-fold cross-validation
  expect_equal(2, ncol(kcv))
})

test_that("repeated k-fold correction works", {
  rkcv <- repkfold_ttest(data = tmp, n1 = 80, n2 = 20, k = 2, r = 2) # Repeated k-fold cross-validation
  expect_equal(2, ncol(rkcv))
})
