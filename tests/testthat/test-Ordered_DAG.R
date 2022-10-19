test_that("Isotone Optimization for arbitrary DAG isotonic constraints works", {
  load("Ordered_DGA.RData")
  x_DAG <- Ordered_DAG(y, w, E_mat)
  expect_equal(x_DAG, x_DAG_check)
})


