test_that("Isotone Optimization for some specific isotonic constraints works", {
  load("Specific_strs.RData")
  x_chain <- Ordered_chain(y, w)
  x_tree <- Ordered_Tree(y,w)
  x_2d_grid <- Ordered_2d_grid(y,w)
  expect_equal(x_chain, x_chain_check)
  expect_equal(x_tree, x_tree_check)
  expect_equal(x_2d_grid, x_2d_grid_check)
})
