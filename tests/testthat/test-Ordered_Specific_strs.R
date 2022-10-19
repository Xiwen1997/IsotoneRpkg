test_that("Isotone Optimization for some specific isotonic constraints works", {
  load("Specific_strs.RData")
  x_chain <- Solve_isotone_chain(y, w)
  x_tree <- Solve_isotone_binary_tree(y,w)
  x_2d_grid <- Solve_isotone_2d_grid(y,w)
  expect_equal(x_chain, x_chain_check)
  expect_equal(x_tree, x_tree_check)
  expect_equal(x_2d_grid, x_2d_grid_check)
})
