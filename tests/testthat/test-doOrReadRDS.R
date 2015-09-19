context("doOrReadRDS")

tdir <- tempdir()

test_that("doOrReadRDS creates recursive directories.", {
  a <- doOrReadRDS(1*2, file=paste0(tdir, "subdir/subdir/filename.rds"))
  expect_true(file.exists(paste0(tdir, "subdir/subdir")))
})

test_that("doOrReadRDS reads from file if it exists.", {
  a <- doOrReadRDS(1*2, file=paste0(tdir, "subdir/subdir/filename.rds"))
  a2 <- doOrReadRDS(1*200, file=paste0(tdir, "subdir/subdir/filename.rds"))
  expect_true(a2==2)
})
