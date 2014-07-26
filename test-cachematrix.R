# This test case can be used to validate cachematrix.R
# Save this file as test-cachematrix.R in the same directory as cachematrix.R
# taken from here https://class.coursera.org/rprog-005/forum/thread?thread_id=1005#comment-2977
#
#############For reference how to run it###########################
#> install.packages("testthat")
#> library(testthat)
#> source("cachematrix.R")
#> test_file("test-cachematrix.R")
#..........   ## this means all tests where all successful
###################################################################
#

test_that("cachematrix inverts a given matrix", {
  
  #define matrix and inverse matrix
  matA <- matrix(c(1, 2, 3, 4), nrow = 2)
  matAI <- matrix(c(-2, 1, 1.5, -0.5), nrow = 2)
  
  matB <- matrix(c(2, 0, 0, 0, 1, 0, 2, 2, 2), nrow = 3)
  matBI <- matrix(c(0.5, 0, 0, 0, 1, 0, -0.5, -1, 0.5), nrow = 3)
  
  cache = makeCacheMatrix(matA)

  ## test cases for matA
  expect_that(cache$get(), equals(matA))
  expect_that(cache$getInverseMatrix(), equals(NULL))
  expect_that(cacheSolve(cache), equals(matAI))
  expect_that(cache$getInverseMatrix(), equals(matAI))
  expect_message(cacheSolve(cache), "^getting cached data\\n$")
  
  ## test cases for matA
  cache$set(matB)
  expect_that(cache$get(), equals(matB))
  expect_that(cache$getInverseMatrix(), equals(NULL))
  expect_that(cacheSolve(cache), equals(matBI))
  expect_that(cache$getInverseMatrix(), equals(matBI))
  expect_message(cacheSolve(cache), "^getting cached data\\n$")
})