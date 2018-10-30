context("myrs")

test_that("myrpois works on mac", {
  skip_if_not(get_os() == "mac")
  set.seed(1)
  expect_equal(myrpois(-5:5), c(-4, -2, -3, -3, -2, 0, 0, 1, 5, 1, 6))
})

test_that("myrpois works on linux", {
  skip_if_not(get_os() == "linux")
  set.seed(1)
  x <- myrpois(-5:5)
  ans <- c(-2, -5, -6, -2, 0, 0, 1, 0, 4, 6, 5)
  if (filesstrings::all_equal(x, ans)) {
    expect_equal(x, ans)
  } else {
    ans <- c(-9, -2, -1, -2, -2, 0, 1, 2, 2, 3, 8)
    if (filesstrings::all_equal(x, ans)) {  # r-hub fedora
    expect_equal(x, ans)
    } else {  # CRAN fedora
      ans <- c(-4, -2, -3, -3, -2, 0, 0, 1, 5, 1, 6)
      expect_equal(x, ans)
    }
  }
})
test_that("myrpois works on windows", {
  skip_if_not(get_os() == "windows")
  set.seed(1)
  myrp <- myrpois(-5:5)
  if (filesstrings::all_equal(myrp, c(-2, -5, -6, -2, 0, 0, 1, 0, 4, 6, 5))) {
    expect_equal(myrp, c(-2, -5, -6, -2, 0, 0, 1, 0, 4, 6, 5))
  } else {  # R 3.5.0 on win-builder
    expect_equal(myrp, c(-6, -4, -2, -1, -1, 0, 1, 6, 4, 6, 7))
  }
})

test_that("myrbern works on mac", {
  skip_if_not(get_os() == "mac")
  set.seed(1)
  expect_equal(myrbern(seq(0.1, 0.9, length.out = 7)),
               c(0, 0, 1, 1, 0, 0, 1))
})
test_that("myrbern works on linux", {
  skip_if_not(get_os() == "linux")
  set.seed(1)
  x <- myrbern(seq(0.1, 0.9, length.out = 7))
  if (filesstrings::all_equal(x, c(0, 0, 1, 1, 0, 0, 1))) {
    expect_equal(x, c(0, 0, 1, 1, 0, 0, 1))
  } else {  # fedora
    expect_equal(x, c(0, 1, 1, 0, 0, 1, 1))
  }
})
test_that("myrbern works on windows", {
  skip_if_not(get_os() == "windows")
  set.seed(1)
  myrb <- myrbern(seq(0.1, 0.9, length.out = 7))
  if (filesstrings::all_equal(myrb, c(0, 0, 1, 1, 0, 0, 1))) {
    expect_equal(myrb, c(0, 0, 1, 1, 0, 0, 1))
  } else {  # R 3.5.0 on win-builder
    expect_equal(myrb, c(0, 0, 0, 0, 1, 1, 1))
  }

})

test_that("rfromboxes works on mac", {
  skip_if_not(get_os() == "mac")
  set.seed(1)
  expect_equal(rfromboxes(10, 1:5), c(0, 2, 2, 1, 5))
})
test_that("rfromboxes works on linux", {
  skip_if_not(get_os() == "linux")
  set.seed(1)
  x <- rfromboxes(10, 1:5)
  if (filesstrings::all_equal(x, c(0, 2, 2, 1, 5))) {
    expect_equal(x, c(0, 2, 2, 1, 5))
  } else {  # fedora
    expect_equal(x, c(1, 2, 2, 3, 2))
  }
})
test_that("rfromboxes works on windows", {
  skip_if_not(get_os() == "windows")
  set.seed(1)
  rfb <- rfromboxes(10, 1:5)
  if (filesstrings::all_equal(rfb, c(0, 2, 2, 1, 5))) {
    expect_equal(rfb, c(0, 2, 2, 1, 5))
  } else {  # R 3.5.0 on win-builder
    expect_equal(rfb, c(1, 2, 2, 3, 2))
  }
})

test_that("rfromboxes errors correctly", {
  expect_error(rfromboxes(10, 1:3),
               paste0("`n` must be less than or equal to the total ",
                      "number of balls.*",
                      "You have.*n = ", 10, ".*6 balls"))
  expect_error(rfromboxes(10, 1:5, 1:11),
               paste0("The length of `weights` must be equal to ",
                      "the length of `balls`.", ".*",
                      "You have 5 elements in `balls` and ",
                      "11 elements in `weights`."))
})

test_that("rtoboxes works on mac", {
  skip_if_not(get_os() == "mac")
  set.seed(1)
  expect_equal(rtoboxes(10, 4), c(0, 4, 1, 5))
})
test_that("rtoboxes works on linux", {
  skip_if_not(get_os() == "linux")
  set.seed(1)
  x <- rtoboxes(10, 4)
  if (filesstrings::all_equal(x, c(0, 4, 1, 5))) {
    expect_equal(x, c(0, 4, 1, 5))
  } else {  # fedora
    expect_equal(x, c(2, 6, 0, 2))
  }
})
test_that("rtoboxes works on windows", {
  skip_if_not(get_os() == "windows")
  set.seed(1)
  rtb <- rtoboxes(10, 4)
  if (filesstrings::all_equal(rtb, c(0, 4, 1, 5))) {
    expect_equal(rtb, c(0, 4, 1, 5))
  } else {  # R 3.5.0 on win-builder
    expect_equal(rtb, c(2, 6, 0, 2))
  }
})

test_that("rtoboxes errors correctly", {
  expect_error(rtoboxes(10, 4, capacities = 1:5),
               paste0("The length of `capacities` must be equal to ",
                      "the number of boxes.*",
                      "You have 4 boxes and 5 .*capacities"))
  expect_error(rtoboxes(10, 4, 1:11),
               paste0("The length of `weights` must be equal to ",
                      "the number of boxes.", ".*",
                      "You have 4 boxes and ",
                      "11 elements in `weights`."))
  expect_error(rtoboxes(20, 5, capacities = 1:5),
               paste("You have `n = 20` balls but your 5 boxes only have the",
                     "capacity for a total of 15 balls"))
})
