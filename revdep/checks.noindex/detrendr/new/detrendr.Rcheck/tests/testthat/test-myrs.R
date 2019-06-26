test_that("myrpois works on mac", {
  skip_if(getRversion() < "3.6.0")
  skip_if_not(get_os() == "mac")
  set.seed(1)
  expect_equal(myrpois(-5:5), c(-7, -3, -7, -3, 0, 0, 1, 0, 3, 4, 3))
})

test_that("myrpois works on linux", {
  skip_if(getRversion() < "3.6.0")
  skip_if_not(get_os() == "linux")
  skip_on_cran()
  set.seed(1)
  x <- myrpois(-5:5)
  expect_equal(x, c(-6, -6, -5, -1, -3, 0, 1, 2, 4, 4, 4))
})

test_that("myrpois works on windows", {
  skip_if(getRversion() < "3.6.0")
  skip_if_not(get_os() == "windows")
  set.seed(1)
  myrp <- myrpois(-5:5)
  expect_equal(myrp, c(-8, -7, -4, -3, 0, 0, 0, 1, 2, 6, 4))
})

test_that("myrbern works on mac", {
  skip_if(getRversion() < "3.6.0")
  skip_if_not(get_os() == "mac")
  set.seed(1)
  expect_equal(
    myrbern(seq(0.1, 0.9, length.out = 7)),
    c(0, 0, 0, 0, 1, 1, 1)
  )
})

test_that("myrbern works on linux", {
  skip_if(getRversion() < "3.6.0")
  skip_if_not(get_os() == "linux")
  skip_on_cran()
  set.seed(1)
  x <- myrbern(seq(0.1, 0.9, length.out = 7))
  if (getRversion() >= "3.6") {
    ans <- c(0, 0, 0, 1, 0, 1, 1)
    expect_equal(x, ans)
  }
})

test_that("myrbern works on windows", {
  skip_if(getRversion() < "3.6.0")
  skip_if_not(get_os() == "windows")
  set.seed(1)
  myrb <- myrbern(seq(0.1, 0.9, length.out = 7))
  if (filesstrings::all_equal(myrb, c(0, 0, 1, 1, 0, 0, 1))) {
    expect_equal(myrb, c(0, 0, 1, 1, 0, 0, 1))
  } else { # R 3.5.0 on win-builder
    expect_equal(myrb, c(0, 0, 0, 0, 1, 1, 1))
  }
})

test_that("rfromboxes works on mac", {
  skip_if(getRversion() < "3.6.0")
  skip_if_not(get_os() == "mac")
  set.seed(1)
  if (getRversion() >= "3.6") {
    expect_equal(rfromboxes(10, 1:5), c(1, 1, 3, 2, 3))
  }
})

test_that("rfromboxes works on linux", {
  skip_if(getRversion() < "3.6.0")
  skip_if_not(get_os() == "linux")
  skip_on_cran()
  set.seed(1)
  x <- rfromboxes(10, 1:5)
  ans <- c(1, 2, 2, 2, 3) # travis
  if (getRversion() >= "3.6") {
    expect_equal(x, ans)
  }
})

test_that("rfromboxes works on windows", {
  skip_if(getRversion() < "3.6.0")
  skip_if_not(get_os() == "windows")
  set.seed(1)
  rfb <- rfromboxes(10, 1:5)
  expect_equal(rfb, c(1, 2, 2, 2, 3))
})

test_that("rfromboxes errors correctly", {
  expect_error(
    rfromboxes(10, 1:3),
    paste0(
      "`n` must be less than or equal to the total ",
      "number of balls.*",
      "You have.*n = ", 10, ".*6 balls"
    )
  )
  expect_error(
    rfromboxes(10, 1:5, 1:11),
    paste0(
      "The length of `weights` must be equal to ",
      "the length of `balls`.", ".*",
      "You have 5 elements in `balls` and ",
      "11 elements in `weights`."
    )
  )
})

test_that("`rfromboxes()` edge cases work correctly", {
  skip_if(getRversion() < "3.6.0")
  expect_equal(rfromboxes(0, 1:3), rep(0, 3))
})

test_that("rtoboxes works on mac", {
  skip_if(getRversion() < "3.6.0")
  skip_if_not(get_os() == "mac")
  set.seed(1)
  if (getRversion() >= "3.6") {
    expect_equal(rtoboxes(10, 4), c(1, 5, 0, 4))
  }
})

test_that("rtoboxes works on linux", {
  skip_if(getRversion() < "3.6.0")
  skip_if_not(get_os() == "linux")
  skip_on_cran()
  set.seed(1)
  x <- rtoboxes(10, 4)
  if (getRversion() >= "3.6") {
    ans <- c(2, 4, 2, 2) # travis
    expect_equal(x, ans)
  }
})
test_that("rtoboxes works on windows", {
  skip_if(getRversion() < "3.6.0")
  skip_if_not(get_os() == "windows")
  set.seed(1)
  rtb <- rtoboxes(10, 4)
  expect_equal(rtb, c(2, 4, 2, 2))
})

test_that("rtoboxes errors correctly", {
  expect_error(
    rtoboxes(10, 4, capacities = 1:5),
    paste0(
      "The length of `capacities` must be equal to ",
      "the number of boxes.*",
      "You have 4 boxes and 5 .*capacities"
    )
  )
  expect_error(
    rtoboxes(10, 4, 1:11),
    paste0(
      "The length of `weights` must be equal to ",
      "the number of boxes.", ".*",
      "You have 4 boxes and ",
      "11 elements in `weights`."
    )
  )
  expect_error(
    rtoboxes(20, 5, capacities = 1:5),
    paste(
      "You have `n = 20` balls but your 5 boxes only have the",
      "capacity for a total of 15 balls"
    )
  )
})
