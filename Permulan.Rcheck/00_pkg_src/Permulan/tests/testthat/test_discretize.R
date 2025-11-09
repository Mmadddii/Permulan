test_that("discretizeEW funciona con numeric y Atributua", {
  x <- c(1, 2, 3, 4, 5)
  res <- discretizeEW(x, num.bins = 2)
  expect_true(is.factor(res$x.discretized))
  expect_length(res$cut.points, 1)

  a <- Atributua("edad", x)
  res2 <- discretizeEW(a, num.bins = 2)
  expect_s4_class(res2$atributua, "Atributua")
  expect_equal(res2$atributua@mota, "categoric")
})

test_that("discretizeEF funciona con numeric y Atributua", {
  x <- c(1, 2, 3, 4, 5)
  res <- discretizeEF(x, num.bins = 2)
  expect_true(is.factor(res$x.discretized))

  a <- Atributua("edad", x)
  res2 <- discretizeEF(a, num.bins = 2)
  expect_s4_class(res2$x.discretized, "Atributua")
})

test_that("discretize con cut.points devuelve factores correctos", {
  x <- c(1, 2, 3, 4, 5)
  res <- discretize(x, cut.points = c(2, 4))
  expect_true(is.factor(res$x.discretized))
  expect_equal(length(levels(res$x.discretized)), 3)
})
