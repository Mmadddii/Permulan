test_that("Atributua y DatuMultzoa se crean correctamente", {
  a1 <- Atributua("edad", c(23, 45, 34))
  a2 <- Atributua("sexo", factor(c("M", "F", "M")))

  expect_s4_class(a1, "Atributua")
  expect_s4_class(a2, "Atributua")
  expect_equal(a1@mota, "numeric")
  expect_equal(a2@mota, "categoric")

  df <- data.frame(edad = c(23, 45, 34), sexo = c("M", "F", "M"))
  d <- DatuMultzoa(df, klase_izena = "sexo")

  expect_s4_class(d, "DatuMultzoa")
  expect_true("edad" %in% names(d@atributuak))
  expect_equal(d@klasea@izena, "sexo")
})
