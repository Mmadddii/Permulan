test_that("korrelazio_matrizea funciona correctamente con data.frame", {
  df <- data.frame(
    edad = c(21, 35, 50, 28, 44, 60),
    ingreso = c(1500, 2300, 1800, 2900, 3100, 4000),
    ciudad = factor(c("A", "B", "A", "C", "B", "C")),
    zona = factor(c("N", "S", "N", "E", "S", "E"))
  )

  # Ejecutar la función
  res <- korrelazio_matrizea(df)

  # Debe devolver una matriz cuadrada
  expect_true(is.matrix(res))
  expect_equal(nrow(res), ncol(res))
  expect_equal(colnames(res), names(df))

  # Debe haber NA en cruces de tipo distinto
  expect_true(is.na(res["edad", "ciudad"]))

  # Correlación numérica debe ser válida
  expect_true(res["edad", "ingreso"] > 0.0)

  # MI entre factores debe ser >= 0
  expect_true(res["ciudad", "zona"] >= 0)
})

test_that("korrelazio_matrizea funciona con DatuMultzoa", {
  df <- data.frame(
    adina = c(20, 25, 30, 35, 40),
    soldata = c(1000, 1200, 1500, 1800, 2000),
    sexua = factor(c("M", "F", "M", "M", "F")),
    etiketa = factor(c("A", "B", "A", "B", "A"))
  )

  dm <- DatuMultzoa(df, klase_izena = "etiketa")
  res <- korrelazio_matrizea(dm)

  # Verificaciones básicas
  expect_true(is.matrix(res))
  expect_equal(nrow(res), ncol(res))
  expect_setequal(colnames(res), sapply(dm@atributuak, function(a) a@izena))

  # Debe devolver valores positivos para factores
  expect_true(all(res[is.finite(res)] >= 0 | res[is.finite(res)] <= 1))
})
