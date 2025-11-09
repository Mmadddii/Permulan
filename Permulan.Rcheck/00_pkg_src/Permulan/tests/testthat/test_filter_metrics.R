test_that("filter_metrics funciona correctamente con data.frame", {
  # DataFrame de ejemplo
  df <- data.frame(
    x1 = c(1, 2, 3, 4, 5),                # varianza alta
    x2 = c(5, 5, 5, 5, 5),                # varianza = 0
    cat = factor(c("A", "B", "A", "B", "C")),  # entropía > 0
    label = c(TRUE, FALSE, TRUE, FALSE, TRUE)  # variable binaria
  )

  # Filtro básico (por varianza)
  res1 <- filter_metrics(df, var_min = 1)
  expect_true("x1" %in% names(res1))
  expect_false("x2" %in% names(res1))

  # Filtro por entropía
  res2 <- filter_metrics(df, entropy_min = 0.5)
  expect_true("cat" %in% names(res2))

  # Filtro combinado (varianza + entropía)
  res3 <- filter_metrics(df, var_min = 1, entropy_min = 0.5)
  expect_true(all(c("x1", "cat") %in% names(res3)))

  # Filtro con AUC
  res4 <- filter_metrics(df, var_min = 0, auc_min = 0.4, klase = df$label)
  expect_true("x1" %in% names(res4))
})

test_that("filter_metrics funciona con DatuMultzoa", {
  # Crear un DatuMultzoa
  df <- data.frame(
    adina = c(20, 25, 30, 35, 40),
    sexua = factor(c("M", "F", "M", "M", "F")),
    etiketa = factor(c("A", "B", "A", "B", "A"))
  )

  dm <- DatuMultzoa(df, klase_izena = "etiketa")

  # Ejecutar filtro
  res <- filter_metrics(dm, var_min = 1, entropy_min = 0.1)

  # Verificar que devuelve DatuMultzoa
  expect_s4_class(res, "DatuMultzoa")

  # Verificar que mantiene atributos
  expect_true(all(sapply(res@atributuak, function(a) a@mota %in% c("numeric", "categoric"))))
})

test_that("filter_metrics maneja casos sin columnas válidas", {
  df <- data.frame(
    x = c(1, 1, 1, 1, 1),
    y = factor(c("A", "A", "A", "A", "A"))
  )

  # Debe devolver un data.frame vacío
  res <- suppressWarnings(filter_metrics(df, var_min = 1, entropy_min = 0.5))
  expect_equal(ncol(res), 0)
})
