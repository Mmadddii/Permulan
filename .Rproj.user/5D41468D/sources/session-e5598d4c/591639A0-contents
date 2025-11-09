test_that("var_col calcula la varianza correctamente", {
  df <- data.frame(a = 1:5, b = 2:6)
  res <- var_col(df)
  expect_named(res, c("a", "b"))
  expect_equal(unname(res["a"]), var(1:5))  # 💡 quitar el nombre

  a <- Atributua("edad", 1:5)
  expect_equal(var_col(a), var(1:5))

  dm <- DatuMultzoa(df)
  expect_equal(unname(var_col(dm)[1]), var(1:5))  # 💡 quitar el nombre
})

test_that("entropy y entropy_by_column funcionan correctamente", {
  f <- factor(c("A", "A", "B", "C"))
  expect_gt(entropy(f), 0)

  a <- Atributua("sexo", f)
  expect_equal(entropy(a), entropy(f))

  df <- data.frame(kol1 = f, kol2 = factor(c("X", "X", "X", "Y")))
  ents <- entropy_by_column(df)
  expect_named(ents, c("kol1", "kol2"))
  expect_true(all(ents >= 0))

  dm <- DatuMultzoa(df)
  ents2 <- entropy_by_column(dm)
  expect_true(all(ents2 >= 0))
})

test_that("normalize y standardize funcionan correctamente", {
  x <- c(1, 2, 3, 4, 5)
  n <- normalize(x)
  expect_equal(min(n), 0)
  expect_equal(max(n), 1)

  s <- standardize(x)
  expect_equal(round(mean(s), 10), 0)
  expect_equal(round(sd(s), 10), 1)

  df <- data.frame(a = 1:5, b = 6:10)
  ndf <- normalize(df)
  expect_true(all(ndf$a >= 0 & ndf$a <= 1))

  sdf <- standardize(df)
  expect_equal(round(mean(sdf$a), 5), 0)

  a <- Atributua("edad", 1:5)
  na <- normalize(a)
  expect_s4_class(na, "Atributua")

  dm <- DatuMultzoa(df)
  expect_s4_class(standardize(dm), "DatuMultzoa")

})
test_that("roc_analisi calcula AUC correctamente con data.frame", {
  df <- data.frame(
    variable = c(0.1, 0.4, 0.8, 0.9),
    etiqueta = c(FALSE, TRUE, TRUE, FALSE)
  )

  res <- roc_analisi(df)

  expect_true("AUC" %in% names(res))
  expect_true(res$AUC > 0 && res$AUC <= 1)
})

test_that("roc_analisi calcula AUC correctamente con DatuMultzoa", {
  df <- data.frame(
    variable = c(0.1, 0.4, 0.8, 0.9),
    etiqueta = c(FALSE, TRUE, TRUE, FALSE)
  )

  dm <- DatuMultzoa(df)
  res2 <- roc_analisi(dm)

  expect_true("AUC" %in% names(res2))
  expect_true(res2$AUC > 0 && res2$AUC <= 1)
})
df_test <- data.frame(
  score = c(0.1, 0.4, 0.35, 0.8, 0.9, 0.2, 0.6, 0.5, 0.7, 0.3),
  label = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
)

res <- roc_analisi(df_test)
print(res$AUC)  # Debería dar algo como 0.95
Visualizar_ROC(res)
