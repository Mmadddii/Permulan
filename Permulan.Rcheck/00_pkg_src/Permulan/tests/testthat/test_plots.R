test_that("Visualizar_ROC() genera un gráfico sin errores", {
  # Crear un resultado simulado de auc_metric()
  resultado <- list(
    curva = data.frame(
      cutoff = seq(0, 1, 0.1),
      FPR = seq(0, 1, 0.1),
      TPR = seq(0, 1, 0.1)
    ),
    AUC = 0.85
  )

  # Esperar que la función no arroje errores ni warnings
  expect_silent(Visualizar_ROC(resultado))
})

test_that("Visualizar_ROC() lanza error con formato incorrecto", {
  # Falta el campo curva
  resultado_mal <- list(AUC = 0.9)
  expect_error(Visualizar_ROC(resultado_mal))
})


test_that("Korrelazioak_irudikatu() devuelve un objeto ggplot válido", {
  df <- data.frame(
    adina = c(20, 25, 30, 35, 40),
    soldata = c(1000, 1500, 2000, 2500, 3000),
    altuera = c(1.60, 1.65, 1.70, 1.75, 1.80),
    pisua = c(50, 55, 63, 70, 75)
  )

  p <- Korrelazioak_irudikatu(df)

  # Verificar que devuelve un objeto ggplot
  expect_s3_class(p, "ggplot")

  # Verificar que no genera errores
  expect_silent(print(p))
})

test_that("Korrelazioak_irudikatu() lanza error si hay variables no numéricas", {
  df <- data.frame(
    adina = c(20, 25, 30),
    hiria = c("Bilbao", "Donostia", "Gasteiz")
  )

  expect_error(Korrelazioak_irudikatu(df))
})
