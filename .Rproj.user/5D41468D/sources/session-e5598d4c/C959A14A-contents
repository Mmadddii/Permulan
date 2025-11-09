#' @importFrom graphics abline
#' @importFrom methods new
#' @importFrom stats cor quantile sd var
#' @import ggplot2
#' @import reshape2
#' @import scales
NULL

utils::globalVariables(c("Aldagaia1", "Aldagaia2", "Balioa"))

#' @title Korrelazioen irudikapena
#' @description
#' Dataset baten korrelazio-matrizea edo informazio mutuala irudikatzen du `ggplot2` erabiliz.
#' Zenbakizko aldagaietarako korrelazioak kalkulatzen dira.
#' *Adi!!!* Jatorrizko datuak sartu behar dira, funtzioak falkulatzen ditu korrelazioak eta bertatik irudikatzen ditu.
#'
#' @param M Data.frame edo matrize bat, non zutabe guztiak zenbakizkoak diren.
#'
#' @return Korrelazio-matrizearen heatmap bat bistaratzen du.
#'
#' @examples
#' df <- data.frame(
#'   adina = c(20, 25, 30, 35, 40, 45, 50),
#'   soldata = c(5000, 4000, 3000, 2000, 1000, 800, 600),
#'   altuera = c(1.60, 1.65, 1.70, 1.75, 1.78, 1.82, 1.85)
#' )
#' Korrelazioak_irudikatu(df)
#'
#' @export
##############################################################

Korrelazioak_irudikatu <- function(M) {
  if (!requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("reshape2", quietly = TRUE)) {
    stop("Funtzio honek 'ggplot2' eta 'reshape2' paketak behar ditu. Instalatu lehenik.")
  }

  # 1. Ziurtatu aldagai guztiak zenbakizkoak direla
  if (!all(sapply(M, is.numeric))) {
    stop("Zutabe guztiak zenbakizkoak izan behar dute!")
  }

  # 2. Korrelazio matrizea kalkulatu
  Kor <- cor(M, use = "complete.obs")

  # 3. Melt formatura bihurtu (reshape2 erabiliz)
  Kor_plot <- reshape2::melt(Kor)
  colnames(Kor_plot) <- c("Aldagaia1", "Aldagaia2", "Balioa")

  # 4. Irudia sortu
  ggplot2::ggplot(Kor_plot, ggplot2::aes(x = Aldagaia1, y = Aldagaia2, fill = Balioa)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradientn(
      colors = c("blue", "white", "yellow", "orange", "red"),
      values = scales::rescale(c(-1, 0, 0.7, 0.9, 1)),
      limits = c(-1, 1),
      name = "Korrelazioa"
    )+
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::ggtitle("Korrelazioen Irudikapena")
}

##############################################################
#' @title ROC kurbaren irudikapena (AUC plot)
#' @description
#' `roc_analisi()` funtzioaren emaitzetatik ROC kurba marrazten du eta
#' AUC balioa irudikatzen du.
#'
#' @param resultado `roc_analisi()` funtzioaren emaitza (`curva` eta `AUC` elementuak dituen zerrenda)
#'
#' @return ROC kurba marrazten du pantailan.
#'
#' @examples
#' df <- data.frame(
#'   adina = c(20, 25, 30, 35, 40, 45, 50),
#'   soldata = c(5000, 4000, 3000, 2000, 1000, 800, 600),
#'   altuera = c(1.60, 1.65, 1.70, 1.75, 1.78, 1.82, 1.85)
#' )
#' Korrelazioak_irudikatu(df)
#'
#' @export

##############################################################
Visualizar_ROC <- function(resultado) {
  if (!("curva" %in% names(resultado)) || !("AUC" %in% names(resultado))) {
    stop("Sartutako objektuaren formatua ez da zuzena. 'roc_analisi()' funtziotik ateratakoa izan behar du.")
  }

  curva <- resultado$curva
  AUC <- resultado$AUC

  plot(curva$FPR, curva$TPR, type = "l", col = "blue", lwd = 2,
       main = paste("ROC Kurba (AUC =", round(AUC, 3), ")"),
       xlab = "Positibo Faltxuen Tasa (FPR)",
       ylab = "Egiazko Positiboen Tasa (TPR)")
  abline(0, 1, col = "red", lty = 2)
}

