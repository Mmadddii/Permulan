##############################################################
#' @title Korrelazioak edo informazio mutua aldagai guztien artean
#' @description
#' Datu-multzo bateko aldagai guztien arteko erlazioa kalkulatzen du:
#' \itemize{
#'   \item Pearson korrelazioa (biak zenbakizkoak direnean)
#'   \item Informazio mutuoa (biak kategorikoak direnean)
#' }
#' Funtzioak aldagai bakoitzaren mota automatikoki detektatzen du.
#'
#' @param x Data frame edo \code{DatuMultzoa} objektua.
#' @param ... Argumentu gehigarriak, erabilera barnekoetarako.
#'
#' @return Korrelazio edo informazio mutua matrize bat.
#'
#' @examples
#' df <- data.frame(
#'   adina = c(20, 25, 30, 35, 40),
#'   soldata = c(1000, 1200, 1500, 1800, 2000),
#'   sexua = factor(c("M", "F", "M", "M", "F")),
#'   hiria = factor(c("A", "B", "A", "C", "B"))
#' )
#'
#' korrelazio_matrizea(df)
#'
#' @export
##############################################################

# Generikoa
setGeneric("korrelazio_matrizea", function(x, ...) standardGeneric("korrelazio_matrizea"))

##############################################################
#' @rdname korrelazio_matrizea
#' @export
setMethod("korrelazio_matrizea", "data.frame", function(x, ...) {
  n <- ncol(x)
  emaitza <- matrix(NA, nrow = n, ncol = n)
  colnames(emaitza) <- rownames(emaitza) <- names(x)

  # Funtzio laguntzaileak
  mutual_info <- function(a, b) {
    joint <- table(a, b)
    pa <- rowSums(joint) / sum(joint)
    pb <- colSums(joint) / sum(joint)
    p_ab <- joint / sum(joint)
    mi <- sum(p_ab[p_ab > 0] * log2(p_ab[p_ab > 0] / (pa[row(p_ab)] * pb[col(p_ab)])))
    return(mi)
  }

  for (i in 1:n) {
    for (j in i:n) {
      xi <- x[[i]]
      xj <- x[[j]]

      if (is.numeric(xi) && is.numeric(xj)) {
        val <- cor(xi, xj, use = "complete.obs")
      } else if (is.factor(xi) && is.factor(xj)) {
        val <- mutual_info(xi, xj)
      } else {
        val <- NA  # Ez da egokia mota desberdinetarako
      }

      emaitza[i, j] <- emaitza[j, i] <- val
    }
  }

  return(emaitza)
})

##############################################################
#' @rdname korrelazio_matrizea
#' @export
setMethod("korrelazio_matrizea", "DatuMultzoa", function(x, ...) {
  # Atributuak eta balioak data.frame bihurtu
  df <- as.data.frame(lapply(x@atributuak, function(a) a@balioak))
  names(df) <- sapply(x@atributuak, function(a) a@izena)
  korrelazio_matrizea(df)
})
