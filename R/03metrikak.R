##############################################################
#' @title Atributu zenbakizkoen bariantza kalkulua
#' @description
#' `var_col()` funtzioak atributu zenbakizkoen bariantza kalkulatzen du.
#' Metodoak `data.frame`, `Atributua` eta `DatuMultzoa` klaseetarako bertsioak ditu.
#'
#' @param x Aztertzeko objektua (`data.frame`, `Atributua` edo `DatuMultzoa`).
#'
#' @return
#' Bariantza balioak dituen bektore izenduna edo balio bakarra (`numeric`).
#'
#' @examples
#' df <- data.frame(a = 1:5, b = c(2,3,4,5,6))
#' var_col(df)
#'
#' @seealso `entropy()` — atributu kategorikoen informazio maila kalkulatzeko.
#' @export
##############################################################

# Funtzio generikoa
setGeneric("var_col", function(x) standardGeneric("var_col"))

#' @rdname var_col
#' @aliases var_col,data.frame-method
#' @export
setMethod("var_col", "data.frame", function(x) {
  apply(x, MARGIN = 2, FUN = var)
})

#' @rdname var_col
#' @aliases var_col,DatuMultzoa-method
#' @export
setMethod("var_col", "DatuMultzoa", function(x) {
  num_atrib <- Filter(function(a) a@mota == "numeric", x@atributuak)
  if (length(num_atrib) == 0) stop("Ez dago zenbakizko atributurik")
  df <- as.data.frame(lapply(num_atrib, function(a) a@balioak))
  apply(df, MARGIN = 2, FUN = var)
})

#' @rdname var_col
#' @aliases var_col,Atributua-method
#' @export
setMethod("var_col", "Atributua", function(x) {
  if (x@mota != "numeric") stop("Atributua ez da zenbakizkoa (numeric izan behar du)")
  var(x@balioak)
})

##############################################################
#' @title AUC kalkulua atributu jarraituentzat
#' @description
#' `roc_analisi()` funtzioak atributu jarraitu baten eta klase binario baten arteko
#' erlazioa ebaluatzen du, ROC kurba eta AUC balioa kalkulatuz.
#'
#' @param df `data.frame` edo `DatuMultzoa` objektua.
#'
#' @return Zerrenda bat: `curva` (ROC kurbaren puntu guztiak) eta `AUC` (azalera).
#'
#' @examples
#' df_test <- data.frame(
#'   aldagaia = c(0.1, 0.4, 0.35, 0.8, 0.9, 0.2, 0.6, 0.5, 0.7, 0.3),
#'   etiketa  = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
#' )
#' emaitza <- roc_analisi(df_test)
#' print(emaitza$AUC)
#'
#' @seealso `var_col()` eta `entropy()`
#' @export
##############################################################

setGeneric("roc_analisi", function(df) standardGeneric("roc_analisi"))

#' @rdname roc_analisi
#' @aliases roc_analisi,data.frame-method
#' @export
setMethod("roc_analisi", "data.frame", function(df) {
  if (!is.numeric(df[,1])) stop("Lehen zenbakiak zenbakizkoa izan behar du")
  if (!is.logical(df[,2])) stop("Bigarren zutabeak logikoa izan behar du")

  df_sorted <- df[order(df[,1]), ]
  TPR <- c()
  FPR <- c()

  for (i in 1:(nrow(df_sorted))) {
    df_sorted$pred <- df_sorted[,1] >= df_sorted[i,1]
    TP <- sum(df_sorted$pred & df_sorted[,2])
    TN <- sum(!df_sorted$pred & !df_sorted[,2])
    FP <- sum(df_sorted$pred & !df_sorted[,2])
    FN <- sum(!df_sorted$pred & df_sorted[,2])
    number <- as.character(df_sorted[i,1])
    TPR[number] <- TP / (TP + FN)
    FPR[number] <- FP / (FP + TN)
  }

  AUC <- 0
  for (i in 2:length(FPR)) {
    AUC <- AUC + (FPR[i] - FPR[i-1]) * (TPR[i] + TPR[i-1]) / 2
  }

  AUC <- abs(AUC)
  names(AUC) <- NULL
  return(list(curva = data.frame(cutoff = as.numeric(names(FPR)), FPR, TPR), AUC = AUC))
})

#' @rdname roc_analisi
#' @aliases roc_analisi,DatuMultzoa-method
#' @export
setMethod("roc_analisi", "DatuMultzoa", function(df) {
  num_atrib <- Filter(function(a) a@mota == "numeric", df@atributuak)
  log_atrib <- Filter(function(a) a@mota == "categoric", df@atributuak)

  if (length(num_atrib) < 1) stop("Ez dago zenbakizko atributurik")
  if (length(log_atrib) < 1) stop("Ez dago atributu logiko/categorikorik etiketarako")

  tmp_df <- data.frame(num = num_atrib[[1]]@balioak,
                       label = as.logical(log_atrib[[1]]@balioak))
  roc_analisi(tmp_df)
})

##############################################################
#' @title Entropiaren kalkulua atributu kategorikoentzat
#' @description
#' `entropy()` funtzioak aldagai diskretu baten entropia kalkulatzen du.
#' `entropy_by_column()` funtzioak dataset edo `DatuMultzoa` batean
#' atributu guztien entropia kalkulatzen du zutabeka.
#'
#' @param x Objektua (`factor`, `Atributua`, `data.frame` edo `DatuMultzoa`).
#'
#' @return
#' `entropy()` → balio bakarra (`numeric`),
#' `entropy_by_column()` → bektore izenduna (`numeric`).
#'
#' @examples
#' fakt <- factor(c("A", "A", "B", "B", "C", "A"))
#' entropy(fakt)
#' df <- data.frame(kol1 = factor(c("A", "A", "B", "C")),
#'                  kol2 = factor(c("X", "X", "X", "Y")))
#' entropy_by_column(df)
#' dm <- DatuMultzoa(df)
#' entropy_by_column(dm)
#'
#' @seealso `var_col()` eta `roc_analisi()`
#' @name entropy
#' @aliases entropy entropy_by_column entropy,factor-method entropy,Atributua-method entropy_by_column,data.frame-method entropy_by_column,DatuMultzoa-method
#' @export
##############################################################

setGeneric("entropy", function(x) standardGeneric("entropy"))

#' @rdname entropy
#' @aliases entropy,factor-method
#' @export
setMethod("entropy", "factor", function(x) {
  frekuentziak <- table(factor(x, levels = levels(x)))
  p <- frekuentziak / sum(frekuentziak)
  -sum(p[p > 0] * log2(p[p > 0]))
})

#' @rdname entropy
#' @aliases entropy,Atributua-method
#' @export
setMethod("entropy", "Atributua", function(x) {
  if (x@mota != "categoric") stop("Atributua has to be categoric!")
  frekuentziak <- table(factor(x@balioak, levels = levels(x@balioak)))
  p <- frekuentziak / sum(frekuentziak)
  -sum(p[p > 0] * log2(p[p > 0]))
})

#' @rdname entropy
#' @aliases entropy_by_column
#' @export
setGeneric("entropy_by_column", function(x) standardGeneric("entropy_by_column"))

#' @rdname entropy
#' @aliases entropy_by_column,data.frame-method
#' @export
setMethod("entropy_by_column", "data.frame", function(x) {
  if (!all(sapply(x, is.factor))) stop("All columns must be factors")
  sapply(x, entropy)
})

#' @rdname entropy
#' @aliases entropy_by_column,DatuMultzoa-method
#' @export
setMethod("entropy_by_column", "DatuMultzoa", function(x) {
  cat_atrib <- Filter(function(a) a@mota == "categoric", x@atributuak)
  if (length(cat_atrib) == 0) stop("Ez dago atributu kategorikorik")
  sapply(cat_atrib, function(a) entropy(a@balioak))
})

