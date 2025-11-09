##############################################################
#' @title Datuen normalizazioa eta estandarizazioa
#' @description
#' Bi funtzio generiko eskaintzen dira:
#' \itemize{
#'   \item `normalize()`: datuak 0 eta 1 arteko tartean eskalatzen ditu.
#'   \item `standardize()`: datuak batezbesteko 0 eta desbideratze estandar 1 duten eskalara eramaten ditu.
#' }
#'
#' Metodoak `numeric`, `Atributua`, `data.frame` eta `DatuMultzoa` klaseetarako definituta daude.
#'
#' @param x Normalizatu edo estandarizatu beharreko objektua.
#'
#' @return
#' Objektu bera, baina balioak normalizatuak edo estandarizatuak:
#' \itemize{
#'   \item `numeric`: bektore zenbakizkoa
#'   \item `Atributua`: `Atributua` objektu berria
#'   \item `data.frame`: datu-markoa eskalatua
#'   \item `DatuMultzoa`: atributuak eskalatu dituen objektua
#' }
#'
#' @examples
#' x <- c(10, 20, 30)
#' normalize(x)
#' standardize(x)
#'
#' df <- data.frame(a = 1:5, b = 6:10)
#' normalize(df)
#' standardize(df)
#'
#' @seealso
#' `var_col()` — bariantza kalkulatzeko,
#' `entropy()` — atributu kategorikoen informazio maila neurtzeko.
#'
#' @name normalize
#' @aliases normalize standardize
#' @export
##############################################################

# Generikoak
setGeneric("normalize", function(x) standardGeneric("normalize"))
setGeneric("standardize", function(x) standardGeneric("standardize"))

##############################################################
#' @rdname normalize
#' @aliases normalize,numeric-method
#' @export
setMethod("normalize", "numeric", function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})

#' @rdname normalize
#' @aliases standardize,numeric-method
#' @export
setMethod("standardize", "numeric", function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
})

##############################################################
# Atributua klasea
#' @rdname normalize
#' @aliases normalize,Atributua-method
#' @export
setMethod("normalize", "Atributua", function(x) {
  if (x@mota != "numeric") stop("Atributua ez da zenbakizkoa")
  new("Atributua",
      balioak = (x@balioak - min(x@balioak, na.rm = TRUE)) /
        (max(x@balioak, na.rm = TRUE) - min(x@balioak, na.rm = TRUE)),
      mota = x@mota, izena = x@izena)
})

#' @rdname normalize
#' @aliases standardize,Atributua-method
#' @export
setMethod("standardize", "Atributua", function(x) {
  if (x@mota != "numeric") stop("Atributua ez da zenbakizkoa")
  new("Atributua",
      balioak = (x@balioak - mean(x@balioak, na.rm = TRUE)) /
        sd(x@balioak, na.rm = TRUE),
      mota = x@mota, izena = x@izena)
})

##############################################################
# DatuMultzoa klasea
#' @rdname normalize
#' @aliases normalize,DatuMultzoa-method
#' @export
setMethod("normalize", "DatuMultzoa", function(x) {
  num_atrib <- Filter(function(a) a@mota == "numeric", x@atributuak)
  if (length(num_atrib) == 0) stop("Ez dago atributu numerikorik")

  nuevos <- lapply(num_atrib, normalize)

  # Ordezkatu atributuak izenaren arabera
  for (i in seq_along(num_atrib)) {
    iz <- num_atrib[[i]]@izena
    pos <- which(sapply(x@atributuak, function(a) a@izena == iz))
    x@atributuak[[pos]] <- nuevos[[i]]
  }
  return(x)
})

#' @rdname normalize
#' @aliases standardize,DatuMultzoa-method
#' @export
setMethod("standardize", "DatuMultzoa", function(x) {
  num_atrib <- Filter(function(a) a@mota == "numeric", x@atributuak)
  if (length(num_atrib) == 0) stop("Ez dago atributu numerikorik")

  nuevos <- lapply(num_atrib, standardize)

  for (i in seq_along(num_atrib)) {
    iz <- num_atrib[[i]]@izena
    pos <- which(sapply(x@atributuak, function(a) a@izena == iz))
    x@atributuak[[pos]] <- nuevos[[i]]
  }

  return(x)
})

##############################################################
# data.frame klasea
#' @rdname normalize
#' @aliases normalize,data.frame-method
#' @export
setMethod("normalize", "data.frame", function(x) {
  num_cols <- sapply(x, is.numeric)
  if (!any(num_cols)) stop("Ez dago atributu numerikorik")
  x[num_cols] <- lapply(x[num_cols], normalize)
  return(x)
})

#' @rdname normalize
#' @aliases standardize,data.frame-method
#' @export
setMethod("standardize", "data.frame", function(x) {
  num_cols <- sapply(x, is.numeric)
  if (!any(num_cols)) stop("Ez dago atributu numerikorik")
  x[num_cols] <- lapply(x[num_cols], standardize)
  return(x)
})


