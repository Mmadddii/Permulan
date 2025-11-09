##############################################################
# R3.1 - Equal Width diskretizazioa
##############################################################

#' @title Equal Width diskretizazioa
#' @description
#' `discretizeEW()` funtzioak balio jarraituak tarte zabalera berdinetan banatzen ditu.
#' Hau da, datuen balio minimo eta maximoaren artean `num.bins` tarte sortzen dira,
#' non tarte bakoitzaren zabalera konstantea den.
#'
#' Funtzio honek `numeric` eta `Atributua` klaseetarako metodoak ditu.
#'
#' @param x Diskretizatzeko datuak (`numeric` edo `Atributua` klaseko objektua)
#' @param num.bins Sortu nahi diren tarte kopurua (zenbakizkoa, >1)
#'
#' @return
#' \itemize{
#'   \item `numeric` sarreran: diskretizatutako balioak eta ebaki puntuak dituen zerrenda.
#'   \item `Atributua` sarreran: kategoriatan bihurtutako `Atributua` objektua eta bere ebaki puntuak.
#' }
#'
#' @examples
#' # --- Adibidea 1: numeric bektore batekin ---
#' x <- c(10, 20, 25, 30, 40, 50, 60, 70)
#' emaitza <- discretizeEW(x, num.bins = 3)
#' emaitza$x.discretized
#' emaitza$cut.points
#'
#' # --- Adibidea 2: Atributua klasearekin ---
#' a <- Atributua("adina", c(20, 30, 40, 50, 60, 70, 80))
#' a_disk <- discretizeEW(a, num.bins = 3)
#' a_disk$atributua@balioak
#' a_disk$cut.points
#'
#' @export
setGeneric("discretizeEW", function(x, num.bins) standardGeneric("discretizeEW"))

#' @rdname discretizeEW
#' @export
setMethod("discretizeEW", "numeric", function(x, num.bins) {
  if (!is.numeric(x)) stop("x has to be numeric")
  if (!is.numeric(num.bins) || num.bins <= 1)
    stop("num.bins balioak zenbakizkoa eta 1 baino handiagoa izan behar du")

  maximoa <- max(x)
  minimoa <- min(x)
  if (maximoa == minimoa)
    stop("Balio guztiak berdinak dira; ezin dira tarte zabalera berdineko bin-ak sortu")

  zabalera <- (maximoa - minimoa) / num.bins
  ebaki_puntuak <- minimoa + (1:(num.bins - 1)) * zabalera
  tarteak <- c(-Inf, ebaki_puntuak, Inf)
  etiketak <- paste0("I", 1:num.bins)

  x.discretized <- cut(x, breaks = tarteak, labels = etiketak, include.lowest = TRUE)

  return(list(x.discretized = x.discretized, cut.points = ebaki_puntuak))
})

#' @rdname discretizeEW
#' @export
setMethod("discretizeEW", "Atributua", function(x, num.bins) {
  if (x@mota != "numeric") stop("Atributua zenbakizkoa izan behar da!")
  emaitza <- discretizeEW(x@balioak, num.bins)

  return(list(
    atributua = new("Atributua",
                    izena = x@izena,
                    mota = "categoric",
                    balioak = emaitza$x.discretized),
    cut.points = emaitza$cut.points
  ))
})


##############################################################
# R3.2 - Equal Frequency diskretizazioa
##############################################################

#' @title Equal Frequency diskretizazioa
#' @description
#' `discretizeEF()` funtzioak balio jarraituak kuantilen arabera
#' tarte berdinetan banatzen ditu (maiztasun berdineko binak).
#' Funtzio honek `numeric` eta `Atributua` klaseetarako metodoak ditu.
#'
#' @param x Diskretizatzeko objektua (`numeric` edo `Atributua`)
#' @param num.bins Zenbat tarte sortu nahi diren adierazten duen balio zenbakizkoa
#'
#' @return
#' \itemize{
#'   \item `numeric` sarreran: diskretizatutako balioak eta ebaki puntuak dituen zerrenda bat.
#'   \item `Atributua` sarreran: kategoria tarteetan bihurtutako `Atributua` objektua eta bere ebaki puntuak.
#' }
#'
#' @examples
#' x <- c(10, 20, 25, 30, 40, 50, 60, 70)
#' discretizeEF(x, num.bins = 3)
#'
#' @export
setGeneric("discretizeEF", function(x, num.bins) standardGeneric("discretizeEF"))

#' @rdname discretizeEF
#' @export
setMethod("discretizeEF", "numeric", function (x, num.bins) {
  if (!is.numeric(x)) stop("x has to be numeric")
  if (!is.numeric(num.bins) || num.bins <= 1)
    stop("num.bins balioak zenbakizkoa eta 1 baino handiagoa izan behar du")

  x <- x[!is.na(x)]
  probs <- seq(0, 1, length.out = num.bins + 1)
  ebaki_puntuak <- unique(quantile(x, probs = probs))

  etiketak <- paste0("I", 1:num.bins)
  x.discretized <- cut(x, breaks = ebaki_puntuak, labels = etiketak, include.lowest = TRUE)

  return(list(x.discretized = x.discretized, cut.points = ebaki_puntuak))
})

#' @rdname discretizeEF
#' @export
setMethod("discretizeEF", "Atributua", function (x, num.bins) {
  if (x@mota != "numeric") stop("Atributua zenbakizkoa izan behar da!")
  if (!is.numeric(num.bins) || num.bins <= 1)
    stop("num.bins balioak zenbakizkoa eta 1 baino handiagoa izan behar du")

  balioak <- x@balioak[!is.na(x@balioak)]
  probs <- seq(0, 1, length.out = num.bins + 1)
  ebaki_puntuak <- unique(quantile(balioak, probs = probs))

  etiketak <- paste0("I", 1:num.bins)
  x.discretized <- cut(balioak, breaks = ebaki_puntuak, labels = etiketak, include.lowest = TRUE)

  attr.new <- new("Atributua",
                  izena = x@izena,
                  mota = "categoric",
                  balioak = x.discretized)

  return(list(x.discretized = attr.new, cut.points = ebaki_puntuak))
})


##############################################################
# R3.3 - Diskretizazioa puntu ebakiekin
##############################################################

#' @title Diskretizazioa puntu ebakiekin
#' @description
#' `discretize()` funtzioak balio jarraituak kategoria tarteetan bihurtzen ditu
#' emandako ebaki puntuak erabiliz.
#' Funtzio honek `numeric`, `Atributua` eta `DatuMultzoa` klaseetarako metodoak ditu.
#'
#' @param x Diskretizatzeko objektua (`numeric`, `Atributua` edo `DatuMultzoa`)
#' @param cut.points Ebaki puntuak adierazten dituen bektore zenbakizkoa
#'
#' @return
#' \itemize{
#'   \item `numeric` sarreran: diskretizatutako balioak eta ebaki puntuak dituen zerrenda bat.
#'   \item `Atributua` sarreran: atributu kategoriko bihurtutako `Atributua` objektu bat.
#'   \item `DatuMultzoa` sarreran: atributu zenbakizko guztiak diskretizatutako `DatuMultzoa` objektu bat.
#' }
#'
#' @examples
#' balioak <- c(20, 30, 40, 50)
#' discretize(balioak, c(25, 35, 45))
#'
#' @export
setGeneric("discretize", function(x, cut.points) standardGeneric("discretize"))

#' @rdname discretize
#' @export
setMethod("discretize", "numeric", function(x, cut.points) {
  if (!is.numeric(x)) stop("x has to be numeric")
  if (!is.numeric(cut.points)) stop("cut.points has to be numeric")

  x <- x[!is.na(x)]
  etiketak <- paste0("I", 1:(length(cut.points) + 1))
  ebaki_puntuak <- c(-Inf, cut.points, Inf)
  x.discretized <- cut(x, breaks = ebaki_puntuak, labels = etiketak, include.lowest = TRUE)
  return(list(x.discretized = x.discretized, cut.points = cut.points))
})

#' @rdname discretize
#' @export
setMethod("discretize", "Atributua", function(x, cut.points) {
  if (x@mota != "numeric") stop("Atributua ez da zenbakizkoa (numeric izan behar du)")
  res <- discretize(x@balioak, cut.points)
  new("Atributua",
      izena = x@izena,
      mota = "categoric",
      balioak = res$x.discretized)
})

#' @rdname discretize
#' @export
setMethod("discretize", "DatuMultzoa", function(x, cut.points) {
  num_atrib <- Filter(function(a) a@mota == "numeric", x@atributuak)
  if (length(num_atrib) == 0) stop("Ez dago zenbakizko atributurik DatuMultzoan.")

  disk_atrib <- lapply(num_atrib, function(a) discretize(a, cut.points))
  beste_atrib <- Filter(function(a) a@mota != "numeric", x@atributuak)
  berriak <- c(beste_atrib, disk_atrib)

  new("DatuMultzoa",
      atributuak = berriak,
      klasea = x@klasea)
})
