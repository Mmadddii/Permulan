#' @keywords internal
"_PACKAGE"

# Imports ----
#' @importFrom methods new
#' @importFrom stats cor quantile sd var
#' @import ggplot2
#' @import reshape2
#' @import scales
NULL

# Evitar notas de variables globales ----
utils::globalVariables(c("Aldagaia1", "Aldagaia2", "Balioa"))

##############################################################
#' @title Atributua klasea
#' @description
#' Datuen atributu baten egitura eta bere propietateak azaltzen ditu.
#'
#' @slot izena Atributuaren izena (karaktere kate bat).
#' @slot mota Atributuaren mota: `"numeric"` edo `"categoric"`.
#' @slot balioak Atributuaren balioak (numeric, factor edo bestelakoak).
#'
#' @examples
#' a <- Atributua("adina", c(20, 30, 40, 50, 60, 70, 80))
#' a
#'
#' @export
##############################################################
setClass(
  "Atributua",
  slots = c(
    izena = "character",
    mota = "character",
    balioak = "ANY"
  ),
  prototype = list(
    izena = "atributu_ezezaguna",
    mota = "numeric",
    balioak = numeric()
  )
)

##############################################################
#' @title Atributua instantzia sortzeko funtzioa
#' @description
#' Atributu bat sortzen du, bere izena, mota eta balioekin.
#'
#' @param izena Atributuaren izena (karaktere kate bat).
#' @param balioak Atributuaren balioak (zenbakizkoak edo kategorikoak).
#' @param mota Atributuaren mota (aukerazkoa: `"numeric"` edo `"categoric"`).
#'
#' @return "Atributua" klaseko objektu bat.
#'
#' @examples
#' Atributua("adina", c(10, 20, 30))
#' @export
##############################################################
Atributua <- function(izena, balioak, mota = NULL) {
  if (is.null(mota)) {
    if (is.numeric(balioak)) {
      mota <- "numeric"
    } else if (is.logical(balioak)) {
      mota <- "categoric"
      balioak <- factor(balioak)
    } else if (is.factor(balioak) || is.character(balioak)) {
      mota <- "categoric"
      balioak <- as.factor(balioak)
    } else {
      stop("Ezin da atributu mota ezagutu (zenbakizko, logiko edo kategorikoa izan behar du)")
    }
  }
  new("Atributua", izena = izena, mota = mota, balioak = balioak)
}

##############################################################
#' @title DatuMultzoa S4 klasearen definizioa
#' @description
#' 'DatuMultzoa' klaseak atributu multzo bat eta klase-aldagai bat gordetzen ditu.
#'
#' @slot atributuak Atributuen zerrenda bat (`Atributua` objektuen lista).
#' @slot klasea Klase aldagaia (`Atributua` klaseko objektu bat).
#'
#' @seealso [Atributua] klasea, atributu indibidualak definitzeko.
#' @export
##############################################################
setClass(
  "DatuMultzoa",
  slots = c(
    atributuak = "list",
    klasea = "Atributua"
  )
)

##############################################################
#' @title DatuMultzoa instantzia sortzeko funtzioa
#' @description
#' 'DatuMultzoa' klaseko objektu bat sortzen du `data.frame` batetik.
#'
#' @param datuak Data.frame bat atributuak eta klase aldagaia jasotzen dituena.
#' @param klase_izena Klase gisa erabili beharreko zutabearen izena (aukerakoa).
#'
#' @return "DatuMultzoa" klaseko objektu bat.
#'
#' @examples
#' datuak <- data.frame(adina = c(23, 45, 34), sexua = c("M", "F", "M"))
#' DatuMultzoa(datuak, klase_izena = "sexua")
#' @export
##############################################################
DatuMultzoa <- function(datuak, klase_izena = NULL) {
  if (!is.data.frame(datuak)) stop("Datuek data.frame motakoak izan behar dute")

  atributu_lista <- lapply(names(datuak), function(col) {
    Atributua(col, datuak[[col]])
  })
  names(atributu_lista) <- names(datuak)

  if (!is.null(klase_izena)) {
    klasea <- atributu_lista[[klase_izena]]
    atributu_lista[[klase_izena]] <- NULL
  } else {
    klasea <- Atributua("Ezezaguna", factor())
  }

  new("DatuMultzoa", atributuak = atributu_lista, klasea = klasea)
}

##############################################################
#' @title DatuMultzoa objektua erakusteko metodoa
#' @description
#' `show()` metodoa 'DatuMultzoa' klaseko objektuak pantailaratzeko.
#' @param object DatuMultzoa klaseko objektua.
#' @export
##############################################################
setMethod(
  f = "show",
  signature = "DatuMultzoa",
  definition = function(object) {
    cat("DatuMultzoa objektua:\n")
    cat("  Atributuak:\n")
    for (a in object@atributuak) {
      cat("   -", a@izena, "(", a@mota, ")\n")
    }
    cat("  Klasea:", object@klasea@izena, "(", object@klasea@mota, ")\n")
  }
)
