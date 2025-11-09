##############################################################
#' @title Atributuen filtraketa metriketan oinarrituta
#' @description
#' Funtzio hauek dataset baten atributuak filtratzen dituzte
#' aurrez kalkulatutako metrikak kontuan hartuta:
#' \itemize{
#'   \item Bariantza minimoa atributu numerikoentzat
#'   \item Entropia minimoa atributu kategorikoentzat
#'   \item AUC minimoa atributu numerikoentzat (klasea emanda)
#' }
#'
#' Metodoak `data.frame` eta `DatuMultzoa` klaseetarako definituta daude.
#'
#' @param x Dataset bat (`data.frame` edo `DatuMultzoa`)
#' @param var_min Bariantza minimoa (lehenetsia = 0)
#' @param entropy_min Entropia minimoa (lehenetsia = 0)
#' @param auc_min AUC minimoa (lehenetsia = 0)
#' @param klase Klase aldagaia edo bektore binarioa AUC kalkulatzeko (`data.frame` metodoan bakarrik erabiltzen da)
#' @param ... Argumentu gehigarriak; metodoen arteko bateragarritasunerako.
#'
#' @return Filtratutako objektu berria (`data.frame` edo `DatuMultzoa`)
#'
#' @examples
#' df <- data.frame(
#'   x1 = c(1,2,3,4,5),
#'   x2 = c(5,5,5,5,5),
#'   cat = factor(c("A","B","A","B","C")),
#'   label = c(TRUE, FALSE, TRUE, FALSE, TRUE)
#' )
#'
#' # Data.frame baten adibidea
#' filter_metrics(df, var_min = 1, entropy_min = 0.5)
#'
#' # DatuMultzoa objektu baten adibidea
#' dm <- DatuMultzoa(df, klase_izena = "label")
#' filter_metrics(dm, var_min = 1, entropy_min = 0.5)
#'
#' @seealso
#' `var_col()` — atributu numerikoen bariantza kalkulatzeko,
#' `entropy()` — atributu kategorikoen entropia kalkulatzeko,
#' `roc_analisi()` — AUC kalkulatzeko.
#'
#' @name filter_metrics
#' @aliases filter_metrics
#' @export
##############################################################

# Generikoa
setGeneric("filter_metrics", function(x, ...) standardGeneric("filter_metrics"))

##############################################################
#' @rdname filter_metrics
#' @aliases filter_metrics,data.frame-method
#' @export
setMethod("filter_metrics", "data.frame", function(x,
                                                   var_min = 0,
                                                   entropy_min = 0,
                                                   auc_min = 0,
                                                   klase = NULL,
                                                   ...) {

  # --- 1. Atributu numerikoak ---
  num_cols <- sapply(x, is.numeric)
  if (any(num_cols)) {
    vars <- var_col(x[, num_cols, drop = FALSE])
    keep_num <- names(vars)[!is.na(vars) & vars >= var_min]
  } else {
    keep_num <- character(0)
  }

  # --- 2. Atributu kategorikoak ---
  cat_cols <- sapply(x, is.factor)
  if (any(cat_cols)) {
    ents <- sapply(x[, cat_cols, drop = FALSE], entropy)
    keep_cat <- names(ents)[!is.na(ents) & ents >= entropy_min]
  } else {
    keep_cat <- character(0)
  }

  # --- 3. AUC kalkulua (klase emanda) ---
  if (!is.null(klase)) {
    aucs <- sapply(names(x)[num_cols], function(col) {
      suppressWarnings({
        res <- try(roc_analisi(data.frame(x[[col]], klase)), silent = TRUE)
        if (inherits(res, "try-error")) return(NA)
        return(res$AUC)
      })
    })
    keep_auc <- names(aucs)[!is.na(aucs) & aucs >= auc_min]
  } else {
    keep_auc <- character(0)
  }

  # --- 4. Guztiak bateratu ---
  keep <- unique(c(keep_num, keep_cat, keep_auc))
  keep <- keep[keep %in% names(x)]

  # --- 5. Emaitza ---
  if (length(keep) == 0) {
    message("Ez da atributurik iragazteko baldintzak betetzen (ninguna columna cumple los filtros).")
    return(x[, 0, drop = FALSE])
  }

  return(x[, keep, drop = FALSE])
})

##############################################################
#' @rdname filter_metrics
#' @aliases filter_metrics,DatuMultzoa-method
#' @export
setMethod("filter_metrics", "DatuMultzoa", function(x,
                                                    var_min = 0,
                                                    entropy_min = 0,
                                                    auc_min = 0,
                                                    ...) {

  # --- 1. Zenbakizko atributuak ---
  num_atrib <- Filter(function(a) a@mota == "numeric", x@atributuak)
  keep_num <- sapply(num_atrib, function(a) var(a@balioak) >= var_min)

  # --- 2. Kategoriko atributuak ---
  cat_atrib <- Filter(function(a) a@mota == "categoric", x@atributuak)
  keep_cat <- sapply(cat_atrib, function(a) entropy(a@balioak) >= entropy_min)

  # --- 3. Filtratutako atributuak ---
  new_atrib <- c(num_atrib[keep_num], cat_atrib[keep_cat])

  if (length(new_atrib) == 0) {
    stop("Ez da atributurik iragazteko baldintzak betetzen.")
  }

  # --- 4. Objektu berria ---
  new("DatuMultzoa", atributuak = new_atrib, klasea = x@klasea)
})
