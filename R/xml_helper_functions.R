# TransXchange import fucntions

#' Import Simple
#' ????
#' @param xml1 XML object
#' @param nm name to find
#' @noRd
import_simple <- function(xml1, nm) {
  xml2::xml_text(xml2::xml_find_all(xml1, nm))
}

#' Import Via Loop
#' Loops over children
#' @param xml1 XML object
#' @param nm name to find
#' @noRd
import_vialoop <- function(xml1, nm) {
  res <- list()
  for (i in seq(1, xml2::xml_length(xml1))) {
    chld <- xml2::xml_child(xml1, i)
    chld <- xml2::xml_text(xml2::xml_child(chld, nm))
    if (length(chld) == 0) {
      chld <- NA
    }
    res[[i]] <- chld
  }
  res <- unlist(res)
  return(res)
}


#' Import all
#' Modified version of xml2::xml_find_all combined with import_simple
#' Shoudl handel missing values at higher speeds
#' @param xml1 XML object
#' @param nm name to find
#' @noRd
import_simple_xml <- function(xml1, nm) {
  if (length(xml1) == 0) {
    return(xml1)
  }
  # return(xml_nodeset())

  nodes <- lapply(xml1, function(x) {
    res <- xml2:::xpath_search(x$node,
                               x$doc,
                               xpath = nm,
                               nsMap = xml2::xml_ns(x),
                               num_results = Inf
    )

    # Alt method see https://stackoverflow.com/questions/35103804/what-is-the-preferred-method-for-sharing-compiled-c-code-in-an-r-package-and-run
    # res <- .Call("xpath_search",
    #   x$node,
    #   x$doc,
    #   xpath = nm,
    #   nsMap = xml2::xml_ns(x),
    #   num_results = Inf,
    #   PACKAGE="xml2"
    # )

    if (length(res) == 0) {
      return(NA)
    } else if (length(res) == 1) {
      res <- xml2::xml_text(res[[1]])
      return(res)
    } else {
      stop("res is not of length 0 or 1")
    }
  })
  nodes <- unlist(nodes, recursive = FALSE)
  return(nodes)
}

#' Import When some rows are missing
#' Checks lengths of obejct against lgth
#' @param xml1 XML object
#' @param nm character name to find
#' @param lgth numeric length check
#' @noRd
import_withmissing <- function(xml1, nm, lgth) {
  xml2 <- import_simple(xml1, nm)
  ids <- xml2::xml_length(xml2::xml_children(xml1))
  ids <- ids == lgth
  ids <- cumsum(ids)
  ids[duplicated(ids)] <- NA
  xml2 <- xml2[ids]
  return(xml2)
}

#' Import When some rows are missing
#' Goes down mulitple layers and returns a value with NA for missing
#' @param xml1 XML object
#' @param nm character name to find
#' @param layers how many layers down
#' @param idvar the id variaible in the higher tree
#' @noRd
import_withmissing2 <- function(xml1, nm, layers, idvar) {
  xml_2 <- xml2::xml_find_all(xml1, nm)
  xml2_parent <- xml2::xml_parent(xml_2)
  if (layers > 1) {
    for (i in seq(2, layers)) {
      xml2_parent <- xml2::xml_parent(xml2_parent)
    }
  }
  xml2_parent_id <- xml2::xml_text(xml2::xml_find_all(xml2_parent, idvar))
  xml1_id <- xml2::xml_text(xml2::xml_find_all(xml1, idvar))

  res <- rep(NA, length(xml1_id))
  res[match(xml2_parent_id, xml1_id)] <- xml2::xml_text(xml_2)
  return(res)
}



