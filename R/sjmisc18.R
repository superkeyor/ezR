# based on sjmisc v 1.8
# remove ' before #, except ' @importFrom
# exported appended to the end

#
mean.labelled <- function(x, trim = 0, na.rm = FALSE, missing_to_na = FALSE, ...) {
  # unclass vector for mean-call
  x <- unclass(x)
  # has any missing attributes?
  has_na <- !is.null(suppressMessages(get_na(x)))
  # warn user
  if (!missing_to_na) {
    if (has_na) {
      message("`x` has valus with missing attribute, which are not converted to NA. Use argument `missing_to_na = TRUE` to convert labelled missings to NA before computing the mean.", call. = F)
    }
  } else {
    x <- to_na(x)
  }
  # mean
  mean(x, trim = trim, na.rm = na.rm)
}

#
is.na.labelled <- function(x) {
  # unclass vector for is.na-call
  x <- unclass(x)
  if (!is.null(suppressMessages(get_na(x)))) {
    warning("`x` has self-defined missing values, which are not counted as NA. Use `to_na` to convert self-defined missing values to NA.", call. = F)
  }
  # return missings
  is.na(x)
}

#' @importFrom nlme getData getCovariateFormula
#
model.matrix.gls <- function(object, ...) {
  mm <- cbind(`(Intercept)` = 1,
              nlme::getData(object)[, all.vars(nlme::getCovariateFormula(object))])
  return(mm)
}

#' @importFrom nlme getResponse getData getCovariateFormula
#
model.frame.gls <- function(formula, ...) {
  if (all(class(formula) != "gls")) {
    stop("`formula` needs to be an object of class `gls`.", call. = F)
    return(NULL)
  } else {
    y <- nlme::getResponse(formula)
    mf <- cbind(y, nlme::getData(formula)[, all.vars(nlme::getCovariateFormula(formula))])
    colnames(mf)[1] <- get_label(y, def.value = "Response")
    return(mf)
  }
}

#' @importFrom dplyr tbl_df trunc_mat
#
print.lbl_df <- function(x, ..., n = NULL, width = NULL) {
  # get labels
  dlab <- get_label(x)
  # if x of class tbl_df?
  if (!"tbl_df" %in% class(x))
    x <- dplyr::tbl_df(x)
  # get df matrix
  dmat <- dplyr::trunc_mat(x, n = n, width = width)
  # set labels, if we have any
  if (!is.null(dlab)) {
    # iterate all columns
    for (i in 1:ncol(dmat[[1]])) {
      # replace first value of each column, which is the class description
      # with variable label
      dmat[[1]][[i]][1] <- dlab[i]
    }
  }
  # use dplyr-print method now
  print(dmat, ..., n = n, width = width)
}


#
print.sjmisc_r2 <- function(x, ...) {
  if (length(x) > 1) {
    if (identical(names(x[[2]]), "Nagelkerke")) {
      s1 <- "Cox & Snell's R-squared"
      s2 <- "Nagelkerke's R-squared"
    } else if (identical(names(x[[2]]), "adj.R2")) {
      s1 <- "R-squared"
      s2 <- "adjusted R-squared"
    } else if (identical(names(x[[2]]), "O2")) {
      s1 <- "R-squared"
      s2 <- "Omega-squared"
    } else if (identical(names(x[[2]]), "R2(tau-11)")) {
      s1 <- "R-squared (tau-00)"
      s2 <- "R-squared (tau-11)"
    } else {
      return(NULL)
    }
    cat(sprintf("%s: %.4f; %s: %.4f\n", s1, x[[1]], s2, x[[2]]))
  } else {
    if (identical(names(x[[1]]), "D")) {
      s1 <- "Tjur's D"
    } else {
      return(NULL)
    }
    cat(sprintf("%s: %.4f\n", s1, x[[1]]))
  }
}


#
print.icc.lme4 <- function(x, comp, ...) {
  # print model information
  cat(sprintf("%s\n Family: %s (%s)\nFormula: %s\n\n",
              attr(x, "model", exact = T),
              attr(x, "family", exact = T),
              attr(x, "link", exact = T),
              paste(as.character(attr(x, "formula"))[c(2, 1, 3)], collapse = " ")))
  if (!missing(comp) && !is.null(comp) && comp == "var") {
    # get parameters
    tau.00 <- attr(x, "tau.00", exact = TRUE)
    tau.01 <- attr(x, "tau.01", exact = TRUE)
    tau.11 <- attr(x, "tau.11", exact = TRUE)
    rho.01 <- attr(x, "rho.01", exact = TRUE)
    # print within-group-variance sigma^2
    tmp <- sprintf("%.3f", attr(x, "sigma_2", exact = TRUE))
    cat(sprintf("      Within-group-variance: %8s\n", tmp))
    # print between-group-variance tau0
    for (i in 1:(length(tau.00))) {
      tmp <- sprintf("%.3f", tau.00[i])
      cat(sprintf("     Between-group-variance: %8s (%s)\n",
                  tmp, names(tau.00)[i]))
    }
    # print random-slop-variance tau1
    for (i in 1:length(tau.11)) {
      tau.rs <- tau.11[i]
      # any random slope?
      if (!is_empty(tau.rs)) {
        tmp <- sprintf("%.3f", tau.rs)
        cat(sprintf("      Random-slope-variance: %8s (%s)\n",
                    tmp, names(tau.rs)))
      }
    }
    # print random-slope-covariance tau01
    for (i in 1:length(tau.01)) {
      tau.rs <- tau.01[i]
      # any random slope?
      if (!is_empty(tau.rs)) {
        tmp <- sprintf("%.3f", tau.rs)
        cat(sprintf(" Slope-Intercept-covariance: %8s (%s)\n",
                    tmp, names(tau.rs)))
      }
    }
    # print random-slope-correlation rho01
    for (i in 1:length(rho.01)) {
      rho.rs <- rho.01[i]
      # any random slope?
      if (!is_empty(rho.rs)) {
        tmp <- sprintf("%.3f", rho.rs)
        cat(sprintf("Slope-Intercept-correlation: %8s (%s)\n",
                    tmp, names(rho.rs)))
      }
    }
  } else {
    # get longest rand. effect name
    len <- max(nchar(names(x)))
    # print icc
    for (i in 1:length(x)) {
      # create info string
      infs <- sprintf("ICC (%s)", names(x[i]))
      # print info line, formatting all ICC values so they're
      # aligned properly
      cat(sprintf("%*s: %f\n",
                  len + 8,
                  infs,
                  as.vector(x[i])))
    }
  }
}


#
print.labelled <- function(x, ...) {
  # code partially taken from haven:::print.labelled
  cat("<Labelled>\n")
  xx <- unclass(x)
  attr(xx, "label") <- NULL
  attr(xx, "labels") <- NULL
  attr(xx, "is_na") <- NULL
  attr(xx, "note") <- NULL
  # print values
  print(xx)
  # print variable label
  cat("\nVariable label:\n")
  cat("  ", attr(x, "label", exact = TRUE), "\n")
  # print value  labels
  cat("\nValue labels:\n")
  print(attr(x, "labels", exact = TRUE))
  # print missing
  cat("\nMissing values:\n")
  cat("  ", format(get_na(x)), "\n")
  # do we have a note?
  note <- attr(x, "note", exact = TRUE)
  if (!is.null(note) && !is_empty(note)) {
    cat("\nNote:\n")
    cat("  ", note, "\n")
  }
  invisible()
}
# @title Add value labels to variables
# @name add_labels
#
# @description This function adds additional labels as attribute to a variable
#                or vector \code{x}, resp. to a set of variables in a
#                \code{data.frame} or \code{list}-object. Unlike \code{\link{set_labels}},
#                \code{add_labels} does not replace existing value labels, but adds
#                \code{value} to the existing value labels of \code{x}.
#
# @seealso \code{\link{set_label}} to manually set variable labels or
#            \code{\link{get_label}} to get variable labels; \code{\link{set_labels}} to
#            add value labels, replacing the existing ones.
#
# @param x Variable (vector), \code{list} of variables or a \code{data.frame}
#          where value label attributes should be added. Does not replaces former
#          value labels.
# @param value Named character vector of labels that will be added to \code{x} as
#          \code{"labels"} or \code{"value.labels"} attribute. If \code{x} is
#          a data frame, \code{value} may also be a \code{\link{list}} of
#          named character vectors. If \code{value} is a list, it must have
#          the same length as number of columns of \code{x}. If \code{value}
#          is a vector and \code{x} is a data frame, \code{value} will be applied
#          to each column of \code{x}.
#
# @return \code{x} with additional value labels.
#
# @note Existing labelled values will be replaced by new labelled values
#         in \code{value}. See 'Examples'.
#
# @examples
# data(efc)
# get_labels(efc$e42dep)
#
# x <- add_labels(efc$e42dep, c(`nothing` = 5))
# get_labels(x)
#
# x <- add_labels(efc$e42dep, c(`nothing` = 5, `zero value` = 0))
# get_labels(x, include.values = "p")
#
# # replace old values
# x <- add_labels(efc$e42dep, c(`not so dependent` = 4, `lorem ipsum` = 5))
# get_labels(x, include.values = "p")
#
# # replace values, alternative function call
# add_labels(x) <- c(`new second` = 2)
#
#
#
add_labels <- function(x, value) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- add_labels_helper(x[[i]], value)
    return(x)
  } else {
    return(add_labels_helper(x, value))
  }
}


add_labels_helper <- function(x, value) {
  # get current labels of `x`
  current.labels <- get_labels(x,
                               attr.only = T,
                               include.values = "n",
                               include.non.labelled = F)

  # if we had already labels, append new ones
  if (!is.null(current.labels)) {
    # remove double values labels
    doubles <- names(current.labels) %in% as.character(value)

    # switch value and names attribute, since get_labels
    # returns the values as names, and the value labels
    # as "vector content"
    val.switch <- as.numeric(names(current.labels))
    names(val.switch) <- as.character(current.labels)

    # update all labels
    all.labels <- c(val.switch[!doubles], value)
    # tell user
    if (any(doubles)) {
      message(sprintf("label '%s' was replaced with new value label.\n",
                      current.labels[doubles]))
    }
  } else {
    all.labels <- value
  }

  # sort labels by values
  all.labels <- all.labels[order(as.numeric(all.labels))]

  # set back labels
  x <- set_labels(x, labels = all.labels)
  return(x)
}

# @rdname add_labels
#
`add_labels<-` <- function(x, value) {
  UseMethod("add_labels<-")
}

#
`add_labels<-.default` <- function(x, value) {
  x <- add_labels(x, value)
  x
}
# @title Convert vector to labelled class
# @name as_labelled
#
# @description Converts a (labelled) vector of any class into a \code{labelled}
#                class vector, resp. adds a \code{labelled} class-attribute.
#
# @param x Variable (vector), \code{data.frame} or \code{list} of variables
#          that should be converted to \code{\link[haven]{labelled}}-class
#          objects.
# @param add.labels Logical, if \code{TRUE}, non-labelled values will be
#          labelled with the corresponding value.
# @param add.class Logical, if \code{TRUE}, \code{x} preserves its former
#          \code{class}-attribute and \code{labelled} is added as additional
#          attribute. If \code{FALSE} (default), all former \code{class}-attributes
#          will be removed and the class-attribute of \code{x} will only
#          be \code{labelled}.
# @return \code{x}, as \code{\link[haven]{labelled}}-class object, including
#           missing-flags (\code{is_na}-attribute).
#
# @examples
#
# data(efc)
# str(efc$e42dep)
#
# x <- as_labelled(efc$e42dep)
# str(x)
# summary(x)
#
# x <- as_labelled(efc$e42dep, add.class = TRUE)
# str(x)
# summary(x)
#
# a <- c(1, 2, 4)
# x <- as_labelled(a, add.class = TRUE)
# str(x)
# summary(x)
#
# data(efc)
# x <- set_labels(efc$e42dep, c(`1` = "independent", `4` = "severe dependency"))
# x1 <- as_labelled(x, add.labels = FALSE)
# x2 <- as_labelled(x, add.labels = TRUE)
#
# str(x1)
# str(x2)
#
# get_values(x1)
# get_values(x2)
#
#' @importFrom stats na.omit
#
as_labelled <- function(x, add.labels = FALSE, add.class = FALSE) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- as_labelled_helper(x[[i]], add.labels, add.class)
    return(x)
  } else {
    return(as_labelled_helper(x, add.labels, add.class))
  }
}


as_labelled_helper <- function(x, add.labels, add.class) {
  # check if we have any value label attributes
  vallabel <- get_labels(x, attr.only = T)
  # nothing?
  if (is.null(vallabel)) {
    # factor levels as labels?
    vallabel <- get_labels(x, attr.only = F)
    # still nothing?
    if (is.null(vallabel)) {
      # get unique values
      vallabel <- as.character(unique(stats::na.omit(x)))
    }
    # set value labels
    x <- suppressWarnings(set_labels(x, vallabel, force.labels = T, force.values = T))
  }
  # fill up missing attributes
  if (add.labels) x <- fill_labels(x)
  # reset missings
  x <- set_na(x, suppressMessages(get_na(x)), as.attr = T)
  # get former class attributes
  xc <- class(x)
  if (add.class)
    class(x) <- c(xc, "labelled")
  else
    class(x) <- "labelled"

  return(x)
}



# @title Create a labelled data frame
# @name lbl_df
#
# @description This method wraps a local data frame and adds a \code{lbl_df} class
#                attribute. Printing a \code{lbl_df}-data frame is comparable
#                to printing \code{\link[dplyr]{tbl_df}} objects, but the class
#                information in the output is replaced by the variable label.
#
# @param x A data frame.
#
# @return \code{x}, with \code{lbl_df} class-attribute.
#
# @examples
# data(efc)
# library(dplyr)
# mydf <- lbl_df(efc %>%
#   select(e15relat, e16sex, e17age) %>%
#   slice(1:3))
#
# mydf
#
# # or...
# mydf <- efc %>%
#   select(e15relat, e16sex, e17age) %>%
#   slice(1:3)
#
# lbl_df(mydf)
#
#
# mydf <- lbl_df(efc %>%
#   select(e15relat, e16sex, e17age) %>%
#   to_label() %>%
#   set_label(c("Relationship", "Elder's gender", "Elder's age")))
#
# mydf
#
#
lbl_df <- function(x) {
  # add class attribute, if necessary
  if (!"lbl_df" %in% class(x))
    class(x) <- c("lbl_df", class(x))
  x
}
# @title Convergence test for mixed effects models
# @name converge_ok
#
# @description This function provides an alternative convergence test for
#                \code{\link[lme4]{merMod}}-objects.
#
# @param x A \code{\link[lme4]{merMod}}-object.
# @param tolerance Indicates up to which value the convergence result is
#          accepted. The smaller \code{tolerance} is, the stricter the test
#          will be.
#
# @return Logical vector, \code{TRUE} if convergence is fine, \code{FALSE}
#           if convergence is suspicious. Additionally, the convergence
#           value is returned as return value's name.
#
# @details This function provides an alternative convergence test for
#                \code{\link[lme4]{merMod}}-objects, as discussed
#                \href{https://github.com/lme4/lme4/issues/120}{here}
#                and suggested by Ben Bolker in
#                \href{https://github.com/lme4/lme4/issues/120#issuecomment-39920269}{this comment}.
#
# @examples
# library(lme4)
# data(efc)
# # create binary response
# efc$hi_qol <- dicho(efc$quol_5)
# # prepare group variable
# efc$grp = as.factor(efc$e15relat)
# # data frame for fitted model
# mydf <- data.frame(hi_qol = as.factor(efc$hi_qol),
#                    sex = as.factor(efc$c161sex),
#                    c12hour = as.numeric(efc$c12hour),
#                    neg_c_7 = as.numeric(efc$neg_c_7),
#                    grp = efc$grp)
# # fit glmer
# fit <- glmer(hi_qol ~ sex + c12hour + neg_c_7 + (1|grp),
#              data = mydf, family = binomial("logit"))
#
# converge_ok(fit)
#
#' @importFrom Matrix solve
#
converge_ok <- function(x, tolerance = 0.001) {
  # check for package availability
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package `Matrix` needed for this function to work. Please install it.", call. = FALSE)
  }
  # is 'x' an lmer object?
  if (is_merMod(x)) {
    relgrad <- with(x@optinfo$derivs, Matrix::solve(Hessian, gradient))
    # copy logical value, TRUE if convergence is OK
    retval <- max(abs(relgrad)) < tolerance
    # copy convergence value
    names(retval) <- max(abs(relgrad))
    # return result
    return(retval)
  } else {
    warning("`x` must be a `merMod` object.", call. = F)
  }
}
# @title Copy value and variable labels to (subsetted) data frames
# @name copy_labels
#
# @description Subsetting-functions usually drop value and variable labels from
#                subsetted data frames (if the original data frame has value and variable
#                label attributes). This function copies these value and variable
#                labels back to subsetted data frames that have been subsetted, for instance,
#                with \code{\link{subset}}.
#                \cr \cr
#                In case \code{df_origin = NULL}, all possible label attributes
#                from \code{df_new} are removed.
#
# @seealso \href{http://www.strengejacke.de/sjPlot/labelleddata/}{sjPlot-manual}
#            on working with labelled data, and \code{\link{remove_all_labels}} for
#            removing label attributes from data frames.
#
# @param df_new The new, subsetted data frame.
# @param df_origin The original data frame where the subset (\code{df_new}) stems from;
#          use \code{NULL}, if value and variable labels from \code{df_new} should be removed.
# @return Returns \code{df_new} with either removed value and variable label attributes
#           (if \code{df_origin = NULL}) or with copied value and variable label
#           attributes (if \code{df_origin} was the original subsetted data frame).
#
# @note In case \code{df_origin = NULL}, all possible label attributes
#         from \code{df_new} are removed. dplyr >= 0.4.2 no longer drops
#         vector attributes; you'll only need to copy labels when using
#         dplyr up to 0.4.1.
#
# @examples
# data(efc)
# efc.sub <- subset(efc, subset = e16sex == 1, select = c(4:8))
# str(efc.sub)
#
# efc.sub <- copy_labels(efc.sub, efc)
# str(efc.sub)
#
# efc.sub <- copy_labels(efc.sub)
# str(efc.sub)
#
#
copy_labels <- function(df_new, df_origin = NULL) {
  # check if old df is NULL. if so, we remove all labels
  # from the data frame.
  if (is.null(df_origin)) {
    # tell user
    message("Removing all variable and value labels from data frame.")
    # remove all labels
    df_new <- remove_all_labels(df_new)
  } else {
    # check params
    if (is.data.frame(df_new) && is.data.frame(df_origin)) {
      # retrieve variables of subsetted data frame
      cn <- colnames(df_new)
      # check for valid colnames, i.e. if all column
      # names really match the original column names.
      if (sum(cn %in% colnames(df_origin) == F) > 0) {
        # if not, return only matching colnames
        cn <- cn[cn %in% colnames(df_origin)]
      }
      # get var-labels of original data frame, and select only those
      # labels from variables that appear in the new (subsetted) data frame
      df_new <- set_label(df_new, get_label(df_origin[, cn]))
      # same for value labels
      df_new <- set_labels(df_new, get_labels(df_origin[, cn],
                                              attr.only = TRUE,
                                              include.values = NULL,
                                              include.non.labelled = FALSE))
    } else {
      warning("both `df_origin` and `df_new` must be of class `data.frame`.", call. = F)
    }
  }
  return(df_new)
}
# @title Cramer's V for a contingency table
# @name cramer
# @description Compute Cramer's V for a table with more than 2x2 fields.
#
# @seealso \code{\link{phi}}
#
# @inheritParams phi
#
# @return The table's Cramer's V.
#
# @examples
# tab <- table(sample(1:2, 30, TRUE), sample(1:3, 30, TRUE))
# cramer(tab)
#
#' @importFrom stats ftable
#
cramer <- function(tab) {
  if (all(class(tab) != "ftable")) tab <- stats::ftable(tab)
  phi_val <- phi(tab)
  cramer <- sqrt(phi_val ^ 2 / min(dim(tab) - 1))
  return(cramer)
}
# @title Cronbach's Alpha for a matrix or data frame
# @name cronb
# @description This function calculates the Cronbach's alpha value
#                of a data frame or matrix.
#
# @seealso \code{\link{reliab_test}}
#
# @param data \code{data.frame} or matrix with more than 2 columns.
# @return The Cronbach's alpha value for \code{data}.
#
# @note See 'Examples' from \code{\link[sjPlot]{sjp.pca}} and \code{\link[sjPlot]{sjt.pca}}.
#
#' @importFrom stats na.omit var
#
cronb <- function(data) {
  .data <- stats::na.omit(data)
  if (is.null(ncol(.data)) || ncol(.data) < 2) {
    warning("Too less columns in `data` to compute Cronbach's Alpha.", call. = F)
    return(NULL)
  }
  return(dim(.data)[2] / (dim(.data)[2] - 1) * (1 - sum(apply(.data, 2, var)) / stats::var(rowSums(.data))))
}
# @title Coefficient of Variation
# @name cv
# @description Compute coefficient of variation for single variables
#                (standard deviation divided by mean) or for fitted
#                linear (mixed effects) models (root mean squared error
#                (RMSE) divided by mean of dependent variable).
#
# @param x (Numeric) vector or a fitted linear model of class
#          \code{\link{lm}}, \code{\link[lme4]{merMod}} (\pkg{lme4}) or
#          \code{\link[nlme]{lme}} (\pkg{nlme}).
# @param ... More fitted model objects, to compute multiple coefficients of
#              variation at once.
# @return The coefficient of variation of \code{x}.
#
# @details The advantage of the cv is that it is unitless. This allows
#            coefficient of variation to be compared to each other in ways
#            that other measures, like standard deviations or root mean
#            squared residuals, cannot be
#            (\href{http://www.ats.ucla.edu/stat/mult_pkg/faq/general/coefficient_of_variation.htm}{source: UCLA-FAQ}).
#
# @seealso \code{\link{rmse}}
#
# @references \href{http://www.ats.ucla.edu/stat/mult_pkg/faq/general/coefficient_of_variation.htm}{UCLA-FAQ: What is the coefficient of variation?}
#
# @examples
# data(efc)
# cv(efc$e17age)
#
# fit <- lm(neg_c_7 ~ e42dep, data = efc)
# cv(fit)
#
# library(lme4)
# fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
# cv(fit)
#
# library(nlme)
# fit <- lme(distance ~ age, data = Orthodont)
# cv(fit)
#
#' @importFrom stats sd
#
cv <- function(x, ...) {
  # return value
  cv_ <- cv_helper(x)
  # check if we have multiple parameters
  if (nargs() > 1) {
    # get input list
    params_ <- list(...)
    cv_ <- c(cv_, sapply(params_, cv_helper))
  }
  return(cv_)
}


cv_helper <- function(x) {
  # check if we have a fitted linear model
  if (class(x) == "lm" || any(class(x) == "lmerMod") ||
      any(class(x) == "lme") || any(class(x) == "merModLmerTest")) {
    if (class(x) == "lm") {
      # dependent variable in lm
      dv <- x$model[[1]]
    } else if (any(class(x) == "lmerMod") || any(class(x) == "merModLmerTest")) {
      # check for package availability
      if (!requireNamespace("lme4", quietly = TRUE)) {
        stop("Package `lme4` needed for this function to work. Please install it.", call. = FALSE)
      }
      # dependent variable in lmerMod
      dv <- lme4::getME(x, "y")
    } else if (any(class(x) == "lme")) {
      # check for package availability
      if (!requireNamespace("nlme", quietly = TRUE)) {
        stop("Package `nlme` needed for this function to work. Please install it.", call. = FALSE)
      }
      # dependent variable in lme
      dv <- unname(nlme::getResponse(x))
    }
    # compute mean of dependent variable
    mw <- mean(dv, na.rm = TRUE)
    # check if mean is zero?
    if (mw != 0) {
      # cv = root mean squared error (RMSE) divided by mean of dep. var.
      return(rmse(x) / mw)
    } else {
      warning("Mean of dependent variable is zero. Cannot compute model's coefficient of variation.", call. = F)
    }
  } else {
    # compute mean of variable
    mw <- mean(x, na.rm = TRUE)
    # check if mean is zero?
    if (mw != 0) {
      #  we assume a simple vector
      return(stats::sd(x, na.rm = TRUE) / mw)
    } else {
      warning("Mean of `x` is zero. Cannot compute coefficient of variation.", call. = F)
    }
  }
  return(NULL)
}

# @title Dichotomize variables
# @name dicho
#
# @description Dichotomizes variables into dummy variables (0/1). Dichotomization is
#                either done by median, mean or a specific value (see \code{dich.by}).
#                Either single vectors, a complete data frame or a list of
#                variables can be dichotomized.
#
# @param x Variable (vector), \code{data.frame} or \code{list} of variables
#          that should be dichotomized
# @param dich.by Indicates the split criterion where a variable is dichotomized.
#          Must be one of the following values (may be abbreviated):
#          \describe{
#            \item{\code{"median"}}{by default, \code{x} is split into two groups at the median.}
#            \item{\code{"mean"}}{splits \code{x} into two groups at the mean of \code{x}.}
#            \item{\code{"value"}}{splits \code{x} into two groups at a specific value (see \code{dich.val}).}
#            }
# @param dich.val Numeric, indicates a value where \code{x} is dichotomized when \code{dich.by = "value"}.
#          \strong{Note that \code{dich.val} is inclusive}, i.e. \code{dich.val = 10} will split \code{x}
#          into one group with values from lowest to 10 and another group with values greater
#          than 10.
# @param as.num Logical, if \code{TRUE}, return value will be numeric, not a factor.
# @param var.label Optional string, to set variable label attribute for the
#          dichotomized variable (see \code{\link{set_label}}). If \code{NULL}
#          (default), variable label attribute of \code{x} will be used (if present).
# @param val.labels Optional character vector (of length two), to set value label
#          attributes of dichotomized variable (see \code{\link{set_labels}}).
#          If \code{NULL} (default), no value labels will be set.
# @return A dichotomized factor (or numeric, if \code{as.num = TRUE}) variable (0/1-coded),
#           respectively a data frame or list of dichotomized factor (or numeric) variables.
#
# @note Variable label attributes (see, for instance, \code{\link{set_label}}) are preserved
#         (unless changes via \code{var.label}-argument).
#
# @examples
# data(efc)
# summary(efc$c12hour)
# table(dicho(efc$c12hour))
# table(dicho(efc$c12hour, "mean"))
# table(dicho(efc$c12hour, "value", 30))
#
# # sample data frame, values from 1-4
# head(efc[, 6:10])
# # dichtomized values (1 to 2 = 0, 3 to 4 = 1)
# head(dicho(efc[, 6:10], "v", 2))
#
# # dichtomize several variables in a list
# dummy <- list(efc$c12hour, efc$e17age, efc$c160age)
# dicho(dummy)
#
# # dichotomize and set labels. requires package
# # sjPlot to test
# \dontrun{
# library(sjPlot)
# sjp.frq(dicho(efc$e42dep, var.label = "Dependency (dichotomized)",
#               val.labels = c("lower", "higher")))}
#
#
dicho <- function(x,
                  dich.by = c("median", "mean", "value"),
                  dich.val = -1,
                  as.num = FALSE,
                  var.label = NULL,
                  val.labels = NULL) {
  # check abbreviations
  dich.by <- match.arg(dich.by)

  # check for correct dichotome types
  if (dich.by != "median" && dich.by != "mean" && dich.by != "value") {
    stop("argument `dich.by` must either be `median`, `mean` or `value`." , call. = FALSE)
  }
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)

    # dichotomize all
    for (i in 1:nvars) {
      x[[i]] <- dicho_helper(x[[i]], dich.by, dich.val, as.num, var.label, val.labels)
    }
    return(x)
  } else {
    return(dicho_helper(x, dich.by, dich.val, as.num, var.label, val.labels))
  }
}


#' @importFrom stats median
dicho_helper <- function(x, dich.by, dich.val, as.num, var.label, val.labels) {
  # do we have labels? if not, try to
  # automatically get variable labels
  if (is.null(var.label))
    varlab <- get_label(x)
  else
    varlab <- var.label

  # check if factor. factors need conversion
  # to numeric before dichtomizing
  if (is.factor(x)) {
    # non-numeric-factor cannot be converted
    if (is_num_fac(x)) {
      # try to convert to numeric
      x <- as.numeric(as.character(x))
    } else {
      # convert non-numeric factor to numeric
      # factor levels are replaced by numeric values
      x <- to_value(x, keep.labels = FALSE)
      message("Trying to dichotomize non-numeric factor.")
    }
  }
  # split at median
  if (dich.by == "median") {
    x <- ifelse(x <= stats::median(x, na.rm = T), 0, 1)
    # split at mean
  } else if (dich.by == "mean") {
    x <- ifelse(x <= mean(x, na.rm = T), 0, 1)
    # split at specific value
  } else {
    x <- ifelse(x <= dich.val, 0, 1)
  }
  if (!as.num) x <- as.factor(x)
  # set back variable labels
  if (!is.null(varlab)) x <- set_label(x, varlab)
  # set value labels
  if (!is.null(val.labels)) x <- set_labels(x, val.labels)
  return(x)
}
# @title Drop labels of zero-count values
# @name drop_labels
#
# @description This function drops all value labels for those values that have
#                no cases (frequencies) in a vector.
#
# @param x Variable (vector), \code{data.frame} or \code{list} of variables
#          with partially added value labels (see \code{\link[haven]{labelled}}).
#
# @return \code{x}, where value labels for non-existing values are removed.
#
# @seealso \code{\link{zap_labels}} and \code{\link{zap_unlabelled}} to convert
#            (non-)labelled values into \code{NA}; \code{\link{fill_labels}} to
#            add labels to existing, but not yet labelled values. The latter
#            function is the counterpart to \code{drop_labels}.
#
# @examples
# rp <- rec_pattern(1, 100)
# rp
#
# # sample data
# data(efc)
# # recode carers age into groups of width 5
# x <- rec(efc$c160age, rp$pattern)
# # add value labels to new vector
# set_labels(x) <- rp$labels
# # watch result. due to recode-pattern, we have age groups with
# # no observations (zero-counts)
# frq(as_labelled(x))
#
# # now, let's drop zero's
# frq(as_labelled(drop_labels(x)))
#
#
drop_labels <- function(x) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # na all
    for (i in 1:nvars) x[[i]] <- drop_labels_helper(x[[i]])
    return(x)
  } else {
    return(drop_labels_helper(x))
  }
}

drop_labels_helper <- function(x) {
  # first, get frequency table
  mydat <- get_frq(x, coerce = TRUE)
  # get all valid values, that have counts
  valid.values <- !is.na(mydat$value) & mydat$count > 0
  # create labels
  value.labels <- as.character(mydat$label[valid.values])
  # get value names
  values <- mydat$value[valid.values]
  # name vector
  names(value.labels) <- values
  # set labels
  set_labels(x, labels = value.labels)
}
# @docType data
# @title Sample dataset from the EUROFAMCARE project
# @name efc
# @keywords data
#
# @description A SPSS sample data set, read with the \code{\link[haven]{read_spss}}
#                function and "converted" with \code{\link{unlabel}}.
#
# @examples
# # Attach EFC-data
# data(efc)
#
# # Show structure
# str(efc)
#
# # show first rows
# head(efc)
#
# # show variables
# \dontrun{
# library(sjPlot)
# view_df(efc)
#
# # show variable labels
# get_label(efc)
#
# # plot efc-data frame summary
# sjt.df(efc, alternateRowColor = TRUE)}
#
NULL

# @title Eta-squared of fitted anova
# @name eta_sq
# @description Returns the eta-squared value for one-way-anovas.
#
# @param ... Fitted one-way-anova model or a dependent and grouping variable (see 'Examples').
# @return The eta-squared value.
#
# @note Interpret eta-squared like r-squared or R-squared; a rule of thumb (Cohen):
#         \itemize{
#          \item .02 ~ small
#          \item .13 ~ medium
#          \item .26 ~ large
#         }
#
# @references \itemize{
#               \item \href{http://stats.stackexchange.com/questions/78808/}{How to compute eta-sq in ANOVA by hand?}
#               \item \href{http://stats.stackexchange.com/questions/15958/}{How to interpret and report eta squared?}
#               \item \href{http://en.wikiversity.org/wiki/Eta-squared}{Wikipedia: Eta-squared}
#               \item Levine TR, Hullett CR (2002): Eta Squared, Partial Eta Squared, and Misreporting of Effect Size in Communication Research (\href{https://www.msu.edu/~levinet/eta\%20squared\%20hcr.pdf}{pdf})
#             }
#
# @examples
# # load sample data
# data(efc)
#
# # fit linear model
# fit <- aov(c12hour ~ as.factor(e42dep), data = efc)
#
# # print eta sqaured
# eta_sq(fit)
#
# # grouping variable will be converted to factor autoamtically
# eta_sq(efc$c12hour, efc$e42dep)
#
#' @importFrom stats aov summary.lm
#
eta_sq <- function(...) {
  # retrieve list of parameters
  input_list <- list(...)

  # check if fitted anova
  if (length(input_list) == 1 && any(class(input_list[[1]]) == "aov")) {
    # retrieve model
    fit <- input_list[[1]]
  } else if (length(input_list) == 2) {
    # retrieve variables
    depVar <- input_list[[1]]
    grpVar <- input_list[[2]]
    # convert to factor
    if (!is.factor(grpVar)) grpVar <- as.factor(grpVar)
    # fit anova
    fit <- stats::aov(depVar ~ grpVar)
  }
  # return eta squared
  return(stats::summary.lm(fit)$r.squared)
  # return (1 - var(fit$residuals, na.rm = T) / var(fit$model[,1], na.rm = T))
}
# @title Add missing value labels to partially labelled vector
# @name fill_labels
#
# @description This function adds value labels to a partially labelled vector,
#                i.e. if not all values are labelled, non-labelled values
#                get labels.
#
# @param x Variable (vector), \code{data.frame} or \code{list} of variables
#          with partially added value labels (see \code{\link[haven]{labelled}}).
#
# @return \code{x}, where labels for non-labelled values are added.
#
# @seealso \code{\link{drop_labels}} is the counterpart to \code{fill_labels}
#            and drops labels from zero-count (non-existing) values.
#
# @examples
# # create labelled integer, with missing flag
# x <- labelled(c(1, 2, 1, 3, 4, 1, 5),
#               c(Good = 1, Bad = 5))
# get_labels(x)
# get_labels(x, include.non.labelled = TRUE)
#
# fill_labels(x)
# get_labels(fill_labels(x))
#
# # create partially labelled vector with missings
# x <- labelled(c(1, 2, 1, 3, 4, 1, 5),
#               c(Male = 1, Female = 2, Refused = 5),
#               c(FALSE, FALSE, TRUE))
# x
# fill_labels(x)
# get_labels(fill_labels(x))
#
# # get summary
# x <- labelled(c(1, 2, 1, 3, 4, 1, NA, 5),
#               c(Male = 1, Female = 2, Refused = 5),
#               c(FALSE, FALSE, TRUE))
# frq(x)
#
#
fill_labels <- function(x) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # na all
    for (i in 1:nvars) x[[i]] <- fill_labels_helper(x[[i]])
    return(x)
  } else {
    return(fill_labels_helper(x))
  }
}

fill_labels_helper <- function(x) {
  # get current labels
  current.values <- get_labels(x, attr.only = T, include.non.labelled = F)
  # get all labels, including non-labelled values
  all.values <- get_labels(x,
                           attr.only = T,
                           include.values = "n",
                           include.non.labelled = T)
  # have any values?
  if (!is.null(all.values)) {
    # get missing values
    missings <- get_na_flags(x)
    # create new missing vector with same length as
    # all present values
    all.missings <- rep(FALSE, length(all.values))
    # "insert" former missings into new missing vector
    if (!is.null(missings)) all.missings[match(current.values, all.values)] <- missings
    # set back all labels, if amount of all labels differ
    # from the "current" values
    if (length(all.values) > length(current.values)) {
      # first, we need to switch name attribute and values
      all.val.switch <- as.numeric(names(all.values))
      names(all.val.switch) <- as.character(all.values)
      # then set labels
      x <- set_labels(x, all.val.switch, force.labels = T, force.values = T)
    }
    # set back missing information
    x <- set_na(x, all.missings, as.attr = T)
  }
  return(x)
}
# @title Summary of labelled vectors
# @name frq
# @description This function prints a summary, including frequency table,
#                of labelled vectors. Unlike \code{\link{summary}}, the
#                \code{frq} method also prints label and missing attributes.
#
# @param x A labelled vector.
# @param print.frq Optional logical, if \code{TRUE} (default), frequency
#          table will be printed to the console.
#
# @return A data frame with the summary information of \code{x}.
#
# @examples
# # create labelled factor, with missing flag
# x <- labelled(c("M", "M", "F", "X", "N/A"),
#               c(Male = "M", Female = "F",
#                 Refused = "X", "Not applicable" = "N/A"),
#               c(FALSE, FALSE, TRUE, TRUE))
# frq(x)
#
# # create labelled numeric vector, with missing flag
# x <- labelled(c(1, 2, 1, 3, 4, 1, NA, 5),
#               c(Male = 1, Female = 2, Refused = 5),
#               c(FALSE, FALSE, TRUE))
# frq(x)
#
#' @importFrom stats quantile median na.omit
#
frq <- function(x, print.frq = TRUE) {
  # check for labelled class
  if (!is_labelled(x)) {
    stop("`x` must be of class `labelled`.", call. = F)
  }
  # copy vector
  object <- x
  # add non-labelled value labels, if we have less
  # labels than values
  x <- fill_labels(x)
  # get value labels
  labels <- attr(x, "labels", exact = T)
  # when we have character vectors, simply do table
  if (is.character(object)) {
    # do we have a labelled vector?
    if (is.null(labels)) {
      warning("could not print frequencies. `x` has no `labels` attribute.", call. = F)
      return(NULL)
    }
    # get values
    values <- unname(labels)
    # prepare freq vector for values
    frq <- rep(0, length(values))
    # get freq of character vector
    ft <- table(object)
    # valid values, i.e. values with counts
    vvvv <- match(names(ft), values)
    # copy valid values
    frq[vvvv] <- as.vector(ft)
    # create data frame as return value
    lab_df <- data.frame(value = values,
                         label = names(labels),
                         count = frq,
                         is_na = attr(x, "is_na"))
    # check if results should be printed
    if (print.frq) {
      print(table(x))
      cat("\n")
      print(lab_df, row.names = FALSE)
    }
    # return
    invisible(lab_df)
  } else {
    # get value without missings
    no_mis <- unclass(stats::na.omit(as.vector(to_na(x))))

    # do we have character vector? if yes, coerce to numeric
    if (is.character(no_mis)) {
      no_mis <- as.numeric(no_mis)
    }

    # create named vector with all necessray summary
    # information, equal to base summary function
    summary_line <- data.frame(round(min(no_mis), 3),
                               round(stats::quantile(no_mis)[2], 3),
                               round(stats::median(no_mis), 3),
                               round(mean(no_mis), 3),
                               round(stats::quantile(no_mis)[4], 3),
                               round(max(no_mis), 3))
    # set column names
    colnames(summary_line) <- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")

    # prepare and print summary
    if (print.frq) {
      cat("\nSummary:\n")
      # output
      print(summary_line, row.names = FALSE)
    }

    # do we have any labels? continuous variables
    # usually don't have label attributes after reading
    # from SPSS
    if (!is.null(labels)) {
      if (print.frq) cat("\n")

      # get all possible values as vector. We may have some labelled
      # values that have no counts in the data. in such cases, we get
      # less values from the table than excpected. Here we set up a
      # vector with all values, and "match" the actual values
      len <- length(labels) + 1
      f.ind <- as.numeric(names(table(x, exclude = NULL)))
      f.ind <- replace_na(f.ind, len)
      # frequencies, including real missings
      fdat <- data.frame(index = c(as.numeric(unname(labels)), len),
                         frq = 0,
                         raw = 0,
                         valid = 0)
      fdat$frq[match(f.ind, fdat$index)] <- as.vector(table(x, exclude = NULL))
      # raw percentage, including real missings
      fdat$raw[match(f.ind, fdat$index)] <- as.vector(prop.table(table(x, exclude = NULL)))
      # valid percentage, excluding real and
      # labelled missings
      vp <- as.vector(prop.table(table(stats::na.omit(as.vector(to_na(x))))))
      fdat$valid[match(f.ind[1:length(vp)], fdat$index)] <-
        as.vector(prop.table(table(stats::na.omit(as.vector(to_na(x))))))
      fdat$valid[length(fdat$valid)] <- NA
      # create df
      lab_df <- data.frame(value = c(unname(labels), NA),
                           label = c(names(labels), "NA"),
                           count = fdat$frq,
                           raw.prc = round(100 * fdat$raw, 2),
                           valid.prc = round(100 * fdat$valid, 2),
                           cum.prc = round(100 * cumsum(fdat$valid), 2),
                           is_na = c(attr(x, "is_na"), NA))
      # print table
      if (print.frq) print(lab_df, row.names = FALSE)
      invisible(lab_df)
    }
  }
}



# @title Get summary of labelled vectors
# @name get_frq
# @description This function returns a summary, including frequency table,
#                of labelled vectors, as data frame. Unlike \code{\link{summary}}, the
#                \code{frq} method also prints label and missing attributes.
#
# @param x A labelled vector.
# @param coerce Logical, if \code{TRUE}, vectors will be coerced to \code{labelled}
#          class if necessary.
#
# @return A data frame with the summary information of \code{x}.
#
# @examples
# # create labelled factor, with missing flag
# x <- labelled(c("M", "M", "F", "X", "N/A"),
#               c(Male = "M", Female = "F",
#                 Refused = "X", "Not applicable" = "N/A"),
#               c(FALSE, FALSE, TRUE, TRUE))
#
# get_frq(x)
#
#' @importFrom stats quantile median na.omit
#
get_frq <- function(x, coerce = TRUE) {
  if (!is_labelled(x) && TRUE == coerce)
    x <- as_labelled(x, add.class = T)
  .dat <- frq(x, print.frq = FALSE)
  .dat
}
# @title Retrieve variable label(s) of labelled data
# @name get_label
#
# @description This function retrieves the value labels of labelled data, which
#                was created with the \pkg{labelled} or \pkg{haven} package, or
#                imported from SPSS, SAS or STATA files (via \code{\link{read_spss}},
#                \code{\link{read_sas}} or \code{\link{read_stata}}) and
#                \itemize{
#                  \item if \code{x} is a data frame or a list of variables, returns the all variable labels as names character vector of length \code{ncol(x)}.
#                  \item or, if \code{x} is a vector, returns the variable label as string.
#                  }
#
# @seealso See package vignettes or \href{http://www.strengejacke.de/sjPlot/}{online documentation}
#            for more details; \code{\link{set_label}} to manually set variable labels or \code{\link{get_labels}}
#            to get value labels.

# @param x \code{data.frame} with variables that have label attributes (e.g.
#          from an imported SPSS, SAS or STATA data set, via \code{\link{read_spss}},
#          \code{\link{read_sas}} or \code{\link{read_stata}}); a variable
#          (vector) with variable label attribute; or a \code{list} of variables
#          with variable label attributes. See 'Examples'.
# @param def.value Optional, a character string which will be returned as label
#          if \code{x} has no label attribute. By default, \code{NULL} is returned.
#
# @return A named char vector with all variable labels from the data frame or list;
#           or a simple char vector (of length 1) with the variable label, if \code{x} is a variable.
#           If \code{x} is a single vector and has no label attribute, the value
#           of \code{def.value} will be returned (which is by default \code{NULL}).
#
# @details See 'Details' in \code{\link{get_labels}}.
#
# @note See 'Note' in \code{\link{get_labels}}.
#
# @examples
# # import SPSS data set
# # mydat <- read_spss("my_spss_data.sav", enc="UTF-8")
#
# # retrieve variable labels
# # mydat.var <- get_label(mydat)
#
# # retrieve value labels
# # mydat.val <- get_labels(mydat)
#
# data(efc)
#
# # get variable lable
# get_label(efc$e42dep)
#
# # alternative way
# get_label(efc)["e42dep"]
#
# # simple barplot
# barplot(table(efc$e42dep))
# # get value labels to annotate barplot
# barplot(table(efc$e42dep),
#         names.arg = get_labels(efc$e42dep),
#         main = get_label(efc$e42dep))
#
# # get labels from multiple variables
# get_label(list(efc$e42dep, efc$e16sex, efc$e15relat))
#
#
get_label <- function(x, def.value = NULL) {
  # auto-detect variable label attribute
  attr.string <- getVarLabelAttribute(x)
  # do we have a df?
  if (is.data.frame(x) || is.matrix(x)) {
    # if yes, check if we have attached label table
    # from foreign import
    labels <- attr(x, "variable.labels", exact = T)
    # if not, get labels from each single vector
    if (is.null(labels) && !is.null(attr.string)) {
      # return value
      all.labels <- c()
      # iterate df
      for (i in 1:ncol(x)) {
        # get label
        label <- attr(x[[i]], attr.string, exact = T)
        # any label?
        if (!is.null(label)) {
          # name label
          names(label) <- colnames(x)[i]
          # append to return result
          all.labels <- c(all.labels, label)
        } else {
          all.labels <- c(all.labels, "")
        }
      }
      return(all.labels)
    } else {
      # return(attr(x, "variable.labels", exact = T))
      # jerry: maybe variable.labels has variables that have already been deleted in x
      # so only return existing variables
      # or maybe added variables
      mylbls = attr(x, "variable.labels", exact = T)
      if (!is.null(mylbls)) {
          mylbls = mylbls[colnames(x)]
          names(mylbls) = colnames(x)       # if added variables
          mylbls[which(is.na(mylbls))] = '' # if added variables  NA --> ''
      }
      return(mylbls)
    }
  } else if (is.list(x)) {
    # nothing found? then leave...
    if (is.null(attr.string)) return(NULL)
    # return attribute of all variables
    return(unlist(lapply(x, attr, attr.string, exact = T)))
  } else {
    # nothing found? then leave...
    if (is.null(attr.string)) return(def.value)
    # else return attribute
    retat <- attr(x, attr.string, exact = T)
    # still NULL? than use default return value
    if (is.null(retat)) retat <- def.value
    return(retat)
  }
}
# @title Retrieve value labels of labelled data
# @name get_labels
#
# @description This function retrieves the value labels of labelled data, which
#                was created with the \pkg{labelled} or \pkg{haven} package, or
#                imported from SPSS, SAS or STATA files (via \code{\link{read_spss}},
#                \code{\link{read_sas}} or \code{\link{read_stata}}) and
#                \itemize{
#                  \item if \code{x} is a data frame or list of variables, returns all variables' value labels as \code{\link{list}}
#                  \item or, if \code{x} is a vector, returns the value labels as string.
#                  }
#
# @seealso See package vignettes or \href{http://www.strengejacke.de/sjPlot/}{online documentation}
#            for more details; \code{\link{set_labels}} to manually set value labels, \code{\link{get_label}}
#            to get variable labels and \code{\link{get_values}} to retrieve value label associated values.
#
# @param x \code{data.frame} with variables that have value label attributes (e.g.
#          from an imported SPSS, SAS or STATA data set, via \code{\link{read_spss}},
#          \code{\link{read_sas}} or \code{\link{read_stata}}); a variable
#          (vector) with value label attributes; or a \code{list} of variables
#          with values label attributes. If \code{x} has no label attributes,
#          factor levels are returned. See 'Examples'.
# @param include.values String, indicating whether the values associated with the
#          value labels are returned as well. If \code{include.values = "as.name"}
#          (or \code{include.values = "n"}), values are set as \code{\link{names}}
#          attribute of the returned object. If \code{include.values = "as.prefix"}
#          (or \code{include.values = "p"}), values are included as prefix
#          to each label. See 'Examples'.
#          'as.name', 'n'
#                         0      1
#                        "BOY" "GIRL"
#          'as.prefix', 'p'
#                        "[0] BOY"  "[1] GIRL"
# @param attr.only Logical, if \code{TRUE}, labels are only searched for
#          in the the vector's \code{\link{attributes}}; else, if \code{x} has no
#          label attributes, factor levels or string values are returned. See
#          'Examples'.
# @param include.non.labelled Logical, if \code{TRUE}, values without labels will
#          also be included in the returned labels.
# @return Either a list with all value labels from all variables if \code{x}
#           is a \code{data.frame} or \code{list}; a string with the value
#           labels, if \code{x} is a variable;
#           or \code{NULL} if no value label attribute was found.
#
# @details This package can add (and read) value and variable labels either in \pkg{foreign}
#            package style (attributes are named \emph{value.labels} and \emph{variable.label})
#            or in \pkg{haven} package style (attributes are named \emph{labels} and
#            \emph{label}). By default, the \pkg{haven} package style is used.
#            \cr \cr
#            Working with labelled data is a key element of the \pkg{sjPlot} package,
#            which accesses these attributes to automatically read label attributes
#            for labelling axis categories and titles or table rows and columns.
#            \cr \cr
#            When working with labelled data, you can, e.g., use
#            \code{\link{get_label}} or \code{\link{get_labels}}
#            to get a vector of value and variable labels, which can then be
#            used with other functions like \code{\link{barplot}} etc.
#            See 'Examples'.
#            \cr \cr
#            Furthermore, value and variable labels are used when saving data, e.g. to SPSS
#            (see \code{\link{write_spss}}), which means that the written SPSS file
#            contains proper labels for each variable.
#            \cr \cr
#            You can set a default label style (i.e. the names of the label
#            attributes, see above) via \code{options(value_labels = "haven")}
#            or \code{options(value_labels = "foreign")}.
#
# @note This function works with vectors that have value and variable
#        label attributes (as provided, for instance, by \code{\link[haven]{labelled}}
#        objects). Adding label attributes is automatically done when importing data sets
#        with the \code{\link{read_spss}}, \code{\link{read_sas}} or \code{\link{read_stata}}
#        functions. Labels can also manually be added using the \code{\link{set_labels}}
#        and \code{\link{set_label}} functions. If vectors \strong{do not} have
#        label attributes, either factor-\code{\link{levels}} or the numeric values
#        of the vector are returned as labels.
#        \cr \cr
#        Most functions of the \pkg{sjPlot} package access value and variable label
#        attributes to automatically detect labels in order to set them as axis,
#        legend or title labels in plots (\code{sjp.}-functions) respectively as
#        column or row headers in table outputs (\code{sjt.}-functions). See
#        \href{http://www.strengejacke.de/sjPlot/datainit/}{this} and
#        \href{http://www.strengejacke.de/sjPlot/labelleddata/}{this}
#        online manuals for more details.
#
# @examples
# # import SPSS data set
# # mydat <- read_spss("my_spss_data.sav")
#
# # retrieve variable labels
# # mydat.var <- get_label(mydat)
#
# # retrieve value labels
# # mydat.val <- get_labels(mydat)
#
# data(efc)
# get_labels(efc$e42dep)
#
# # simple barplot
# barplot(table(efc$e42dep))
# # get value labels to annotate barplot
# barplot(table(efc$e42dep),
#         names.arg = get_labels(efc$e42dep),
#         main = get_label(efc$e42dep))
#
# # include associated values
# get_labels(efc$e42dep, include.values = "as.name")
#
# # include associated values
# get_labels(efc$e42dep, include.values = "as.prefix")
#
# # get labels from multiple variables
# get_labels(list(efc$e42dep, efc$e16sex, efc$e15relat))
#
#
# # create a dummy factor
# f1 <- factor(c("hi", "low", "mid"))
# # search for label attributes only
# get_labels(f1, attr.only = TRUE)    # NULL
# # search for factor levels as well
# get_labels(f1)                      # "hi"  "low" "mid"
#
# # same for character vectors
# c1 <- c("higher", "lower", "mid")
# # search for label attributes only
# get_labels(c1, attr.only = TRUE)
# # search for string values as well
# get_labels(c1)
#
#
# # create vector
# x <- c(1, 2, 3, 2, 4, NA)
# # add less labels than values
# x <- set_labels(x, c("yes", "maybe", "no"), force.values = FALSE)
# # get labels for labelled values only
# get_labels(x)
# # get labels for all values
# get_labels(x, include.non.labelled = TRUE)
#
#
#
get_labels <- function(x,
                       attr.only = FALSE,
                       include.values = NULL,
                       include.non.labelled = FALSE) {
  if (is.data.frame(x) || is.matrix(x) || is.list(x)) {
    a <- lapply(x, FUN = get_labels_helper,
                attr.only,
                include.values,
                include.non.labelled)
  } else {
    a <- get_labels_helper(x,
                           attr.only,
                           include.values,
                           include.non.labelled)
  }
  return(a)
}


# Retrieve value labels of a data frame or variable
# See 'get_labels'
get_labels_helper <- function(x, attr.only, include.values, include.non.labelled) {
  labels <- NULL
  # haven or sjPlot?
  attr.string <- getValLabelAttribute(x)
  # nothing found? then check for factor levels
  if (is.null(attr.string)) {
    # does user want to look everywhere?
    if (!attr.only) {
      # get levels of vector
      lv <- levels(x)
      # do we have any levels?
      if (!is.null(lv)) {
        labels <- lv
      } else if (is.character(x)) {
        # finally, if we even don't have values, check for
        # character elements
        labels <- unique(x)
      }
    }
  } else {
    # retrieve named labels
    lab <- attr(x, attr.string, exact = T)
    # check if we have anything
    if (!is.null(lab)) {
      # retrieve values associated with labels
      if (is.character(x) || (is.factor(x) && !is_num_fac(x)))
        values <- unname(lab)
      else
        values <- as.numeric(unname(lab))
      # retrieve label values in correct order
      labels <- names(lab)
      # do we want to include non-labelled values as well?
      if (include.non.labelled) {
        # get values of variable
        valid.vals <- sort(unique(stats::na.omit(as.vector(x))))
        # check if we have different amount values than labels
        # or, if we have same amount of values and labels, whether
        # values and labels match or not
        if (length(valid.vals) != length(labels) || anyNA(match(values, valid.vals))) {
          # We now need to know, which values of "x" don't
          # have labels.
          add_vals <- valid.vals[!valid.vals %in% values]
          # add to labels
          labels <- c(labels, as.character(add_vals))
          # fix value prefix
          new_vals <- c(as.character(values), as.character(add_vals))
          # check if values are numeric or not. if not,
          # make sure it's character, so we can order
          # consistently
          if (suppressWarnings(anyNA(as.numeric(values)))) {
            orderpart <- as.character(values)
          } else {
            orderpart <- as.numeric(values)
          }
          # sort values and labels
          labels <- labels[order(c(orderpart, add_vals))]
          new_vals <- new_vals[order(c(orderpart, add_vals))]
          # set back new values
          values <- new_vals
        }
      }
      # include associated values?
      if (!is.null(include.values)) {
        # for backwards compatibility, we also accept "TRUE"
        # here we set values as names-attribute
        if ((is.logical(include.values) && isTRUE(include.values)) ||
            include.values == "as.name" || include.values == "n") {
          names(labels) <- values
        }
        # here we include values as prefix of labels
        if (include.values == "as.prefix" || include.values == "p") {
          if (is.numeric(values))
            labels <- sprintf("[%i] %s", values, labels)
          else
            labels <- sprintf("[%s] %s", values, labels)
        }
      }
    }
  }
  # foreign? then reverse order
  if (is_foreign(attr.string)) labels <- rev(labels)
  # return them
  return(labels)
}
# @title Retrieve missing values of labelled variables
# @name get_na
#
# @description This function retrieves the value codes associated with missing values
#                of variables of an imported SPSS, SAS or STATA data set (via \code{\link{read_spss}},
#                \code{\link{read_sas}} or \code{\link{read_stata}}), where missing
#                values have not been replaced with \code{NA}s after import,
#                or of \code{\link[haven]{labelled}} vectors.
#
# @seealso \code{\link{get_labels}} to get value labels, or \code{\link{get_values}}
#            to get values associated with labels; see \code{\link{set_na}} to
#            replace specific values with \code{NA} and \code{\link{to_na}} to
#            convert missing value codes into \code{NA}; see \code{\link{get_na_flags}}
#            to get a logical vector of missing flags.
#
# @param x Variable (vector) with value label attributes, including
#          missing value codes (see \code{\link{labelled}}).
# @return The missing values associated with value labels from \code{x},
#           or \code{NULL} if \code{x} has no missing value attribute.
#
# @details Other statistical software packages (like 'SPSS') allow to define
#            multiple missing values, e.g. \emph{not applicable}, \emph{refused answer}
#            or "real" missing. These missing types may be assigned with
#            different values, so it is possible to distinguish between these
#            missing types. In R, multiple declared missings cannot be represented
#            in a similar way. However, \code{\link{labelled}} vectors
#            allow to indicate different missings through the
#            \code{is_na}-\code{\link{attr}}. Technically, these "missings" are
#            stored as normal values. Thus, the \code{\link{table}} command,
#            for instance, would include these values by default. The
#            \pkg{sjmisc} package offers capabilities to deal with multiple
#            declared missings and enhances the possibilities to work with
#            labelled data, allowing for easy access of multiple declared
#            missings or conversion into \code{NA} etc.
#            \cr \cr
#            Furthermore, see 'Details' in \code{\link{get_values}}.
#
# @examples
# # create labelled factor, with missing flag
# x <- labelled(c("M", "M", "F", "X", "N/A"),
#               c(Male = "M", Female = "F",
#                 Refused = "X", "Not applicable" = "N/A"),
#               c(FALSE, FALSE, TRUE, TRUE))
# get_na(x)
#
# # create labelled integer, with missing flag
# x <- labelled(c(1, 2, 1, 3, 4, 1),
#               c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#               c(FALSE, FALSE, TRUE, TRUE))
# get_na(x)
#
#
get_na <- function(x) {
  # get values
  values <- get_values(x, sort.val = FALSE, drop.na = FALSE)
  # get NA logicals
  na.flag <- get_na_flags(x)
  # do we have missing flag?
  if (is.null(na.flag)) {
    message("Variable has no assigned missing value codes.")
    return(NULL)
  }
  # copy NA-codes to new vector, so we can check length
  nas <- values[na.flag]
  # set return value to NULL, if no missing values
  if (length(nas) == 0) nas <- NULL
  # return missing values
  return(nas)
}


getNaAttribute <- function() return("is_na")
# @title Retrieve missing value flags of labelled variables
# @name get_na_flags
#
# @description This function retrieves the logical missing flags for a
#                \code{\link{labelled}} variable.
#
# @seealso \code{\link{get_na}} to get value codes of labelled missing values;
#            \code{\link{get_values}} to get values associated with labels;
#            see \code{\link{set_na}} to replace specific values with \code{NA}
#            and \code{\link{to_na}} to convert missing value codes into \code{NA}.
#
# @param x Variable (vector) with value label attributes, including
#          missing value codes (see \code{\link{labelled}}).
# @return Logical vector with missing flags that indicate which labelled value
#           is considered as missing.
#
# @details See 'Details' in \code{\link{get_na}}.
#
# @examples
# # create labelled integer, with missing flag
# x <- labelled(c(1, 2, 1, 3, 4, 1),
#               c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#               c(FALSE, FALSE, TRUE, TRUE))
# get_na_flags(x)
#
#
get_na_flags <- function(x) return(attr(x, getNaAttribute(), exact = T))
# @title Retrieve notes (annotations) from labelled variables
# @name get_note
#
# @description This function retrieves the value of the \code{note}-attribute
#                of vectors.
#
# @param x Variable (vector) with note-attribute.
# @return The the value of the \code{note}-attribute of \code{x}.
#
# @examples
# # create labelled factor, with missing flag
# x <- labelled(c("M", "M", "F", "X", "N/A"),
#               c(Male = "M", Female = "F",
#                 Refused = "X", "Not applicable" = "N/A"),
#               c(FALSE, FALSE, TRUE, TRUE))
#
# set_label(x) <- "A labelled vector with note"
# set_note(x) <- "Test annotation."
# get_note(x)
# x
#
#
get_note <- function(x) {
  return(attr(x, "note", exact = TRUE))
}


# @title Add notes (annotations) to (labelled) variables
# @name set_note
#
# @description This function adds a note (string) as \code{note}-attribute
#                to \code{x}.
#
# @param x Variable (vector).
# @param value The note (annotation) as character string that will be added as
#          \code{note}-attribute to \code{x}.
#
# @return \code{x}, with \code{value} stored as attribute.
#
# @examples
# # create labelled factor, with missing flag
# x <- labelled(c("M", "M", "F", "X", "N/A"),
#               c(Male = "M", Female = "F",
#                 Refused = "X", "Not applicable" = "N/A"),
#               c(FALSE, FALSE, TRUE, TRUE))
#
# set_label(x) <- "A labelled vector with note"
# set_note(x) <- "Test annotation."
# get_note(x)
# x
#
#
set_note <- function(x, value = NULL) {
  if (is.null(value) || is_empty(value)) {
    attr(x, "note") <- NULL
  } else {
    attr(x, "note") <- value
  }
  x
}

# @rdname set_note
#
`set_note<-` <- function(x, value) {
  UseMethod("set_note<-")
}

#
`set_note<-.default` <- function(x, value) {
  x <- set_note(x = x, value = value)
  x
}
# @title Retrieve values of labelled variables
# @name get_values
#
# @description This function retrieves the values associated with value labels
#                of an imported SPSS, SAS or STATA data set (via \code{\link{read_spss}},
#                \code{\link{read_sas}} or \code{\link{read_stata}}),
#                or of a \code{\link[haven]{labelled}} vector.
#
# @seealso \code{\link{get_labels}} for getting value labels and \code{\link{get_na}}
#            to get values for missing values.
#
# @param x Variable (vector) with value label attributes.
# @param sort.val Logical, if \code{TRUE} (default), values of associated value labels
#          are sorted.
# @param drop.na Logical, if \code{TRUE}, missing code values are excluded from
#          the return value. See 'Examples' and \code{\link{get_na}}.
# @return The values associated with value labels from \code{x},
#           or \code{NULL} if \code{x} has no label attributes.
#
# @details \code{\link[haven]{labelled}} vectors are numeric by default (when imported with read-functions
#            like \code{\link{read_spss}}) and have variable and value labels attributes.
#            The value labels are associated with those values from the labelled vector.
#            This function returns the values associated with the vector's value labels,
#            which may differ from actual values in the vector (e.g. due to missings)
#            or are not represented in sorted order.
#
# @examples
# data(efc)
# str(efc$e42dep)
# get_values(efc$e42dep)
# get_labels(efc$e42dep)
#
# # create labelled integer, with missing flag
# x <- labelled(c(1, 2, 1, 3, 4, 1),
#               c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#               c(FALSE, FALSE, TRUE, TRUE))
# # get all values
# get_values(x)
# # drop NA
# get_values(x, , TRUE)
#
#
#
get_values <- function(x, sort.val = FALSE, drop.na = FALSE) {
  # haven or sjPlot?
  attr.string <- getValLabelAttribute(x)
  # nothing found? then leave...
  if (is.null(attr.string)) return(NULL)
  # get values
  if (is.character(x) || (is.factor(x) && !is_num_fac(x)))
    values <- unname(attr(x, attr.string, exact = T))
  else
    values <- as.numeric(unname(attr(x, attr.string, exact = T)))
  # sort values
  if (sort.val) values <- sort(values)
  # remove missing value codes?
  if (drop.na) {
    # get NA logicals
    na.flag <- get_na_flags(x)
    # do we have missing flag? if yes, remove missing code value
    if (!is.null(na.flag)) values <- values[!na.flag]
  }
  # foreign? then reverse order
  if (is_foreign(attr.string)) values <- rev(values)
  # return sorted
  return(values)
}
# @title Chi-square goodness-of-fit-test
# @name chisq_gof
#
# @description This method performs a Chi-square goodness-of-fit-test (GOF)
#                either on a numeric vector against probabilities, or
#                a Goodness-of-fit test for \code{\link{glm}}s for binary data.
#
# @param x Numeric vector, or a \code{\link{glm}}-object.
# @param prob Vector of probabilities (indicating the population probabilities) of the same length
#          as \code{x}'s amount of categories / factor levels. Use \code{nrow(table(x))} to
#          determine the amount of necessary values for \code{prob}. Only used,
#          when \code{x} is a vector, and not a \code{glm}-object.
# @param weights Vector with weights, used to weight \code{x}.
# @return For vectors, returns the object of the computed \code{\link{chisq.test}}.
#           \cr \cr
#           For \code{glm}-objects, an object of class \code{chisq_gof} with
#           following values:
#           \itemize{
#            \item \code{p.value}	the p-value for the goodness-of-fit test
#            \item \code{z.score} the standardized z-score for the goodness-of-fit test
#            \item \code{RSS} the residual sums of squares term
#            \item \code{X2} the pearson chi-squared statistic
#           }
#
# @note For vectors, this function is a convenient function for the \code{\link{chisq.test}},
#         performing goodness-of-fit test.
#         \cr \cr
#         For \code{glm}-objects, this function performs a goodness-of-fit test
#         based on the \code{X2GOFtest} function of the \pkg{binomTools} package.
#         A well-fitting model shows no significant difference between
#         the model and the observed data, i.e. the reported p-values should be
#         greater than 0.05.
#
# @examples
# data(efc)
# # differing from population
# chisq_gof(efc$e42dep, c(0.3,0.2,0.22,0.28))
# # equal to population
# chisq_gof(efc$e42dep, prop.table(table(efc$e42dep)))
#
# # goodness-of-fit test for logistic regression
# efc$services <- dicho(efc$tot_sc_e, "v", 0, as.num = TRUE)
# fit <- glm(services ~ neg_c_7 + c161sex + e42dep,
#            data = efc,
#            family = binomial(link = "logit"))
# chisq_gof(fit)
#
#' @importFrom stats na.omit fitted resid formula as.formula lm pnorm chisq.test
#
chisq_gof <- function(x, prob = NULL, weights = NULL) {
  if (any(class(x) == "glm")) {

    # This is an adapted version from the
    # "binomTools" package. The "X2GOFtest()"
    # function did not work when model data frame
    # had missing values.
    y_hat <- stats::fitted(x)
    wt <- x$prior.weight
    vJ <- wt * y_hat * (1 - y_hat)
    cJ <- (1 - 2 * y_hat) / vJ
    X2 <- sum(stats::resid(x, type = "pearson") ^ 2)
    form <- stats::as.formula(x$formula)
    form[[2]] <- as.name("cJ")

    # use model matrix instead of data values,
    # because data may contain more variables
    # than needed, and due to missing may have
    # different row length
    dat <- stats::na.omit(x$model)
    dat$cJ <- cJ
    dat$vJ <- vJ
    RSS <- sum(stats::resid(stats::lm(form, data = dat, weights = vJ)) ^ 2)
    A <- 2 * (length(y_hat) - sum(1 / wt))
    z <- (X2 - x$df.residual) / sqrt(A + RSS)
    p.value <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
    chi2gof <- list(p.value = p.value,
                    z.score = z,
                    RSS = RSS,
                    X2 = X2)
    class(chi2gof) <- "chi2gof"
  } else {
    # check if we have probs
    if (is.null(prob)) {
      warning("`prob` needs to be specified.", call. = F)
      return(invisible(NULL))
    }
    # performs a Chi-square goodnes-of-fit-test
    if (!is.null(weights)) x <- weight(x, weights)
    dummy <- as.vector(table(x))
    # goodness of fit-test. x is one-dimensional and
    # y not given
    chi2gof <- stats::chisq.test(dummy, p = prob)
  }
  return(chi2gof)
}


# @title Hosmer-Lemeshow Goodness-of-fit-test
# @name hoslem_gof
#
# @description This method performs a Hosmer-Lemeshow goodness-of-fit-test
#                for generalized linear (mixed) models for binary data.
#
# @param x Fitted \code{\link{glm}} or \code{\link[lme4]{glmer}} model.
# @param g Number of bins to divide the data. Default is 10.
#
# @return An object of class \code{hoslem_test} with
#           following values:
#           \itemize{
#            \item \code{chisq} the Hosmer-Lemeshow chi-squared statistic
#            \item \code{df} degrees of freedom
#            \item \code{p.value} the p-value for the goodness-of-fit test
#           }
#
# @note A well-fitting model shows no significant difference between
#         the model and the observed data, i.e. the reported p-value should be
#         greater than 0.05.
#
# @seealso \code{\link{r2}}
#
# @examples
# data(efc)
#
# # goodness-of-fit test for logistic regression
# efc$services <- dicho(efc$tot_sc_e, "v", 0, as.num = TRUE)
# fit <- glm(services ~ neg_c_7 + c161sex + e42dep,
#            data = efc,
#            family = binomial(link = "logit"))
# hoslem_gof(fit)
#
#' @importFrom stats fitted pchisq quantile xtabs
#
hoslem_gof <- function(x, g = 10) {
  # check for valid object class
  if (!any(class(x) == "glmerMod") && !any(class(x) == "glm")) {
    stop("'x' must be an object of class 'glm' or 'glmerMod'.", call. = F)
  }

  # mixed models (lme4)
  if (any(class(x) == "glmerMod")) {
    # check for package availability
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
    }
    y <- lme4::getME(x, "y")
    yhat <- stats::fitted(x)
  } else {
    y <- x$y
    yhat <- stats::fitted(x)
  }
  cutyhat <- cut(yhat,
                 breaks = stats::quantile(yhat, probs = seq(0, 1, 1 / g)),
                 include.lowest = TRUE)
  obs <- stats::xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- stats::xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2 / expect)
  p.value <- 1 - stats::pchisq(chisq, g - 2)
  hoslem <- list(chisq = chisq,
                 df = g - 2,
                 p.value = p.value)
  class(hoslem) <- "hoslem_test"
  return(hoslem)
}
# @title Group near elements of string vectors
# @name group_str
#
# @seealso \code{\link{str_pos}}
#
# @description This function groups elements of a string vector (character or string
#                variable) according to the element's distance ('similatiry'). The
#                more similar two string elements are, the higher is the
#                chance to be combined into a group.
#
# @param strings Character vector with string elements.
# @param maxdist Maximum distance between two string elements, which is allowed to treat two
#          elements as similar or equal.
# @param method Method for distance calculation. The default is \code{"lv"}. See
#          \code{\link[stringdist]{stringdist}} for details.
# @param strict Logical; if \code{TRUE}, value matching is more strictly. See 'Examples'.
# @param trim.whitespace Logical; if \code{TRUE} (default), leading and trailing white spaces will
#          be removed from string values.
# @param remove.empty Logical; if \code{TRUE} (default), empty string values will be removed from the
#          character vector \code{strings}.
# @param showProgressBar Logical; if \code{TRUE}, the progress bar is displayed when computing the distance matrix.
#          Default in \code{FALSE}, hence the bar is hidden.
#
# @return A character vector where similar string elements (values) are recoded
#           into a new, single value. The return value is of same length as
#           \code{strings}, i.e. grouped elements appear multiple times, so
#           the count for each grouped string is still avaiable (see 'Examples').
#
# @examples
# oldstring <- c("Hello", "Helo", "Hole", "Apple",
#                "Ape", "New", "Old", "System", "Systemic")
# newstring <- group_str(oldstring)
#
# # see result
# newstring
#
# # count for each groups
# table(newstring)
#
# \dontrun{
# library(sjPlot)
# # print table to compare original and grouped string
# sjt.frq(data.frame(oldstring, newstring),
#         removeStringVectors = FALSE,
#         autoGroupStrings = FALSE)
#
# # larger groups
# newstring <- group_str(oldstring, maxdist = 3)
# sjt.frq(data.frame(oldstring, newstring),
#         removeStringVectors = FALSE,
#         autoGroupStrings = FALSE)
#
# # be more strict with matching pairs
# newstring <- group_str(oldstring, maxdist = 3, strict = TRUE)
# sjt.frq(data.frame(oldstring, newstring),
#         removeStringVectors = FALSE,
#         autoGroupStrings = FALSE)}
#
#' @importFrom utils txtProgressBar
#' @importFrom stringdist stringdistmatrix
#
group_str <- function(strings,
                      maxdist = 2,
                      method = "lv",
                      strict = FALSE,
                      trim.whitespace = TRUE,
                      remove.empty = TRUE,
                      showProgressBar = FALSE) {
  # check if required package is available
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("Package `stringdist` needed for this function to work. Please install it.", call. = FALSE)
  }

  # coerce to character, if necessary
  if (!is.character(strings)) strings <- as.character(strings)

  # trim white spaces
  if (trim.whitespace) {
    for (i in 1:length(strings)) strings[i] <- trim(strings[i])
  }

  # remove empty values
  if (remove.empty) {
    removers <- c()
    for (i in 1:length(strings)) {
      if (is_empty(strings[i])) removers <- c(removers, i)
    }
    if (length(removers) > 0) strings <- strings[-removers]
  }

  # create matrix from string values of variable
  m <- stringdist::stringdistmatrix(strings, strings, method = method, useNames = "strings")

  # init variable that contains "close" pairs
  pairs <- list()

  # helper function that finds elements in
  # final list of grouped elements
  findInPairs <- function(curel) {
    elfound <- FALSE
    if (length(pairs) > 0) {
      for (ll in 1:length(pairs)) {
        pel <- pairs[[ll]]
        if (any(pel == curel)) elfound <- TRUE
      }
    }
    return(elfound)
  }

  # create progress bar
  # if (showProgressBar) pb <- utils::txtProgressBar(min = 1, max = ncol(m), style = 3)

  # iterate matrix
  for (i in 1:nrow(m)) {
    # update progress bar
    # if (showProgressBar) utils::setTxtProgressBar(pb, i)

    # check if current element is already grouped
    if (!findInPairs(rownames(m)[i])) {
      # current row element has not been grouped
      # yet, so go on...
      pairvector <- c()

      for (j in 1:ncol(m)) {
        # check if we found a pair's distance that
        # is within the maximum requested distance
        # i.e. which are "close" enough
        if (m[i, j] <= maxdist) {
          # go through all rows of this column and
          # check if there's a better match for the
          # currently compared token
          foundBetterToken <- !strict
          for (cnt in 1:nrow(m)) {
            if (strict) {
              if (m[cnt, j] > 0 && m[cnt, j] < m[i, j]) foundBetterToken <- TRUE
            } else {
              if (m[cnt, j] <= maxdist && m[i, cnt] <= maxdist) foundBetterToken <- FALSE
            }
          }

          # in the current column, there's no better
          # matching of strings, so we pick this values
          # and add it to our results
          if (!foundBetterToken) {
            # remember string value
            token <- colnames(m)[j]

            # check if we already found a string value
            # within this column. if not, add string values
            # to "close" pairs of this column
            if (!any(pairvector == token) && !findInPairs(token)) pairvector <- c(pairvector, token)
          }
        }
      }

      # now we have a vector with all "close" string values
      # from the current row's value
      pairvector <- sort(pairvector)

      # check if we already have saved these values to our list
      # if not, add "close" values as new list element
      if (!any(unlist(lapply(pairs, function(x) length(x) == length(pairvector) && any(x == pairvector))))) pairs <- c(pairs, list(pairvector))
    }
  }

  # we now have a list, where each list element
  # is a vector of "close" string values
  strings.new <- c()

  # go through each list element
  for (i in 1:length(pairs)) {
    r <- pairs[[i]]

    # find vector indices of "close" values in
    # original string
    indices <- unlist(lapply(r, function(x) which(strings == x)))
    newvalue <- r[1]
    count <- 2

    # "merge" each close values into one
    # single value that combines all close values
    while (count <= length(r)) {
      newvalue <- paste0(newvalue, ", ", r[count])
      count <- count + 1
    }
    strings.new[indices] <- newvalue
  }
  # if (showProgressBar) close(pb)

  # return new vector, where all single "close"
  # values are replaced by the group of closed values.
  # e.g. the three values "hello", "holle" and "hole"
  # will be "recoded" into on value "hello, holle, hole"
  return(strings.new)
}
# @title Recode numeric variables into equal-ranged groups
# @name group_var
#
# @description Recode numeric variables into \emph{equal spaced} grouped factors,
#                i.e. a variable is cut into a smaller number of groups,
#                where each group has values of equal range.
#
# @seealso \code{\link{group_labels}} to create the associated value labels for
#          grouped variables, \code{\link{split_var}} to split variables into
#          equal sized groups, \code{\link{group_str}} for grouping string vectors
#          or \code{\link{rec_pattern}} and \code{\link{rec}} for another
#          convenbient way of recoding variables into smaller groups.
#
# @param var Numeric; variable, which should recoded into groups.
# @param groupsize Numeric; group-size, i.e. the range for grouping. By default,
#          for each 5 categories of \code{var} a new group is defined, i.e. \code{groupsize=5}.
#          Use \code{groupsize = "auto"} to automatically resize a variable into
#          a maximum of 30 groups (which is the ggplot-default grouping when
#          plotting histograms). Use \code{groupcount} to determine the amount
#          of groups.
# @param as.num Logical; if \code{TRUE}, the recoded variable will
#          be returned as numeric vector. If \code{FALSE}, a factor is returned.
# @param right.interval Logical; if \code{TRUE}, grouping starts with the lower
#          bound of \code{groupsize}. See 'Details'.
# @param groupcount Sets the maximum number of groups that are defined when auto-grouping is on
#          (\code{groupsize="auto"}). Default is 30. If \code{groupsize} is not set to \code{"auto"},
#          this argument will be ignored.
#
# @return A grouped variable, either as numeric or as factor (see paramter \code{as.num}).
#
# @note Variable label attributes (see, for instance, \code{\link{set_label}}) are preserved.
#
# @details If \code{groupsize} is set to a specific value, the variable is recoded
#            into several groups, where each group has a maximum range of \code{groupsize}.
#            Hence, the amount of groups differ depending on the range of \code{var}.
#            \cr \cr
#            If \code{groupsize = "auto"}, the variable is recoded into a maximum of
#            \code{groupcount} groups. Hence, independent from the range of
#            \code{var}, always the same amount of groups are created, so the range
#            within each group differs (depending on \code{var}'s range).
#            \cr \cr
#            \code{right.interval} determins which boundary values to include when
#            grouping is done. If \code{TRUE}, grouping starts with the \strong{lower
#            bound} of \code{groupsize}. For example, having a variable ranging from
#            50 to 80, groups cover the ranges from  50-54, 55-59, 60-64 etc.
#            If \code{FALSE} (default), grouping starts with the \code{upper bound}
#            of \code{groupsize}. In this case, groups cover the ranges from
#            46-50, 51-55, 56-60, 61-65 etc. \strong{Note:} This will cover
#            a range from 46-50 as first group, even if values from 46 to 49
#            are not present. See 'Examples' in \code{\link{group_labels}}.
#            \cr \cr
#            If you want to split a variable into a certain amount of equal
#            sized groups (instead of having groups where values have all the same
#            range), use the \code{\link{split_var}} function!
#
# @examples
# age <- abs(round(rnorm(100, 65, 20)))
# age.grp <- group_var(age, 10)
# hist(age)
# hist(age.grp)
#
#
# # histogram with EUROFAMCARE sample dataset
# # variable not grouped
# data(efc)
# hist(efc$e17age, main = get_label(efc$e17age))
#
# # bar plot with EUROFAMCARE sample dataset
# # grouped variable
# ageGrp <- group_var(efc$e17age)
# ageGrpLab <- group_labels(efc$e17age)
# barplot(table(ageGrp), main = get_label(efc$e17age), names.arg = ageGrpLab)
#
#
group_var <- function(var,
                      groupsize = 5,
                      as.num = TRUE,
                      right.interval = FALSE,
                      groupcount = 30) {
  # do we have labels?
  varlab <- get_label(var)
  # group variable
  var <- group_helper(var, groupsize, right.interval, groupcount)
  # set new levels of grouped variable
  levels(var) <- c(1:nlevels(var))
  # convert to numeric?
  if (as.num) var <- as.numeric(as.character(var))
  # set back variable labels
  if (!is.null(varlab)) var <- set_label(var, varlab)
  return(var)
}


# @title Create labels for recoded groups
# @name group_labels
#
# @description Creates the related labels for the grouped variable created by
#                \code{\link{group_var}}.
#
# @seealso \itemize{
#            \item \code{\link{group_var}}
#            \item \code{\link{group_str}}
#          }
#
# @note Usually you should use the same values for \code{groupsize} and
#         \code{right.interval} as used in the \code{\link{group_var}} function
#         if you want to create labels for the related recoded variable.
#
# @return A string vector containing labels based on the grouped categories of \code{var},
#           formatted as "from lower bound to upper bound", e.g. \code{"10-19"  "20-29"  "30-39"} etc.
#           See examples below.
#
# @inheritParams group_var
#
# @details See 'Details' in \code{\link{group_var}}.
#
# @note Variable label attributes (see, for instance, \code{\link{set_label}}) are preserved.
#
# @examples
# age <- abs(round(rnorm(100, 65, 20)))
# age.grp <- group_var(age, 10)
# hist(age)
# hist(age.grp)
#
# age.grpvar <- group_labels(age, 10)
# table(age.grp)
# print(age.grpvar)
#
#
# # create vector with values from 50 to 80
# dummy <- round(runif(200, 50, 80))
# # labels with grouping starting at lower bound
# group_labels(dummy)
# # labels with grouping startint at upper bound
# group_labels(dummy, right.interval = TRUE)
#
#
# # histogram with EUROFAMCARE sample dataset
# # variable not grouped
# data(efc)
# hist(efc$e17age, main = get_label(efc$e17age))
#
# # bar plot with EUROFAMCARE sample dataset
# # grouped variable
# ageGrp <- group_var(efc$e17age)
# ageGrpLab <- group_labels(efc$e17age)
# barplot(table(ageGrp), main = get_label(efc$e17age), names.arg = ageGrpLab)
#
#
group_labels <- function(var,
                         groupsize = 5,
                         right.interval = FALSE,
                         groupcount = 30) {
  # do we have labels?
  varlab <- get_label(var)
  # group variable
  var <- group_helper(var, groupsize, right.interval, groupcount)
  # Gruppen holen
  lvl <- levels(var)
  # rückgabewert init
  retval <- rep(c(""), length(lvl))
  # alle Gruppierungen durchgehen
  for (i in 1:length(lvl)) {
    # Länge jedes Labels der Gruppeneinteilungen auslesen
    sublength <- nchar(lvl[i])
    # "(" und "]", das bei "cut"-Funktion automatisch erstellt wird,
    # aus dem Label entfernen
    lvlstr <- substr(lvl[i], 2, sublength - 1)
    # Unter- und Obergrenze in jeweils einem string
    subs <- strsplit(lvlstr, ",")
    # Untergrenze als Zahlenwert
    lower <- as.numeric(subs[[1]][1])
    # Obergrenze als Zahlenwert
    upper <- as.numeric(subs[[1]][2])
    # Prüfen, welche Intervallgrenze ein-
    # und welche ausgeschlossen werden soll
    if (right.interval) {
      lower <- lower + 1
    } else {
      upper <- upper - 1
    }
    # Rückgabe des Strings
    retval[i] <- c(paste(lower, "-", upper, sep = ""))
  }
  # set back variable labels
  if (!is.null(varlab)) retval <- set_label(retval, varlab)
  return(retval)
}


group_helper <- function(var, groupsize, right.interval, groupcount) {
  # minimum range. will be changed when autogrouping
  minval <- 0
  multip <- 2
  # check for auto-grouping
  if (groupsize == "auto") {
    # determine groupsize, which is 1/30 of range
    size <- ceiling((max(var, na.rm = TRUE) - min(var, na.rm = TRUE)) / groupcount)
    # reset groupsize var
    groupsize <- as.numeric(size)
    # change minvalue
    minval <- min(var, na.rm = TRUE)
    multip <- 1
  }
  # Einteilung der Variablen in Gruppen. Dabei werden unbenutzte
  # Faktoren gleich entfernt
  var <- droplevels(cut(var,
                        breaks = c(seq(minval, max(var, na.rm = TRUE) + multip * groupsize, by = groupsize)),
                        right = right.interval))
  return(var)
}
# Help-functions


is_foreign <- function(x) return(!is.null(x) && x == "value.labels")

is_merMod <- function(fit) {
  return(any(class(fit) %in% c("lmerMod", "glmerMod", "nlmerMod", "merModLmerTest")))
}


# auto-detect attribute style for variable labels.
# either haven style ("label") or foreign style
# ("variable.label")
getVarLabelAttribute <- function(x) {
  attr.string <- NULL
  # check if x is data frame. if yes, retrieve one "example" variable
  if (is.data.frame(x) || is.list(x)) {
    # define length for loop
    if (is.data.frame(x))
      counter <- ncol(x)
    else
      counter <- length(x)
    # we need to check all variables until first variable
    # that has any attributes at all - SPSS variables without
    # labels would return NULL, so if -e.g.- first variable
    # of data set has no label attribute, but second had, this
    # function would stop after first attribute and return NULL
    for (i in 1:counter) {
      # retrieve attribute names
      an <- names(attributes(x[[i]]))
      # check for label attributes
      if (any(an == "label") || any(an == "variable.label")) {
        x <- x[[i]]
        break
      }
    }
  }
  # check if vector has label attribute
  if (!is.null(attr(x, "label", exact = T))) attr.string <- "label"
  # check if vector has variable label attribute
  if (!is.null(attr(x, "variable.label", exact = T))) attr.string <- "variable.label"
  # not found any label yet?
  if (is.null(attr.string)) {
    # check value_labels option
    opt <- getOption("value_labels")
    if (!is.null(opt)) attr.string <- ifelse(opt == "haven", "label", "variable.label")
  }
  return(attr.string)
}


# auto-detect attribute style for value labels.
# either haven style ("labels") or foreign style
# ("value.labels")
getValLabelAttribute <- function(x) {
  attr.string <- NULL
  # check if x is data frame. if yes, just retrieve one "example" variable
  if (is.data.frame(x)) {
    # find first variable with labels or value.labels attribute
    for (i in 1:ncol(x)) {
      # has any attribute?
      if (!is.null(attr(x[[i]], "labels", exact = T))) {
        attr.string <- "labels"
        break
      } else if (!is.null(attr(x[[i]], "value.labels", exact = T))) {
        attr.string <- "value.labels"
        break
      }
    }
  } else {
    # check if vector has labels attribute
    if (!is.null(attr(x, "labels", exact = T))) attr.string <- "labels"
    # check if vector has value.labels attribute
    if (!is.null(attr(x, "value.labels", exact = T))) attr.string <- "value.labels"
  }
  # not found any label yet?
  if (is.null(attr.string)) {
    # check value_labels option
    opt <- getOption("value_labels")
    if (!is.null(opt)) attr.string <- ifelse(opt == "haven", "label", "variable.label")
  }
  return(attr.string)
}
# @title Intraclass-Correlation Coefficient
# @name icc
# @description This function calculates the intraclass-correlation
#                (icc) for random intercepts of mixed effects models.
#                Currently, only \code{\link[lme4]{merMod}} objects
#                are supported.
#
# @param x Fitted mixed effects model (\code{\link[lme4]{merMod}}-class).
# @param ... More fitted model objects, to compute multiple intraclass-correlation
#              coefficients at once.
#
# @return A numeric vector with all random intercept intraclass-correlation-coefficients,
#           or a list of numeric vectors, when more than one model were used
#           as arguments. Furthermore, between- and within-group variances as well
#           as random-slope variance are returned as attributes.
#
# @references \itemize{
#               \item Aguinis H, Gottfredson RK, Culpepper SA. 2013. Best-Practice Recommendations for Estimating Cross-Level Interaction Effects Using Multilevel Modeling. Journal of Management 39(6): 1490–1528 (\doi{10.1177/0149206313478188})
#               \item Aly SS, Zhao J, Li B, Jiang J. 2014. Reliability of environmental sampling culture results using the negative binomial intraclass correlation coefficient. Springerplus [Internet] 3. Available from: \url{http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3916583/}
#               \item Hox J. 2002. Multilevel analysis: techniques and applications. Mahwah, NJ: Erlbaum
#               \item Stryhn H, Sanchez J, Morley P, Booker C, Dohoo IR. 2006. Interpretation of variance parameters in multilevel Poisson regression models. Proceedings of the 11th International Symposium on Veterinary Epidemiology and Economics, 2006 Available at \url{http://www.sciquest.org.nz/node/64294}
#               \item Wu S, Crespi CM, Wong WK. 2012. Comparison of methods for estimating the intraclass correlation coefficient for binary responses in cancer prevention cluster randomized trials. Contempory Clinical Trials 33: 869-880 (\doi{10.1016/j.cct.2012.05.004})
#               \item \href{http://stats.stackexchange.com/questions/18088/intraclass-correlation-icc-for-an-interaction/28100#28100}{CrossValidated (2012) \emph{Intraclass correlation (ICC) for an interaction?}}
#               \item \href{http://stats.stackexchange.com/questions/113577/interpreting-the-random-effect-in-a-mixed-effect-model/113825#113825}{CrossValidated (2014) \emph{Interpreting the random effect in a mixed-effect model}}
#               \item \href{http://stats.stackexchange.com/questions/67247/how-to-partition-the-variance-explained-at-group-level-and-individual-level/67356#67356}{CrossValidated (2014) \emph{how to partition the variance explained at group level and individual level}}
#             }
#
#
# @note Some notes on why the ICC is useful, based on Grace-Martin K: \emph{The Intraclass Correlation Coefficient in Mixed Models}, \href{http://www.theanalysisfactor.com/the-intraclass-correlation-coefficient-in-mixed-models/}{web}:
#       \itemize{
#        \item It can help you determine whether or not a linear mixed model is even necessary. If you find that the correlation is zero, that means the observations within clusters are no more similar than observations from different clusters. Go ahead and use a simpler analysis technique.
#        \item It can be theoretically meaningful to understand how much of the overall variation in the response is explained simply by clustering. For example, in a repeated measures psychological study you can tell to what extent mood is a trait (varies among people, but not within a person on different occasions) or state (varies little on average among people, but varies a lot across occasions).
#        \item It can also be meaningful to see how the ICC (as well as the between and within cluster variances) changes as variable are added to the model.
#       }
#       In short, the ICC can be interpreted as "the proportion of the variance
#       explained by the grouping structure in the population" (Hox 2002: 15).
#       \cr \cr
#       \strong{Caution:} For three-level-models, depending on the nested structure
#       of the model, the ICC only reports the proportion of variance explained
#       for each grouping level. However, the proportion of variance for specific
#       levels related to each other (e.g., similarity of level-1-units within
#       level-2-units or level-2-units within level-3-units) must be computed
#       manually. Use \code{\link{get_re_var}} to get the between-group-variances
#       and residual variance of the model, and calculate the ICC for the various level
#       correlations.
#       \cr \cr
#       For example, for the ICC between level 1 and 2: \cr
#       \code{sum(get_re_var(fit)) / (sum(get_re_var(fit)) + get_re_var(fit, "sigma_2"))}
#       \cr \cr
#       or for the ICC between level 2 and 3: \cr
#       \code{get_re_var(fit)[2] / sum(get_re_var(fit))}
#
# @details The calculation of the ICC for generalized linear mixed models with binary outcome is based on
#       Wu et al. (2012). For Poisson multilevel models, please refere to Stryhn et al. (2006). Aly et al. (2014)
#       describe computation of ICC for negative binomial models.
#       \cr \cr
#       There is a \code{print}-method that prints the variance parameters using
#       the \code{comp}-argument set to \code{"var"}: \code{print(x, comp = "var")}
#       (see 'Examples'). The \code{\link{re_var}}-function is a convenient wrapper.
#       \cr \cr
#       The random effect variances indicate the between- and within-group
#         variances as well as random-slope variance and random-slope-intercept
#         correlation. The components are denoted as following:
#         \itemize{
#          \item Within-group (residual) variance: sigma_2
#          \item Between-group-variance: tau.00 (variation between individual intercepts and average intercept)
#          \item Random-slope-variance: tau.11 (variation between individual slopes and average slope)
#          \item Random-Intercept-Slope-covariance: tau.01
#          \item Random-Intercept-Slope-correlation: rho.01
#         }
#
# @seealso \code{\link{re_var}} to print random effect variances and \code{\link{get_re_var}}
#            to get the values from a particular variance component.
#
# @examples
# \dontrun{
# library(lme4)
# fit1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
# icc(fit1)
#
# sleepstudy$mygrp <- sample(1:45, size = 180, replace = TRUE)
# fit2 <- lmer(Reaction ~ Days + (1 | mygrp) + (Days | Subject), sleepstudy)
# icc(fit2)
#
# # return icc for all models at once
# icc(fit1, fit2)
#
# icc1 <- icc(fit1)
# icc2 <- icc(fit2)
#
# print(icc1, comp = "var")
# print(icc2, comp = "var")}
#
#
#' @importFrom stats family
#
icc <- function(x, ...) {
  # return value
  icc_ <- icc.lme4(x)
  # check if we have multiple parameters
  if (nargs() > 1) {
    # get input list
    params_ <- list(...)
    icc_ <- list(icc_)
    for (p_ in params_) {
      icc_[[length(icc_) + 1]] <- icc.lme4(p_)
    }
    names(icc_) <- NULL
  }
  return(icc_)
}

#' @importFrom lme4 VarCorr fixef getME
#' @importFrom stats family formula
icc.lme4 <- function(fit) {
  # check if suggested package is available
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needed for this function to work. Please install it.", call. = FALSE)
  }

  # check object class
  if (is_merMod(fit)) {
    # get family
    fitfam <- stats::family(fit)$family
    # is neg. binomoal?
    is_negbin <- str_contains(fitfam, "Negative Binomial", ignore.case = TRUE)

    # random effects variances
    # for details on tau and sigma, see
    # Aguinis H, Gottfredson RK, Culpepper SA2013. Best-Practice Recommendations for Estimating Cross-Level Interaction Effects Using Multilevel Modeling. Journal of Management 39(6): 1490–1528. doi:10.1177/0149206313478188.
    reva <- lme4::VarCorr(fit)
    # retrieve only intercepts
    vars <- lapply(reva, function(x) x[[1]])

    # random intercept-variances, i.e.
    # between-subject-variance (tau 00)
    tau.00 <- sapply(vars, function(x) x[1])

    # random slope-variances (tau 11)
    tau.11 <- unlist(lapply(reva, function(x) diag(x)[-1]))

    # residual variances, i.e.
    # within-cluster-variance (sigma^2)
    if (any(class(fit) == "glmerMod") && fitfam == "binomial") {
      # for logistic models, we use pi / 3
      resid_var <- (pi ^ 2) / 3
    } else if (any(class(fit) == "glmerMod") && is_negbin) {
      # for negative binomial models, we use 0
      resid_var <- 0
    } else {
      # for linear and poisson models, we have a clear
      # residual variance
      resid_var <- attr(reva, "sc") ^ 2
    }
    # total variance, sum of random intercept and residual variances
    total_var <- sum(sapply(vars, sum), resid_var)
    # check whether we have negative binomial
    if (is_negbin) {
      # for negative binomial models, we also need the intercept...
      beta <- as.numeric(lme4::fixef(fit)["(Intercept)"])
      # ... and the theta value to compute the ICC
      r <- lme4::getME(fit, "glmer.nb.theta")
      ri.icc <-
        (exp(tau.00) - 1) /
        ((exp(total_var) - 1) + (exp(total_var) / r) + exp(-beta - (total_var / 2)))
    } else {
      # random intercept icc
      ri.icc <- tau.00 / total_var
    }

    # get random slope random intercep correlations
    # do we have any rnd slopes?
    has_rnd_slope <- unlist(lapply(reva, function(x) dim(attr(x, "correlation"))[1] > 1))
    tau.01 <- rho.01 <- NULL

    # get rnd slopes
    if (any(has_rnd_slope)) {
      rnd_slope <- reva[has_rnd_slope]
      # get slope-intercept-correlations
      cor_ <- lapply(rnd_slope, function(x) attr(x, "correlation")[1, 2])
      # get standard deviations, multiplied
      std_ <- lapply(rnd_slope, function(x) prod(attr(x, "stddev")))
      # bind to matrix
      tau.01 <- apply(as.matrix(cbind(unlist(cor_), unlist(std_))), MARGIN = 1, FUN = prod)
      rho.01 <- unlist(cor_)
    }

    # name values
    names(ri.icc) <- names(reva)

    # add attributes, for print method
    class(ri.icc) <- c("icc.lme4", class(ri.icc))
    attr(ri.icc, "family") <- stats::family(fit)$family
    attr(ri.icc, "link") <- stats::family(fit)$link
    attr(ri.icc, "formula") <- stats::formula(fit)
    attr(ri.icc, "model") <- ifelse(any(class(fit) == "glmerMod"), "Generalized inear mixed model", "Linear mixed model")
    attr(ri.icc, "tau.00") <- tau.00
    attr(ri.icc, "tau.01") <- tau.01
    attr(ri.icc, "rho.01") <- rho.01
    attr(ri.icc, "tau.11") <- tau.11
    attr(ri.icc, "sigma_2") <- resid_var
    # return results
    return(ri.icc)
  } else {
    warning("Function `icc` currently only supports `merMod` objects (package `lme4`).", call. = TRUE)
  }
}


# @title Random effect variances
# @name re_var
# @description This function extracts the random effect variances as well as
#                the random-intercept-slope-correlation of mixed effects models.
#                Currently, only \code{\link[lme4]{merMod}} objects
#                are supported.
#
# @param x Fitted mixed effects model (\code{\link[lme4]{merMod}}-class).
#
# @return \code{NULL}, as this is just a convenient \code{print}-wrapper for
#           the variance components returned by the \code{\link{icc}} function.
#
# @references Aguinis H, Gottfredson RK, Culpepper SA. 2013. Best-Practice Recommendations for Estimating Cross-Level Interaction Effects Using Multilevel Modeling. Journal of Management 39(6): 1490–1528 (\doi{10.1177/0149206313478188})
#
# @seealso \code{\link{icc}} and \code{\link{get_re_var}}
#
# @examples
# library(lme4)
# fit1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
# re_var(fit1)
#
# sleepstudy$mygrp <- sample(1:45, size = 180, replace = TRUE)
# fit2 <- lmer(Reaction ~ Days + (1 | mygrp) + (Days | Subject), sleepstudy)
# re_var(fit2)
#
#' @importFrom stats family
#
re_var <- function(x) {
  # return value
  revar_ <- icc(x)
  print(revar_, comp = "var")
}


# @title Random effect variances
# @name get_re_var
# @description This function extracts specific components from the random effect
#                variances or the random-intercept-slope-correlation of mixed
#                effects models. Currently, only \code{\link[lme4]{merMod}} objects
#                are supported.
#
# @param x An object of class \code{icc.lme4}, as returned by the \code{\link{icc}} function,
#          or a fitted mixed effects model (\code{\link[lme4]{merMod}}-class).
# @param comp Name of the variance component to be returned. See 'Details'.
#
# @return The required variance component.
#
# @seealso \code{\link{icc}} and \code{\link{re_var}}
#
# @details The random effect variances indicate the between- and within-group
#         variances as well as random-slope variance and random-slope-intercept
#         correlation. Use following values for \code{comp} to get the particular
#         variance component:
#         \describe{
#          \item{\code{"sigma_2"}}{Within-group (residual) variance}
#          \item{\code{"tau.00"}}{Between-group-variance (variation between individual intercepts and average intercept)}
#          \item{\code{"tau.11"}}{Random-slope-variance (variation between individual slopes and average slope)}
#          \item{\code{"tau.01"}}{Random-Intercept-Slope-covariance}
#          \item{\code{"rho.01"}}{Random-Intercept-Slope-correlation}
#         }
#
# @examples
# library(lme4)
# fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#
# # icc
# icc(fit)
#
# # all random effect variance components
# re_var(fit)
#
# # just the rand. slope-intercept covariance
# get_re_var(fit, "tau.01")
#
# sleepstudy$mygrp <- sample(1:45, size = 180, replace = TRUE)
# fit <- lmer(Reaction ~ Days + (1 | mygrp) + (Days | Subject), sleepstudy)
# get_re_var(fit, "rho.01")
#
#
get_re_var <- function(x, comp = c("tau.00", "tau.01", "tau.11", "rho.01", "sigma_2")) {
  # check if we have a valid object
  if (!any(class(x) == "icc.lme4") && !is_merMod(x)) {
    stop("`x` must either be an object returned by the `icc` function, or a merMod-object.", call. = F)
  }

  # check arguments
  comp <- match.arg(comp)

  # do we have a merMod object? If yes, get ICC and var components
  if (is_merMod(x)) x <- icc(x)

  # return results
  return(attr(x, comp, exact = TRUE))
}

# @title Check whether two factors are crossed
# @name is_crossed
# @description This function checks whether two factors are crossed,
#                i.e. if each level of one factor occurs in combination
#                with each level of the other factor.
#
# @param f1 Numeric vector or \code{\link{factor}}.
# @param f2 Numeric vector or \code{\link{factor}}.
# @return Logical, \code{TRUE} if factors are crossed, \code{FALSE} otherwise.
#
# @seealso \code{\link{is_nested}}
#
# @references Grace, K. The Difference Between Crossed and Nested Factors. \href{http://www.theanalysisfactor.com/the-difference-between-crossed-and-nested-factors/}{(web)}
#
# @examples
# # crossed factors, each category of
# # x appears in each category of y
# x <- c(1,4,3,2,3,2,1,4)
# y <- c(1,1,1,2,2,1,2,2)
# # show distribution
# table(x, y)
# # check if crossed
# is_crossed(x, y)
#
# # not crossed factors
# x <- c(1,4,3,2,3,2,1,4)
# y <- c(1,1,1,2,1,1,2,2)
# # show distribution
# table(x, y)
# # check if crossed
# is_crossed(x, y)
#
#
is_crossed <- function(f1, f2) {
  tab <- table(f1, f2)
  # for crossed factors, we should have no zeros in any rows
  # (i.e. each level of f1 also contains any level of f2)
  return(!any(apply(tab, 1, function(x) any(x == 0)) == TRUE))
}
# @title Check whether string or vector is empty
# @name is_empty
# @description This function checks whether a string or character vector (of
#                length 1) or any vector (numeric, atomic) is empty or not.
#
#
# @param x String, character vector of length 1, or vector.
# @return Logical, \code{TRUE} if \code{x} is a character vector or string and
#           is empty, \code{TRUE} if \code{x} is any vector and of length 0,
#           \code{FALSE} otherwise.
#
# @note \code{NULL}- or \code{NA}-values are also considered as "empty" (see
#         'Examples') and will return \code{TRUE}.
#
# @examples
# x <- "test"
# is_empty(x)
#
# x <- ""
# is_empty(x)
#
# x <- NA
# is_empty(x)
#
# x <- NULL
# is_empty(x)
#
# # string is not empty
# is_empty(" ")
#
# # however, this trimmed string is
# is_empty(trim(" "))
#
# # numeric vector
# x <- 1
# is_empty(x)
# x <- x[-1]
# is_empty(x)
#
#
is_empty <- function(x) {
  # do we have a valid vector?
  if (!is.null(x)) {
    # if it's a character, check if we have only one element in that vector
    if (is.character(x)) {
      if (length(x) > 1) warning("`x` must be of length 1. Evaluating first element only.", call. = TRUE)
      # zero chats, so empty?
      zero_len <- nchar(x) == 0
      # we have a non-character vector here. check for length
    } else {
      zero_len <- length(x) == 0
    }
  }
  return(is.null(x) || zero_len || is.na(x))
}
# @title Check whether value is even
# @name is_even
#
# @description Checks whether \code{x} is an even number or not. Only
#                accepts numeric vectors.
#
# @param x Numeric vector or single numeric value.
#
# @return \code{TRUE} for each even value of \code{x}, \code{FALSE} for
#           odd values.
#
# @seealso \code{\link{is_odd}}
#
# @examples
# is_even(4)
# is_even(5)
# is_even(1:4)
#
#
is_even <- function(x) (x %% 2) == 0
# @title Check whether object is of class "labelled"
# @name is_labelled
# @description This function checks whether \code{x} is of class \code{labelled}.
#
# @param x An object.
# @return Logical, \code{TRUE} if \code{any(class(x))} is \code{labelled},
#           \code{FALSE} otherwise.
#
#
is_labelled <- function(x) {
  # check if object has multiple class attributes
  if (length(class(x)) > 1) return(any(class(x) == "labelled"))
  # return if labelled
  return(class(x) == "labelled")
}
# @title Check whether two factors are nested
# @name is_nested
# @description This function checks whether two factors are nested,
#                i.e. if each category of the first factor co-occurs
#                with only one category of the other.
#
# @param f1 Numeric vector or \code{\link{factor}}.
# @param f2 Numeric vector or \code{\link{factor}}.
# @return Logical, \code{TRUE} if factors are nested, \code{FALSE} otherwise.
#
# @note If factors are nested, a message is displayed to tell whether \code{f1}
#         is nested within \code{f2} or vice versa.
#
# @seealso \code{\link{is_crossed}}
#
# @references Grace, K. The Difference Between Crossed and Nested Factors. \href{http://www.theanalysisfactor.com/the-difference-between-crossed-and-nested-factors/}{(web)}
#
# @examples
# # nested factors, each category of
# # x appears in one category of y
# x <- c(1,2,3,4,5,6,7,8,9)
# y <- c(1,1,1,2,2,2,3,3,3)
# # show distribution
# table(x, y)
# # check if nested
# is_nested(x, y)
# is_nested(y, x)
#
# # not nested factors
# x <- c(1,2,3,4,5,6,7,8,9,1,2)
# y <- c(1,1,1,2,2,2,3,3,3,2,3)
# # show distribution
# table(x, y)
# # check if nested
# is_nested(x, y)
# is_nested(y, x)
#
#
is_nested <- function(f1, f2) {
  tab <- table(f1, f2)
  # cross tabulation of nested factors should have only 1 value per row
  # (or column) that is not zero. If we found more, factors are not nested
  # or rows and columns have to be swapped.
  # check if f1 is nested within f2
  nested <- !any(apply(tab, 1, function(x) sum(x != 0) > 1))
  if (nested) message("'f1' is nested within 'f2'")
  # swap rows and columns to check whether factors are nested
  # check whether f2 is nested within f1
  if (!nested) {
    nested <- !any(apply(tab, 2, function(x) sum(x != 0) > 1))
    if (nested) message("'f2' is nested within 'f1'")
  }
  return(nested)
}
# @title Check whether a factor has numeric levels only
# @name is_num_fac
# @description This function checks whether a factor has only numeric or
#                any non-numeric factor levels.
#
# @param x A \code{\link{factor}}.
# @return Logical, \code{TRUE} if factor has numeric factor levels only,
#           \code{FALSE} otherwise.
#
# @examples
# # numeric factor levels
# f1 <- factor(c(NA, 1, 3, NA, 2, 4))
# is_num_fac(f1)
#
# # not completeley numeric factor levels
# f2 <- factor(c(NA, "C", 1, 3, "A", NA, 2, 4))
# is_num_fac(f2)
#
# # not completeley numeric factor levels
# f3 <- factor(c("Justus", "Bob", "Peter"))
# is_num_fac(f3)
#
#
is_num_fac <- function(x) {
  # check if we have numeric levels
  return(!anyNA(suppressWarnings(as.numeric(levels(x)))))
}
# @title Check whether value is odd
# @name is_odd
#
# @description Checks whether \code{x} is an odd number or not. Only
#                accepts numeric vectors.
#
# @param x Numeric vector or single numeric value.
#
# @return \code{TRUE} for each odd value of \code{x}, \code{FALSE} for
#           even values.
#
# @seealso \code{\link{is_even}}
#
# @examples
# is_odd(4)
# is_odd(5)
# is_odd(1:4)
#
#
is_odd <- function(x) (x %% 2) == 1
# @title Create a labelled vector
# @name labelled
#
# @description A labelled vector is a common data structure in other statistical
# environments.
#
# @param x Vector to label. Must be either numeric or character.
# @param labels Named vector. The vector should be the same type as
#   \code{x}. Unlike factors, labels don't need to be exhaustive: only a fraction
#   of the values might be labelled.
# @param is_na Optionally, logical vector describing which levels should
#   be translated to missing values
#
# @note This method is derived from the \code{\link[haven]{labelled}} method
#         of the \pkg{haven} package. \pkg{haven} up to version 0.2 \emph{does not}
#         support the \code{is_na} attribute, however, the current
#         \href{github.com/hadley/haven}{dev-version} does. Some of the
#         \pkg{sjmisc} functions make use of this feature in advance, assuming
#         that the \code{labelled} class supported by the \pkg{haven} package
#         will be enhanced accordingly in a forthcoming update. Once the
#         \pkg{haven} package is updated and introducing the new \code{labelled}
#         class, this method might be removed.
#
# @examples
# # labelled vector with multiple types of missing values
# x <- labelled(c("M", "M", "F", "X", "N/A"),
#               c(Male = "M", Female = "F", Refused = "X", "Not applicable" = "N/A"),
#               c(FALSE, FALSE, TRUE, TRUE))
#
# x <- labelled(c(1, 2, 1, 5, 1, 5, 9),
#               c(Male = 1, Female = 2, Refused = 5, Missing = 9),
#               c(FALSE, FALSE, TRUE, TRUE))
#
#
labelled <- function(x, labels, is_na = NULL) {
  if (!is.numeric(x) && !is.character(x)) {
    stop("`x` must be either numeric or a character vector", call. = FALSE)
  }
  if (typeof(x) != typeof(labels)) {
    stop("`x` and `labels` must be same type", call. = FALSE)
  }
  if (is.null(labels)) {
    stop("`labels` must be a named vector", call. = FALSE)
  }
  if (is.null(is_na)) {
    is_na <- rep(FALSE, length(labels))
  } else {
    if (!is.logical(is_na) || length(is_na) != length(labels)) {
      stop("`is_na` must be a logical vector the same length as `labels`",
           call. = FALSE)
    }
  }

  structure(x,
            labels = labels,
            is_na = is_na,
            class = "labelled"
  )
}
# @title Row means with min amount of valid values
# @name mean_n
# @description This function is similar to the SPSS \code{MEAN.n} function and computes
#                row means from a \code{\link{data.frame}} or \code{\link{matrix}} if at least \code{n}
#                values of a row are valid (and not \code{\link{NA}}).
#
# @param dat \code{\link{data.frame}} with at least two columns, where row means are applied.
# @param n May either be
#          \itemize{
#            \item a numeric value that indicates the amount of valid values per row to calculate the row mean;
#            \item or a value between 0 and 1, indicating a proportion of valid values per row to calculate the row mean (see 'Details').
#          }
#          If a row's amount of valid values is less than \code{n}, \code{\link{NA}} will be returned as row mean value.
# @param digits Numeric value indicating the number of decimal places to be used for rounding mean
#          value. Negative values are allowed (see ‘Details’).
#
# @return A vector with row mean values of \code{df} for those rows with at least \code{n}
#           valid values. Else, \code{\link{NA}} is returned.
#
# @details Rounding to a negative number of \code{digits} means rounding to a power of
#            ten, so for example mean_n(df, 3, digits = -2) rounds to the
#            nearest hundred. \cr \cr
#          For \code{n}, must be a numeric value from \code{0} to \code{ncol(dat)}. If
#            a \emph{row} in \code{dat} has at least \code{n} non-missing values, the
#            row mean is returned. If \code{n} is a non-integer value from 0 to 1,
#            \code{n} is considered to indicate the proportion of necessary non-missing
#            values per row. E.g., if \code{n = .75}, a row must have at least \code{ncol(dat) * n}
#            non-missing values for the row mean to be calculated. See 'Examples'.
#
# @references \href{http://r4stats.com/2014/09/03/adding-the-spss-mean-n-function-to-r/}{r4stats.com}
#
# @examples
# dat <- data.frame(c1 = c(1,2,NA,4),
#                   c2 = c(NA,2,NA,5),
#                   c3 = c(NA,4,NA,NA),
#                   c4 = c(2,3,7,8))
#
# # needs at least 4 non-missing values per row
# mean_n(dat, 4) # 1 valid return value
#
# # needs at least 3 non-missing values per row
# mean_n(dat, 3) # 2 valid return values
#
# # needs at least 2 non-missing values per row
# mean_n(dat, 2)
#
# # needs at least 1 non-missing value per row
# mean_n(dat, 1) # all means are shown
#
# # needs at least 50% of non-missing values per row
# mean_n(dat, .5) # 3 valid return values
#
# # needs at least 75% of non-missing values per row
# mean_n(dat, .75) # 2 valid return values
#
#
mean_n <- function(dat, n, digits = 2) {
  # is 'n' indicating a proportion?
  digs <- n %% 1
  if (digs != 0) n <- round(ncol(dat) * digs)

  # coerce matrix to data frame
  if (is.matrix(dat)) dat <- as.data.frame(dat)

  # check if we have a data framme with at least two columns
  if (!is.data.frame(dat) || ncol(dat) < 2) {
    warning("`dat` must be a data frame with at least two columns.", call. = TRUE)
    return(NA)
  }

  # n may not be larger as df's amount of columns
  if (ncol(dat) < n) {
    warning("`n` must be smaller or equal to number of columns in data frame.", call. = TRUE)
    return(NA)
  }
  round(apply(dat, 1, function(x) ifelse(sum(!is.na(x)) >= n, mean(x, na.rm = TRUE), NA)), digits)
}
# @title Merge labelled data frames
# @name merge_df
#
# @description Merges (full join) two (or more) data frames and preserve value and variable labels.
#
# @param x1 First data frame to be merged.
# @param x2 Second data frame to be merged.
# @param ... More data frames to be merged.
# @param id Optional name for ID column that will be created to indicate the
#          source data frames for appended rows.
#
# @return A full joined data frame.
#
# @details This function merges two data frames, where equal named columns
#            will be joined together. This function is a convenient wrapper for
#            \code{merge(x1, x2, all = TRUE)}, however, unlike base
#            \code{\link{merge}}, this function preserves value and
#            variable labels. If matching columns have different value
#            label attributes, attributes from first data frame will be
#            used. For more details on the join operation, see
#            'Details' in \code{\link{merge}} on \code{all}-argument.
#
# @examples
# library(dplyr)
# data(efc)
# x1 <- efc %>% select(1:5) %>% slice(1:10)
# x2 <- efc %>% select(3:7) %>% slice(1:10)
#
# mydf <- merge_df(x1, x2)
# mydf
# str(mydf)
#
# \dontrun{
# library(sjPlot)
# view_df(mydf)}
#
# x3 <- efc %>% select(5:9) %>% slice(1:10)
# x4 <- efc %>% select(11:14) %>% slice(1:10)
#
# mydf <- merge_df(x1, x2, x3, x4, id = "subsets")
# mydf
# str(mydf)
#
#
merge_df <- function(x1, x2, ..., id = NULL) {
  # retrieve list of parameters
  more_dfs <- list(...)

  # first step, initial merge
  x_final <- merge_df_helper(x1, x2)

  # merge remaining df's if we have more data frames
  if (!is.null(more_dfs) && length(more_dfs) > 0) {
    # iterate all remaining data frames
    for (i in 1:length(more_dfs)) {
      # create ID vector
      x_final <- merge_df_helper(x_final, more_dfs[[i]])
    }
  }

  # create ID column?
  if (!is.null(id)) {
    # check whether column name already exists
    if (id %in% colnames(x_final)) {
      warning("Value of `id` already exists as column name. ID column was not created.", call. = F)
    } else {
      # create ID vector
      id_col <- c(rep(deparse(substitute(x1)), times = nrow(x1)),
                  rep(deparse(substitute(x2)), times = nrow(x2)))
      # do we have more data frames?
      if (!is.null(more_dfs) && length(more_dfs) > 0) {
        # iterate all remaining data frames
        for (i in 1:length(more_dfs)) {
          # create ID vector
          id_col <- c(id_col, rep(sprintf("%s_%i", deparse(substitute(x1)), i),
                                  times = nrow(more_dfs[[i]])))
        }
      }
      # bind id column
      x_final <- cbind(x_final, id_col)
      # name column
      colnames(x_final)[ncol(x_final)] <- id
    }
  }
  # return merged df
  x_final
}

merge_df_helper <- function(x1, x2) {
  # check if both data frames have same column names
  # in case, someone forgets that rbind exists...
  if (isTRUE(all.equal(sort(colnames(x1)), sort(colnames(x2))))) {
    return(rbind(x1, x2))
  }

  # find matching columns in both data frames
  x2_match <- match(colnames(x1), colnames(x2))
  x1_match <- which(!is.na(x2_match))
  # clean up NA
  x2_match <- x2_match[!is.na(x2_match)]

  # now we have the matching columns of x1 in x1_match
  # and of x2 in x2_match. Next, create empty data frame with
  # correct dimension to append rows of matching columns from x2
  # to x1
  tmp <- as.data.frame(matrix(nrow = nrow(x2), ncol = ncol(x1)))
  colnames(tmp) <- colnames(x1)
  tmp[, x1_match] <- x2[, x2_match]

  # x1_new has now all variables from x1, plus all variables
  # of x2 that also appear in x1
  x1_new <- rbind(x1, tmp)

  # which columns are still in x2 and have not been merged yet?
  # in certain cases, e.g. when we have no matching columns at all,
  # x2_match is of length 0. in this case, all columns are still remaining,
  # so we need to check this here
  if (is_empty(x2_match))
    x2_remain <- seq_len(ncol(x2))
  else
    x2_remain <- seq_len(ncol(x2))[-x2_match]

  # create dummy df for remaining data of x2
  tmp <- as.data.frame(matrix(nrow = nrow(x1), ncol = length(x2_remain)))
  colnames(tmp) <- colnames(x2)[x2_remain]

  # append rows of x2. now we have a data frame of same length as merged
  # x1 and x2
  tmp <- rbind(tmp, x2[, x2_remain])
  # copy attributes
  for (i in 1:length(x2_remain)) {
    attributes(tmp[[i]]) <- attributes(x2[, x2_remain[i]])
  }

  # final merge
  x_final <- cbind(x1_new, tmp)
  # return merged df
  x_final
}

# @title Mean Inter-Item-Correlation
# @name mic
# @description This function calculates a mean inter-item-correlation, i.e.
#                a correlation matrix of \code{data} will be computed (unless
#                \code{data} is already a matrix as returned by the
#                \code{\link{cor}}-function) and the mean
#                of the sum of all item's correlation values is returned.
#                Requires either a data frame or a computed \code{\link{cor}}-object.
#
# @param data A \code{matrix} as returned by the \code{\link{cor}}-function, or
#          a \code{data.frame}, where correlations will be calculated.
# @param cor.method Indicates the correlation computation method. May be one of
#          \code{"spearman"} (default), \code{"pearson"} or \code{"kendall"}.
#          You may use initial letter only.
# @return The value of the computed mean inter-item-correlation.
#
# @note \dQuote{Ideally, the average inter-item correlation for a set of
#          items should be between .20 and .40, suggesting that while the
#          items are reasonably homogenous, they do contain sufficiently
#          unique variance so as to not be isomorphic with each other.
#
#          When values are lower than .20, then the items may not be
#          representative of the same content domain. If values are higher than
#          .40, the items may be only capturing a small bandwidth of the construct.}
#          \emph{(Piedmont 2014)}
#
# @references Piedmont RL (2014) Inter-item Correlations. In: Michalos AC (eds)
#             Encyclopedia of Quality of Life and Well-Being Research.
#             Dordrecht: Springer, 3303-3304
#             \doi{10.1007/978-94-007-0753-5_1493}
#
# @examples
# # Data from the EUROFAMCARE sample dataset
# data(efc)
# # recveive first item of COPE-index scale
# start <- which(colnames(efc) == "c82cop1")
# # recveive last item of COPE-index scale
# end <- which(colnames(efc) == "c90cop9")
# # create data frame with COPE-index scale
# mydat <- data.frame(efc[, c(start:end)])
#
# mic(mydat)
#
#' @importFrom stats cor na.omit
#
mic <- function(data, cor.method = c("pearson", "spearman", "kendall")) {
  # Check parameter
  cor.method <- match.arg(cor.method)

  # Mean-interitem-corelation
  if (class(data) == "matrix") {
    corr <- data
  } else {
    data <- stats::na.omit(data)
    corr <- stats::cor(data, method = cor.method)
  }

  # Sum up all correlation values
  meanic <- c()
  for (j in 1:(ncol(corr) - 1)) {
    # first correlation is always "1" (self-correlation)
    for (i in (j + 1):nrow(corr)) {
      # check for valid bound
      if (i <= nrow(corr) && j <= ncol(corr)) {
        # add up all subsequent values
        meanic <- c(meanic, corr[i, j])
      } else {
        meanic <- c(meanic, "NA")
      }
    }
  }
  return(mean(meanic))
}
# @title Mann-Whitney-U-Test
# @name mwu
# @description This function performs a Mann-Whitney-U-Test (or \code{Wilcoxon rank sum test},
#                see \code{\link{wilcox.test}} and \code{\link[coin]{wilcox_test}})
#                for \code{x}, for each group indicated by \code{grp}. If \code{grp}
#                has more than two categories, a comparison between each combination of
#                two groups is performed. \cr \cr
#                The function reports U, p and Z-values as well as effect size r
#                and group-rank-means.
#
# @param x Numeric vector / variable, where the Mann-Whitney-U-Test should be applied to
# @param grp Grouping variable indicating the groups that should be used for comparison
# @param distribution Indicates how the null distribution of the test statistic should be computed.
#          May be one of \code{"exact"}, \code{"approximate"} or \code{"asymptotic"}
#          (default). See \code{\link[coin]{wilcox_test}} for details.
# @param weights Integer valued weights for the observations. By default,
#          this is \code{NULL}.
# @return (Invisibly) returns a data frame with U, p and Z-values for each group-comparison
#         as well as effect-size r; additionally, group-labels and groups' n's are
#         also included.
#
# @note This function calls the \code{\link[coin]{wilcox_test}} with formula. If \code{grp}
#         has more than two groups, additionally a Kruskal-Wallis-Test (see \code{\link{kruskal.test}})
#         is performed. \cr \cr
#         Interpretation of effect sizes, as a rule-of-thumb:
#         \itemize{
#          \item small effect >= 0.1
#          \item medium effect >= 0.3
#          \item large effect >= 0.5
#        }
#
# @examples
# data(efc)
# # Mann-Whitney-U-Tests for elder's age by elder's dependency.
# mwu(efc$e17age, efc$e42dep)
#
#' @importFrom stats na.omit wilcox.test kruskal.test
#' @importFrom coin wilcox_test pvalue statistic
#
mwu <- function(x, grp, distribution = "asymptotic", weights = NULL) {
  # check if suggested package is available
  if (!requireNamespace("coin", quietly = TRUE)) {
    stop("Package 'coin' needed for this function to work. Please install it.", call. = FALSE)
  }
  # coerce factor and character to numeric
  if (is.factor(grp) || is.character(grp)) grp <- to_value(grp)
  # group "counter" (index) should start with 1, not 0
  if (min(grp, na.rm = TRUE) == 0) grp <- grp + 1
  # retrieve unique group values. need to iterate all values
  grp_values <- sort(unique(stats::na.omit(grp)))
  # length of value range
  cnt <- length(grp_values)
  labels <- get_labels(grp,
                       attr.only = F,
                       include.values = NULL,
                       include.non.labelled = T)
  message("Performing Mann-Whitney-U-Test...")
  message("---------------------------------")
  message("showing statistics between groups (x|y)")
  df <- data.frame()
  for (i in 1:cnt) {
    for (j in i:cnt) {
      if (i != j) {
        # retrieve cases (rows) of subgroups
        xsub <- x[which(grp == grp_values[i] | grp == grp_values[j])]
        ysub <- grp[which(grp == grp_values[i] | grp == grp_values[j])]
        # only use rows with non-missings
        ysub <- ysub[which(!is.na(xsub))]
        # adjust weights, pick rows from subgroups (see above)
        if (!is.null(weights)) {
          wsub <- as.integer(stats::na.omit(weights[which(!is.na(xsub))]))
        }
        # remove missings
        xsub <- as.numeric(stats::na.omit(xsub))
        ysub.n <- stats::na.omit(ysub)
        # grouping variable is a factor
        ysub <- as.factor(ysub.n)
        if (is.null(weights)) {
          wt <- coin::wilcox_test(xsub ~ ysub, distribution = distribution)
        } else {
          wt <- coin::wilcox_test(xsub ~ ysub,
                                  distribution = distribution,
                                  weights = as.formula("~wsub"))
        }
        # compute statistics
        u <- as.numeric(coin::statistic(wt, type = "linear"))
        z <- as.numeric(coin::statistic(wt, type = "standardized"))
        p <- coin::pvalue(wt)
        r <- abs(z / sqrt(length(x)))
        w <- stats::wilcox.test(xsub, ysub.n, paired = TRUE)$statistic
        rkm.i <- mean(rank(xsub)[which(ysub.n == grp_values[i])], na.rm = TRUE)
        rkm.j <- mean(rank(xsub)[which(ysub.n == grp_values[j])], na.rm = TRUE)
        # compute n for each group
        n_grp1 <- length(xsub[which(ysub.n == grp_values[i])])
        n_grp2 <- length(xsub[which(ysub.n == grp_values[j])])
        # print to console
        if (is.null(labels)) {
          cat(sprintf("Groups (%i|%i), n = %i/%i:\n", grp_values[i],
                      grp_values[j], n_grp1, n_grp2))
        } else {
          cat(sprintf("Groups %i = %s (n = %i) | %i = %s (n = %i):\n",
                      grp_values[i], labels[i], n_grp1, grp_values[j],
                      labels[j], n_grp2))
        }
        if (p < 0.001) {
          p <- 0.001
          p.string <- "<"
        } else {
          p.string <- "="
        }
        cat(sprintf("  U = %.3f, W = %.3f, p %s %.3f, Z = %.3f\n  effect-size r = %.3f\n  rank-mean(%i) = %.2f\n  rank-mean(%i) = %.2f\n\n", u, w, p.string, p, z, r, i, rkm.i, j, rkm.j))
        df <- rbind(df, cbind(grp1 = grp_values[i], grp1.label = labels[i],
                              grp1.n = n_grp1, grp2 = grp_values[j], grp2.label = labels[j],
                              grp2.n = n_grp2, u = u, w = w, p = p, z = z, r = r,
                              rank.mean.grp1 = rkm.i, rank.mean.grp2 = rkm.j))
      }
    }
  }
  # if we have more than 2 groups, also perfom kruskal-wallis-test
  if (cnt > 2) {
    message("Performing Kruskal-Wallis-Test...")
    message("---------------------------------")
    kw <- stats::kruskal.test(x, grp)
    cat(sprintf("chi-squared = %.3f\n", kw$statistic))
    cat(sprintf("df = %i\n", kw$parameter))
    if (kw$p.value < 0.001) {
      p  <- 0.001
      p.string <- "<"
    } else {
      p <- kw$p.value
      p.string <- "="
    }
    cat(sprintf("p %s %.3f\n", p.string, p))
  }
  # prepare a data frame that can be used for 'sjt.df'.
  tab.df <- data.frame(Groups = sprintf("%s<br>%s", df$grp1.label, df$grp2.label),
                       N = sprintf("%s<br>%s", df$grp1.n, df$grp2.n),
                       'Mean Rank' = sprintf("%.2f<br>%.2f",
                                             as.numeric(as.character(df$rank.mean.grp1)),
                                             as.numeric(as.character(df$rank.mean.grp2))),
                       'Mann-Whitney-U' = df$u,
                       'Wilcoxon-W' = df$w,
                       Z = sprintf("%.3f", as.numeric(as.character(df$z))),
                       'Effect Size' = sprintf("%.3f", as.numeric(as.character(df$r))),
                       p = sprintf("%.3f", as.numeric(as.character(df$p))))
  # replace 0.001 with <0.001
  levels(tab.df$p)[which(levels(tab.df$p) == "0.001")] <- "<0.001"
  # return both data frames
  invisible(structure(class = "mwu",list(df = df, tab.df = tab.df)))
}
# @title Check overdispersion of GL(M)M's
# @name overdisp
# @description This function checks generalized linear (mixed) models for
#                overdispersion.
#
# @param x Fitted GLMM (\code{\link[lme4]{merMod}}-class) or \code{glm} model.
# @param trafo A specification of the alternative, can be numeric or a
#          (positive) function or \code{NULL} (the default). See 'Details'
#          in \code{\link[AER]{dispersiontest}} in package \pkg{AER}. Does not
#          apply to \code{merMod} objects.
#
# @return Information on the overdispersion test.
#
# @note The interpretation of the returned p-value differs between GLM and
#         GLMM. For GLMs, a p-value < .05 indicates overdispersion, while
#         for GLMMs, a p-value > .05 indicates overdispersion.
#
# @details For \code{merMod}-objects, this function is based on the code in the
#            \href{http://glmm.wikidot.com/faq}{DRAFT r-sig-mixed-models FAQ},
#            section \emph{How can I deal with overdispersion in GLMMs?}.
#            Note that this function only returns an \emph{approximate} estimate
#            of an overdispersion parameter.
#            \cr \cr
#            For \code{glm}'s, this function simply wraps the \code{dispersiontest}
#            from the \pkg{AER}-package.
#
# @references \href{http://glmm.wikidot.com/faq}{DRAFT r-sig-mixed-models FAQ}
#
# @examples
# data(efc)
#
# # response has many zero-counts, poisson models
# # might be overdispersed
# barplot(table(efc$tot_sc_e))
#
# fit <- glm(tot_sc_e ~ neg_c_7 + e42dep + c160age,
#            data = efc, family = poisson)
# overdisp(fit)
#
# library(lme4)
# efc$e15relat <- to_factor(efc$e15relat)
# fit <- glmer(tot_sc_e ~ neg_c_7 + e42dep + c160age + (1 | e15relat),
#              data = efc, family = poisson)
# overdisp(fit)
#
#
#' @importFrom stats df.residual residuals pchisq
#
overdisp <- function(x, trafo = NULL) {
  if (str_contains(class(x), "merMod", ignore.case = TRUE))
    return(overdisp.lme4(x))
  else
    return(overdisp.default(x, trafo))
}

overdisp.default <- function(x, trafo) {
  # check if suggested package is available
  if (!requireNamespace("AER", quietly = TRUE)) {
    stop("Package `AER` needed for this function to work. Please install it.", call. = FALSE)
  }
  result <- AER::dispersiontest(x, trafo = trafo, alternative = "greater")
  print(result)
  if (result$p.value < 0.05)
    message("Overdispersion detected.")
  else
    message("No overdispersion detected.")
  return(invisible(result))
}


overdisp.lme4 <- function(x) {
  # check if suggested package is available
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needed for this function to work. Please install it.", call. = FALSE)
  }

  # check object class
  if (any(class(x) == "glmerMod")) {
    rdf <- stats::df.residual(x)
    rp <- stats::residuals(x, type = "pearson")
    Pearson.chisq <- sum(rp ^ 2)
    prat <- Pearson.chisq / rdf
    pval <- stats::pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
    cat(sprintf("\n        Overdispersion test\n\ndispersion ratio = %.4f, p-value = %.4f\n\n",
                prat, pval))
    if (pval > 0.05)
      message("No overdispersion detected.")
    else
      message("Overdispersion detected.")
    return(invisible(list(
      chisq = Pearson.chisq,
      ratio = prat,
      rdf = rdf,
      p = pval
    )))
  } else {
    warning("This method currently only supports `glmer` fitted models.", call. = F)
  }
}
# @title Phi value for contingency tables
# @name phi
# @description Compute Phi value for a contingency table.
#
# @seealso \code{\link{cramer}}

# @param tab A \code{\link{table}} or \code{\link{ftable}}. Tables of class
#          \code{\link{xtabs}} and other will be coerced to \code{\link{ftable}}
#          objects.
#
# @return The table's Phi value.
#
# @examples
# tab <- table(sample(1:2, 30, TRUE), sample(1:2, 30, TRUE))
# phi(tab)
#
#' @importFrom MASS loglm
#
phi <- function(tab) {
  # convert to flat table
  if (all(class(tab) != "ftable")) tab <- ftable(tab)
  tb <- summary(MASS::loglm(~1 + 2, tab))$tests
  phi_val <- sqrt(tb[2, 1] / sum(tab))
  return(phi_val)
}
# @title Tjur's Coefficient of Discrimination
# @name cod
#
# @description This method calculates the Coefficient of Discrimination \code{D}
#                for generalized linear (mixed) models for binary data. It is
#                an alternative to other Pseudo-R-squared values
#                like Nakelkerke's R2 or Cox-Snell R2.
#
# @param x Fitted \code{\link{glm}} or \code{\link[lme4]{glmer}} model.
#
# @return The \code{D} Coefficient of Discrimination, also known as
#           Tjur's R-squared value.
#
# @note The Coefficient of Discrimination \code{D} can be read like any
#         other (Pseudo-)R-squared value.
#
# @references Tjur T (2009) Coefficients of determination in logistic regression models -
#               a new proposal: The coefficient of discrimination. The American Statistician,
#               63(4): 366-372
#
# @seealso \code{\link{r2}} for Nagelkerke's and Cox and Snell's pseudo
#            r-squared coefficients.
#
# @examples
# data(efc)
#
# # Tjur's R-squared value
# efc$services <- dicho(efc$tot_sc_e, "v", 0, as.num = TRUE)
# fit <- glm(services ~ neg_c_7 + c161sex + e42dep,
#            data = efc, family = binomial(link = "logit"))
# cod(fit)
#
#' @importFrom stats predict predict.glm residuals
#
cod <- function(x) {
  # check for valid object class
  if (!any(class(x) == "glmerMod") && !any(class(x) == "glm")) {
    stop("'x' must be an object of class 'glm' or 'glmerMod'.", call. = F)
  }

  # mixed models (lme4)
  if (any(class(x) == "glmerMod")) {
    # check for package availability
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
    }
    y <- lme4::getME(x, "y")
    pred <- stats::predict(x, type = "response", re.form = NULL)
  } else {
    y <- x$y
    pred <- stats::predict.glm(x, type = "response")
  }
  # delete pred for cases with missing residuals
  if (anyNA(stats::residuals(x))) pred <- pred[!is.na(stats::residuals(x))]

  categories <- unique(y)
  m1 <- mean(pred[which(y == categories[1])], na.rm = T)
  m2 <- mean(pred[which(y == categories[2])], na.rm = T)

  cod = abs(m2 - m1)
  names(cod) <- "D"

  return(structure(class = "sjmisc_r2", list(cod = cod)))
}



# @title Compute R-squared of (generalized) linear (mixed) models
# @name r2
#
# @description Compute R-squared values of linear (mixed) models, or
#                pseudo-R-squared values for generalized linear (mixed) models.
#
# @param x Fitted model of class \code{lm}, \code{glm}, \code{lmerMod}/\code{lme}
#            or \code{glmerMod}.
# @param n Optional, a \code{lmerMod} object, representing the fitted null-model
#          to \code{x} (unconditional model). If \code{n} is given, the pseudo-r-squared
#          for random intercept and random slope variances are computed (see Kwok et al. 2008;
#          see 'Examples' and 'Details').
#
# @return \itemize{
#           \item For linear models, the r-squared and adjusted r-squared values.
#           \item For linear mixed models, the r-squared and Omega-squared values.
#           \item For \code{glm} objects, Cox & Snell's and Nagelkerke's pseudo r-squared values.
#           \item For \code{glmerMod} objects, Tjur's coefficient of determination.
#         }
#
# @details If \code{n} is given, the Pseudo-R2 statistic is the proportion of
#          explained variance in the random effect after adding co-variates or
#          predictors to the model, or in short: the proportion of the explained
#          variance in the random effect of the full (conditional) model \code{x}
#          compared to the null (unconditional) model \code{n}.
#
# @note For linear models, the r-squared and adjusted r-squared value is returned,
#         as provided by the \code{summary}-function.
#         \cr \cr
#         For linear mixed models, an r-squared approximation by computing the
#         correlation between the fitted and observed values, as suggested by
#         Byrnes (2008), is returned as well as the Omega-squared value as
#         suggested by Xu (2003), unless \code{n} is specified. If \code{n}
#         is given, pseudo r-squared measures based on the variances of random
#         intercept (tau 00, between-group-variance) and random slope (tau 11,
#         random-slope-variance) are returned.
#         \cr \cr
#         For generalized linear models, Cox & Snell's and Nagelkerke's
#         pseudo r-squared values are returned.
#         \cr \cr
#         For generalized linear mixed models, the coefficient of determination
#         as suggested by Tjur (2009) (see also \code{\link{cod}}).
#
# @references \itemize{
#               \item \href{http://glmm.wikidot.com/faq}{DRAFT r-sig-mixed-models FAQ}
#               \item Byrnes, J. 2008. Re: Coefficient of determination (R^2) when using lme(). \href{http://thread.gmane.org/gmane.comp.lang.r.lme4.devel/684}{gmane.comp.lang.r.lme4.devel}
#               \item Kwok OM, Underhill AT, Berry JW, Luo W, Elliott TR, Yoon M. 2008. Analyzing Longitudinal Data with Multilevel Models: An Example with Individuals Living with Lower Extremity Intra-Articular Fractures. Rehabilitation Psychology 53(3): 370–86 (\doi{10.1037/a0012765})
#               \item Xu, R. 2003. Measuring explained variation in linear mixed effects models. Statist. Med. 22:3527-3541. \doi{10.1002/sim.1572}
#               \item Tjur T. 2009. Coefficients of determination in logistic regression models - a new proposal: The coefficient of discrimination. The American Statistician, 63(4): 366-372
#             }
#
# @examples
# library(lme4)
# fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
# r2(fit)
#
# data(efc)
# fit <- lm(barthtot ~ c160age + c12hour, data = efc)
# r2(fit)
#
# # Pseudo-R-squared values
# efc$services <- dicho(efc$tot_sc_e, "v", 0, as.num = TRUE)
# fit <- glm(services ~ neg_c_7 + c161sex + e42dep,
#            data = efc, family = binomial(link = "logit"))
# r2(fit)
#
# # Pseudo-R-squared values for random effect variances
# fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
# fit.null <- lmer(Reaction ~ 1 + (Days | Subject), sleepstudy)
# r2(fit, fit.null)
#
#
#' @importFrom stats model.response model.frame fitted var residuals
#
r2 <- function(x, n = NULL) {
  rsq <- NULL
  osq <- NULL
  adjr2 <- NULL
  # do we have a glm? if so, report pseudo_r2
  if (any(class(x) == "glm")) {
    return(pseudo_ralt(x))
    # do we have a glmer?
  } else if (any(class(x) == "glmerMod")) {
    return(cod(x))
    # do we have a simple linear model?
  } else if (identical(class(x), "lm")) {
    rsq <- summary(x)$r.squared
    adjr2 <- summary(x)$adj.r.squared
    # name vectors
    names(rsq) <- "R2"
    names(adjr2) <- "adj.R2"
    # return results
    return(structure(class = "sjmisc_r2", list(r2 = rsq, adjr2 = adjr2)))
    # else do we have a mixed model?
  } else if (str_contains(class(x), pattern = c("lmerMod", "lme"),
                          ignore.case = T, logic = "OR")) {
    # do we have null model?
    if (!is.null(n)) {
      # compute tau for both models
      tau_full <- icc(x)
      tau_null <- icc(n)
      # get taus. tau.00 is the random intercept variance, i.e. for growth models,
      # the difference in the outcome's mean at first time point
      rsq0 <- (attr(tau_null, "tau.00") - attr(tau_full, "tau.00")) / attr(tau_null, "tau.00")
      # tau.11 is the variance of the random slopes, i.e. how model predictors
      # affect the trajectory of subjects over time (for growth models)
      rsq1 <- (attr(tau_null, "tau.11") - attr(tau_full, "tau.11")) / attr(tau_null, "tau.11")
      # if model has no random slope, we need to set this value to NA
      if (is.null(rsq1) || is_empty(rsq1)) rsq1 <- NA
      # name vectors
      names(rsq0) <- "R2(tau-00)"
      names(rsq1) <- "R2(tau-11)"
      # return results
      return(structure(class = "sjmisc_r2", list(r2_tau00 = rsq0, r2_tau11 = rsq1)))
    } else {
      # compute "correlation"
      lmfit <-  lm(stats::model.response(stats::model.frame(x)) ~ stats::fitted(x))
      # get r-squared
      rsq <- summary(lmfit)$r.squared
      # get omega squared
      osq <- 1 - stats::var(stats::residuals(x)) / stats::var(stats::model.response(stats::model.frame(x)))
      # name vectors
      names(rsq) <- "R2"
      names(osq) <- "O2"
      # return results
      return(structure(class = "sjmisc_r2", list(r2 = rsq, o2 = osq)))
    }
  } else {
    stop("`r2` only works on linear (mixed) models of class \"lm\", \"lme\" or \"lmerMod\".", call. = F)
    return(NULL)
  }
}

pseudo_ralt <- function(x) {
  n <- nrow(x$model)
  CoxSnell <- 1 - exp((x$deviance - x$null) / n)
  Nagelkerke <- CoxSnell / (1 - exp(-x$null / n))
  names(CoxSnell) <- "CoxSnell"
  names(Nagelkerke) <- "Nagelkerke"
  return(structure(class = "sjmisc_r2", list(CoxSnell = CoxSnell, Nagelkerke = Nagelkerke)))
}
# @title Import SPSS dataset as data frame into R
# @name read_spss
#
# @description Import data from SPSS, including NA's, value and variable labels.
#
# @seealso \itemize{
#            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#            \item \href{http://www.strengejacke.de/sjPlot/labelleddata/}{sjPlot-manual: working with labelled data}
#            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#            \item \code{\link{write_spss}}
#            }
#
# @param path File path to the data file.
# @param enc File encoding of the data file. See 'Details'.
# @param attach.var.labels Logical, if \code{TRUE}, variable labels will automatically be
#          added to each variable as \code{"variable.label"} attribute. Use this
#          parameter, if \code{option = "foreign"}, where variable labels are added
#          as list-attribute to the imported data frame.
#          \emph{Not needed if \code{option = "haven"} (default).}
# @param atomic.to.fac Logical, if \code{TRUE}, factor variables imported from
#          the dataset (which are imported as \code{\link{atomic}}) will be converted
#          to \code{\link{factor}}s.
# @param keep.na Logical, if \code{TRUE}, user-defined missing values will be
#          left as their original codes. If \code{FALSE} (default), corresponding
#          values are converted to \code{NA}.
# @param option String, indicating which package will be used to read the SPSS data file.
#          By default, \code{option = "haven"}, which means, the \code{read_spss} function
#          from the \pkg{haven} package is used. Use \code{option = "foreign"} to
#          use foreign's \code{\link[foreign]{read.spss}} function. Use \code{options(read_spss = "foreign")}
#          to make this function always use the \pkg{foreign} package \code{\link[foreign]{read.spss}} function.
# @return A data frame containing the SPSS data. Retrieve value labels with \code{\link{get_labels}}
#   and variable labels with \code{\link{get_label}}.
#
# @note This is a wrapper function for \code{\link[haven]{read_spss}} of the
#         \pkg{haven} package and \code{\link[foreign]{read.spss}} of the
#         \pkg{foreign} package. This function adds value and variable
#         labels as attributes to the imported variables of the data frame.
#         \cr \cr
#         Most functions of the \pkg{sjPlot} package access value and variable label
#         attributes to automatically detect labels in order to set them as axis,
#         legend or title labels in plots (\code{sjp.}-functions) respectively as
#         column or row headers in table outputs (\code{sjt.}-functions).  See
#         \href{http://www.strengejacke.de/sjPlot/datainit/}{online manual}
#         for more details.
#         \cr \cr
#         When working with labelled data, you can, e.g., use
#         \code{\link{get_label}} or \code{\link{get_labels}}
#         to get a vector of value and variable labels, which can then be
#         used with other functions like \code{\link{barplot}} etc.
#         See 'Examples' from \code{\link{get_labels}}.
#
# @details In some cases, column names of the imported data set are not properly
#            encoded. Use the \code{enc}-argument to specify the character
#            encoding for the SPSS data set (like \code{enc = "UTF-8"}, see
#            \code{\link{Encoding}}).
#
# @examples
# \dontrun{
# # import SPSS data set. uses haven's read function
# # by default
# mydat <- read_spss("my_spss_data.sav")
#
# # use foreign's read function
# mydat <- read_spss("my_spss_data.sav",
#                    enc = "UTF-8",
#                    option = "foreign")
#
# # use haven's read function, convert atomic to factor
# mydat <- read_spss("my_spss_data.sav", atomic.to.fac = TRUE)
#
# # retrieve variable labels
# mydat.var <- get_label(mydat)
#
# # retrieve value labels
# mydat.val <- get_labels(mydat)}
#
#' @importFrom haven read_spss
#
read_spss <- function(path,
                      enc = NA,
                      attach.var.labels = FALSE,
                      atomic.to.fac = FALSE,
                      keep.na = FALSE,
                      option = "haven") {

  # check read_spss option
  if (is.null(option)) {
    opt <- getOption("read_spss")
    if (is.null(opt) || opt == "foreign") {
      option <- "foreign"
    } else {
      option <- "haven"
    }
  }

  # check parameter
  if (!is.null(option) && option != "foreign" && option != "haven") {
    warning("`option` must be either `foreign` or `haven`. Defaulting to `foreign`.", call. = F)
    option <- "foreign"
  }

  # foreign import
  if (option == "foreign") {
    # check if suggested package is available
    if (!requireNamespace("foreign", quietly = TRUE)) {
      stop("Package 'foreign' needed for this function to work. Please install it.", call. = FALSE)
    }
    # import data as data frame
    data.spss <- suppressWarnings(foreign::read.spss(path,
                                                     to.data.frame = TRUE,
                                                     use.value.labels = FALSE,
                                                     use.missings = !keep.na,
                                                     reencode = enc))
    # convert atomic values to factors
    if (atomic.to.fac) data.spss <- atomic_to_fac(data.spss, getValLabelAttribute(data.spss))
    # auto attach labels
    if (attach.var.labels) {
      # message("Attaching variable labels. Please wait...\n")
      data.spss <- set_label(data.spss, get_label(data.spss))
    }
  } else {
    # check if suggested package is available
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Package 'haven' needed for this function to work. Please install it.", call. = FALSE)
    }
    # read data file
    data.spss <- haven::read_spss(path)
    # encoding?
    if (!is.na(enc) && !is.null(enc)) Encoding(colnames(data.spss)) <- enc
    # convert NA
    if (!keep.na) data.spss <- to_na(data.spss)
    # convert to sjPlot
    data.spss <- unlabel(data.spss)
    # convert atomic values to factors
    if (atomic.to.fac) data.spss <- atomic_to_fac(data.spss, getValLabelAttribute(data.spss))
  }
  # return data frame
  return(data.spss)
}


# converts atomic numeric vectors into factors with
# numerical factor levels
#' @importFrom utils txtProgressBar setTxtProgressBar
atomic_to_fac <- function(data.spss, attr.string) {
  # check for valid attr.string
  if (!is.null(attr.string)) {
    # create progress bar
    # pb <- utils::txtProgressBar(min = 1, max = ncol(data.spss), style = 3)
    # tell user...
    # message("Converting atomic to factors. Please wait...\n")
    # iterate all columns
    for (i in 1:ncol(data.spss)) {
      # copy column to vector
      x <- data.spss[[i]]
      # capture labels attribute first
      labs <- attr(x, attr.string, exact = T)
      varlab <- attr(x,'label',exact=T)
      # is atomic, which was factor in SPSS?
      if (is.atomic(x) && !is.null(labs)) {
        # so we have value labels (only typical for factors, not
        # continuous variables) and a variable of type "atomic" (SPSS
        # continuous variables would be imported as numeric) - this
        # indicates we have a factor variable. now we convert to
        # factor
        x <- as.factor(x)
        # set back labels attribute
        attr(x, attr.string) <- labs
        attr(x, 'label') <- varlab
        # copy vector back to data frame
        data.spss[[i]] <- x
      }
      # update progress bar
      # utils::setTxtProgressBar(pb, i)
    }
    # close(pb)
  }
  return(data.spss)
}


# @title Import SAS dataset as data frame into R
# @name read_sas
#
# @description Imports data from SAS (\code{.sas7bdat}), including NA's,
#                value and variable labels.
#
# @seealso \code{\link{read_spss}}
#
# @param path.cat Optional, the file path to the SAS catalog file.
# @return A data frame containing the SAS data. Retrieve value labels with \code{\link{get_labels}}
#   and variable labels with \code{\link{get_label}}.
#
# @inheritParams read_spss
#
# @note This is a wrapper function for \code{\link[haven]{read_sas}} function of the
#         \pkg{haven} package. This function converts the imported data
#         into a common class format (see \code{\link{unlabel}}).
#
#' @importFrom haven read_sas
#
read_sas <- function(path, path.cat = NULL, atomic.to.fac = FALSE) {
  # check if suggested package is available
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package 'haven' needed for this function to work. Please install it.", call. = FALSE)
  }
  # read data file
  data <- haven::read_sas(path, path.cat)
  # convert to sjPlot
  data <- unlabel(data)
  # convert atomic values to factors
  if (atomic.to.fac) data <- atomic_to_fac(data, getValLabelAttribute(data))
  # return data frame
  return(data)
}


# @title Import STATA dataset as data frame into R
# @name read_stata
#
# @description Imports data from STATA dta-files, including NA's,
#                value and variable labels.
#
# @seealso \code{\link{read_spss}}
#
# @inheritParams read_spss
#
# @return A data frame containing the STATA data. Retrieve value labels with \code{\link{get_labels}}
#   and variable labels with \code{\link{get_label}}.
#
# @note This is a wrapper function for \code{\link[haven]{read_dta}} function of the
#         \pkg{haven} package. This function converts the imported data
#         into a common class format (see \code{\link{unlabel}}).
#
#' @importFrom haven read_dta
#
read_stata <- function(path, atomic.to.fac = FALSE) {
  # check if suggested package is available
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package 'haven' needed for this function to work. Please install it.", call. = FALSE)
  }
  # read data file
  data <- haven::read_dta(path)
  # convert to sjPlot
  data <- unlabel(data)
  # convert atomic values to factors
  if (atomic.to.fac) data <- atomic_to_fac(data, getValLabelAttribute(data))
  # return data frame
  return(data)
}


# @title Write content of data frame to SPSS sav-file
# @name write_spss
#
# @description This function saves the content of a data frame to an SPSS sav-file.
#
# @seealso \itemize{
#            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#            \item \code{\link{read_spss}}
#            }
#
# @note You don't need to take care whether variables have been imported with
#         the \code{\link{read_spss}} function from this package or from \pkg{haven}
#         or even the \pkg{foreign} package, or if you have imported SPSS data and
#         created new variables. This function does all necessary data preparation
#         to write a properly labelled SPSS sav file.
#
# @param x \code{data.frame} that should be saved as file.
# @param path File path of the output file.
# @param enc.to.utf8 Logical, if \code{TRUE}, character encoding of variable and
#          value labels will be converted to UTF-8.
#
#
write_spss <- function(x, path, enc.to.utf8=FALSE) {
  write_data(x = x, path = path, type = "spss", enc.to.utf8 = enc.to.utf8)
}


# @title Write content of data frame to STATA dta-file
# @name write_stata
#
# @description This function saves the content of a data frame to an STATA dta-file.
#
# @seealso \code{\link{write_spss}}
#
# @note You don't need to take care whether variables have been imported with
#         the \code{\link{read_stata}} function from this package or from \pkg{haven},
#         or if you have imported STATA data and
#         created new variables. This function does all necessary data preparation
#         to write a properly labelled STATA file.
#
# @inheritParams write_spss
#
#
write_stata <- function(x, path, enc.to.utf8 = TRUE) {
  write_data(x = x, path = path, type = "stata", enc.to.utf8 = enc.to.utf8)
}


#' @importFrom haven write_sav write_dta
#' @importFrom utils txtProgressBar setTxtProgressBar
write_data <- function(x, path, type, enc.to.utf8) {
  # check if suggested package is available
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package 'haven' needed for this function to work. Please install it.", call. = FALSE)
  }

  # create progress bar
  # pb <- utils::txtProgressBar(min = 1, max = ncol(x), style = 3)
  # tell user...
  # message(sprintf("Writing %s file...", type))
  # check if variables should be converted to factors
  for (i in 1:ncol(x)) {
    # get value and variable labels
    val.lab <- get_labels(x[[i]], include.values = "n")
    var.lab <- get_label(x[[i]])
    # Encode to UTF-8
    if (enc.to.utf8) {
      if (!is.null(val.lab)) x[[i]] <- set_labels(x[[i]], enc2utf8(val.lab))
      if (!is.null(var.lab)) x[[i]] <- set_label(x[[i]], enc2utf8(var.lab))
    }
    # haven labelled objects don't need conversion
    if (!is_labelled(x[[i]])) {
      # convert variable to labelled factor, so it can be saved
      x[[i]] <- suppressWarnings(to_label(x[[i]]))
      # set back variable label
      x[[i]] <- set_label(x[[i]], var.lab, "label")
    }
    # update progress bar
    # utils::setTxtProgressBar(pb, i)
  }
  # hide pb
  # close(pb)
  if (type == "spss") {
    # tell user
    # message(sprintf("Writing %s file to '%s'. Please wait...\n", type, path))
    # write SPSS
    haven::write_sav(x, path)
  } else if (type == "stata") {
    # tell user
    # message(sprintf("Writing %s file to '%s'. Please wait...\n", type, path))
    # write Stata
    haven::write_dta(x, path)
  }
}
# @title Recode variable categories into new values
# @name recode_to
#
# @description Recodes (or "renumbers") the categories of \code{var} into new category values, beginning
#                with the lowest value specified by \code{lowest}. Useful if you want
#                to recode dummy variables with 1/2 coding to 0/1 coding, or recoding scales from
#                1-4 to 0-3 etc.
#
# @seealso \code{\link{rec}} for general recoding of variables and \code{\link{set_na}}
#            for setting \code{\link{NA}} values.
#
# @param x Variable (vector), \code{data.frame} or \code{list} of variables that should be recoded.
# @param lowest Indicating the lowest category value for recoding. Default is 0, so the new
#          variable starts with value 0.
# @param highest If specified and larger than \code{lowest}, all category values larger than
#          \code{highest} will be set to \code{NA}. Default is \code{-1}, i.e. this argument is ignored
#          and no NA's will be produced.
# @return A new variable with recoded category values, where \code{lowest} indicates the lowest
#           value; or a data frame or list of variables where variables have
#           been recoded as described.
#
# @note Value and variable label attributes (see, for instance, \code{\link{get_labels}}
#         or \code{\link{set_labels}}) are preserved.
#
# @examples
# # recode 1-4 to 0-3
# dummy <- sample(1:4, 10, replace = TRUE)
# recode_to(dummy)
#
# # recode 3-6 to 0-3
# # note that numeric type is returned
# dummy <- as.factor(3:6)
# recode_to(dummy)
#
# # lowest value starting with 1
# dummy <- sample(11:15, 10, replace = TRUE)
# recode_to(dummy, 1)
#
# # lowest value starting with 1, highest with 3
# # all others set to NA
# dummy <- sample(11:15, 10, replace = TRUE)
# recode_to(dummy, 1, 3)
#
# # create list of variables
# data(efc)
# dummy <- list(efc$c82cop1, efc$c83cop2, efc$c84cop3)
# # check original distribution of categories
# lapply(dummy, table)
# # renumber from 1 to 0
# lapply(recode_to(dummy), table)
#
#
recode_to <- function(x, lowest = 0, highest = -1) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- rec_to_helper(x[[i]], lowest, highest)
    return(x)
  } else {
    return(rec_to_helper(x, lowest, highest))
  }
}


rec_to_helper <- function(x, lowest, highest) {
  # retrieve value labels
  val_lab <- get_labels(x,
                        attr.only = TRUE,
                        include.values = NULL,
                        include.non.labelled = TRUE)
  # retrieve variable label
  var_lab <- get_label(x)
  # check if factor
  if (is.factor(x)) {
    # try to convert to numeric
    x <- as.numeric(as.character(x))
  }
  # retrieve lowest category
  minval <- min(x, na.rm = TRUE)
  # check substraction difference between current lowest value
  # and requested lowest value
  downsize <- minval - lowest
  x <- sapply(x, function(y) y - downsize)
  # check for highest range
  # set NA to all values out of range
  if (highest > lowest) x[x > highest] <- NA
  # set back labels, if we have any
  if (!is.null(val_lab)) x <- suppressWarnings(set_labels(x, val_lab))
  if (!is.null(var_lab)) x <- suppressWarnings(set_label(x, var_lab))
  # return recoded x
  return(x)
}


# @title Recode variables
# @name rec
#
# @description Recodes the categories / values of a variable \code{x} into new
#                category values.
#
# @seealso \code{\link{set_na}} for setting \code{NA} values, \code{\link{replace_na}}
#            to replace \code{\link{NA}}'s with specific value, \code{\link{recode_to}}
#            for re-shifting value ranges and \code{\link{ref_lvl}} to change the
#            reference level of (numeric) factors.
#
# @param x Numeric, charactor or factor variable that should be recoded;
#          or a \code{data.frame} or \code{list} of variables.
# @param recodes String with recode pairs of old and new values. See
#          'Details' for examples.
# @param value See \code{recodes}.
# @param as.fac Logical, if \code{TRUE}, recoded variable is returned as factor.
#          Default is \code{FALSE}, thus a numeric variable is returned.
# @param var.label Optional string, to set variable label attribute for the
#          recoded variable (see \code{\link{set_label}}). If \code{NULL}
#          (default), variable label attribute of \code{x} will be used (if present).
# @param val.labels Optional character vector, to set value label attributes
#          of recoded variable (see \code{\link{set_labels}}).
#          If \code{NULL} (default), no value labels will be set.
# @return A numeric variable (or a factor, if \code{as.fac = TRUE} or if \code{x}
#           was a character vector) with recoded category values, or a data
#           frame or \code{list}-object with recoded categories for all variables.
#
# @details  The \code{recodes} string has following syntax:
#           \describe{
#            \item{recode pairs}{each recode pair has to be separated by a \code{;}, e.g. \code{recodes = "1=1; 2=4; 3=2; 4=3"}}
#            \item{multiple values}{multiple old values that should be recoded into a new single value may be separated with comma, e.g. \code{"1,2=1; 3,4=2"}}
#            \item{value range}{a value range is indicated by a colon, e.g. \code{"1:4=1; 5:8=2"} (recodes all values from 1 to 4 into 1, and from 5 to 8 into 2)}
#            \item{\code{"min"} and \code{"max"}}{minimum and maximum values are indicates by \emph{min} (or \emph{lo}) and \emph{max} (or \emph{hi}), e.g. \code{"min:4=1; 5:max=2"} (recodes all values from minimum values of \code{x} to 4 into 1, and from 5 to maximum values of \code{x} into 2)}
#            \item{\code{"else"}}{all other values except specified are indicated by \emph{else}, e.g. \code{"3=1; 1=2; else=3"} (recodes 3 into 1, 1 into 2 and all other values into 3)}
#            \item{\code{"copy"}}{the \code{"else"}-token can be combined with \emph{copy}, indicating that all remaining, not yet recoded values should stay the same (are copied from the original value), e.g. \code{"3=1; 1=2; else=copy"} (recodes 3 into 1, 1 into 2 and all other values like 2, 4 or 5 etc. will not be recoded, but copied, see 'Examples')}
#            \item{\code{NA}'s}{\code{\link{NA}} values are allowed both as old and new value, e.g. \code{"NA=1; 3:5=NA"} (recodes all NA from old value into 1, and all old values from 3 to 5 into NA in the new variable)}
#            \item{\code{"rev"}}{\code{"rev"} is a special token that reverses the value order (see 'Examples')}
#           }
#
# @note Please note following behaviours of the function:
#       \itemize{
#         \item the \code{"else"}-token should always be the last argument in the \code{recodes}-string.
#         \item Non-matching values will be set to \code{\link{NA}}, unless captured by the \code{"else"}-token.
#         \item Variable label attributes (see, for instance, \code{\link{get_label}}) are preserved (unless changes via \code{var.label}-argument), however, value label attributes are removed (except for \code{"rev"}, where present value labels will be automatically reversed as well). Use \code{val.labels}-argument to add labels for recoded values.
#         \item If \code{x} is a \code{data.frame} or \code{list} of variables, all variables should have the same categories resp. value range (else, see second bullet, \code{NA}s are produced).
#       }
#
# @examples
# data(efc)
# table(efc$e42dep, exclude = NULL)
#
# # replace NA with 5
# table(rec(efc$e42dep, "1=1;2=2;3=3;4=4;NA=5"), exclude = NULL)
#
# # recode 1 to 2 into 1 and 3 to 4 into 2
# table(rec(efc$e42dep, "1,2=1; 3,4=2"), exclude = NULL)
#
# # or:
# # rec(efc$e42dep) <- "1,2=1; 3,4=2"
# # table(efc$e42dep, exclude = NULL)
#
# # keep value labels. variable label is automatically preserved
# str(rec(efc$e42dep, "1,2=1; 3,4=2",
#         val.labels = c("low dependency", "high dependency")))
#
# # recode 1 to 3 into 4 into 2
# table(rec(efc$e42dep, "min:3=1; 4=2"), exclude = NULL)
#
# # recode 2 to 1 and all others into 2
# table(rec(efc$e42dep, "2=1; else=2"), exclude = NULL)
#
# # reverse value order
# table(rec(efc$e42dep, "rev"), exclude = NULL)
#
# # recode only selected values, copy remaining
# table(efc$e15relat)
# table(rec(efc$e15relat, "1,2,4=1; else=copy"))
#
# # recode variables with same categorie in a data frame
# head(efc[, 6:9])
# head(rec(efc[, 6:9], "1=10;2=20;3=30;4=40"))
#
# # recode list of variables. create dummy-list of
# # variables with same value-range
# dummy <- list(efc$c82cop1, efc$c83cop2, efc$c84cop3)
# # show original distribution
# lapply(dummy, table, exclude = NULL)
# # show recodes
# lapply(rec(dummy, "1,2=1; NA=9; else=copy"), table, exclude = NULL)
#
#
# # recode character vector
# dummy <- c("M", "F", "F", "X")
# rec(dummy, "M=Male; F=Female; X=Refused")
#
#
# # recode non-numeric factors
# data(iris)
# rec(iris$Species, "setosa=huhu; else=copy")
#
#
rec <- function(x,
                recodes,
                as.fac = FALSE,
                var.label = NULL,
                val.labels = NULL) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- rec_helper(x[[i]], recodes, as.fac, var.label, val.labels)
    return(x)
  } else {
    return(rec_helper(x, recodes, as.fac, var.label, val.labels))
  }
}


#' @importFrom stats na.omit
rec_helper <- function(x, recodes, as.fac, var.label, val.labels) {
  # retrieve variable label
  if (is.null(var.label))
    var_lab <- get_label(x)
  else
    var_lab <- var.label
  # do we have any value labels?
  val_lab <- val.labels
  # remember if NA's have been recoded...
  na_recoded <- FALSE

  # do we have a factor with "x"?
  if (is.factor(x)) {
    # save variable labels before in case we just want
    # to reverse the order
    if (is.null(val_lab) && recodes == "rev") {
      val_lab <- rev(get_labels(
        x,
        attr.only = TRUE,
        include.values = NULL,
        include.non.labelled = TRUE
      ))
    }

    if (is_num_fac(x)) {
      # numeric factors coerced to numeric
      x <- as.numeric(as.character(x))
    } else {
      # non-numeric factors coerced to character
      x <- as.character(x)
      # non-numeric factors will always be factor again
      as.fac = TRUE
    }
  }

  # retrieve min and max values
  min_val <- min(x, na.rm = T)
  max_val <- max(x, na.rm = T)

  # do we have special recode-token?
  if (recodes == "rev") {
    # retrieve unique valus, sorted
    ov <- sort(unique(stats::na.omit(as.vector(x))))
    # new values should be reversed order
    nv <- rev(ov)
    # create recodes-string
    recodes <- paste(sprintf("%i=%i", ov, nv), collapse = ";")
    # when we simply reverse values, we can keep value labels
    if (is.null(val_lab)) {
      val_lab <- rev(get_labels(
        x,
        attr.only = TRUE,
        include.values = NULL,
        include.non.labelled = TRUE
      ))
    }
  }

  # prepare and clean recode string
  # retrieve each single recode command
  rec_string <- unlist(strsplit(recodes, ";", fixed = TRUE))
  # remove spaces
  rec_string <- gsub(" ", "", rec_string, fixed = TRUE)
  # replace min and max placeholders
  rec_string <- gsub("min", as.character(min_val), rec_string, fixed = TRUE)
  rec_string <- gsub("lo", as.character(min_val), rec_string, fixed = TRUE)
  rec_string <- gsub("max", as.character(max_val), rec_string, fixed = TRUE)
  rec_string <- gsub("hi", as.character(max_val), rec_string, fixed = TRUE)
  # retrieve all recode-pairs, i.e. all old-value = new-value assignments
  rec_pairs <- strsplit(rec_string, "=", fixed = TRUE)

  # check for correct syntax
  correct_syntax <- unlist(lapply(rec_pairs, function(r) if (length(r) != 2) r else NULL))
  # found any errors in syntax?
  if (!is.null(correct_syntax)) {
    stop(sprintf("?Syntax error in argument \"%s\"", paste(correct_syntax, collapse = "=")), call. = F)
  }

  # the new, recoded variable
  new_var <- rep(-Inf, length(x))

  # now iterate all recode pairs
  # and do each recoding step
  for (i in 1:length(rec_pairs)) {
    # retrieve recode pairs as string, and start with separaring old-values
    # at comma separator
    old_val_string <- unlist(strsplit(rec_pairs[[i]][1], ",", fixed = TRUE))
    new_val_string <- rec_pairs[[i]][2]
    new_val <- c()

    # check if new_val_string is correct syntax
    if (new_val_string == "NA") {
      # here we have a valid NA specification
      new_val <- NA
    } else if (new_val_string == "copy") {
      # copy all remaining values, i.e. don't recode
      # remaining values that have not else been specified
      # or recoded. NULL indicates the "copy"-token
      new_val <- NULL
    } else {
      # can new value be converted to numeric?
      new_val <- suppressWarnings(as.numeric(new_val_string))
      # if not, assignment is wrong
      if (is.na(new_val)) {
        new_val <- new_val_string
      }
    }

    # retrieve and check old values
    old_val <- c()
    for (j in 1:length(old_val_string)) {
      # copy to shorten code
      ovs <- old_val_string[j]

      # check if old_val_string is correct syntax
      if (ovs == "NA") {
        # here we have a valid NA specification
        # add value to vector of old values that
        # should be recoded
        old_val <- c(old_val, NA)
      } else if (ovs == "else") {
        # here we have a valid "else" specification
        # add all remaining values (in the new variable
        # created as "-Inf") to vector that should be recoded
        old_val <- -Inf
        break
      } else if (length(grep(":", ovs, fixed = TRUE)) > 0) {
        # this value indicates a range of values to be recoded, because
        # we have found a colon. now copy from and to values from range
        from <- suppressWarnings(as.numeric(unlist(strsplit(ovs, ":", fixed = T))[1]))
        to <- suppressWarnings(as.numeric(unlist(strsplit(ovs, ":", fixed = T))[2]))
        # check for valid range values
        if (is.na(from) || is.na(to)) {
          stop(sprintf("?Syntax error in argument \"%s\"", ovs), call. = F)
        }
        # add range to vector of old values
        old_val <- c(old_val, seq(from, to))
      } else {
        # can new value be converted to numeric?
        ovn <- suppressWarnings(as.numeric(ovs))
        # if not, assignment is wrong
        if (is.na(ovn)) {
          # stop(sprintf("?Syntax error in argument \"%s\"", ovs), call. = F)
          ovn <- ovs
        }
        # add old recode values to final vector of values
        old_val <- c(old_val, ovn)
      }
    }

    # now we have all recode values and want
    # to replace old with new values...
    for (k in 1:length(old_val)) {
      # check for "else" token
      if (is.infinite(old_val[k])) {
        # else-token found. we first need to preserve NA, but only,
        # if these haven't been copied before
        if (!na_recoded) new_var[which(is.na(x))] <- NA
        # find replace-indices. since "else"-token has to be
        # the last argument in the "recodes"-string, the remaining,
        # non-recoded values are still "-Inf". Hence, find positions
        # of all not yet recoded values
        rep.pos <- which(new_var == -Inf)
        # else token found, now check whether we have a "copy"
        # token as well. in this case, new_val would be NULL
        if (is.null(new_val)) {
          # all not yet recodes values in new_var should get
          # the values at that position of "x" (the old variable),
          # i.e. these values remain unchanged.
          new_var[rep.pos] <- x[rep.pos]
        } else {
          # find all -Inf in new var and replace them with replace value
          new_var[rep.pos] <- new_val
        }
        # check for "NA" token
      } else if (is.na(old_val[k])) {
        # replace all NA with new value
        new_var[which(is.na(x))] <- new_val
        # remember that we have recoded NA's. Might be
        # important for else-token above.
        na_recoded <- TRUE
      } else {
        # else we have numeric values, which should be replaced
        new_var[which(x == old_val[k])] <- new_val
      }
    }
  }
  # replace remaining -Inf with NA
  if (any(is.infinite(new_var))) new_var[which(new_var == -Inf)] <- NA
  # set back variable and value labels
  new_var <- suppressWarnings(set_label(x = new_var, lab = var_lab))
  new_var <- suppressWarnings(set_labels(x = new_var, labels = val_lab))
  # return result as factor?
  if (as.fac) new_var <- to_factor(new_var)
  return(new_var)
}

# @rdname rec
#
`rec<-` <- function(x, as.fac = FALSE, var.label = NULL, val.labels = NULL, value) {
  UseMethod("rec<-")
}

#
`rec<-.default` <- function(x, as.fac = FALSE, var.label = NULL, val.labels = NULL, value) {
  x <- rec(x = x, recodes = value, as.fac = as.fac, var.label = var.label, val.labels = val.labels)
  x
}
# @title Create recode pattern for 'rec' function
# @name rec_pattern
#
# @description Convenient function to create a recode pattern for the
#                \code{\link{rec}} function, which recodes (numeric)
#                vectors into smaller groups.
#
# @param from Minimum value that should be recoded.
# @param to Maximum value that should be recoded.
# @param width Numeric, indicating the range of each group.
# @param other String token, indicating how to deal with all other values
#          that have not been captured by the recode pattern. See 'Details'
#          on the \code{else}-token in \code{\link{rec}}.
# @return A list with two values:
#           \describe{
#            \item{\code{pattern}}{string pattern that can be used as \code{recodes} argument for the \code{\link{rec}}-function.}
#            \item{\code{labels}}{the associated values labels that can be used with \code{\link{set_labels}}.}
#           }
#
# @seealso \code{\link{group_var}} for recoding variables into smaller groups, and
#           \code{\link{group_labels}} to create the asssociated value labels.
#
# @examples
# rp <- rec_pattern(1, 100)
# rp
#
# # sample data, inspect age of carers
# data(efc)
# table(efc$c160age, exclude = NULL)
# table(rec(efc$c160age, rp$pattern), exclude = NULL)
#
# # recode carers age into groups of width 5
# x <- rec(efc$c160age, rp$pattern)
# # add value labels to new vector
# set_labels(x) <- rp$labels
# # watch result
# frq(as_labelled(x))
#
#
rec_pattern <- function(from, to, width = 5, other = NULL){
  # init variables
  rec.pat <- c()
  rec.labels <- c()
  # create sequence of recode-groups
  values <- seq(from, to + width, by = width)
  # create pattern for each group
  for (x in 1:(length(values) - 1)) {
    rec.pat <- paste0(rec.pat,
                      sprintf("%i:%i=%i", values[x], values[x + 1] - 1, x),
                      sep = ";")
    # also create associated labels
    rec.labels <- c(rec.labels, sprintf("%i-%i", values[x], values[x + 1] - 1))
  }
  # do we have an "else"-token?
  if (!is.null(other) && !is_empty(other))
    rec.pat <- paste0(rec.pat, "else=", other, sep = "")
  # name labels
  names(rec.labels) <- c(1:(length(values) - 1))
  # return results
  list(pattern = rec.pat, labels = rec.labels)
}
# @title Change reference level of (numeric) factors
# @name ref_lvl
#
# @description Changes the reference level of numeric factor. See 'Details'.
#
# @seealso \code{\link{to_factor}} to convert numeric vectors into factors;
#            \code{\link{rec}} to recode variables.
#
# @param x \code{\link{factor}} with numeric levels where a new reference
#          level should be set.
# @param value Numeric, the new reference level.
# @return \code{x} with new reference level. See 'Details'.
#
# @details Unlike \code{\link[stats]{relevel}}, this function a) only accepts
#            numeric factors and b) changes the reference level by recoding
#            the factor's values using the \code{\link{rec}} function. Hence,
#            all values from lowest up to the reference level indicated by
#            \code{value} are recoded, with \code{value} starting as lowest
#            factor value. See 'Examples'.
#
#
# @examples
# data(efc)
# x <- to_factor(efc$e42dep)
# str(x)
# table(x)
#
# ref_lvl(x) <- 3
# str(x)
# table(x)
#
#
ref_lvl <- function(x, value = NULL) {
  # check correct arguments
  if (is.null(x)) {
    warning("`x` is NULL.", call. = F)
    return(x)
  }
  if (!is.factor(x)) {
    warning("`x` needs to be a factor.", call. = F)
    return(x)
  }
  if (!is_num_fac(x)) {
    warning("`x` needs to be a factor with numeric factor levels.", call. = F)
    return(x)
  }
  # get values from factor
  vals <- as.numeric(levels(x))
  # check if ref-lvl exists in values
  if (!value %in% vals) {
    warning("`x` has no factor level indicated by the reference level `value`.", call. = F)
    return(x)
  }
  # get value labels
  val.labs <- get_labels(x)
  # get variable label
  var.lab <- get_label(x)
  # find position of reference level
  refpos <- which(vals == value)
  # new order of factor levels, if reference level
  # is on first position
  neword <- c(vals[refpos], vals[-refpos])
  # now recode variable. therefore, we need a string pattern
  # for the recoding
  rec.pattern <- paste0(sprintf("%i=%i;", neword, vals), collapse = "")
  # recode now
  x <- rec(x, rec.pattern, as.fac = TRUE)
  # set back labels
  if (!is.null(var.lab) && !is_empty(var.lab)) {
    set_label(x) <- var.lab
  }
  if (!is.null(val.labs)) {
    # we need "order" twice here, because "neword" refers to the actual
    # values of "x", so "neword" might have negative values, or zero.
    # so we first need the "order" function to have numeric values from
    # 1 to length(x) - and a second "order" call to get the correct order
    # of these values.
    set_labels(x) <- val.labs[order(order(neword))]
  }
  return(x)
}

# @rdname ref_lvl
#
`ref_lvl<-` <- function(x, value) {
  UseMethod("ref_lvl<-")
}

#
`ref_lvl<-.default` <- function(x, value) {
  x <- ref_lvl(x = x, value = value)
  x
}
# @title Performs a reliability test on an item scale
# @name reliab_test
# @description This function calculates the item discriminations (corrected item-total
#                correlations for each item of \code{x} with the remaining items) and
#                the Cronbach's alpha for each item, if it was deleted from the
#                scale.
#
# @seealso \code{\link{cronb}}
#
# @param x \code{data.frame} with items (from a scale).
# @param scale.items Logical, if \code{TRUE}, the data frame's vectors will be scaled. Recommended,
#          when the variables have different measures / scales.
# @param digits Amount of digits for Cronbach's Alpha and correlation values in
#          returned data frame.
# @return A data frame with the corrected item-total correlations (item discrimination)
#           and Cronbach's alpha (if item deleted) for each item of the scale, or
#           \code{NULL} if data frame had too less columns.
#
# @note This function is similar to a basic reliability test in SPSS. The correlations in
#         the Item-Total-Statistic are a computed correlation of each item against the sum
#         of the remaining items (which are thus treated as one item).
#
# @examples
# # Data from the EUROFAMCARE sample dataset
# data(efc)
#
# # retrieve variable and value labels
# varlabs <- get_label(efc)
#
# # recveive first item of COPE-index scale
# start <- which(colnames(efc) == "c82cop1")
# # recveive last item of COPE-index scale
# end <- which(colnames(efc) == "c90cop9")
#
# # create data frame with COPE-index scale
# x <- data.frame(efc[, c(start:end)])
# colnames(x) <- varlabs[c(start:end)]
#
# \dontrun{
# library(sjPlot)
# sjt.df(reliab_test(x), describe = FALSE, showCommentRow = TRUE,
#        commentString = sprintf("Cronbach's &alpha;=%.2f", cronb(x)))}
#
# # Compute PCA on Cope-Index, and perform a
# # reliability check on each extracted factor.
# \dontrun{
# factors <- sjt.pca(x)$factor.index
# findex <- sort(unique(factors))
# library(sjPlot)
# for (i in 1:length(findex)) {
#  rel.df <- subset(x, select = which(factors == findex[i]))
#  if (ncol(rel.df) >= 3) {
#    sjt.df(reliab_test(rel.df), describe = FALSE, showCommentRow = TRUE,
#           useViewer = FALSE, title = "Item-Total-Statistic",
#           commentString = sprintf("Scale's overall Cronbach's &alpha;=%.2f",
#                                   cronb(rel.df)))
#    }
#  }}
#
#' @importFrom stats cor
#
reliab_test <- function(x,
                        scale.items = FALSE,
                        digits = 3) {
  # check param
  if (!is.matrix(x) && !is.data.frame(x)) {
    warning("`x` needs to be a data frame or matrix.", call. = F)
    return(NULL)
  }

  # remove missings, so correlation works
  x <- stats::na.omit(x)

  # remember item (column) names for return value
  # return value gets column names of initial data frame
  df.names <- colnames(x)

  # check for minimum amount of columns
  # can't be less than 3, because the reliability
  # test checks for Cronbach's alpha if a specific
  # item is deleted. If data frame has only two columns
  # and one is deleted, Cronbach's alpha cannot be calculated.
  if (ncol(x) > 2) {
    # Check whether items should be scaled. Needed,
    # when items have different measures / scales
    if (scale.items) x <- data.frame(scale(x, center = TRUE, scale = TRUE))

    # init vars
    totalCorr <- c()
    cronbachDeleted <- c()

    # iterate all items
    for (i in 1:ncol(x)) {
      # create subset with all items except current one
      # (current item "deleted")
      sub.df <- subset(x, select = c(-i))

      # calculate cronbach-if-deleted
      cronbachDeleted <- c(cronbachDeleted, cronb(sub.df))

      # calculate corrected total-item correlation
      totalCorr <- c(totalCorr, stats::cor(x[, i],
                                           apply(sub.df, 1, sum),
                                           use = "pairwise.complete.obs"))
    }

    # create return value
    ret.df <- data.frame(cbind(round(cronbachDeleted, digits),
                               round(totalCorr, digits)))

    # set names of data frame
    colnames(ret.df) <- c("Cronbach's &alpha; if item deleted", "Item discrimination")
    rownames(ret.df) <- df.names
  } else {
    warning("Data frame needs at least three columns for reliability-test.", call. = F)
    ret.df <- NULL
  }
  return(ret.df)
}
# @title Remove value and variable labels from vector or data frame
# @name remove_all_labels
#
# @description This function removes value and variable label attributes
#                from a vector or data frame. These attributes are typically
#                added to variables when importing foreign data (see
#                \code{\link{read_spss}}) or manually adding label attributes
#                with \code{\link{set_labels}}.
#
# @seealso \href{http://www.strengejacke.de/sjPlot/labelleddata/}{sjPlot-manual}
#            on working with labelled data, and \code{\link{copy_labels}} for
#            adding label attributes (subsetted) data frames.
#
# @param x Vector or \code{data.frame} with variable and/or value label attributes
# @return \code{x} with removed value and variable label attributes.
#
# @examples
# data(efc)
# str(efc)
# str(remove_all_labels(efc))
#
#
remove_all_labels <- function(x) {
  if (is.data.frame(x)) {
    for (i in 1:ncol(x)) x[[i]] <- remove_all_labels_helper(x[[i]])
  } else {
    x <- remove_all_labels_helper(x)
  }
  return(x)
}


remove_all_labels_helper <- function(x) {
  # find label-attribute string
  attr.string <- getValLabelAttribute(x)
  # remove attributes
  if (!is.null(attr.string)) attr(x, attr.string) <- NULL

  # jerry: do not remove variable label
  # # find label-attribute string
  # attr.string <- getVarLabelAttribute(x)
  # # remove attributes
  # if (!is.null(attr.string)) attr(x, attr.string) <- NULL

  # remove is_na attribute
  na.flags <- get_na_flags(x)
  if (!is.null(na.flags)) attr(x, getNaAttribute()) <- NULL
  # unclass, if labelled. labelled class may throw
  # errors / warnings, when not havin label attributes
  if (is_labelled(x)) x <- unclass(x)
  # return var
  return(x)
}
# @title Remove value labels from variables
# @name remove_labels
#
# @description This function removes labels from a label attribute of a
#                vector \code{x}, resp. from a set of vectors in a
#                \code{data.frame} or \code{list}-object. The counterpart
#                to this function is \code{\link{add_labels}}.
#
# @seealso \code{\link{set_labels}} to add value labels, replacing the existing ones;
#            \code{\link{add_labels}} to add new labels to a vector.
#
# @param x Variable (vector), \code{list} of variables or a \code{data.frame}
#          where value label attributes should be removed.
# @param value Either a numeric vector, indicating one or more label attributes that
#          should be removed (see \code{\link{get_labels}} to retrieve a vector's
#          label attributes), or a character vector with names of label attributes
#          that should be removed.
#
# @return \code{x} with removed value labels.
#
# @examples
# data(efc)
# get_labels(efc$e42dep)
#
# x <- remove_labels(efc$e42dep, 2)
# get_labels(x, include.values = "p")
#
# x <- remove_labels(efc$e42dep, "independent")
# get_labels(x, include.values = "p")
#
#
#
remove_labels <- function(x, value) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- remove_labels_helper(x[[i]], value)
    return(x)
  } else {
    return(remove_labels_helper(x, value))
  }
}


remove_labels_helper <- function(x, value) {
  # get current labels of `x`
  current.labels <- get_labels(x,
                               attr.only = T,
                               include.values = "n",
                               include.non.labelled = F)

  # if we have no labels, return
  if (is.null(current.labels)) {
    message("`x` has no value label attributes.")
    return(x)
  }

  # remove by index?
  if (is.numeric(value)) {
    current.labels <- current.labels[-value]
  } else if (is.character(value)) {
    # find value labels that should be removes
    removers <- as.vector(current.labels) %in% value
    # remove them
    current.labels <- current.labels[!removers]
  }

  # switch value and names attribute, since get_labels
  # returns the values as names, and the value labels
  # as "vector content"
  all.labels <- as.numeric(names(current.labels))
  names(all.labels) <- as.character(current.labels)

  # sort labels by values
  all.labels <- all.labels[order(as.numeric(all.labels))]

  # set back labels
  x <- set_labels(x, labels = all.labels)
  return(x)
}

# @rdname remove_labels
#
`remove_labels<-` <- function(x, value) {
  UseMethod("remove_labels<-")
}

#
`remove_labels<-.default` <- function(x, value) {
  x <- remove_labels(x, value)
  x
}
# @title Replace NA with specific values
# @name replace_na
#
# @description This function replaces NA's of a variable, data frame
#                or list of variables with \code{value}.
#
# @seealso \code{\link{set_na}} for setting \code{NA} values, \code{\link{rec}}
#            for general recoding of variables and \code{\link{recode_to}}
#            for re-shifting value ranges.
#
# @param x Variable (vector), \code{data.frame} or \code{list} of variables where
#          missing values should be replaced with \code{value}.
# @param value Value that will replace the \code{\link{NA}}'s.
# @param na.label Optional character vector, used to label the NA-value (i.e.
#          adding a \code{labels} attribute to \code{x}).
#
# @return \code{x}, where \code{NA}'s are replaced with \code{value}.
#
# @note Value and variable label attributes (see, for instance, \code{\link{get_labels}}
#         or \code{\link{set_labels}}) are preserved.
#
# @examples
# data(efc)
# table(efc$e42dep, exclude = NULL)
# table(replace_na(efc$e42dep, 99), exclude = NULL)
#
# dummy <- list(efc$c82cop1, efc$c83cop2, efc$c84cop3)
# # show original distribution
# lapply(dummy, table, exclude = NULL)
# # show variables, NA's replaced with 99
# lapply(replace_na(dummy, 99), table, exclude = NULL)
#
#
replace_na <- function(x, value, na.label = NULL) {
  # check for valid value
  if (is.null(value) || is.na(value)) return(x)
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # replace NA
    for (i in 1:nvars) x[[i]] <- replace_na_helper(x[[i]], value, na.label)
    return(x)
  } else {
    return(replace_na_helper(x, value, na.label))
  }
  return(x)
}


replace_na_helper <- function(x, value, na.label) {
  # create named vector, for labelleing
  if (!is.null(na.label)) {
    na.vec <- value
    names(na.vec) <- as.character(na.label)
  }
  if (anyNA(x)) {
    # do we have a factor? then check for levels
    if (is.factor(x)) {
      # is value in levels?
      if (!any(levels(x) %in% as.character(value))) {
        # if not, add value to levels
        levels(x) <- c(levels(x), as.character(value))
      }
    }
    x[is.na(x)] <- value
    # add NA label
    if (!is.null(na.label)) add_labels(x) <- na.vec
  } else {
    message("`x` has no missings.")
  }
  return(x)
}


# @rdname replace_na
#
`replace_na<-` <- function(x, na.label = NULL, value) {
  UseMethod("replace_na<-")
}

#
`replace_na<-.default` <- function(x, na.label = NULL, value) {
  x <- replace_na(x = x, value = value, na.label = na.label)
  x
}
# @title Root Mean Squared Error (RMSE)
# @name rmse
# @description Compute root mean squared error  of fitted linear (mixed effects) models.
#
# @param fit Fitted linear model of class \code{\link{lm}},
#          \code{\link[lme4]{merMod}} (lme4) or \code{\link[nlme]{lme}} (nlme).
# @param normalized Logical, use \code{TRUE} if normalized rmse should be returned.
#
# @return The root mean squared error of \code{fit}; or the normalized
#           root mean squared error of \code{fit} if \code{normalized = TRUE}.
#
# @seealso \code{\link{cv}}
#
# @references \itemize{
#              \item \href{http://en.wikipedia.org/wiki/Root-mean-square_deviation}{Wikipedia: RMSD}
#              \item \href{http://www.theanalysisfactor.com/assessing-the-fit-of-regression-models/}{Grace-Martin K: Assessing the Fit of Regression Models}
#             }
#
# @note The RMSE is the square root of the variance of the residuals and indicates
#         the absolute fit of the model to the data (difference between observed data
#         to model's predicted values). "RMSE can be interpreted as the standard
#         deviation of the unexplained variance, and has the useful property
#         of being in the same units as the response variable. Lower values
#         of RMSE indicate better fit. RMSE is a good measure of how accurately
#         the model predicts the response, and is the most important criterion
#         for fit if the main purpose of the model is prediction."
#         (Grace-Martin K: Assessing the Fit of Regression Models).
#         \cr \cr
#         The normalized RMSE is the proportion of the RMSE related to the
#         range of the response variable. Hence, lower values indicate
#         less residual variance.
#
# @examples
# data(efc)
# fit <- lm(barthtot ~ c160age + c12hour, data = efc)
# rmse(fit)
#
# library(lme4)
# fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
# rmse(fit)
#
# # normalized RMSE
# library(nlme)
# fit <- lme(distance ~ age, data = Orthodont)
# rmse(fit, normalized = TRUE)
#
#' @importFrom stats residuals
#
rmse <- function(fit, normalized = FALSE) {
  # compute rmse
  rmse_val <- sqrt(mean(stats::residuals(fit) ^ 2, na.rm = TRUE))

  # if normalized, divide by range of response
  if (normalized) {
    if (any(class(fit) == "lmerMod") || any(class(fit) == "merModLmerTest")) {
      # check for package availability
      if (!requireNamespace("lme4", quietly = TRUE)) {
        stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
      }
      resp <- lme4::getME(fit, "y")
    } else if (any(class(fit) == "lme")) {
      # check for package availability
      if (!requireNamespace("nlme", quietly = TRUE)) {
        stop("Package 'nlme' needed for this function to work. Please install it.", call. = FALSE)
      }
      resp <- unname(nlme::getResponse(fit))
    } else {
      resp <- fit$model[[1]]
    }
    rmse_val <- rmse_val / (max(resp, na.rm = T) - min(resp, na.rm = T))
  }
  rmse_val
}
# @title Sample size for linear mixed models
# @name smpsize_lmm
#
# @description Compute an approximated sample size for linear mixed models
#                (two-level-designs), based on power-calculation for standard
#                design and adjusted for design effect for 2-level-designs.
#
# @param eff.size Effect size.
# @param df.n Optional argument for the degrees of freedom for numerator. See 'Details'.
# @param power Power of test (1 minus Type II error probability).
# @param sig.level Significance level (Type I error probability).
# @param k Number of cluster groups (level-2-unit) in multilevel-design.
# @param icc Expected intraclass correlation coefficient for multilevel-model.
#
# @return A list with two values: The number of subjects per cluster, and the
#           total sample size for the linear mixed model.
#
# @references \itemize{
#              \item Cohen J. 1988. Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.
#              \item Hsieh FY, Lavori PW, Cohen HJ, Feussner JR. 2003. An Overview of Variance Inflation Factors for Sample-Size Calculation. Evaluation & the Health Professions 26: 239–257. \doi{10.1177/0163278703255230}
#              \item Snijders TAB. 2005. Power and Sample Size in Multilevel Linear Models. In: Everitt BS, Howell DC (Hrsg.). Encyclopedia of Statistics in Behavioral Science. Chichester, UK: John Wiley & Sons, Ltd. \doi{10.1002/0470013192.bsa492}
#             }
#
# @details The sample size calculation is based on a power-calculation for the
#          standard design. If \code{df.n} is not specified, a power-calculation
#          for an unpaired two-sample t-test will be computed (using
#          \code{\link[pwr]{pwr.t.test}} of the \pkg{pwr}-package).
#          If \code{df.n} is given, a power-calculation for general linear models
#          will be computed (using \code{\link[pwr]{pwr.f2.test}} of the
#          \pkg{pwr}-package). The sample size of the standard design
#          is then adjusted for the design effect of two-level-designs (see
#          \code{\link{deff}}). Thus, the sample size calculation is appropriate
#          in particular for two-level-designs (see Snijders 2005). Models that
#          additionally include repeated measures (three-level-designs) may work
#          as well, however, the computed sample size may be less accurate.
#
# @examples
# # Sample size for multilevel model with 30 cluster groups and a small to
# # medium effect size (Cohen's d) of 0.3. 29 subjects per cluster and
# # hence a total sample size of about 859 observations is needed.
# smpsize_lmm(eff.size = .3, k = 30)
#
# # Sample size for multilevel model with 20 cluster groups and a medium
# # to large effect size for linear models of 0.2. Nine subjects per cluster and
# # hence a total sample size of about 172 observations is needed.
# smpsize_lmm(eff.size = .2, df.n = 5, k = 20, power = .9)
#
#
smpsize_lmm <- function(eff.size, df.n = NULL, power = .8, sig.level = .05, k, icc = 0.05) {
  if (!requireNamespace("pwr", quietly = TRUE)) {
    stop("Package `pwr` needed for this function to work. Please install it.", call. = FALSE)
  }
  # compute sample size for standard design
  if (is.null(df.n))
    # if we have no degrees of freedom specified, use t-test
    n <- 2 * pwr::pwr.t.test(d = eff.size, sig.level = sig.level, power = power)$n
  else
    # we have df, so power-calc for linear models
    n <- pwr::pwr.f2.test(u = df.n, f2 = eff.size, sig.level = sig.level, power = power)$v + df.n + 1
  # adjust standard design by design effect
  total.n <- n * deff(n = k, icc = icc)
  # sample size for each group and total n
  smpsz <- list(round(total.n / k), round(total.n))
  # name list
  names(smpsz) <- c("Subjects per Cluster", "Total Sample Size")
  smpsz
}


# @title Design effects for two-level mixed models
# @name deff
#
# @description Compute the design effect for mixed models with two-level design.
#
# @param n Average number of observations per grouping cluster (i.e. level-2 unit).
# @param icc Assumed intraclass correlation coefficient for multilevel-model.
#
# @return The design effect for the two-level model.
#
# @references \itemize{
#              \item Hsieh FY, Lavori PW, Cohen HJ, Feussner JR. 2003. An Overview of Variance Inflation Factors for Sample-Size Calculation. Evaluation & the Health Professions 26: 239–257. \doi{10.1177/0163278703255230}
#              \item Snijders TAB. 2005. Power and Sample Size in Multilevel Linear Models. In: Everitt BS, Howell DC (Hrsg.). Encyclopedia of Statistics in Behavioral Science. Chichester, UK: John Wiley & Sons, Ltd. \doi{10.1002/0470013192.bsa492}
#             }
#
# @examples
# # Design effect for two-level model with 30 cluster groups
# # and an assumed intraclass correlation coefficient of 0.05.
# deff(n = 30)
#
#
deff <- function(n, icc = 0.05) {
  return(1 + (n - 1) * icc)
}
# @title Standard Error for variables
# @name se
# @description Compute standard error for a variable, for all variables
#                of a data frame or for joint random and fixed effects
#                coefficients of mixed models.
#
# @param x (Numeric) vector, a data frame or a \code{merMod}-object
#          as returned by the \code{\link[lme4]{lmer}}-method.
# @return The standard error of \code{x}, or for each variable
#           if \code{x} is a data frame, or for the coefficients
#           of a mixed model (see \code{\link[lme4]{coef.merMod}}).
#
# @note Computation of standard errors for coefficients of mixed models
#         is based \href{http://stackoverflow.com/questions/26198958/extracting-coefficients-and-their-standard-error-from-lme}{on this code}.
#
# @details Unlike \code{\link[arm]{se.coef}}, which returns the standard error
#            for fixed and random effects separately, this function computes
#            the standard errors for joint (sums of) random and fixed
#            effects coefficients. Hence, \code{se} returns the appropriate
#            standard errors for \code{\link[lme4]{coef.merMod}}.
#
# @examples
# se(rnorm(n = 100, mean = 3))
#
# data(efc)
# se(efc[, 1:3])
#
# library(lme4)
# fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
# se(fit)
#
#
se <- function(x) {
  if (is_merMod(x)) {
    return(std_merMod(x))
  } else if (is.matrix(x) || is.data.frame(x)) {
    # init return variables
    stde <- c()
    stde_names <- c()
    # iterate all columns
    for (i in 1:ncol(x)) {
      # get and save standard error for each variable
      # of the data frame
      stde <- c(stde, std_e_helper(x[[i]]))
      # save column name as variable name
      stde_names <- c(stde_names, colnames(x)[i])
    }
    # set names to return vector
    names(stde) <- stde_names
    # return results
    return(stde)
  } else {
    return(std_e_helper(x))
  }
}

std_e_helper <- function(x) sqrt(var(x, na.rm = TRUE) / length(stats::na.omit(x)))

#' @importFrom stats coef setNames
#' @importFrom lme4 ranef
std_merMod <- function(fit) {
  # check for package availability
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needed for this function to work. Please install it.", call. = FALSE)
  }
  se.merMod <- list()
  # get coefficients
  cc <- stats::coef(fit)
  # get names of intercepts
  inames <- names(cc)
  # variances of fixed effects
  fixed.vars <- diag(as.matrix(lme4::vcov.merMod(fit)))
  # extract variances of conditional modes
  r1 <- lme4::ranef(fit, condVar = TRUE)
  # we may have multiple random intercepts, iterate all
  for (i in 1:length(cc)) {
    cmode.vars <- t(apply(attr(r1[[i]], "postVar"), 3, diag))
    seVals <- sqrt(sweep(cmode.vars, 2, fixed.vars, "+"))
    # add results to return list
    se.merMod[[length(se.merMod) + 1]] <- stats::setNames(as.vector(seVals[1, ]),
                                                          c("intercept_se", "slope_se"))
  }
  # set names of list
  names(se.merMod) <- inames
  return(se.merMod)
}
# @title Add variable label(s) to variables
# @name set_label
#
# @description This function adds variable labels as attribute
#                (named \code{"label"} or \code{"variable.label"}) to a variable
#                or vector \code{x}, resp. to a set of variables in a
#                \code{data.frame} or a \code{list}-object. Most functions of the
#                \pkg{sjPlot} package can automatically retrieve the variable
#                labels to use it as axis labels or plot title (see 'Details').
#
# @seealso The sjPlot manual on \href{http://www.strengejacke.de/sjPlot/datainit/}{data initialization} or
#            \href{http://www.strengejacke.de/sjPlot/view_spss/}{inspecting (SPSS imported) data frames} for
#            more details; \code{\link{set_labels}} to manually set value labels or \code{\link{get_label}}
#            to get variable labels.
#
# @param x Variable (vector), \code{list} of variables or a \code{data.frame}
#          where variables labels should be added as attribute
# @param lab If \code{x} is a vector (single variable), use a single character string with
#          the variable label for \code{x}. If \code{x} is a data frame, use a
#          vector with character labels of same length as \code{ncol(x)}.
#          Use \code{lab = ""} to remove labels-attribute from \code{x}, resp.
#          set any value of vector \code{lab} to \code{""} to remove specific variable
#          label attributes from a data frame's variable.
# @param value See \code{lab}.
# @param attr.string Attribute string for the variable label. \strong{Note:}
#          Usually, this argument should be ignored. It is only used internally
#          for the \code{\link{write_spss}} and \code{\link{write_stata}} functions.
# @return \code{x}, with variable label attribute(s), which contains the
#           variable name(s); or with removed label-attribute if
#            \code{lab = ""}.
#
# @details See 'Details' in \code{\link{get_labels}}
#
# @note See 'Note' in \code{\link{get_labels}}
#
# @examples
# # sample data set, imported from SPSS.
# data(efc)
#
# \dontrun{
# library(sjPlot)
# sjt.frq(efc$e42dep)
# sjt.frq(data.frame(efc$e42dep, efc$e16sex))}
#
#
# # manually set value and variable labels
# dummy <- sample(1:4, 40, replace = TRUE)
# dummy <- set_labels(dummy, c("very low", "low", "mid", "hi"))
# dummy <- set_label(dummy, "Dummy-variable")
#
# # or use:
# # set_label(dummy) <- "Dummy-variable"
#
# # auto-detection of value labels by default, auto-detection of
# # variable labels if argument "title" set to NULL.
# \dontrun{
# library(sjPlot)
# sjp.frq(dummy, title = NULL)}
#
#
# # Set variable labels for data frame
# dummy <- data.frame(a = sample(1:4, 10, replace = TRUE),
#                     b = sample(1:4, 10, replace = TRUE),
#                     c = sample(1:4, 10, replace = TRUE))
# dummy <- set_label(dummy, c("Variable A", "Variable B", "Variable C"))
# str(dummy)
#
# # remove one variable label
# dummy <- set_label(dummy, c("Variable A", "", "Variable C"))
# str(dummy)
#
#
# # setting same variable labels to multiple vectors
#
# # create a set of dummy variables
# dummy1 <- sample(1:4, 40, replace = TRUE)
# dummy2 <- sample(1:4, 40, replace = TRUE)
# dummy3 <- sample(1:4, 40, replace = TRUE)
# # put them in list-object
# dummies <- list(dummy1, dummy2, dummy3)
# # and set variable labels for all three dummies
# dummies <- set_label(dummies, c("First Dummy", "2nd Dummy", "Third dummy"))
# # see result...
# get_label(dummies)
#
#
set_label <- function(x, lab, attr.string = NULL) {
  # auto-detect variable label attribute
  if (is.null(attr.string)) attr.string <- getVarLabelAttribute(x)
  # still nothing found? then leave...
  if (is.null(attr.string)) attr.string <- "label"

  # do we have all necessary arguments?
  if (!is.null(lab) && !is.null(x)) {
    # if we have a data frame, we need a variable label
    # for each column (variable) of the data frame
    if (is.data.frame(x) || is.list(x)) {
      # get length of data frame or list, i.e.
      # determine number of variables
      if (is.data.frame(x))
        nvars <- ncol(x)
      else
        nvars <- length(x)

      # check for matching length of supplied labels
      if (nvars != length(lab)) {
        message("Argument `lab` must be of same length as numbers of columns in `x`.")
      } else {
        # do we have a data frame? If yes, save column names
        if (is.data.frame(x)) cnames <- colnames(x)

        # iterate all columns / list elements
        for (i in 1:nvars) {
          if (is_empty(lab[i])) {
            # empty label value means, remove
            # the label attribute
            attr(x[[i]], attr.string) <- NULL
          } else {
            # set variable label
            attr(x[[i]], attr.string) <- lab[i]
            # set names attribute. equals variable name
            if (is.data.frame(x)) names(attr(x[[i]], attr.string)) <- cnames[i]
          }
        }
      }
    } else {
      if (is_empty(lab))
        # empty label, so remove label attribute
        attr(x, attr.string) <- NULL
      else
        # set label attribute
        attr(x, attr.string) <- lab
    }
  }
  return(x)
}


# @rdname set_label
#
`set_label<-` <- function(x, attr.string = NULL, value) {
  UseMethod("set_label<-")
}

#
`set_label<-.default` <- function(x, attr.string = NULL, value) {
  x <- set_label(x, value, attr.string)
  x
}
# @title Add value labels to variables
# @name set_labels
#
# @description This function adds character \code{labels} as attribute
#                (named \code{"labels"} or \code{"value.labels"}) to a variable
#                or vector \code{x}, resp. to a set of variables in a
#                \code{data.frame} or a \code{list}-object. These value labels will be accessed
#                by functions of the \pkg{sjPlot} package, in order to automatically set values
#                or legend labels, however, \pkg{sjmisc} provides functions to
#                quickly access these attributes for other purposes.
#
# @seealso See package vignettes or \href{http://www.strengejacke.de/sjPlot/}{online documentation}
#            for more details; \code{\link{set_label}} to manually set variable labels or
#            \code{\link{get_label}} to get variable labels; \code{\link{add_labels}} to
#            add additional value labels without replacing the existing ones.
#
# @param x Variable (vector), \code{list} of variables or a \code{data.frame}
#          where value label attributes should be added. Replaces former value labels.
# @param labels (Named) character vector of labels that will be added to \code{x} as
#          \code{"labels"} or \code{"value.labels"} attribute.
#          \itemize{
#            \item if \code{labels} is \strong{not} a \emph{named vector}, its length must equal the value range of \code{x}, i.e. if \code{x} has values from 1 to 3, \code{labels} should have a length of 3;
#            \item if length of \code{labels} is intended to differ from length of unique values of \code{x}, a warning is given. You can still add missing labels with the \code{force.labels} or \code{force.values} arguments; see 'Note'.
#            \item if \code{labels} \strong{is} a \emph{named vector}, value labels will be set accordingly, even if \code{x} has a different length of unique values. See 'Note' and 'Examples'.
#            \item if \code{x} is a data frame, \code{labels} may also be a \code{\link{list}} of (named) character vectors;
#            \item if \code{labels} is a \code{list}, it must have the same length as number of columns of \code{x};
#            \item if \code{labels} is a vector and \code{x} is a data frame, \code{labels} will be applied to each column of \code{x}.
#            }
#          Use \code{labels = ""} to remove labels-attribute from \code{x}.
# @param value See \code{labels},
# @param force.labels Logical; if \code{TRUE}, all \code{labels} are added as value label
#          attribute, even if \code{x} has less unique values than length of \code{labels}
#          or if \code{x} has a smaller range than length of \code{labels}. See 'Examples'.
#          This parameter will be ignored, if \code{labels} is a named vector.
# @param force.values Logical, if \code{TRUE} (default) and \code{labels} has less
#          elements than unique values of \code{x}, additional values not covered
#          by \code{labels} will be added as label as well. See 'Examples'.
#          This parameter will be ignored, if \code{labels} is a named vector.
# @return \code{x} with value label attributes; or with removed label-attributes if
#            \code{labels = ""}.
#
# @details See 'Details' in \code{\link{get_labels}}.
#
# @note \itemize{
#         \item if \code{labels} is a named vector, \code{force.labels} and \code{force.values} will be ignored, and only values defined in \code{labels} will be labelled;
#         \item if \code{x} has less unique values than \code{labels}, redundant labels will be dropped, see \code{force.labels};
#         \item if \code{x} has more unique values than \code{labels}, only matching values will be labelled, other values remain unlabelled, see \code{force.values};
#         }
#         If you only want to change partial value labels, use \code{\link{add_labels}} instead.
#         Furthermore, see 'Note' in \code{\link{get_labels}}.
#
# @examples
# \dontrun{
# library(sjPlot)
# dummy <- sample(1:4, 40, replace = TRUE)
# sjp.frq(dummy)
#
# dummy <- set_labels(dummy, c("very low", "low", "mid", "hi"))
# sjp.frq(dummy)}
#
# # force using all labels, even if not all labels
# # have associated values in vector
# x <- c(2, 2, 3, 3, 2)
# # only two value labels
# x <- set_labels(x, c("1", "2", "3"))
# x
#
# # or use:
# # set_labels(x) <- c("1", "2", "3")
#
# \dontrun{
# sjp.frq(x)}
# # all three value labels
# x <- set_labels(x, c("1", "2", "3"), force.labels = TRUE)
# x
# \dontrun{
# sjp.frq(x)}
#
# # create vector
# x <- c(1, 2, 3, 2, 4, NA)
# # add less labels than values
# x <- set_labels(x, c("yes", "maybe", "no"), force.values = FALSE)
# x
# # add all necessary labels
# x <- set_labels(x, c("yes", "maybe", "no"), force.values = TRUE)
# x
#
# # set labels and missings
# x <- c(1, 1, 1, 2, 2, -2, 3, 3, 3, 3, 3, 9)
# x <- set_labels(x, c("Refused", "One", "Two", "Three", "Missing"))
# x
#
# x <- set_na(x, c(-2, 9), as.attr = TRUE)
# x
# frq(as_labelled(x))
#
#
# # set labels via named vector,
# # not using all possible values
# data(efc)
# get_labels(efc$e42dep)
#
#x <- set_labels(efc$e42dep, c(`independent` = 1,
#                              `severe dependency` = 2,
#                              `missing value` = 9))
# get_labels(x, include.values = "p")
#
# get_labels(x, include.values = "p", include.non.labelled = TRUE)
#
#
# # setting same value labels to multiple vectors
# # create a set of dummy variables
# dummy1 <- sample(1:4, 40, replace = TRUE)
# dummy2 <- sample(1:4, 40, replace = TRUE)
# dummy3 <- sample(1:4, 40, replace = TRUE)
# # put them in list-object
# dummies <- list(dummy1, dummy2, dummy3)
# # and set same value labels for all three dummies
# dummies <- set_labels(dummies, c("very low", "low", "mid", "hi"))
# # see result...
# get_labels(dummies)
#
#
set_labels <- function(x,
                       labels,
                       force.labels = FALSE,
                       force.values = TRUE) {
  return(set_labels_helper(x, labels, force.labels, force.values))
}


set_labels_helper <- function(x, labels, force.labels, force.values) {
  # any valid labels? if not, return vector
  if (is.null(labels) || length(labels) == 0) return(x)

  # valid vector?
  if (is.null(x) || all(is.na(x)) {
    # can't add value labels to NULL/NA vectors.
    return(x)
  }
  
  # convert single vector
  if (!is.list(x) && (is.vector(x) || is.atomic(x))) {
    return(set_values_vector(x,
                             labels,
                             NULL,
                             force.labels,
                             force.values))
  } else if (is.data.frame(x) || is.matrix(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    for (i in 1:nvars) {
      # list of labels makes sense if multiple variable
      # should be labelled with different labels
      if (is.list(labels)) {
        # check for valid length of supplied label-list
        if (i <= length(labels)) {
          x[[i]] <- set_values_vector(x[[i]],
                                      labels[[i]],
                                      colnames(x)[i],
                                      force.labels,
                                      force.values)
        }
      } else if (is.vector(labels)) {
        # user supplied only one vector of labels.
        # so each variable gets the same labels
        x[[i]] <- set_values_vector(x[[i]],
                                    labels,
                                    colnames(x)[i],
                                    force.labels,
                                    force.values)
      } else {
        warning("`labels` must be a list of same length as `ncol(x)` or a vector.", call. = TRUE)
      }
    }
    return(x)
  }
}


#' @importFrom stats na.omit
get_value_range <- function(x) {
  # check if var is a factor
  if (is.factor(x)) {
    # check if we have numeric levels
    if (!is_num_fac(x)) {
      # retrieve levels. since levels are numeric, we
      # have minimum and maximum values
      minval <- 1
      maxval <- nlevels(x)
    } else {
      # levels are not numeric. we need to convert them
      # first to retrieve minimum level, as numeric
      minval <- min(as.numeric(levels(x)), na.rm = T)

      # check range, add minimum, so we have max
      maxval <- diff(range(as.numeric(levels(x)))) + minval
    }
  } else if (is.character(x)) {
    # if we have a character vector, we don't have
    # min and max values. instead, we count the
    # amount of unique string values
    minval <- 1
    maxval <- length(unique(stats::na.omit(x)))
  } else {
    # retrieve values
    minval <- min(x, na.rm = TRUE)
    maxval <- max(x, na.rm = TRUE)
  }
  # determine value range
  valrange <- maxval - minval + 1
  # return all
  return(list(minval = minval,
              maxval = maxval,
              valrange = valrange))
}


#' @importFrom stats na.omit
set_values_vector <- function(x, labels, var.name, force.labels, force.values) {
  # valid vector?
  if (is.null(x)) {
    warning("can't add value labels to NULL vectors.", call. = T)
    return(x)
  }
  # auto-detect variable label attribute
  attr.string <- getValLabelAttribute(x)
  # do we have any label attributes?
  if (is.null(attr.string)) attr.string <- "labels"
  # check for null
  if (!is.null(labels)) {
    # if labels is empty string, remove labels attribute
    if (length(labels) == 1 && nchar(labels) == 0) {
      attr(x, attr.string) <- NULL

      # set labels for character vectors here!
    } else if (is.character(x)) {
      # string vectors can only get labels of type string
      if (typeof(labels) == typeof(x)) {
        # reverse names and labels
        dummy.labels <- names(labels)
        # but first check if we have named vector or not...
        if (is.null(dummy.labels)) {
          warning("`labels` must be a named vector.", call. = T)
        } else {
          names(dummy.labels) <- unname(labels)
          attr(x, attr.string) <- dummy.labels
        }
      } else {
        warning("Character vectors can only get labels of same type.", call. = T)
      }

      # set labels for numeric vectors or factors here
    } else {
      # determine value range
      vr <- get_value_range(x)
      # copy values to variables
      valrange <- vr$valrange
      minval <- vr$minval
      maxval <- vr$maxval
      # check for unlisting
      if (is.list(labels)) labels <- unlist(labels)

      # determine amount of labels and unique values
      lablen <- length(labels)
      values <- sort(unique(stats::na.omit(as.vector(x))))

      # do we have an ordered factor?
      if (is.ordered(x)) values <- values[order(levels(x))]

      # set var name string
      if (is.null(var.name) || nchar(var.name) < 1) {
        name.string <- "x"
      } else {
        name.string <- var.name
      }

      # check for valid bounds of values
      if (is.infinite(valrange)) {
        warning("can't set value labels. Infinite value range.", call. = T)

        # check if we have named vector. in this
        # case, just add these values
      } else if (!is.null(names(labels))) {
        # check names and value attributes. value labels
        # and values might be reversed
        if (!anyNA(suppressWarnings(as.numeric(names(labels)))) &&
            anyNA(suppressWarnings(as.numeric(labels))) &&
            !anyNA(suppressWarnings(as.numeric(values)))) {
          dummy.lab.values <- as.numeric(names((labels)))
          dummy.lab.labels <- as.character(labels)
          labels <- dummy.lab.values
          names(labels) <- dummy.lab.labels
        }

        # set attributes
        if (anyNA(suppressWarnings(as.numeric(labels)))) {
          # here we have also non-numeric labels, so we set
          # names as character string
          attr(x, attr.string) <- labels
        } else {
          # we have only numeric labels, so we set them
          # as numeric values
          attr(x, attr.string) <- as.numeric(labels)
        }
        names(attr(x, attr.string)) <- as.character(names(labels))
        # check for valid length of labels
        # if amount of labels and values are equal,
        # we assume matching labels
      } else if (length(values) == lablen) {
        # set attributes
        # check whether values is numeric, or - if character -
        # only has numeric character values. If yes, add values
        # as numeric labels-attribute
        if (is.numeric(values) || !anyNA(suppressWarnings(as.numeric(values))))
          attr(x, attr.string) <- as.numeric(values)
        else
          attr(x, attr.string) <- as.character(values)
        names(attr(x, attr.string)) <- labels
        # check for valid length of labels
        # here, we have a smaller value range (i.e. less values)
        # than amount of labels
      } else if (valrange < lablen) {
        # do we want to force to set labels, even if we have more labels
        # than values in variable?
        if (force.labels) {
          attr(x, attr.string) <- as.numeric(c(1:lablen))
          names(attr(x, attr.string)) <- labels
        } else {
          # we have more labels than values, so just take as many
          # labes as values are present
          message(sprintf("More labels than values of \"%s\". Using first %i labels.", name.string, valrange))
          attr(x, attr.string) <- as.numeric(c(minval:maxval))
          names(attr(x, attr.string)) <- labels[1:valrange]
        }
        # value range is larger than amount of labels. we may
        # have not continuous value range, e.g. "-2" as filter and
        # 1 to 4 as valid values, i.e. -1 and 0 are missing
      } else if (valrange > lablen) {
        # check if user wants to add missing values
        if (force.values) {
          # get amount of unique values
          valrange <- length(values)

          # still no match?
          if (valrange != lablen) {
            # check which one is longer, and get missing values
            add_values <- ifelse(valrange > lablen, valrange[-lablen], lablen[-valrange])
            # add missing values to labels
            labels <- c(labels, as.character(add_values))
            # tell user about modification
            message(sprintf("More values in \"%s\" than length of \"labels\". Additional values were added to labels.", name.string))
          }

          # set attributes
          attr(x, attr.string) <- as.numeric(c(1:valrange))
          names(attr(x, attr.string)) <- labels
        } else {
          # tell user about modification
          message(sprintf("\"%s\" has more values than \"labels\", hence not all values are labelled.", name.string))
          # drop values with no associated labels
          attr(x, attr.string) <- as.numeric(c(1:length(labels)))
          names(attr(x, attr.string)) <- labels
        }
      } else {
        attr(x, attr.string) <- as.numeric(c(minval:maxval))
        names(attr(x, attr.string)) <- labels
      }
    }
  }
  return(x)
}

# @rdname set_labels
#
`set_labels<-` <- function(x, force.labels = FALSE, force.values = TRUE, value) {
  UseMethod("set_labels<-")
}

#
`set_labels<-.default` <- function(x, force.labels = FALSE, force.values = TRUE, value) {
  x <- set_labels(x, value, force.labels, force.values)
  x
}
# @title Set NA for specific variable values
# @name set_na
#
# @description This function sets specific values of a variable, data frame
#                or list of variables as missings (\code{NA}).
#
# @seealso \code{\link{replace_na}} to replace \code{\link{NA}}'s with specific
#            values, \code{\link{rec}} for general recoding of variables and
#            \code{\link{recode_to}} for re-shifting value ranges. See
#            \code{\link{get_na}} to get values of missing values in
#            labelled vectors and \code{\link{to_na}} to convert missing value
#            codes into \code{NA}.
#
# @param x Variable (vector), \code{data.frame} or \code{list} of variables where new
#          missing values should be defined. If \code{x} is a \code{data.frame}, each
#          column is assumed to be a new variable, where missings should be defined.
# @param value Numeric vector with values that should be replaced with \code{\link{NA}}'s.
#          Thus, for each variable in \code{x}, \code{value} are replaced by \code{NA}'s.
#          Or: a logical vector describing which values should be translated
#          to missing values. See 'Details' and 'Examples'.
# @param as.attr Logical, if \code{TRUE}, \code{value} of \code{x} will \strong{not}
#          be converted to \code{NA}. Rather, the missing code values of \code{value}
#          will be added as missing-attribute \code{is_na} to the vector. See
#          \code{\link{labelled}} for more details, and 'Examples'.
#
# @return \code{x}, where each value of \code{value} is replaced by an \code{NA}.
#
# @note Value and variable label attributes (see, for instance, \code{\link{get_labels}}
#         or \code{\link{set_labels}}) are preserved.
#
# @details \code{set_na} converts those values to \code{NA} that are
#            specified in the function's \code{value} argument; hence,
#            by default, \code{set_na} ignores any missing code attributes
#            like \code{is_na}. \code{\link{to_na}}, by contrast, converts
#            values to \code{NA}, which are defined as missing through the
#            \code{is_na}-attribute of a vector (see \code{\link{labelled}}).
#            \cr \cr
#            If \code{as.attr = TRUE}, \code{value} in \code{x} will \strong{not}
#            be converted to \code{NA}. Instead, the attribute \code{is_na}
#            will be added to \code{x}, indicating which values should be coded
#            as missing. \code{value} may either be numeric, with each number
#            indicating a value that should be defined as missing; or a vector
#            of logicals, describing which values should be translated to
#            missing values (see 'Examples').
#            \cr \cr
#            Furthermore, see 'Details' in \code{\link{get_na}}.
#
# @examples
# # create random variable
# dummy <- sample(1:8, 100, replace = TRUE)
# # show value distribution
# table(dummy)
# # set value 1 and 8 as missings
# dummy <- set_na(dummy, c(1, 8))
# # show value distribution, including missings
# table(dummy, exclude = NULL)
#
# # create sample data frame
# dummy <- data.frame(var1 = sample(1:8, 100, replace = TRUE),
#                     var2 = sample(1:10, 100, replace = TRUE),
#                     var3 = sample(1:6, 100, replace = TRUE))
# # show head of data frame
# head(dummy)
# # set value 2 and 4 as missings
# dummy <- set_na(dummy, c(2, 4))
# # show head of new data frame
# head(dummy)
#
# # create list of variables
# data(efc)
# dummy <- list(efc$c82cop1, efc$c83cop2, efc$c84cop3)
# # check original distribution of categories
# lapply(dummy, table, exclude = NULL)
# # set 3 to NA
# lapply(set_na(dummy, 3), table, exclude = NULL)
#
# # create random variable
# dummy <- sample(1:5, 100, replace = TRUE)
# # declare missing values, but only as attribute
# dummy <- set_na(dummy, c(3, 5), as.attr = TRUE)
#
# str(dummy)
# table(dummy)
# get_na(dummy)
#
# # create random variable
# dummy <- sample(1:5, 100, replace = TRUE)
# # declare missing values, but only as attribute
# # missing code definition may be logical indices
# dummy <- set_na(dummy, c(FALSE, FALSE, FALSE, TRUE, TRUE), as.attr = TRUE)
#
# str(dummy)
# table(dummy)
# get_na(dummy)
#
#
set_na <- function(x, value, as.attr = FALSE) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # dichotomize all
    for (i in 1:nvars) x[[i]] <- set_na_helper(x[[i]], value, as.attr)
    return(x)
  } else {
    return(set_na_helper(x, value, as.attr))
  }
}


#' @importFrom stats na.omit
set_na_helper <- function(x, value, as.attr = FALSE) {
  # does user want to add missing codes as is_na attribute?
  # if yes, do so here...
  if (as.attr) {
    x <- set_na_attr(x, value)
  } else {
    # check if we have any values at all?
    if (is.null(value)) return(x)

    # find associated values in x
    # and set them to NA
    x[x %in% value] <- NA

    # auto-detect variable label attribute
    attr.string <- getValLabelAttribute(x)
    # check if x has label attributes
    if (!is.null(attr.string)) {
      # retrieve value labels
      vl <- attr(x, attr.string, exact = T)
      # retrieve label names
      ln <- names(vl)

      # check if value labels exist, and if yes, remove them
      labelpos <- suppressWarnings(which(as.numeric(vl) %in% value))

      # remove NA label
      if (length(labelpos > 0)) {
        vl <- vl[-labelpos]
        ln <- ln[-labelpos]
      } else {
        # if vl were not numeric convertable, try character conversion
        # check if value labels exist, and if yes, remove them
        labelpos <- suppressWarnings(which(as.character(vl) %in% value))
        # remove NA label
        if (length(labelpos > 0)) {
          vl <- vl[-labelpos]
          ln <- ln[-labelpos]
        }
      }

      # do we have any labels left?
      if (length(vl) > 0) {
        # if yes, set back label attribute
        attr(x, attr.string) <- vl
        names(attr(x, attr.string)) <- ln

        # shorten is_na attribute
        na.flag <- get_na_flags(x)
        if (!is.null(na.flag)) {
          # do we have is_na attribute? if yes,
          # remove missing flags of values set to NA
          attr(x, getNaAttribute()) <- na.flag[-labelpos]
        }
      } else {
        # else remove attribute
        attr(x, attr.string) <- NULL
        # remove is_na attribute, no longer needed
        attr(x, getNaAttribute()) <- NULL

        # unclass labelled, because it may result
        # in errors when printing a labelled-class-vector
        # without labelled and is_na attribute
        if (is_labelled(x)) x <- unclass(x)
      }
    }

    # if we have a factor, remove unused levels
    if (is.factor(x)) x <- droplevels(x)
  }
  return(x)
}


set_na_attr <- function(x, na.values) {
  # get values
  all.values <- get_values(x, sort.val = FALSE, drop.na = FALSE)
  # do we have value attributes?
  if (is.null(all.values)) {
    # we assume a simple numeric vector, so let's add
    # some label attributes
    all.values <- sort(unique(stats::na.omit(x)))
    x <- set_labels(x, as.character(all.values))
  }
  if (is.null(na.values)) {
    # is na.values NULL? Then set FALSE (no missing)
    # for all value codes
    na.values <- rep(FALSE, length(all.values))
  } else if (!is.logical(na.values)) {
    # if we do not have logical indices,
    # set TRUE for all NA-codes and FALSE for all other
    na.values <- !is.na(match(all.values, na.values))
  }
  # same length?
  if (length(na.values) != length(all.values))
    # If not, warn user
    warning("Length of logical indices for missing codes did not match length of values.", call. = F)
  # set is_na attribute
  attr(x, getNaAttribute()) <- na.values
  return(x)
}

# @rdname set_na
#
`set_na<-` <- function(x, as.attr = FALSE, value) {
  UseMethod("set_na<-")
}

#
`set_na<-.default` <- function(x, as.attr = FALSE, value) {
  x <- set_na(x, value, as.attr)
  x
}
# @title Expected and relative table values
# @name table_values
# @description This function calculates a table's cell, row and column percentages as
#                well as expected values and returns all results as lists of tables.
#
# @param tab Simple \code{\link{table}} or \code{\link{ftable}} of which cell, row and column percentages
#          as well as expected values are calculated. Tables of class \code{\link{xtabs}} and other will
#          be coerced to \code{\link{ftable}} objects.
# @param digits Amount of digits for the table percentage values.
#
# @return (Invisibly) returns a list with four tables:
#         \enumerate{
#          \item \code{cell} a table with cell percentages of \code{tab}
#          \item \code{row} a table with row percentages of \code{tab}
#          \item \code{col} a table with column percentages of \code{tab}
#          \item \code{expected} a table with expected values of \code{tab}
#         }
#
# @examples
# tab <- table(sample(1:2, 30, TRUE), sample(1:3, 30, TRUE))
# # show expected values
# table_values(tab)$expected
# # show cell percentages
# table_values(tab)$cell
#
#
table_values <- function(tab, digits = 2) {
  # convert to ftable object
  if (all(class(tab) != "ftable")) tab <- ftable(tab)
  tab.cell <- round(100 * prop.table(tab), digits)
  tab.row <- round(100 * prop.table(tab, 1), digits)
  tab.col <- round(100 * prop.table(tab, 2), digits)
  tab.expected <- as.table(round(as.array(margin.table(tab, 1)) %*% t(as.array(margin.table(tab, 2))) / margin.table(tab)))

  # return results
  invisible(structure(class = "sjutablevalues",
                      list(cell = tab.cell,
                           row = tab.row,
                           col = tab.col,
                           expected = tab.expected)))
}


# @title Plot Levene-Test for One-Way-Anova
# @name levene_test
#
# @description Plot results of Levene's Test for Equality of Variances for One-Way-Anova.
#
# @param depVar Dependent variable.
# @param grpVar Grouping (independent) variable, as unordered factor.
#
# @examples
# data(efc)
# levene_test(efc$c12hour, efc$e42dep)
#
#
levene_test <- function(depVar, grpVar) {
  # check if grpVar is factor
  if (!is.factor(grpVar)) grpVar <- factor(grpVar)
  # remove missings
  df <- stats::na.omit(data.frame(depVar, grpVar))
  # calculate means
  means <- tapply(df$depVar, df$grpVar, mean)
  depVarNew <- abs(df$depVar - means[df$grpVar])
  message("\nLevene's Test for Homogeneity of Variances\n------------------------------------------")
  fit <- aov(depVarNew ~ df$grpVar)
  print(summary(fit))
  pval <- summary(fit)[[1]]['Pr(>F)'][1,1]
  # print "summary" of test
  message("\nConclusion:")
  if (pval > 0.05) {
    message("Groups are homogeneous. Everything's fine.\n")
  } else {
    message("Groups are not homogeneous!\n")
  }
}


# retrieve variance of random intercepts
# and residuals
lmer_var <- function(fit) {
  reva <- summary(fit)$varcor
  # retrieve only intercepts
  vars <- unlist(lapply(reva, function(x) x[[1]]))
  names(vars) <- names(reva)
  # residual variances
  if (any(class(fit) == "glmerMod")) {
    # for logistic models, we use pi / 3
    resid_var <- (pi^2) / 3
  } else {
    # for linear models, we have a clear
    # residual variance
    resid_var <- attr(reva, "sc")^2
  }
  return(list('Between group variance' = vars,
              'Within group variance' = resid_var))
}


#' @importFrom stats pf
lm_pval_fstat <- function(x) {
  if (class(x) != "lm") stop("Not an object of class 'lm'.", call. = F)
  f <- summary(x)$fstatistic
  p <- stats::pf(f[1], f[2], f[3], lower.tail = F)
  return(as.vector(p))
}
# @title Split numeric variables into smaller groups
# @name split_var
#
# @description Recode numeric variables into equal sized groups, i.e. a
#                variable is cut into a smaller number of groups at
#                specific cut points.
#
# @seealso \itemize{
#            \item \code{\link{group_var}}
#            \item \code{\link{rec}}
#          }
#
# @param x Numeric vector, data frame or list of numeric vectors,
#            which should split into groups.
# @param groupcount The new number of groups that \code{x} should be split into.
# @param inclusive Logical; if \code{TRUE}, cut point value are included in
#          the preceeding group. This may be necessary if cutting a vector into
#          groups does not define proper ("equal sized") group sizes.
#          See 'Note' and 'Examples'.
#
# @inheritParams group_var
# @inheritParams rec
#
# @return A grouped variable with equal sized groups.
#
# @details \code{split_var} splits a variable into equal sized groups, where the
#            amount of groups depends on the \code{groupcount}-argument. Thus,
#            this functions \code{\link{cut}s} a variable into groups at the
#            specified \code{\link[stats]{quantile}s}.
#            \cr \cr
#            By contrast, \code{\link{group_var}} recodes a variable into
#            groups, where all values within a group have the same range
#            (e.g., from 1-5, 6-10, 11-15 etc.).
#
# @note In case a vector has only few different unique values, splitting into
#         equal sized groups may fail. In this case, use the \code{inclusive}-argument
#         to shift a value at the cut point into the lower, preceeding group to
#         get equal sized groups. See 'Examples'.
#
# @examples
# data(efc)
# # non-grouped
# table(efc$neg_c_7)
#
# # split into 3 groups
# table(split_var(efc$neg_c_7, 3))
#
#
# # original
# table(efc$e42dep)
#
# # two groups, non-inclusive cut-point
# # vector split leads to unequal group sizes
# table(split_var(efc$e42dep, 2))
#
# # two groups, inclusive cut-point
# # group sizes are equal
# table(split_var(efc$e42dep, 2, inclusive = TRUE))
#
#' @importFrom stats quantile
#
split_var <- function(x, groupcount, as.num = FALSE, val.labels = NULL, var.label = NULL, inclusive = FALSE) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # na all
    for (i in 1:nvars) x[[i]] <- split_var_helper(x[[i]],
                                                  groupcount,
                                                  as.num,
                                                  val.labels,
                                                  var.label,
                                                  inclusive)
    return(x)
  } else {
    return(split_var_helper(x,
                            groupcount,
                            as.num,
                            val.labels,
                            var.label,
                            inclusive))
  }
}

split_var_helper <- function(x, groupcount, as.num, val.labels, var.label, inclusive) {
  # retrieve variable label
  if (is.null(var.label))
    var_lab <- get_label(x)
  else
    var_lab <- var.label
  # do we have any value labels?
  val_lab <- val.labels
  # amount of "cuts" is groupcount - 1
  zaehler <- seq_len(groupcount - 1)
  # prepare division
  nenner <- rep(groupcount, length(zaehler))
  # get quantiles
  qu_prob <- zaehler / nenner
  # get quantile values
  grp_cuts <- stats::quantile(x, qu_prob, na.rm = TRUE)
  # cut variables into groups
  retval <- cut(x,
                c(0, grp_cuts, max(x, na.rm = T)),
                include.lowest = !inclusive,
                right = inclusive)
  # rename factor levels
  levels(retval) <- c(1:groupcount)
  # to numeric?
  if (as.num) retval <- to_value(retval)
  # set back variable and value labels
  retval <- suppressWarnings(set_label(retval, var_lab))
  retval <- suppressWarnings(set_labels(retval, val_lab))
  # return value
  return(retval)
}
# @title Standardized Beta coefficients and CI of lm and mixed models
# @name std_beta
# @description Returns the standardized beta coefficients and confidence intervals
#                of a fitted linear (mixed) models, i.e. \code{fit} must either
#                be of class \code{lm} or \code{\link[lme4]{merMod}}.
#
# @param fit Fitted linear (mixed) model of class \code{\link{lm}} or
#          \code{\link[lme4]{merMod}} (\pkg{lme4} package).
# @param include.ci Logical, if \code{TRUE}, a data frame with confidence
#          intervals will be returned, when \code{fit} is of class \code{lm}.
#          If \code{fit} is a \code{lmerMod} object (\pkg{lme4} package),
#          always returns standard error instead of confidence intervals
#          (hence, this parameter will be ignored when \code{fit} is a
#          \code{lmerMod} object).
# @param type If \code{fit} is of class \code{lm}, normal standardized coefficients
#          are computed by default. Use \code{type = "std2"} to follow
#          \href{http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf}{Gelman's (2008)}
#          suggestion, rescaling the estimates by deviding them by two standard
#          deviations, so resulting coefficients are directly comparable for
#          untransformed binary predictors.
# @return A vector with standardized beta coefficients of the fitted linear model, or a data frame
#           with standardized beta coefficients and confidence intervals, if
#           \code{include.ci = TRUE}.
#
# @details  "Standardized coefficients refer to how many standard deviations a dependent variable will change,
#         per standard deviation increase in the predictor variable. Standardization of the coefficient is
#         usually done to answer the question of which of the independent variables have a greater effect
#         on the dependent variable in a multiple regression analysis, when the variables are measured
#         in different units of measurement (for example, income measured in dollars and family size
#         measured in number of individuals)." (Source: Wikipedia)
#
# @note For \code{\link[nlme]{gls}}-objects, standardized beta coefficients may be wrong
#         for categorical variables (\code{factors}), because the \code{model.matrix} for
#         \code{gls} objects returns the original data of the categorical vector,
#         and not the 'dummy' coded vectors as for other classes. See, as example: \cr \cr
#         \code{head(model.matrix(lm(neg_c_7 ~ as.factor(e42dep), data = efc, na.action = na.omit)))}
#         \cr \cr and \cr \cr
#         \code{head(model.matrix(nlme::gls(neg_c_7 ~ as.factor(e42dep), data = efc, na.action = na.omit)))}.
#         \cr \cr
#         In such cases, use \code{\link{to_dummy}} to create dummies from
#         factors.
#
# @references \itemize{
#              \item \href{http://en.wikipedia.org/wiki/Standardized_coefficient}{Wikipedia: Standardized coefficient}
#              \item Gelman A (2008) "Scaling regression inputs by dividing by two standard deviations." \emph{Statistics in Medicine 27: 2865–2873.} \url{http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf}
#              }
#
# @examples
# # fit linear model
# fit <- lm(Ozone ~ Wind + Temp + Solar.R, data = airquality)
# # print std. beta coefficients
# std_beta(fit)
#
# # print std. beta coefficients and ci
# std_beta(fit, include.ci = TRUE)
#
# # print std. beta coefficients and ci, using
# # 2 sd and center binary predictors
# std_beta(fit, include.ci = TRUE, type = "std2")
#
#' @importFrom stats model.matrix coef terms
#' @importFrom nlme getResponse
#
std_beta <- function(fit,
                     include.ci = FALSE,
                     type = "std") {
  # if we have merMod object (lme4), we need
  # other function to compute std. beta
  if (any(class(fit) == "lmerMod") || any(class(fit) == "merModLmerTest")) {
    return(sjs.stdmm(fit))
  } else if (type == "std2") {
    # is package available?
    if (!requireNamespace("arm", quietly = TRUE)) {
      stop("Package `arm` needed for computing this type of standardized estimates. Please install it.", call. = FALSE)
    }
    # has model intercept?
    tmp_i <- attr(stats::terms(fit), "intercept")
    has_intercept <- !is.null(tmp_i) & tmp_i == 1
    # get standardized model parameter
    stdbv2_all <- arm::standardize(fit)
    # get standardized estimates
    beta <- stats::coef(stdbv2_all)
    # remove intercept?
    if (has_intercept) beta <- beta[-1]
    # get standardized se
    std2se <- summary(stdbv2_all)$coefficients[, 2]
    # remove intercept?
    if (has_intercept) std2se <- std2se[-1]
    # check if confidence intervals should also be returned
    # if yes, create data frame with sb and ci
    if (include.ci) {
      return(data.frame(beta = beta,
                        ci.low = beta - std2se * 1.96,
                        ci.hi = beta + std2se * 1.96))
    } else {
      return(beta)
    }
  } else {
    # has model intercept?
    tmp_i <- attr(terms(fit), "intercept")
    has_intercept <- !is.null(tmp_i) & tmp_i == 1
    # get coefficients
    b <- stats::coef(fit)
    # remove intercept?
    if (has_intercept) b <- b[-1]
    # get data as data frame
    fit.data <- as.data.frame(stats::model.matrix(fit))
    # remove intercept?
    if (has_intercept) fit.data <- fit.data[, -1]
    # convert factor to numeric, else sd throws a warning
    fit.data <- as.data.frame(sapply(fit.data,
                                     function(x)
                                       if (is.factor(x))
                                         to_value(x, keep.labels = F)
                                       else
                                         x))
    # get standard deviations for predictors
    sx <- sapply(fit.data, sd, na.rm = T)
    if (any(class(fit) == "gls"))
      sy <- sapply(as.data.frame(as.vector(nlme::getResponse(fit))), sd, na.rm = T)
    else
      sy <- sapply(as.data.frame(fit$model)[1], sd, na.rm = T)
    beta <- b * sx / sy
    if (any(class(fit) == "gls"))
      se <- summary(fit)$tTable[, 2]
    else
      se <- summary(fit)$coef[, 2]
    # remove intercept?
    if (has_intercept) se <- se[-1]
    # compute standard error
    beta.se <- se * sx / sy
    # check if confidence intervals should also be returned
    # if yes, create data frame with sb and ci
    if (include.ci) {
      return(data.frame(beta = beta,
                        ci.low = (beta - beta.se * 1.96),
                        ci.hi = (beta + beta.se * 1.96)))
    } else {
      return(beta)
    }
  }
}


#' @importFrom stats sd coef
#' @importFrom lme4 fixef getME
sjs.stdmm <- function(fit) {
  # code from Ben Bolker, see
  # http://stackoverflow.com/a/26206119/2094622

  # check if suggested package is available
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
  }
  sdy <- stats::sd(lme4::getME(fit, "y"))
  sdx <- apply(lme4::getME(fit, "X"), 2, sd)
  sc <- lme4::fixef(fit) * sdx / sdy
  se.fixef <- stats::coef(summary(fit))[, "Std. Error"]
  se <- se.fixef * sdx / sdy
  mydf <- data.frame(stdcoef = sc, stdse = se)
  rownames(mydf) <- names(lme4::fixef(fit))
  return(mydf)
}
# @title Check if string contains pattern
# @name str_contains
# @description This functions checks whether a string \code{x} contains
#                the string \code{pattern}. By default, this function is
#                case sensitive.
#
# @param x Character string where matches are sought.
# @param pattern Character string to be matched in \code{x}. May also be a
#          character vector of length > 1 (see 'Examples').
# @param ignore.case Logical, whether matching should be case sensitive or not.
# @param logic Indicates whether a logical combination of multiple search pattern
#          should be made.
#          \itemize{
#            \item Use \code{"or"}, \code{"OR"} or \code{"|"} for a logical or-combination, i.e. at least one element of \code{pattern} is in \code{x}.
#            \item Use \code{"and"}, \code{"AND"} or \code{"&"} for a logical AND-combination, i.e. all elements of \code{pattern} are in \code{x}.
#            \item Use \code{"not"}, \code{"NOT"} or \code{"!"} for a logical NOT-combination, i.e. no element of \code{pattern} is in \code{x}.
#            \item By default, \code{logic = NULL}, which means that \code{TRUE} or \code{FALSE} is returned for each element of \code{pattern} separately.
#          }
#
# @return \code{TRUE} if \code{x} contains \code{pattern}.
#
# @examples
# str_contains("hello", "hel")
# str_contains("hello", "hal")
#
# str_contains("hello", "Hel")
# str_contains("hello", "Hel", ignore.case = TRUE)
#
# # which patterns are in "abc"?
# str_contains("abc", c("a", "b", "e"))
#
# # any pattern in "abc"?
# str_contains("abc", c("a", "b", "e"), logic = "or")
#
# # all patterns in "abc"?
# str_contains("abc", c("a", "b", "e"), logic = "and")
# str_contains("abc", c("a", "b"), logic = "and")
#
# # no patterns in "abc"?
# str_contains("abc", c("a", "b", "e"), logic = "not")
# str_contains("abc", c("d", "e", "f"), logic = "not")
#
#
str_contains <- function(x, pattern, ignore.case = FALSE, logic = NULL) {
  # ignore case in search term
  if (ignore.case) x <- tolower(x)
  # counter for matches
  cnt <- c()
  # iterate patterns
  for (k in pattern) {
    # ignore case for
    if (ignore.case) k <- tolower(k)
    # append result
    cnt <- c(cnt, !is_empty(grep(k, x, fixed = T)))
  }
  # which logical combination?
  if (is.null(logic))
    return(cnt)
  else if (logic %in% c("or", "OR", "|"))
    return(any(cnt))
  else if (logic %in% c("and", "AND", "&"))
    return(all(cnt))
  else if (logic %in% c("not", "NOT", "!"))
    return(!any(cnt))
  return(cnt)
}
# @title Find partial matching and close distance elements in strings
# @name str_pos
# @description This function finds the element indices of partial matching or similar strings
#                in a character vector. Can be used to find exact or slightly mistyped elements
#                in a string vector.
#
# @seealso \code{\link{group_str}}
#
# @param searchString Character vector with string elements.
# @param findTerm String that should be matched against the elements of \code{searchString}.
# @param maxdist Maximum distance between two string elements, which is allowed to treat them
#          as similar or equal.
# @param part.dist.match Activates similar matching (close distance strings) for parts (substrings)
#          of the \code{searchString}. Following values are accepted:
#          \itemize{
#            \item 0 for no partial distance matching
#            \item 1 for one-step matching, which means, only substrings of same length as \code{findTerm} are extracted from \code{searchString} matching
#            \item 2 for two-step matching, which means, substrings of same length as \code{findTerm} as well as strings with a slightly wider range are extracted from \code{searchString} matching
#          }
#          Default value is 0. See 'Details' for more information.
# @param showProgressBar Logical; f \code{TRUE}, the progress bar is displayed when computing the distance matrix.
#          Default in \code{FALSE}, hence the bar is hidden.
#
# @return A numeric vector with index position of elements in \code{searchString} that
#           partially match or are similar to \code{findTerm}. Returns \code{-1} if no
#           match was found.
#
# @note This function does \emph{not} return the position of a matching string \emph{inside}
#         another string, but the element's index of the \code{searchString} vector, where
#         a (partial) match with \code{findTerm} was found. Thus, searching for "abc" in
#         a string "this is abc" will not return 9 (the start position of the substring),
#         but 1 (the element index, which is always 1 if \code{searchString} only has one element).
#
# @details For \code{part.dist.match = 1}, a substring of \code{length(findTerm)} is extracted
#            from \code{searchString}, starting at position 0 in \code{searchString} until
#            the end of \code{searchString} is reached. Each substring is matched against
#            \code{findTerm}, and results with a maximum distance of \code{maxdist}
#            are considered as "matching". If \code{part.dist.match = 2}, the range
#            of the extracted substring is increased by 2, i.e. the extracted substring
#            is two chars longer and so on.
#
# @examples
# \dontrun{
# string <- c("Hello", "Helo", "Hole", "Apple", "Ape", "New", "Old", "System", "Systemic")
# str_pos(string, "hel")   # partial match
# str_pos(string, "stem")  # partial match
# str_pos(string, "R")     # no match
# str_pos(string, "saste") # similarity to "System"
#
# # finds two indices, because partial matching now
# # also applies to "Systemic"
# str_pos(string,
#         "sytsme",
#         part.dist.match = 1)
#
# # finds nothing
# str_pos("We are Sex Pistols!", "postils")
# # finds partial matching of similarity
# str_pos("We are Sex Pistols!", "postils", part.dist.match = 1)}
#
#' @importFrom stringdist stringdist
#' @importFrom utils txtProgressBar setTxtProgressBar
#
str_pos <- function(searchString,
                    findTerm,
                    maxdist = 2,
                    part.dist.match = 0,
                    showProgressBar = FALSE) {
  # init return value
  indices <- c()

  # find element indices from partial matching of string and find term
  pos <- as.numeric(grep(findTerm, searchString, ignore.case = T))
  if (length(pos) > 0) indices <- c(indices, pos)

  # check if required package is available
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    warning("Package 'stringdist' needed for this function to fully work. Please install it. Only partial matching indices are returned.", call. = F)
    return(indices)
  }

  # find element indices from similar strings
  pos <- which(stringdist::stringdist(tolower(findTerm), tolower(searchString)) <= maxdist)
  if (length(pos) > 0) indices <- c(indices, pos)

  # find element indices from partial similar (distance)
  # string matching
  if (part.dist.match > 0) {
    ftlength <- nchar(findTerm)
    # create progress bar
    # if (showProgressBar) pb <- utils::txtProgressBar(min = 1, max = length(searchString), style = 3)

    # iterate search string vector
    for (ssl in 1:length(searchString)) {
      # retrieve each element of search string vector
      # we do this step by step instead of vectorizing
      # due to the substring approach
      sst <- searchString[ssl]

      # we extract substrings of same length as findTerm
      # starting from first char of searchString until end
      # and try to find similar matches
      steps <- nchar(sst) - ftlength + 1
      for (pi in 1:steps) {
        # retrieve substring
        sust <- trim(substr(sst, pi, pi + ftlength - 1))

        # find element indices from similar substrings
        pos <- which(stringdist::stringdist(tolower(findTerm), tolower(sust)) <= maxdist)
        if (length(pos) > 0) indices <- c(indices, ssl)
      }
      if (part.dist.match > 1) {

        # 2nd loop picks longer substrings, because similarity
        # may also be present if length of strings differ
        # (e.g. "app" and "apple")
        steps <- nchar(sst) - ftlength
        if (steps > 1) {
          for (pi in 2:steps) {
            # retrieve substring
            sust <- trim(substr(sst, pi - 1, pi + ftlength))

            # find element indices from similar substrings
            pos <- which(stringdist::stringdist(tolower(findTerm), tolower(sust)) <= maxdist)
            if (length(pos) > 0) indices <- c(indices, ssl)
          }
        }
      }
      # update progress bar
      # if (showProgressBar) utils::setTxtProgressBar(pb, ssl)
    }
  }
  # if (showProgressBar) close(pb)

  # return result
  if (length(indices) > 0) return(sort(unique(indices)))
  return(-1)
}
# @title Split (categorical) vectors into dummy variables
# @name to_dummy
#
# @description This function splits categorical or numeric vectors with
#                more than two categories into 0/1-coded dummy variables.
#
# @param x a \code{\link{vector}} (variable).
# @param var.name indicates how the new dummy variables are named. Use
#          \code{"name"} to use the variable name or any other string that will
#          be used as is. See 'Examples'.
# @param suffix indicates which suffix will be added to each dummy variable.
#          Use \code{"numeric"} to number dummy variables, e.g. \emph{x_1},
#          \emph{x_2}, \emph{x_3} etc. Use \code{"label"} to add value label,
#          e.g. \emph{x_low}, \emph{x_mid}, \emph{x_high}. May be abbreviated.
# @param data optional, a data frame where the new dummy variables are appended
#          as additional columns.
# @return A data frame with dummy variables for each category of \code{x}, or
#           \code{data} where new dummy variables are appended as additional
#           columns. The dummy coded variables are of type \code{\link{atomic}}.
#
# @note \code{NA} values will be copied from \code{x}, so each dummy variable
#         has the same amount of \code{NA}'s at the same position as \code{x}.
#
# @examples
# data(efc)
# head(to_dummy(efc$e42dep))
#
# # add value label as suffix to new variable name
# head(to_dummy(efc$e42dep, suffix = "label"))
#
# # use "dummy" as new variable name
# head(to_dummy(efc$e42dep, var.name = "dummy"))
#
#
to_dummy <- function(x,
                     var.name = "name",
                     suffix = c("numeric", "label"),
                     data = NULL) {
  # check for abbr
  suffix <- match.arg(suffix)
  # save variable name
  varname <- deparse(substitute(x))
  # remove "data frame name"
  dollar_pos <- regexpr("$", varname, fixed = T)[1]
  if (dollar_pos != -1)
    varname <-
    substr(varname, start = dollar_pos + 1, stop = nchar(varname))
  # check whether we have labels
  labels <-
    get_labels(
      x,
      attr.only = F,
      include.values = "n",
      include.non.labelled = T
    )
  # get resp. set variable label for new dummy variables
  # get variable label
  label <- get_label(x, def.value = varname)
  if (var.name != "name") varname <- var.name
  # get unique values
  values <- sort(unique(x))
  # find which labels / categories were
  # actually used
  if (is.null(names(labels))) {
    # find labels index numbers
    labels.nr <- seq_len(length(labels))[labels %in% values]
    # copy only used labels
    labels <- labels[labels %in% values]
  } else {
    # get label value labels
    label.names <- names(labels)
    # numeric?
    if (!anyNA(as.numeric(label.names)))
      label.names <- as.numeric(label.names)
    # find labels index numbers
    labels.nr <- seq_len(length(labels))[label.names %in% values]
    # copy only used labels
    labels <- labels[label.names %in% values]
  }
  # return value
  mydf <- data.frame()
  # create all dummy variables
  for (i in 1:length(values)) {
    # create dummy var
    dummy <- rep(0, length(x))
    # set NA
    dummy[is.na(x)] <- NA
    # copy dummy level
    dummy[which(x == values[i])] <- 1
    # set variable name
    set_label(dummy) <- sprintf("%s: %s", label, labels[i])
    # bind to df
    if (nrow(mydf) == 0)
      mydf <- data.frame(dummy)
    else
      mydf <- cbind(mydf, dummy)
  }
  # prepare col.names
  col.nam <- rep(varname, ncol(mydf))
  if (suffix == "numeric")
    col.nam <- sprintf("%s_%i", col.nam, labels.nr)
  else
    col.nam <- sprintf("%s_%s", col.nam, labels)
  colnames(mydf) <- col.nam
  # append data?
  if (!is.null(data))
    return(cbind(data, mydf))
  return(mydf)
}
# @title Convert variable into factor and keep value labels
# @name to_factor
#
# @description This function converts a variable into a factor, but preserves
#                variable and value label attributes. See 'Examples'.
#
# @seealso \code{\link{to_value}} to convert a factor into a numeric value and
#            \code{\link{to_label}} to convert a value into a factor with labelled
#            factor levels.
#
# @param x Numeric, atomic or character vector or a data frame with
#          such vectors.
# @param add.non.labelled Logical, if \code{TRUE}, non-labelled values also
#          get value labels.
# @param drop.na Logical, if \code{TRUE}, all types of missing value codes are
#          converted into NA before \code{x} is converted as factor. If
#          \code{FALSE}, missing values will be left as their original codes.
#          See 'Examples' and \code{\link{get_na}}.
# @param ref.lvl Numeric, specifies the reference level for the new factor. Use
#          this parameter if a different factor level than the lowest value
#          should be used as reference level. If \code{NULL}, lowest value
#          will become the reference level. See \code{\link{ref_lvl}} for
#          details.
# @return A factor variable, including variable and value labels, respectively
#           a data frame with factor variables (including variable and value labels)
#           if \code{x} was a data frame.
#
# @note This function is intended for use with vectors that have value and variable
#        label attributes. Unlike \code{\link{as.factor}}, \code{to_factor} converts
#        a variable into a factor and preserves the value and variable label attributes.
#        \cr \cr
#        Adding label attributes is automatically done by importing data sets
#        with one of the \code{read_*}-functions, like \code{\link{read_spss}}.
#        Else, value and variable labels can be manually added to vectors
#        with \code{\link{set_labels}} and \code{\link{set_label}}.
#
# @details See 'Details' in \code{\link{get_na}}.
#
# @examples
# \dontrun{
# data(efc)
# library(sjPlot)
# # normal factor conversion, loses value attributes
# efc$e42dep <- as.factor(efc$e42dep)
# sjt.frq(efc$e42dep)
#
# # factor conversion, which keeps value attributes
# efc$e42dep <- to_factor(efc$e42dep)
# sjt.frq(efc$e42dep)}
#
# data(efc)
# # create parially labelled vector
# x <- set_labels(efc$e42dep, c(`1` = "independent", `4` = "severe dependency",
#                               `9` = "missing value"))
#
# # only copy existing value labels
# to_factor(x)
# get_labels(to_factor(x), include.values = "p")
#
# # also add labels to non-labelled values
# to_factor(x, add.non.labelled = TRUE)
# get_labels(to_factor(x, add.non.labelled = TRUE), include.values = "p")
#
# # create labelled integer, with missing flag
# x <- labelled(c(1, 2, 1, 3, 4, 1),
#               c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#               c(FALSE, FALSE, TRUE, TRUE))
# # to factor, with missing labels
# to_factor(x, drop.na = FALSE)
# # to factor, missings removed
# to_factor(x, drop.na = TRUE)
#
#
# # Convert to factor, using different reference level
# x <- to_factor(efc$e42dep)
# str(x)
# table(x)
#
# x <- to_factor(efc$e42dep, ref.lvl = 3)
# str(x)
# table(x)
#
#
to_factor <- function(x, add.non.labelled = FALSE, drop.na = TRUE, ref.lvl = NULL) {
  if (is.matrix(x) || is.data.frame(x)) {
    for (i in 1:ncol(x)) x[[i]] <- to_fac_helper(x[[i]],
                                                 add.non.labelled,
                                                 drop.na,
                                                 ref.lvl)
    return(x)
  } else {
    return(to_fac_helper(x,
                         add.non.labelled,
                         drop.na,
                         ref.lvl))
  }
}


to_fac_helper <- function(x, add.non.labelled, drop.na, ref.lvl) {
  # is already factor?
  if (is.factor(x)) {
    return(x)
  }
  # remove missings?
  if (drop.na) x <- to_na(x)

  # retrieve value labels
  lab <- get_labels(x,
                    attr.only = TRUE,
                    include.values = "n",
                    include.non.labelled = add.non.labelled)
  # retrieve variable labels
  varlab <- get_label(x)
  # retrieve missing codes
  nas <- suppressMessages(get_na(x))

  # switch value and names attribute, since get_labels
  # returns the values as names, and the value labels
  # as "vector content"
  if (!is.null(lab)) {
    if (is.character(x) || (is.factor(x) && !is_num_fac(x)))
      lab.switch <- names(lab)
    else
      lab.switch <- as.numeric(names(lab))

    names(lab.switch) <- as.character(lab)
  } else {
    lab.switch <- NULL
  }

  # convert variable to factor
  x <- as.factor(x)

  # set back value labels
  x <- suppressMessages(set_labels(x,
                                   lab.switch,
                                   force.labels = TRUE,
                                   force.values = FALSE))
  # set back variable labels
  x <- set_label(x, varlab)
  # set back missing codes
  x <- set_na(x, nas, as.attr = TRUE)
  # change reference level?
  if (!is.null(ref.lvl)) ref_lvl(x) <- ref.lvl
  return(x)
}
# @title Convert variable into factor and replaces values with associated value labels
# @name to_label
#
# @description This function converts (replaces) variable values (also of factors
#                or character vectors) with their associated value labels. Might
#                be helpful for factor variables.
#                For instance, if you have a Gender variable with 0/1 value, and associated
#                labels are male/female, this function would convert all 0 to male and
#                all 1 to female and returns the new variable as \code{\link{factor}}.
#
# @seealso \code{\link{to_factor}} to convert a numeric variable into a factor (and
#            preserve labels) and \code{\link{to_value}} to convert a factor into
#            a numeric variable.
#
# @param x A labelled vector (see \code{\link{set_labels}}),
#          respectively a data frame with such variables.
# @param add.non.labelled logical, if \code{TRUE}, values without associated
#          value label will also be converted to labels (as is). See 'Examples'.
# @param prefix Logical, if \code{TRUE}, the value labels used as factor levels
#          will be prefixed with their associated values. See 'Examples'.
# @param drop.na logical, if \code{TRUE}, all types of missing value codes are
#          converted into NA before \code{x} is converted as factor. If
#          \code{FALSE}, missing values will be left as their original codes.
#          See 'Examples' and \code{\link{get_na}}.
# @return A factor variable with the associated value labels as factor levels, or a
#           data frame with such factor variables (if \code{x} was a data frame).
#
# @note Value and variable label attributes (see, for instance, \code{\link{get_labels}}
#         or \code{\link{set_labels}}) will be removed  when converting variables to factors.
#         \cr \cr
#         Factors with non-numeric factor-levels won't be changed and returned "as is"
#         (see 'Examples').
#
# @details See 'Details' in \code{\link{get_na}}.
#
# @examples
# data(efc)
# print(get_labels(efc)['c161sex'])
# head(efc$c161sex)
# head(to_label(efc$c161sex))
#
# print(get_labels(efc)['e42dep'])
# table(efc$e42dep)
# table(to_label(efc$e42dep))
#
# head(efc$e42dep)
# head(to_label(efc$e42dep))
#
# # structure of numeric values won't be changed
# # by this function, it only applies to labelled vectors
# # (typically categorical or factor variables)
# str(efc$e17age)
# str(to_label(efc$e17age))
#
#
# # factor with non-numeric levels
# to_label(factor(c("a", "b", "c")))
#
# # factor with non-numeric levels, prefixed
# x <- factor(c("a", "b", "c"))
# set_labels(x) <- c("ape", "bear", "cat")
# to_label(x, prefix = TRUE)
#
#
# # create vector
# x <- c(1, 2, 3, 2, 4, NA)
# # add less labels than values
# x <- set_labels(x, c("yes", "maybe", "no"),
#                 force.labels = FALSE,
#                 force.values = FALSE)
# # convert to label w/o non-labelled values
# to_label(x)
# # convert to label, including non-labelled values
# to_label(x, add.non.labelled = TRUE)
#
#
# # create labelled integer, with missing flag
# x <- labelled(c(1, 2, 1, 3, 4, 1),
#               c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#               c(FALSE, FALSE, TRUE, TRUE))
# # to labelled factor, with missing labels
# to_label(x, drop.na = FALSE)
# # to labelled factor, missings removed
# to_label(x, drop.na = TRUE)
#
#
# # convert labelled character to factor
# dummy <- c("M", "F", "F", "X")
# set_labels(dummy) <- c(`M` = "Male", `F` = "Female", `X` = "Refused")
# get_labels(dummy,, "p")
# to_label(dummy)
#
#
to_label <- function(x, add.non.labelled = FALSE, prefix = FALSE, drop.na = TRUE) {
  if (is.matrix(x) || is.data.frame(x)) {
    for (i in 1:ncol(x)) {
      x[[i]] <- to_label_helper(x[[i]], add.non.labelled, prefix, drop.na)
    }
    return(x)
  } else {
    return(to_label_helper(x, add.non.labelled, prefix, drop.na))
  }
}


to_label_helper <- function(x, add.non.labelled, prefix, drop.na) {
  # prefix labels?
  if (prefix)
    iv <- "p"
  else
    iv <- 0
  # remove missings?
  if (drop.na) x <- to_na(x)
  # get value labels
  vl <- get_labels(x,
                   attr.only = TRUE,
                   include.values = iv,
                   include.non.labelled = add.non.labelled)
  # check if we have any labels, else
  # return variable "as is"
  if (!is.null(vl)) {
      # jerry fix!
      tmp <- get_labels(x,
                   attr.only = TRUE,
                   include.values = 'n',
                   include.non.labelled = add.non.labelled)
      vn = names(tmp)
      varlab <- attr(x,'label',exact=T)
      # convert to character for compare == in case of numeric vector
      x = as.character(x)
      for (i in 1:length(vl)) x[x == vn[i]] <- vl[i]
      # to factor
      # here, if vl does not have one element, that element will be NA by factor()
      x <- factor(x, levels = vl)
      attr(x,'label') <- varlab
      # # do not return labels
      # attr(x,'labels') <- tmp
  }
  return(x)
}
# @title Convert wide data to long format
# @name to_long
# @description This function converts wide data into long format. It allows
#                to transform multiple key-value pairs to be transformed
#                from wide to long format in one single step.
#
# @param data A \code{data.frame} that should be tansformed from wide to
#          long format.
# @param keys Character vector with name(s) of key column(s) to create in output.
#          Either one key value per column group that should be gathered, or
#          a single string. In the latter case, this name will be used as
#          key column, and only one key column is created. See 'Examples'.
# @param values Character vector with names of value columns (variable names)
#          to create in output. Must be of same length as number of column
#          groups that should be gathered. See 'Examples'.
# @param ... Specification of columns that should be gathered. Must be one
#          character vector with variable names per column group, or a numeric
#          vector with column indices indicating those columns that should be
#          gathered. See 'Examples'.
# @param labels Character vector of same length as \code{values} with variable
#          labels for the new variables created from gathered columns.
#          See 'Examples' and 'Details'.
# @param recode.key Logical, if \code{TRUE}, the values of the \code{key}
#          column will be recoded to numeric values, in sequential ascending
#          order.
#
# @details This function enhances \pkg{tidyr}'s \code{\link[tidyr]{gather}}
#            function that you can gather multiple column groups at once.
#            Value and variable labels for non-gathered variables are preserved.
#            However, gathered variables may have different variable label
#            attributes. In this case, \code{\link[tidyr]{gather}} will drop
#            these attributes. Hence, the new created variables from gathered
#            columns don't have any variable label attributes. In such cases,
#            use \code{labels} argument to set variable label attributes.
#
# @examples
# # create sample
# mydat <- data.frame(age = c(20, 30, 40),
#                     sex = c("Female", "Male", "Male"),
#                     score_t1 = c(30, 35, 32),
#                     score_t2 = c(33, 34, 37),
#                     score_t3 = c(36, 35, 38),
#                     speed_t1 = c(2, 3, 1),
#                     speed_t2 = c(3, 4, 5),
#                     speed_t3 = c(1, 8, 6))
#
# # check tidyr. score is gathered, however, speed is not
# tidyr::gather(mydat, "time", "score", score_t1, score_t2, score_t3)
#
# # gather multiple columns. both time and speed are gathered.
# to_long(mydat, "time", c("score", "speed"),
#         c("score_t1", "score_t2", "score_t3"),
#         c("speed_t1", "speed_t2", "speed_t3"))
#
# # gather multiple columns, use numeric key-value
# to_long(mydat, "time", c("score", "speed"),
#         c("score_t1", "score_t2", "score_t3"),
#         c("speed_t1", "speed_t2", "speed_t3"),
#         recode.key = TRUE)
#
# # gather multiple columns by colum names and colum indices
# to_long(mydat, "time", c("score", "speed"),
#         c("score_t1", "score_t2", "score_t3"),
#         c(6:8),
#         recode.key = TRUE)
#
# # gather multiple columns, use separate key-column for each value-vector
# to_long(mydat, c("time_score", "time_speed"), c("score", "speed"),
#         c("score_t1", "score_t2", "score_t3"),
#         c("speed_t1", "speed_t2", "speed_t3"))
#
# # gather multiple columns, label columns
# mydat <- to_long(mydat, "time", c("score", "speed"),
#                  c("score_t1", "score_t2", "score_t3"),
#                  c("speed_t1", "speed_t2", "speed_t3"),
#                  labels = c("Test Score", "Time needed to finish"))
#
# str(mydat$score)
# get_label(mydat$speed)
# lbl_df(mydat)
#
#' @importFrom tidyr gather_
#' @importFrom dplyr bind_cols
#
to_long <- function(data, keys, values, ..., labels = NULL, recode.key = FALSE) {
  # get variable names for gather columns
  data_cols <- eval(substitute(list(...)))
  # init output
  dummy <- list()
  # if we have just one key value, repeat it to required length
  if (length(keys) < length(data_cols))
    keys <- rep(keys, times = length(data_cols))
  # check for correct length
  if (length(values) < length(data_cols)) {
    stop("`values` must be of same length as column groups to gather.", call. = F)
  }
  # check for correct length
  if (!is.null(labels) && length(labels) < length(data_cols)) {
    warning("`labels` must be of same length as `values`. Dropping variable labels for gathered columns.")
    labels <- NULL
  }
  # check for numeric indices, and get column names then
  for (i in length(data_cols)) {
    # check if all values are numeric
    if (all(is.numeric(data_cols[[i]]))) {
      # get column names instead
      data_cols[[i]] <- colnames(data)[data_cols[[i]]]
    }
  }
  # get all columns that should be gathered
  all_data_cols <- unlist(data_cols)
  # iterate each column group
  for (i in 1:length(data_cols)) {
    # which of all column groups should be gathered in this step,
    # which not?
    remove_cols <- all_data_cols[!all_data_cols %in% data_cols[[i]]]
    # remove those columns that should not be gathered
    tmp <- data[, -match(remove_cols, colnames(data))]
    # gather data frame
    tmp <- suppressWarnings(tidyr::gather_(tmp, keys[i], values[i], data_cols[[i]]))
    # need to recode key-value?
    if (recode.key)
      tmp[[keys[i]]] <- sort(to_value(tmp[[keys[i]]], keep.labels = FALSE))
    # set variable label
    if (!is.null(labels))
      set_label(tmp[[values[i]]]) <- labels[i]
    # add output to list
    dummy[[length(dummy) + 1]] <- tmp
  }
  # we have at least one gathered data frame
  mydat <- dummy[[1]]
  # if we have multiple column groups to gather, go on here
  if (length(dummy) > 1) {
    # iterate remaining groups
    for (i in 2:length(dummy)) {
      # find gathered columns that do not already exist in our
      # output data frame
      add_cols <- dummy[[i]][!colnames(dummy[[i]]) %in% colnames(mydat)]
      # and bind them to the output
      mydat <- dplyr::bind_cols(mydat, add_cols)
    }
  }
  # return results
  return(mydat)
}

# @title Convert missing values of labelled variables into NA
# @name to_na
#
# @description This function converts defined missing values that are stored as
#                original value code into \code{NA}.
#
# @seealso \code{\link{get_na}} to get value codes of missing values.
#
# @param x Variable (vector), \code{data.frame} or \code{list} of variables
#          with value label attributes and defined missing value codes
#          (see \code{\link[haven]{labelled}}).
# @return \code{x}, where each value code of missing values is converted
#            to \code{NA}.
#
# @details \code{to_na} converts values to \code{NA}, which are defined
#            as missing through the \code{is_na}-attribute of a vector
#            (see \code{\link{labelled}}). \code{\link{set_na}},
#            by contrast, converts those values to \code{NA} that are
#            specified in the function's \code{values} argument; hence,
#            \code{\link{set_na}} ignores the \code{is_na}-attribute.
#            \cr \cr
#            Furthermore, see 'Details' in \code{\link{get_values}}
#            and \code{\link{get_na}}.
#
# @note This is a convenient function for \code{set_na(x, get_na(x))}.
#
# @examples
# # create labelled factor, with missing flag
# x <- labelled(c("M", "M", "F", "X", "N/A"),
#               c(Male = "M", Female = "F",
#                 Refused = "X", "Not applicable" = "N/A"),
#               c(FALSE, FALSE, TRUE, TRUE))
# x
# get_na(x)
# to_na(x)
#
# # create labelled integer, with missing flag
# x <- labelled(c(1, 2, 1, 3, 4, 1),
#              c(Male = 1, Female = 2, Refused = 3, "N/A" = 4),
#              c(FALSE, FALSE, TRUE, TRUE))
# x
# get_na(x)
# to_na(x)
#
# # get summary
# x <- labelled(c(1, 2, 1, 3, 4, 1, NA, 5),
#               c(Male = 1, Female = 2, Refused = 5),
#               c(FALSE, FALSE, TRUE))
# frq(x)
#
#
to_na <- function(x) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # na all
    for (i in 1:nvars) x[[i]] <- to_na_helper(x[[i]])
    return(x)
  } else {
    return(to_na_helper(x))
  }
}

to_na_helper <- function(x) set_na(x, suppressMessages(get_na(x)), as.attr = FALSE)
# @title Convert factors to numeric variables
# @name to_value
#
# @description This function converts (replaces) factor values with the
# related factor level index number, thus the factor is converted to
# a numeric variable.
#
# @seealso \code{\link{to_label}} to convert a labelled vector into a factor with labelled
#            factor levels and \code{\link{to_factor}} to convert a numeric variable
#            into a factor (and preserve labels)
#
# @param x \code{\link{factor}} or a data frame with \code{factor}s. May also be
#          a character vector.
# @param start.at starting index, i.e. the lowest numeric value of the variable's
#          value range. By default, this argument is \code{NULL}, hence the lowest
#          value of the returned numeric variable corresponds to the lowest factor
#          level (if factor is \code{\link{numeric}}) or to \code{1} (if factor levels
#          are not numeric).
# @param keep.labels logical, if \code{TRUE}, former factor levels will be added as
#          value labels. See \code{\link{set_labels}} for more details.
# @return A numeric variable with values ranging either from \code{start.at} to
#           \code{start.at} + length of factor levels, or to the corresponding
#           factor levels (if these were numeric). Or a data frame with numeric
#           variables, if \code{x} was a data frame.
#
# @examples
# data(efc)
# test <- to_label(efc$e42dep)
# table(test)
#
# table(to_value(test))
# hist(to_value(test, 0))
#
# # set lowest value of new variable
# # to "5".
# table(to_value(test, 5))
#
# # numeric factor keeps values
# dummy <- factor(c("3", "4", "6"))
# table(to_value(dummy))
#
# # do not drop unused factor levels
# dummy <- ordered(c(rep("No", 5), rep("Maybe", 3)),
#                  levels = c("Yes", "No", "Maybe"))
# to_value(dummy)
#
# # non-numeric factor is converted to numeric
# # starting at 1
# dummy <- factor(c("D", "F", "H"))
# table(to_value(dummy))
#
#
to_value <- function(x,
                     start.at = NULL,
                     keep.labels = TRUE) {
  if (is.matrix(x) || is.data.frame(x)) {
    for (i in 1:ncol(x)) x[[i]] <- to_value_helper(x[[i]], start.at, keep.labels)
    return(x)
  } else {
    return(to_value_helper(x, start.at, keep.labels))
  }
}


to_value_helper <- function(x, start.at, keep.labels) {
  labels <- NULL
  # is already numeric?
  if (is.numeric(x)) return(x)
  labels <- get_labels(x, attr.only = T, include.values = "n")
  varlab <- get_label(x)
  # is character?
  if (is.character(x)) {
    # has labels?
    if (!is.null(labels)) {
      # sort labels correctly, therefor get "levels"
      lvls <- levels(as.factor(x))
      # do we have more labels than values? If yes, drop unused labels
      if (length(labels) > length(lvls)) labels <- labels[names(labels) %in% lvls]
      # it might be that we have more levels than labels, in this case
      # drop unused levels - else, ordering won't work
      if (length(lvls) > length(labels)) lvls <- lvls[lvls %in% names(labels)]
      # sort labels correctly
      labels <- unname(labels[order(names(labels), lvls)])
    }
    # convert to factor
    x <- as.factor(x)
  }
  # retrieve "value labels"
  if (is.null(labels)) labels <- levels(x)
  # check if we have numeric factor levels
  if (is_num_fac(x)) {
    # convert to numeric via as.vector
    new_value <- as.numeric(as.vector((x)))
    # new minimum value?
    if (!is.null(start.at) && is.numeric(start.at)) {
      # check if lowest value of variable differs from
      # requested minimum conversion value
      val_diff <- start.at - min(new_value, na.rm = T)
      # adjust new_value
      new_value <- new_value + val_diff
    }
  } else {
    # check start.at value
    if (is.null(start.at)) start.at <- 1
    # get amount of categories
    l <- length(levels(x))
    # determine highest category value
    end <- start.at + l - 1
    # replace labels with numeric values
    levels(x) <- c(start.at:end)
    # convert to numeric
    new_value <- as.numeric(as.character(x))
  }
  # check if we should attach former labels as value labels
  if (keep.labels) {
    new_value <- set_labels(new_value, labels, force.labels = T)
    new_value <- set_label(new_value, lab = varlab)
  }
  return(new_value)
}
# @title Trim leading and trailing whitespaces from strings
# @name trim
#
# @description Trims leading and trailing whitespaces from strings or
#                character vectors.
#
# @param x Character vector or string. Function is vectorized, i.e. vector
#          may have a length greater than 1. See 'Examples'.
#
# @return Trimmed \code{x}, i.e. with leading and trailing spaces removed.
#
# @examples
# trim("white space at end ")
# trim(" white space at start and end ")
# trim(c(" string1 ", "   string2", "string 3   "))
#
#
trim <- function(x) gsub("^\\s+|\\s+$", "", x)# @title Convert labelled vectors into normal classes
# @name unlabel
#
# @description This function converts \code{\link[haven]{labelled}} class vectors
#                into a generic data format, which means that simply all \code{\link[haven]{labelled}}
#                class attributes will be removed, so all vectors / variables will most
#                likely become \code{\link{atomic}}. Additionally, \code{tbl_df} and
#                \code{tbl} class attributes will be removed from data frames, and
#                a \code{\link{lbl_df}} class attribute will be added. See 'Note'.
#
# @seealso \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#
# @param x data frame, which contains \code{\link[haven]{labelled}} class vectors or a single vector
#          of class \code{labelled}.
# @return a data frame or single vector (depending on \code{x}) with common object classes.
#
# @note This function is currently only used to avoid possible compatibility issues
#         with \code{\link[haven]{labelled}} class vectors and \code{tbl_df} resp.
#         \code{tbl} class attributes for data frames. Some known issues with \code{\link[haven]{labelled}}
#         class vectors have already been fixed, so it might be that this function
#         will become redundant in the future. Currently, data frames with \code{tbl_df} and
#         \code{tbl} class attributes may cause difficulties when indexing columns
#         like \code{data.frame[, colnr]} - only \code{data.frame[[colnr]]} seems
#         to be safe when accessing data frame columns from within function calls.
#         \cr \cr
#         Data frames will be converted into labelled data frames (see \code{\link{lbl_df}}).
#
#' @importFrom utils txtProgressBar setTxtProgressBar
#
unlabel <- function(x) {
  # check if complete data frame or only single
  # vector should be converted
  if (is.data.frame(x) || is.matrix(x)) {
    # create progress bar
    # pb <- utils::txtProgressBar(min = 1, max = ncol(x), style = 3)
    # tell user...
    # message("Converting labelled-classes. Please wait...\n")
    for (i in 1:ncol(x)) {
      # remove labelled class
      if (is_labelled(x[[i]])) x[[i]] <- unclass(x[[i]])
      # update progress bar
      # utils::setTxtProgressBar(pb, i)
    }
    # close(pb)
    # remove redundant class attributes
    class(x) <- c("lbl_df", "data.frame")
  } else {
    # remove labelled class
    if (is_labelled(x)) x <- unclass(x)
  }
  return(x)
}
# @title Weight a variable
# @name weight2
#
# @description This function weights the variable \code{x} by
#                a specific vector of \code{weights}. It's an
#                alternative weight calculation to \code{\link{weight}},
#                though \code{\link{weight}} usage is recommended.
#
# @seealso \code{\link{weight}}
#
# @inheritParams weight
#
# @return The weighted \code{x}.
#
# @details This function sums up all \code{weights} values of the associated
#            categories of \code{x}, whereas the \code{\link{weight}} function
#            uses a \code{\link{xtabs}} formula to weight cases. Thus, this function
#            may return a vector of different length than \code{x}.
#
# @note See 'Note' in \code{\link{weight}}
#
# @examples
# v <- sample(1:4, 20, TRUE)
# table(v)
# w <- abs(rnorm(20))
# table(weight2(v, w))
#
#
#
weight2 <- function(x, weights) {
  items <- unique(x)
  newvar <- c()
  for (i in 1:length(items)) {
    newcount = round(sum(weights[which(x == items[i])]))
    newvar <- c(newvar, rep(items[i], newcount))
  }
  return(newvar)
}


# @title Weight a variable
# @name weight
# @description This function weights the variable \code{x} by
#                a specific vector of \code{weights}.
#
# @seealso \code{\link{weight2}}
#
# @param x (Unweighted) variable
# @param weights Vector with same length as \code{x}, which
#          contains weight factors. Each value of \code{x} has a
#          specific assigned weight in \code{weights}.
# @param digits Numeric value indicating the number of decimal places to be
#          used for rounding the weighted values. By default, this value is
#          \code{0}, i.e. the returned values are integer values.
#
# @return The weighted \code{x}.
#
# @note The values of the returned vector are in sorted order, whereas the values'
#        order of the original \code{x} may be spread randomly. Hence, \code{x} can't be
#        used, for instance, for further cross tabulation. In case you want to have
#        weighted contingency tables or (grouped) box plots etc., use the \code{weightBy}
#        argument of most functions.
#
# @examples
# v <- sample(1:4, 20, TRUE)
# table(v)
# w <- abs(rnorm(20))
# table(weight(v, w))
#
# set.seed(1)
# x <- sample(letters[1:5], size = 20, replace = TRUE)
# w <- runif(n = 20)
#
# table(x)
# table(weight(x, w))
#
#' @importFrom stats na.pass xtabs
#
weight <- function(x, weights, digits = 0) {
  # init values
  weightedvar <- c()
  wtab <- round(stats::xtabs(weights ~ x,
                             data = data.frame(weights = weights, x = x),
                             na.action = stats::na.pass,
                             exclude = NULL),
                digits = digits)
  # iterate all table values
  for (w in 1:length(wtab)) {
    # retrieve count of each table cell
    w_count <- wtab[[w]]
    # retrieve "cell name" which is identical to the variable value
    # first check whether values are numeric or not
    nval_ <- suppressWarnings(as.numeric(names(wtab[w])))
    # if value is not numeric, use as is
    if (is.na(nval_))
      w_value <- names(wtab[w])
    else
      # else, use numeric value
      w_value <- nval_
    # append variable value, repeating it "w_count" times.
    weightedvar <- c(weightedvar, rep(w_value, w_count))
  }
  return(weightedvar)
}


# @title Weighted standard deviation for variables
# @name wtd_sd
# @description Compute weighted standard deviation for a variable or for all variables
#                of a data frame.
#
# @param x (Numeric) vector or a data frame.
# @param weights Numeric vector of weights.
# @return The weighted standard deviation of \code{x}, or for each variable
#           if \code{x} is a data frame.
#
# @examples
# wtd_sd(rnorm(n = 100, mean = 3),
#        runif(n = 100))
#
# data(efc)
# wtd_sd(efc[, 1:3], runif(n = nrow(efc)))
#
#
wtd_sd <- function(x, weights = NULL) {
  # check if suggested packages are available
  if (!requireNamespace("Hmisc", quietly = TRUE)) {
    stop("Package `Hmisc` needed for this function to work. Please install it.", call. = FALSE)
  }
  if (is.matrix(x) || is.data.frame(x)) {
    # init return variables
    stdd <- c()
    stdd_names <- c()
    # iterate all columns
    for (i in 1:ncol(x)) {
      # get and save standard error for each variable
      # of the data frame
      stdd <- c(stdd,
                sqrt(Hmisc::wtd.var(x[[i]], weights = weights, na.rm = TRUE)))
      # save column name as variable name
      stdd_names <- c(stdd_names, colnames(x)[i])
    }
    # set names to return vector
    names(stdd) <- stdd_names
    # return results
    return(stdd)
  } else {
    return(sqrt(Hmisc::wtd.var(x, weights = weights, na.rm = TRUE)))
  }
}


# @title Weighted standard error for variables
# @name wtd_se
# @description Compute weighted standard error for a variable or for all variables
#                of a data frame.
#
# @param x (Numeric) vector or a data frame.
# @param weights Numeric vector of weights.
# @return The weighted standard error of \code{x}, or for each variable
#           if \code{x} is a data frame.
#
# @examples
# wtd_se(rnorm(n = 100, mean = 3),
#        runif(n = 100))
#
# data(efc)
# wtd_se(efc[, 1:3], runif(n = nrow(efc)))
#
#
wtd_se <- function(x, weights = NULL) {
  # check if suggested packages are available
  if (!requireNamespace("Hmisc", quietly = TRUE)) {
    stop("Package `Hmisc` needed for this function to work. Please install it.", call. = FALSE)
  }
  if (is.matrix(x) || is.data.frame(x)) {
    # init return variables
    stde <- c()
    stde_names <- c()
    # iterate all columns
    for (i in 1:ncol(x)) {
      # get and save standard error for each variable
      # of the data frame
      stde <- c(stde,
                sqrt(Hmisc::wtd.var(x[[i]], weights = weights, na.rm = TRUE) / length(stats::na.omit(x[[i]]))))
      # save column name as variable name
      stde_names <- c(stde_names, colnames(x)[i])
    }
    # set names to return vector
    names(stde) <- stde_names
    # return results
    return(stde)
  } else {
    return(sqrt(Hmisc::wtd.var(x, weights = weights, na.rm = TRUE) / length(stats::na.omit(x))))
  }
}
# @title Insert line breaks in long labels
# @name word_wrap
#
# @description Insert line breaks in long character strings. Useful if you want to wordwrap
#                labels / titles for plots or tables.
#
# @param labels Label(s) as character string, where a line break should be
#          inserted. Several strings may be passed as vector  (see 'Examples').
# @param wrap Maximum amount of chars per line (i.e. line length). If code{wrap = Inf},
#          no word wrap will be performed (i.e. \code{labels} will be returned as is).
# @param linesep By default, this argument is \code{NULL} and a regular new line
#          string (\code{"\\n"}) is used. For HTML-purposes, for instance, \code{linesep}
#          could be \code{"<br>"}.
# @return New label(s) with line breaks inserted at every \code{wrap}'s position.
#
# @examples
# word_wrap(c("A very long string", "And another even longer string!"), 10)
#
# message(word_wrap("Much too long string for just one line!", 15))
#
#' @importFrom stats na.omit
#
word_wrap <- function(labels, wrap, linesep = NULL) {
  # check if labels have NA values and remove them
  if (anyNA(labels)) labels <- as.character(stats::na.omit(labels))
  # check for valid value
  if (is.null(labels) || length(labels) == 0) return(NULL)
  # infinite wrap? then return labels
  if (is.infinite(wrap)) return(labels)
  # default line separator is \n
  if (is.null(linesep)) {
    linesep <- '\\1\n'
    lsub <- 0
    ori.linesep <- '\n'
  } else {
    # however, for html-function we can use "<br>"
    # as argument
    lsub <- nchar(linesep) - 1
    ori.linesep <- linesep
    linesep <- sprintf("\\1%s", linesep)
  }
  # create regex pattern for line break
  pattern <- c(paste('(.{1,', wrap, '})(\\s|$)', sep = ""))
  # iterate all labels
  for (n in 1:length(labels)) {
    # check if wrap exceeds lengths of labels
    if (wrap > 0 && nchar(labels[n]) > wrap) {
      # insert line breaks
      labels[n] <- gsub(pattern, linesep, labels[n])

      # in case label was short enough, we still have a line break
      # at the end of the label. here we remove any trailing line breaks
      l <- nchar(labels[n])
      # get last char
      lc <- substr(labels[n], l - lsub, l)
      # check if line break
      if (lc == ori.linesep) {
        # if yes, remove it
        labels[n] <- substr(labels[n], 0, l - (lsub + 1))
      }
    }
  }
  return(labels)
}
# @title Convert labelled values into NA
# @name zap_labels
#
# @description For (partially) labelled vectors, all values that have
#                a value label attribute will be replaced by \code{NA}.
#
# @param x (partially) \code{\link[haven]{labelled}} vector, \code{data.frame} or \code{list}
#            of (partially) labelled vectors
# @return \code{x}, where all labelled values are converted to \code{NA}.
#
# @seealso \code{\link{get_values}} and \code{\link{zap_unlabelled}};
#          \code{\link{drop_labels}} to drop labels from zero-count values.
#
# @examples
#
# data(efc)
# str(efc$e42dep)
#
# x <- set_labels(efc$e42dep,
#                 c(`1` = "independent",
#                   `4` = "severe dependency"))
# table(x)
# get_values(x)
# str(x)
#
# # zap all labelled values
# x <- set_labels(efc$e42dep,
#                 c(`1` = "independent",
#                   `4` = "severe dependency"))
# table(zap_labels(x))
# get_values(zap_labels(x))
# str(zap_labels(x))
#
# # zap all unlabelled values
# x <- set_labels(efc$e42dep,
#                 c(`1` = "independent",
#                   `4` = "severe dependency"))
# table(zap_unlabelled(x))
# get_values(zap_unlabelled(x))
# str(zap_unlabelled(x))
#
#' @importFrom stats na.omit
#
zap_labels <- function(x) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # na all
    for (i in 1:nvars) x[[i]] <- zap_labels_helper(x[[i]])
    return(x)
  } else {
    return(zap_labels_helper(x))
  }
}


# @title Convert non-labelled values into NA
# @name zap_unlabelled
#
# @description For (partially) labelled vectors, all values that don't have
#                a value label attribute will be replaced by \code{NA}.
#
# @inheritParams zap_labels
# @return \code{x}, where all non-labelled values are converted to \code{NA}.
#
# @seealso \code{\link{get_values}} and \code{\link{zap_labels}};
#          \code{\link{drop_labels}} to drop labels from zero-count values.
#
# @examples
#
# data(efc)
# str(efc$e42dep)
#
# x <- set_labels(efc$e42dep,
#                 c(`1` = "independent",
#                   `4` = "severe dependency"))
# table(x)
# get_values(x)
# str(x)
#
# # zap all labelled values
# x <- set_labels(efc$e42dep,
#                 c(`1` = "independent",
#                   `4` = "severe dependency"))
# table(zap_labels(x))
# get_values(zap_labels(x))
# str(zap_labels(x))
#
# # zap all unlabelled values
# x <- set_labels(efc$e42dep,
#                 c(`1` = "independent",
#                   `4` = "severe dependency"))
# table(zap_unlabelled(x))
# get_values(zap_unlabelled(x))
# str(zap_unlabelled(x))
#
#' @importFrom stats na.omit
#
zap_unlabelled <- function(x) {
  if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
    # get length of data frame or list, i.e.
    # determine number of variables
    if (is.data.frame(x) || is.matrix(x))
      nvars <- ncol(x)
    else
      nvars <- length(x)
    # na all
    for (i in 1:nvars) x[[i]] <- zap_unlabelled_helper(x[[i]])
    return(x)
  } else {
    return(zap_unlabelled_helper(x))
  }
}

zap_labels_helper <- function(x) {
  x <- set_na(x, get_values(x), as.attr = F)
  if (is_labelled(x)) class(x) <- NULL
  return(x)
}

zap_unlabelled_helper <- function(x) {
  vals <- get_values(x)
  x <- set_na(x, stats::na.omit(unique(x)[!unique(x) %in% vals]), as.attr = F)
  if (is_labelled(x)) class(x) <- NULL
  return(x)
}



####************************************************************************************************
                                     ####*export*####
####************************************************************************************************
#' sjmisc 1.8 hack
#' @description sjmisc 1.8 hack
#' @export
sjmisc_copy_labels=copy_labels

#' sjmisc 1.8 hack
#' @description sjmisc 1.8 hack
#' @export
sjmisc_get_values=get_values

#' sjmisc 1.8 hack
#' @description sjmisc 1.8 hack
#' @export
sjmisc_to_label=to_label

#' sjmisc 1.8 hack
#' @description sjmisc 1.8 hack
#' @export
sjmisc_to_factor=to_factor

#' sjmisc 1.8 hack
#' @description sjmisc 1.8 hack
#' @export
sjmisc_to_value=to_value

#' sjmisc 1.8 hack
#' @description sjmisc 1.8 hack
#' @export
sjmisc_rec=rec

#' sjmisc 1.8 hack
#' @description sjmisc 1.8 hack
#' @export
sjmisc_read_spss=read_spss

#' sjmisc 1.8 hack
#' @description sjmisc 1.8 hack
#' @export
sjmisc_write_spss=write_spss

####************************************************************************************************
                                     ####*export with additional alias*####
# # example:
# labs <- ez.getlabels(e)
# lab <- ez.getlabel(e)
# ee = as.character(e)
# ee <- ez.setlabels(ee, labels)  # labels could be NULL or '' = remove labels attr
# ee <- ez.getlabel(ee, lab)      # lab could be NULL or '' = remove label attr



# # Or use sjmisc_
# labs <- sjmisc_get_labels(e, attr.only = T, include.values = "n", include.non.labelled=F)
# lab <- sjmisc_get_label(e, def.value = NULL)
# ee = as.character(e)
# ee <- sjmisc_set_labels(ee, labels)  # labels could be NULL or '' = remove labels attr
# ee <- sjmisc_set_label(ee, lab)      # lab could be NULL or '' = remove label attr



# # Or manually change attributes:
# lab = attr(x, 'label', exact = T)
# labs1 <- attr(x, 'labels', exact = T)
# labs2 <- attr(x, 'value.labels', exact = T)

# # trimws will convert factor to character
# x = trimws(x,'both')
# if (! is.null(lab)) lab = trimws(lab,'both')
# if (! is.null(labs1)) labs1 = trimws(labs1,'both')
# if (! is.null(labs2)) labs2 = trimws(labs2,'both')

# if (!is.null(na.strings)) {
#     # convert to NA
#     x[x %in% na.strings] = NA
#     # also remember to remove na.strings from value labels
#     labs1 = labs1[! labs1 %in% na.strings]
#     labs2 = labs2[! labs2 %in% na.strings]
# }

# # the levels will be reset here
# x = factor(x)

# attr(x, 'label') <- lab             # NULL removes the attr, but '' results in empty attr
# attr(x, 'labels') <- labs1          # NULL removes the attr, but '' results in empty attr
# attr(x, 'value.labels') <- labs2    # NULL removes the attr, but '' results in empty attr
####************************************************************************************************
#' sjmisc 1.8 hack
#' @description sjmisc 1.8 hack, value labels
#' @note include.values, how values are returned: NULL = unnamed vector; as named vector = "as.name" or "n"; "as.prefix" or "p", c("[1] Caucasian", "[3] Asian")
#' \cr attr.only = T, only search attributes, T desired (if F, also search factor levels, character strings)
#' \cr include.non.labelled, if T, values without labels will also be included in the returned labels
#' \cr returns NULL if no value labels attribute
#' @export
sjmisc_get_labels=get_labels

#' get value labels, wrapper of \code{\link{sjmisc_get_labels}}.
#' @description get value labels, wrapper of \code{\link{sjmisc_get_labels}}.
#' @param x df or var (I generally use x as var)
#' @details see also \code{\link{sjmisc_get_values}}
#' \cr include.values, how values are returned, as named vector = "as.name" or "n" or "as.prefix" or "p"
#' \cr attr.only = T, only search attributes, T desired (if F, also search factor levels, character strings)
#' \cr include.non.labelled, if T, values without labels (when less labels than values) will also be included in the returned labels
#' \cr returns NULL if no value labels attribute
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
ez.labels.get = function(x, include.values='n', attr.only=T, include.non.labelled=F, ...){
    result=sjmisc_get_labels(x, include.values=include.values, attr.only=attr.only, include.non.labelled=include.non.labelled, ...)
    return(result)
}

#' @rdname ez.labels.get
#' @export
ez.getlabels=ez.labels.get



#' sjmisc 1.8 hack
#' @description sjmisc 1.8 hack, value labels
#' @note The usage of the original function is a bit confusing. Here, I summarize:
#' \cr 1) Always use a named vector for valuelabels, in which case force.labels and force.values will be ignored
#' \cr valuelabels:
#' \cr 1 4 5 9 do not have to all appear in x, that is, you can assign more labels than values (or vice versa, assign less labels)
#' \cr notice the particular order and symbol: "strongly agree" <- 1
#' \cr sjmisc_set_labels(x, c("strongly agree"=1,"totally disagree"=4,"refused"=5,"missing"=9))
#' \cr
#' \cr 2) when valuelabels="" or NULL, essentially clear value labels attribute
#' \cr
#' \cr 3) if no exisiting attr, using haven style: labels
#' \cr
#' \cr 4) though x could be df, I generally use x as var
#' @export
sjmisc_set_labels=set_labels

#' set value labels, wrapper of \code{\link{sjmisc_set_labels}}.
#' @description set value labels, wrapper of \code{\link{sjmisc_set_labels}}.
#' @note The usage of the original function is a bit confusing. Here, I summarize:
#' \cr 1) Always use a named vector for valuelabels, in which case force.labels and force.values will be ignored
#' \cr valuelabels:
#' \cr 1 4 5 9 do not have to all appear in x, that is, you can assign more labels than values (or vice versa, assign less labels)
#' \cr notice the particular order and symbol: "strongly agree" <- 1
#' \cr sjmisc_set_labels(x, c("strongly agree"=1,"totally disagree"=4,"refused"=5,"missing"=9))
#' \cr
#' \cr 2) when valuelabels="", essentially clear value labels attribute
#' \cr
#' \cr 3) if no exisiting attr, using haven style: labels
#' \cr
#' \cr 4) though x could be df, I generally use x as var
#' @return returns a new changed df
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
ez.labels.set = function(x, valuelabels, force.labels=FALSE, force.values=FALSE, ...){
    result=sjmisc_set_labels(x, valuelabels, force.labels=force.labels, force.values=force.values, ...)
    return(result)
}

#' @rdname ez.labels.set
#' @export
ez.setlabels=ez.labels.set




#' sjmisc 1.8 hack
#' @description sjmisc 1.8 hack, variable labels
#' @param x a df: (efc), or a single var: (efc$e42dep)
#' @param def.value the value returned when no label attribute for a single var. if x is a df, def.value is not effective, instead returns '' if no label for a col in df
#' @export
sjmisc_get_label=get_label

#' get variable label, wrapper of \code{\link{sjmisc_get_label}}
#' @description get variable label, wrapper of \code{\link{sjmisc_get_label}}
#' @param x a df: (efc), or a single var: (efc$e42dep)
#' @param col evaluated by \code{\link{ez.selcol}}(x,col). Or, NULL=all cols. returns only labels for specified cols of the df (ignored if x is a var)
#' @return returns a named character vector (if x is df), or character (if x is a single var)
#' \cr If df has no variable label for all variables or for specified cols, returns NULL. If df has label for some variables, returns a string vector with some "".
#' \cr If a single variable has no label, returns NULL
#' @note if df uses foreign style 'variable.labels' and
#' \cr because of renaming, new col name is not in 'variable.labels',
#' \cr the function will not retrieve label for that new col name during the process, which is obvious
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
ez.label.get = function(x,col=NULL){
    # sjmisc_get_label for variable label
    # sjmisc_get_labels for value label
    # the value returned when no label attribute for a single var. if x is a df, def.value is not effective, instead returns '' if no label for a col in df
    # although sjmisc_get_label(list(var1,var2,var3)) ok, if var2 has no label, the returned labels would be a vector (var1lbl,var3lbl)--this might be confusing. So not to use this syntax
    result=sjmisc_get_label(x, def.value = NULL)
    if (is.data.frame(x) & !is.null(result)) {names(result)=names(x)}
    if (is.data.frame(x) & !is.null(col)) {col=ez.selcol(x,col); result=result[col]}
    if (all(ez.is.empty(result))) {result=NULL}
    return(result)
}

#' @rdname ez.label.get
#' @export
ez.getlabel=ez.label.get



#' sjmisc 1.8 hack
#' @description sjmisc 1.8 hack, variable labels
#' @param x var or df
#' @param lab if x is var, single vector; if x is df, vector of the same length as ncol(df). If "" or NULL, essentially clear label attribute
#' @note if no exisiting attr, using haven style: label
#' @return returns a new changed x
#' @export
sjmisc_set_label=set_label

#' set variable label, wrapper of \code{\link{sjmisc_set_label}}
#' @description set variable label, wrapper of \code{\link{sjmisc_set_label}}
#' @param x var or df
#' @param label if x is var, single vector; if x is df, vector of the same length as ncol(df), or a named vector of equal or smaller length--eg, c('var lab 1'='var1', 'var lab 3'='var3'), in this case only change var1 and var3, the rest unchanged. If "" or NULL, essentially clear label attribute
#' @note if no exisiting attr, using haven style: label
#' @return returns a new changed df
#' @family data transformation functions
#' @export
#' @seealso \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}
#' \cr \code{\link[dplyr]{select}}, \code{\link[dplyr]{slice}}
#' \cr \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#' \cr \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}
#' \cr \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{right_join}}, \code{\link[dplyr]{inner_join}}, \code{\link[dplyr]{full_join}}, \code{\link[dplyr]{semi_join}}, \code{\link[dplyr]{anti_join}}
#' \cr \code{\link[dplyr]{intersect}}, \code{\link[dplyr]{union}}, \code{\link[dplyr]{setdiff}}
#' \cr \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{bind_cols}}
ez.label.set = function(x,label=NULL){
    if (!is.data.frame(x)) {
        result = sjmisc_set_label(x,label)
    } else {
        # label is NULL, '' or unnamed vector
        if (is.null(names(label))) {
            result = sjmisc_set_label(x,label)
        } else {
            cols = label
            labs = names(label)
            for (i in 1:length(cols)) {
                col = cols[i]
                y = x[[col]]
                # attr() assignment changes numeric to atomic
                attr(y, 'label') <- labs[i]
                x[[col]] = y
            }
            result = x
        }
    }
    return(result)
}

#' @rdname ez.label.set
#' @export
ez.setlabel=ez.label.set



#' swap variable label mode, in general, mode 2 (col attr, haven style, "label") is preferred to mode 1 (df attr, foreign style, "variable.labels")
#' @description swap variable label mode, in general, mode 2 (col attr, haven style, "label") is preferred to mode 1 (df attr, foreign style, "variable.labels")
#' @param mode 1: returned df uses df attribute, attr(df,'variable.labels')
#' \cr Pros(+) same: slicing df rows, but not cols (df[1:10,])
#' \cr Cons(-) mess: rename column, change df structure (eg, left_join)
#' \cr
#' \cr 2: returned df uses column attribute, attr(df[[col]],'label')
#' \cr Pros(+) same: rename column, change df structure (eg, left_join)
#' \cr Cons(-) mess: slicing df rows, but not cols (df[1:10,])
#' \cr
#' \cr 1: foreign: variable.labels (as df attributes), value.labels
#' \cr 2: haven: label = variable label, labels = value labels
#' @export
ez.label.swap = function(df, mode=2) {
    if (mode==1) {
        lbls = ez.label.get(df)
        df[]=lapply(df, function(x) {attr(x,'label') <- NULL; return(x)})
        attr(df,'variable.labels') <- lbls
    } else if (mode==2) {
        # if df uses foreign style 'variable.labels' and
        # because of renaming, new col name is not in 'variable.labels',
        # the function will not retrieve label for that new col name during the process, which is obvious
        lbls = ez.label.get(df)
        if (!is.null(lbls)) {
            for (j in colnames(df)) {
                lbl = lbls[[j]]  # or unname(lbls[j])
                if (lbl == '') lbl = NULL
                attr(df[[j]],'label') <- lbl
            }
            attr(df,'variable.labels') <- NULL
        } else {
            df[]=lapply(df, function(x) {attr(x,'label') <- NULL; return(x)})
            attr(df,'variable.labels') <- NULL
        }
    }
    return(df)
}

#' @rdname ez.label.swap
#' @export
ez.swaplabel=ez.label.swap



#' swap value labels format, to be consistent, haven style ("labels", mode 2) is preferred to foreign ("value.labels", mode 1)
#' @description swap value labels format, to be consistent, haven style ("labels", mode 2) is preferred to foreign ("value.labels", mode 1)
#' @param mode 1: foreign, "value.labels";  2: haven, "labels"
#' \cr
#' \cr 1: foreign: variable.labels (as df attributes), value.labels
#' \cr 2: haven: label = variable label, labels = value labels
#' @export
ez.labels.swap = function(df, mode=2) {
    change_attr_name = function(x,old,new){
        value = attr(x, old, exact = T)
        attr(x, old) <- NULL
        attr(x, new) <- value
        x
    }
    if (mode==1) df[]=lapply(df,change_attr_name,old="labels",new="value.labels")
    if (mode==2) df[]=lapply(df,change_attr_name,old="value.labels",new="labels")
    return(df)
}

#' @rdname ez.labels.swap
#' @export
ez.swaplabels=ez.labels.swap
