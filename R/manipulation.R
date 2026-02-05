#' @importFrom EBImage rotate flip flop

####
# Main ####
####

#' @describeIn ImageArray-methods rotate image array to 90, 180, 270 degrees
setMethod("rotate", signature = "ImageArray", function(x, angle) {
  # validate rotation
  if (!angle %in% c(0, 90, 180, 270, 360)) {
    stop("Only rotations of 0,90,180,270,360 degrees are supported!")
  }

  # check dimensions
  .check_dim(x)
  dim_img <- dim(x[[1]])
  ax <- axes(x)

  # array perm.
  if (angle %in% c(90, 270)) {
    cur_perm <- .swap(
      seq_len(length(dim_img)),
      which(ax == "x"),
      which(ax == "y")
    )
    x <- aperm(x, perm = cur_perm)
  }

  # flop
  if (angle %in% c(90, 180)) {
    x <- flop(x)
  }

  # flip
  if (angle %in% c(180, 270)) {
    x <- flip(x)
  }

  # return
  x
})

#' @describeIn ImageArray-methods permute image
#' @exportMethod aperm
setMethod("aperm", signature = "ImageArray", function(a, perm) {
  n.levels <- length(a@levels)
  for (i in seq_len(n.levels)) {
    a[[i]] <- aperm(a[[i]], perm = perm)
  }
  a
})

#' @describeIn ImageArray-methods negate image
#' @exportMethod negate
setMethod("negate", signature = "ImageArray", function(object) {
  n.levels <- length(object@levels)
  for (i in seq_len(n.levels)) {
    object[[i]] <- 255L - object[[i]]
  }
  object
})

#' @describeIn ImageArray-methods modulate image
#' @exportMethod modulate
setMethod("modulate", signature = "ImageArray", function(object, brightness) {
  if (brightness < 0) {
    stop("Brightness should be more than 0, typically more than 100")
  }
  n.levels <- length(object@levels)
  for (i in seq_len(n.levels)) {
    tmp <- ceiling(object[[i]] * (brightness / 100))
    max <- if (type(object[[i]]) == "double") 1 else 255
    tmp[tmp > max] <- max
    if (max == 255) {
      type(tmp) <- "integer"
    }
    object[[i]] <- tmp
  }
  object
})

#' @importFrom stats setNames
#' @noRd
.flipflop <- function(object, direction = "x") {
  n.levels <- length(object@levels)
  ax <- axes(object)

  # check dim
  .check_dim(object)

  # flip all
  for (i in seq_len(n.levels)) {
    img <- object[[i]]
    dim_img <- stats::setNames(dim(img), ax)
    cur_ind <- stats::setNames(lapply(dim_img, seq_len), ax)
    cur_ind[[direction]] <- rev(cur_ind[[direction]])
    object[[i]] <- .subset_array(object[[i]], cur_ind, drop = FALSE)
  }
  object
}

#' @describeIn ImageArray-methods vertical flipping image
setMethod("flip", signature = "ImageArray", function(x) {
  .flipflop(x, direction = "y")
})

#' @describeIn ImageArray-methods horizontal flipping image
setMethod("flop", signature = "ImageArray", function(x) {
  .flipflop(x, direction = "x")
})

#' @describeIn ImageArray-methods cropping image
#' @importFrom utils head tail
#' @importFrom stats setNames
#' @exportMethod crop
setMethod("crop", signature = "ImageArray", function(object, ind) {
  # get axes
  ax <- axes(object)
  dim_img <- stats::setNames(dim(object), ax)

  # check ind
  if (!is.list(ind)) {
    stop("'ind' should be a list of integers")
  }

  # check_dim
  .check_dim(object)

  # ind control
  if (length(ind) == 2) {
    ind <- stats::setNames(ind, c("x", "y"))
    if (length(dim_img) == 3) {
      ind <- c(ind, list(c = seq_len(dim_img["c"])))
    }
    ind <- ind[ax]
  }

  # check sequential
  check_sequential <- all(vapply(ind[c("x", "y")], is.sequential, logical(1)))
  if (!check_sequential) {
    stop(
      "'ind' should be a list of sequantial integer 
                   vectors (hence slice)"
    )
  }

  # crop all images
  n.levels <- length(object@levels)
  for (i in seq_len(n.levels)) {
    img <- object[[i]]
    dim_img <- stats::setNames(dim(img), ax)[c("x", "y")]
    cur_ind <- ind
    cur_ind[c("x", "y")] <-
      lapply(seq_len(length(ind[c("x", "y")])), function(j) {
        curind <- ind[c("x", "y")][[j]]
        id <- c(
          floor(utils::head(curind, 1) / (2^(i - 1))),
          ceiling(utils::tail(curind, 1) / (2^(i - 1)))
        )
        seq(max(id[1], 1), min(id[2], dim_img[j]))
      })
    object[[i]] <- .subset_array(img, cur_ind, drop = FALSE)
  }

  object
})

#' @describeIn ImageArray-methods get axes metadata of the ImageArray object
#' @exportMethod axes
setMethod("axes", "ImageArray", function(object) object@meta[["axes"]])

####
# Auxiliary ####
####

#' @noRd
.subset_array <- function(x, idx, drop = FALSE) {
  d <- dim(x)
  if (is.null(d)) {
    stop("x must be an array or matrix.")
  }
  if (length(idx) > length(d)) {
    stop("Too many index dimensions provided.")
  }

  # pad missing dimensions with full slices
  while (length(idx) < length(d)) {
    idx[[length(idx) + 1]] <- seq_len(d[length(idx) + 1])
  }

  if (length(idx) == 3) {
    x[idx[[1]], idx[[2]], idx[[3]], drop = drop]
  } else {
    x[idx[[1]], idx[[2]], drop = drop]
  }
}

.swap <- function(x, i, j) {
  x[c(i, j)] <- x[c(j, i)]
  x
}

.check_dim <- function(object) {
  if (!(length(dim(object)) %in% c(2, 3))) {
    stop("This operation can only be performed on 2D or 3D image arrays")
  }
}
