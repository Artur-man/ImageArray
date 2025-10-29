.ImgArray <- setClass(
  Class = "ImgArray",
  slots = c(
    meta = "list",
    series = "list"
  )
)

.BFArraySeed <- setClass(
  "BFArraySeed",
  contains = "Array",
  slots = c(
    filepath = "character",
    series = "numeric",
    resolution = "numeric",
    shape = "numeric",
    type = "character"
  )
)

.BFArray <- setClass(
  Class = "BFArray",
  contains = c("DelayedArray"),
  slots = c(seed = "BFArraySeed")
)
