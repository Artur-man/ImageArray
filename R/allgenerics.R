# generics of ImageArray
setGeneric("crop", function(object, ...) standardGeneric("crop"))
setGeneric("negate", function(object, ...) standardGeneric("negate"))
setGeneric("modulate", function(object, ...) standardGeneric("modulate"))
setGeneric("axes", function(object, ...) standardGeneric("axes"))

# generics from EBImage
setGeneric("rotate")
setGeneric("flip")
setGeneric("flop")