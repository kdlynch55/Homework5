## Kevin Lynch HW5 Class/Methods

setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

setValidity('sparse_numeric', function(object) {
  if (length(object@pos) != length(object@value)) {
    return('The length of the positions and values do not match')}
  if (any(object@pos < 1) | any(object@pos > object@length)) {
    return('The positions are not within the length')}
  TRUE})

setGeneric('sparse_add', function(first,second,...) {standardGeneric('sparse_add')})
setGeneric('sparse_mult', function(first,second,...) {standardGeneric('sparse_mult')})
setGeneric('sparse_sub', function(first,second,...) {standardGeneric('sparse_sub')})
setGeneric('sparse_crossprod', function(first,second,...) {standardGeneric('sparse_crossprod')}) 

setMethod('sparse_add', c('sparse_numeric', 'sparse_numeric'), function(first, second) {
  if (first@length != second@length) {stop()}
  both_pos <- c(first@pos, second@pos)
  both_values <- c(first@value, second@value)

  added <- tapply(both_values, both_pos, sum)
  
  non_zero <- added[added != 0]
  new('sparse_numeric', value = as.numeric(non_zero), pos = as.integer(as.numeric(names(non_zero))), length = first@length) # Creates sparse numeric
})

setMethod('sparse_mult', c('sparse_numeric', 'sparse_numeric'), function(first, second) {
  if (first@length != second@length) {stop()}
  both_combined <- data.frame(pos = c(first@pos, second@pos), value = c(first@value, second@value))
  
  both_pos <- intersect(first@pos, second@pos)
  
  mult <- first@value[match(both_pos, first@pos)] * 
    second@value[match(both_pos, second@pos)]
  
  new('sparse_numeric', value = mult, pos = both_pos, length = first@length) # Creates sparse numeric
})

setMethod('sparse_sub', c('sparse_numeric', 'sparse_numeric'), function(first, second) {
  if (first@length != second@length) {stop()}
  both_pos <- c(first@pos, second@pos)
  both_values <- c(first@value, -second@value)
  
  sub <- tapply(both_values, both_pos, sum)
  
  non_zero <- sub[sub != 0]
  new('sparse_numeric', value = as.numeric(non_zero), pos = as.integer(as.numeric(names(non_zero))), length = first@length) # Creates sparse numeric
})

setMethod('sparse_crossprod', c('sparse_numeric', 'sparse_numeric'), function(first, second) {
  if (first@length != second@length) {stop()}
  both_combined <- data.frame(pos = c(first@pos, second@pos), value = c(first@value, second@value))
  
  both_pos <- intersect(first@pos, second@pos)
  
  crossprod <- first@value[match(both_pos, first@pos)] *
    second@value[match(both_pos, second@pos)]
  
  return(c(sum(crossprod))) # Creates numeric vector
})

setMethod('+', c('sparse_numeric', 'sparse_numeric'), function(e1, e2) {sparse_add(e1, e2)})
setMethod('*', c('sparse_numeric', 'sparse_numeric'), function(e1, e2) {sparse_mult(e1, e2)})
setMethod('-', c('sparse_numeric', 'sparse_numeric'), function(e1, e2) {sparse_sub(e1, e2)})

setAs('numeric', 'sparse_numeric', function(from) {
  if (!is.numeric(from)) {stop()}
  non_zero <- which(from != 0) # Removes 0s
  new('sparse_numeric', value = from[non_zero], pos = as.integer(non_zero), length = length(from))
})

setAs("sparse_numeric", "numeric", function(vector) {
  numeric_vector <- numeric(vector@length) # Makes vector of 0
  numeric_vector[vector@pos] <- vector@value # Replaces 0s with values
  numeric_vector})

setMethod('show', 'sparse_numeric', function(vector) {
  df <- data.frame(pos = vector@pos, value = vector@value)
  df})

setMethod('plot', c('sparse_numeric', 'sparse_numeric'), function(x, y) {
  if (x@length != y@length) {stop()}
  
  both_pos <- intersect(x@pos, y@pos)
  
  plot(x@value[match(both_pos, x@pos)], y@value[match(both_pos, y@pos)], 
       xlab = 'first sparse numeric vector', 
       ylab = 'second sparse numeric vector', 
       main = 'overlapping non-zero elements of two sparse_numeric vectors')
})

setGeneric('sparse_div', function(first,second,...) {standardGeneric('sparse_div')})
setMethod('sparse_div', c('sparse_numeric', 'sparse_numeric'), function(first, second) {
  if (first@length != second@length) {stop()}
  
  both_pos <- intersect(first@pos, second@pos)
  
  div <- first@value[match(both_pos, first@pos)] / 
    second@value[match(both_pos, second@pos)]
  
  new('sparse_numeric', value = div, pos = both_pos, length = first@length) # Creates sparse numeric
})
