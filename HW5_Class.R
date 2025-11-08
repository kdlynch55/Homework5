## Kevin Lynch HW5 Class/Methods

setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

setValidity('sparse_numeric', function(x) {
  if (length(x@pos) != length(x@value)) {
    return('The length of the positions and values do not match')}
  TRUE})

setGeneric('sparse_add', function(x,y,...) {standardGeneric('sparse_add')})
setGeneric('sparse_mult', function(x,y,...) {standardGeneric('sparse_mult')})
setGeneric('sparse_sub', function(x,y,...) {standardGeneric('sparse_sub')})
setGeneric('sparse_crossprod', function(x,y,...) {standardGeneric('sparse_crossprod')}) 

setMethod('sparse_add', c('sparse_numeric', 'sparse_numeric'), function(first, second) {
  if (first@length != second@length) {stop()}
  both_pos <- c(first@pos, second@pos)
  both_values <- c(first@value, second@value)

  added <- tapply(both_values, both_pos, sum)
  
  non_zero <- added[added != 0]
  new('sparse_numeric', value = non_zero, pos = as.numeric(names(non_zero)), length = first@length) # Creates sparse numeric
})

setMethod('sparse_mult', c('sparse_numeric', 'sparse_numeric'), function(first, second) {
  if (first@length != second@length) {stop()}
  both_combined <- data.frame(pos = c(first@pos, second@pos), value = c(first@value, second@value))
  
  both_pos <- intersect(first@pos, second@pos)
  both_combined <- both_combined[both_combined$pos %in% both_pos, ]
  
  mult <- aggregate(both_combined$value ~ both_combined$pos, prod)
  
  new('sparse_numeric', value = mult$value, pos = mult$pos, length = first@length) # Creates sparse numeric
})

setMethod('sparse_sub', c('sparse_numeric', 'sparse_numeric'), function(first, second) {
  if (first@length != second@length) {stop()}
  both_pos <- c(first@pos, second@pos)
  both_values <- c(first@value, -second@value)
  
  sub <- tapply(both_values, both_pos, sum)
  
  non_zero <- sub[sub != 0]
  new('sparse_numeric', value = non_zero, pos = as.numeric(names(non_zero)), length = first@length) # Creates sparse numeric
})

setMethod('sparse_crossprod', c('sparse_numeric', 'sparse_numeric'), function(first, second) {
  if (first@length != second@length) {stop()}
  both_combined <- data.frame(pos = c(first@pos, second@pos), value = c(first@value, second@value))
  
  both_pos <- intersect(first@pos, second@pos)
  both_combined <- both_combined[both_combined$pos %in% both_pos, ]
  
  crossprod <- aggregate(both_combined$value ~ both_combined$pos, prod)
  
  c(sum(crossprod$value)) # Creates numeric vector
})

setMethod('+', c('sparse_numeric', 'sparse_numeric'), function(first, second) {sparse_add(first, second)})
setMethod('*', c('sparse_numeric', 'sparse_numeric'), function(first, second) {sparse_mult(first, second)})
setMethod('-', c('sparse_numeric', 'sparse_numeric'), function(first, second) {sparse_sub(first, second)})

setAs('numeric', 'sparse_numeric', function(vector) {
  non_zero <- which(vector != 0) # Removes 0s
  new('sparse_numeric', values = vector[non_zero], pos = as.integer(non_zero), length = length(vector))
})

setAs("sparse_numeric", "numeric", function(vector) {
  numeric_vector <- numeric(vector@length) # Makes vector of 0
  numeric_vector[vector@pos] <- vector@values # Replaces 0s with values
  numeric_vector})

setMethod('show', 'sparse_numeric', function(vector) {
  df <- data.frame(pos = vector@pos, value = vector@value)
  print(df)})

setMethod('plot', c('sparse_numeric', 'sparse_numeric'), function(first, second) {
  if (first@length != second@length) {stop()}
  
  both_pos <- intersect(first@pos, second@pos)
  
  plot(first@values[match(both_pos, first@pos)], second@values[match(both_pos, second@pos)], 
       xlab = 'first sparse numeric vector', 
       ylab = 'second sparse numeric vector', 
       main = 'overlapping non-zero elements of two sparse_numeric vectors')
})

setGeneric('sparse_div', function(x,y,...) {standardGeneric('sparse_div')})
setMethod('sparse_div', c('sparse_numeric', 'sparse_numeric'), function(first, second) {
  if (first@length != second@length) {stop()}
  
  both_pos <- intersect(first@pos, second@pos)
  
  div <- first@values[match(both_pos, first@pos)] / 
    second@values[match(both_pos, second@pos)]
  
  new('sparse_numeric', value = div, pos = both_pos, length = first@length) # Creates sparse numeric
})
