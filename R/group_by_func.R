#' Summarise a Dataframe

#' This function takes a column from a dataframe, groups by it and summarises the result of the columns
#' The user can choose between "mean", "min", "max" and "sum" as summarisation functions

#' @export
#' @param data dataframe input
#' @param groupcol colname to group by
#' @param func summarisation function
#' @return newdata asummarised dataframe
#'

#
# mydata = data.frame(
#   "Name" = c("John", "John", "John", "Peter", "Peter"),
#   "Subject" = c("Maths", "Physics", "Italian", "Maths", "Physics"),
#   "Mark" = c(90, 86, 44, 67, 89)
# )
#
# groupcol = "Subject"
# func = "min"

group_by_func <- function(data, groupcol, func) {
  colindex <- as.data.frame(as.numeric(colnames(data) == groupcol))
  colindex$index <- rownames(colindex)
  index <- as.numeric(colindex$index[colindex[, 1] == 1])
  nonindex <- as.numeric(colindex$index[colindex[, 1] != 1])
  unival <- unique(data[, index])[order(unique(data[, index]))]

  newdata <-
    as.data.frame(matrix(
      data = NA,
      ncol = ncol(data),
      nrow = length(unival)
    ))
  colnames(newdata)[1] <- groupcol
  colnames(newdata)[2:ncol(newdata)] <- colnames(data)[nonindex]

  newdata[, 1] <- unival

  y = 2

    while (y <= ncol(newdata)) {
      x = 1
      while (x <= length(unival)) {
        try ({
          z <- data[data[, index] == unival[x], nonindex]
          z <- z[, y - 1]
          if (func == "mean") {
            metric <- mean(z)
          } else if (func == "max") {
            metric <- max(z)
          } else if (func == "min") {
            metric <- min(z)
          } else if (func == "sum") {
            metric <- sum(z)
          } else {
            metric = NA
          }
          newdata[x, y] <- metric
        })
        x = x + 1
      }
      y = y + 1
    }

    colnames(newdata)[2:ncol(newdata)] <-
      paste0(func, "_", colnames(newdata)[2:ncol(newdata)])

    y = vector(mode = "logical", length = ncol(newdata))
    y[1:length(y)] = TRUE
    x = 1
    while (x <= ncol(newdata)) {
      if (max(is.na(newdata[, x])) == 1) {
        y[x] <- FALSE
      }
      x = x + 1
    }

    newdata <- newdata[, y]

  return(newdata)
}
