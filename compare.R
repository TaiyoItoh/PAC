compare <- function(input.data, instruction) {
  # return value
  result <- NULL

  # data formatting
  # clustering is processed in the rank order
  input.data <- input.data[input.data$rank,]
  data <- data.frame(item = input.data$item, image = input.data$image, order = as.numeric(row.names(input.data)))
  n <- length(data$item)
  mat <- matrix(0, nrow = n, ncol = n)

  # not Fisher=Yates shuffle but sample function
  # use if statement because of the specification of sample function
  if (n == 2) {
    random.vec <- 2
  } else {
    m <- n * n
    x <- 1:m
    # under triangle matrix index
    x <- x[(x - 1) %% n > (x - 1) %/% n]
    random.vec <- sample(x)
  }

  # window
  win <- tktoplevel()
  tktitle(win) <- "comparison"
  tkgrid(tk2label(win, text = instruction), padx = 10, pady = c(15, 10), columnspan = 2)

  # input widgets
  # use distance or dissimilarity, not similarity
  i = 1
  item1.var <- tclVar(as.character(data$item[(random.vec[i] - 1) %% n + 1]))
  item2.var <- tclVar(as.character(data$item[(random.vec[i] - 1) %/% n + 1]))
  tkgrid(tk2label(win, textvariable = item1.var), tk2label(win, textvariable = item2.var), padx = 10, pady = c(20, 5))
  slider.var <- tclVar(5)
  slider <- tk2scale(win, from = 0, to = 10, variable = slider.var, orient = "horizontal", length = 400)
  tkgrid(slider, padx = 10, pady = c(10, 0), columnspan = 2)
  win$env$close <- tk2label(win, text = "Close")
  win$env$distant <- tk2label(win, text = "Distant")
  tkgrid(win$env$close, win$env$distant, padx = 15)
  tkgrid.configure(win$env$close, sticky = "w")
  tkgrid.configure(win$env$distant, sticky = "e")

  # button and remaining label
  # use superassignment operator
  remaining.var <- tclVar(paste("Remaining:", length(x)))
  proceed <- function() {
    mat[(random.vec[i] - 1) %% n + 1, (random.vec[i] - 1) %/% n + 1] <<- as.numeric(tclvalue(slider.var))
    if(i < length(random.vec)) {
      tclvalue(slider.var) <- 5
      tclvalue(remaining.var) <- paste("Remaining:", length(random.vec) - i)
      i <<- i + 1
      tclvalue(item1.var) <- as.character(data$item[(random.vec[i] - 1) %% n + 1])
      tclvalue(item2.var) <- as.character(data$item[(random.vec[i] - 1) %/% n + 1])
      if (i == length(random.vec)) {
        tclvalue(button.var) <- "Submit"
      }
    } else {
      result <<- list(data = data, mat = mat)
      tkdestroy(win)
    }
  }
  button.var <- tclVar("Next")
  proceed.but <- tk2button(win, textvariable = button.var, command = proceed)
  tkgrid(tk2label(win, textvariable = remaining.var), proceed.but, padx = 10, pady = c(5, 15))

  # wait for executtion
  tkwait.window(win)

  # return
  return(result)
}
