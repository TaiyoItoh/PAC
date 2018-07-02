input <- function(item.min, item.max, stimulus) {
  # return value
  data <- NULL

  # window
  win <- tktoplevel()
  tktitle(win) <- "Input"
  tkgrid(tk2label(win, text = stimulus), padx = 10, pady = c(15, 10), columnspan = 4)
  tkgrid(tk2label(win, text = ""), tk2label(win, text = "Item"), tk2label(win, text = "Rank"), tk2label(win, text = "Image"), padx = 5, pady = 2)

  # input widgets
  # need to set NULL as a initial value
  item.var <- NULL
  rank.var <- NULL
  image.var <- NULL
  item.entry <- NULL
  rank.combo <- NULL
  image.combo <- NULL
  rank.list <- c("", 1:item.max)
  image.list <- c("", "+", "0", "-")
  for(i in 1:item.max) {
    item.var[[i]] <- tclVar("")
    rank.var[[i]] <- tclVar("")
    image.var[[i]] <- tclVar("")
    item.entry[[i]] <- tkentry(win, textvariable = item.var[[i]])
    rank.combo[[i]] <- tk2combobox(win, values = rank.list, textvariable = rank.var[[i]], state = "readonly")
    image.combo[[i]] <- tk2combobox(win, values = image.list, textvariable = image.var[[i]], state = "readonly")
    tkgrid(tk2label(win, text = as.character(i)), item.entry[[i]], rank.combo[[i]], image.combo[[i]], padx = 5, pady = 2)
  }

  # reset button
  reset <- function() {
    for(i in 1:item.max) {
      tclvalue(item.var[[i]]) <- ""
      tclvalue(rank.var[[i]]) <- ""
      tclvalue(image.var[[i]]) <- ""
    }
  }
  reset.but <- tkbutton(win, text = "Reset", command = reset)

  # submit button
  # use superassignment operator
  submit <- function() {
    item <- NULL
    rank <- NULL
    image <- NULL
    for(i in 1:item.max) {
      # validation check
      if(tclvalue(item.var[[i]]) != "" && tclvalue(rank.var[[i]]) != "" && tclvalue(image.var[[i]]) != "") {
        item <- append(item, tclvalue(item.var[[i]]))
        rank <- append(rank, as.numeric(tclvalue(rank.var[[i]])))
        image <- append(image, tclvalue(image.var[[i]]))
      } else if (tclvalue(item.var[[i]]) == "" && tclvalue(rank.var[[i]]) == "" && tclvalue(image.var[[i]]) == "") {
        if (i < item.min) {
          tkmessageBox(message = paste("You need to answer more than ", item.min, " items.", sep= "") , icon = "error", type = "ok")
        } else {
          if (setequal(rank, 1:(i-1))) {
            data <<- data.frame(item = item, rank = rank, image = image)
            tkdestroy(win)
          } else {
            tkmessageBox(message = "Your rank discription is incorrect.", icon = "error", type = "ok")
          }
        }
        break
      } else {
        tkmessageBox(message = paste("Your answer is incomplete on line ", i, ".", sep = ""), icon = "error", type = "ok")
        break
      }
    }
  }
  submit.but <- tkbutton(win, text = "Submit", command = submit)

  # buttons
  tkgrid(tk2label(win, text = ""), tk2label(win, text = ""), reset.but, submit.but, padx = 5, pady = c(10, 15))

  # wait for executtion
  tkwait.window(win)

  # return
  return(data)
}
