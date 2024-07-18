## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=FALSE--------------------------------------------------------------
library(htmltools)
tags$div(
  class = "shiny-app-frame",
  tags$iframe(
    src = "https://andrie-de-vries.shinyapps.io/sortable_update_rank_list",
    width = 800,
    height = 700
  )
)

## ----echo=FALSE, cache=FALSE--------------------------------------------------
knitr::read_chunk(
  system.file("shiny-examples/update_rank_list/app.R", package = "sortable")
)

## ----update-rank-list-app, eval=FALSE-----------------------------------------
#  NA

