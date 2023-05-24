skCollapsePanel = function(title, ..., titleUI=NULL, id = NULL, value = NULL)
{
  content <- list(...)
  if (is.null(id))
      id <- paste0("cpanel", sprintf("%07i", as.integer(stats::runif(1,
          1, 1e+06))))
  if (is.null(value)) {
      value = title
  }
  tags$div(class = "accordion-group",

    tags$div(class = "accordion-heading",
      HTML("<table><tr><td>"),
      titleUI,
      HTML("</td><td>"),
      tags$a(`data-toggle` = "collapse", href = paste0("#", id), title),
      HTML("</td></tr></table>")
    ),
    tags$div(class = "accordion-body collapse", id = id, `data-value` = value,
      tags$div(class = "accordion-inner", content)
    )
  )
}
