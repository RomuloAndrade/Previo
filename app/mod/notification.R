box::use(
  bs4Dash[toast]
)

#' @export
notify <- function(title = "", position = "bottomRight", class = "bg-primary",
                   icon = "fa-home", autohide = TRUE, close = TRUE) {
  toast(
    title = title,
    options = list(
      autohide = autohide,
      class = class,
      close = close,
      icon = sprintf("fas %s", icon),
      position = position
    )
  )
}
