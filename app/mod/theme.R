box::use(
  fresh[...]
)

box::use(
  ./paleta_atlas_capital_humano[
    yellow,
    red,
    light_blue,
    cyan
  ],
  ./paleta_atlas_economia[
    blue2,
    orange1,
    orange2,
    violet2
  ]
)

###################
## Status colors ##
###################
#' @export
primary <- yellow
secondary <- violet2
success <- NULL
info <- blue2
warning <- orange1
danger <- red
light <- NULL
dark <- NULL

#################
## Main colors ##
#################
blue <- light_blue
lightblue <- cyan
navy <- NULL
cyan <- NULL
teal <- NULL
olive <- NULL
green <- NULL
lime <- NULL
orange <- NULL
yellow <- NULL
fuchsia <- NULL
purple <- NULL
maroon <- NULL
red <- red
black <- NULL
gray_x_light <- NULL
gray_600 <- NULL
gray_800 <- NULL
gray_900 <- NULL
white <- NULL

main_bg <- NULL
text_dark <- NULL
text_light <- NULL
sidebar_light_bg <- main_bg
sidebar_light_color <- text_light
sidebar_light_hover_color <- NULL
sidebar_light_submenu_bg <- main_bg
sidebar_light_submenu_color <- NULL
sidebar_light_submenu_hover_color <- NULL

#' @export
font1 <- "Exo 2"
#' @export
font2 <- "Roboto"
#' @export
font3 <- "Roboto Mono"
#' @export
main_font <- "'Exo 2', sans-serif"
#' @export
secondary_font <- "'Roboto', sans-serif"
#' @export
monospace_font <- "'Roboto Mono', monospace"
#' @export
base_font <- "Exo 2"

#' @export
diobs_theme <- create_theme(
  bs4dash_font(
    size_base = "0.9rem",
    size_lg = NULL,
    size_sm = NULL,
    size_xs = NULL,
    size_xl = NULL,
    weight_light = NULL,
    weight_normal = NULL,
    weight_bold = NULL,
    family_sans_serif = main_font,
    family_monospace = monospace_font,
    family_base = main_font
  ),
  bs4dash_color(
    blue = blue,
    lightblue = lightblue,
    navy = navy,
    cyan = cyan,
    teal = teal,
    olive = olive,
    green = green,
    lime = lime,
    orange = orange,
    yellow = yellow,
    fuchsia = fuchsia,
    purple = purple,
    maroon = maroon,
    red = red,
    black = black,
    gray_x_light = gray_x_light,
    gray_600 = gray_600,
    gray_800 = gray_800,
    gray_900 = gray_900,
    white = white
  ),
  bs4dash_status(
    primary = primary,
    secondary = secondary,
    success = success,
    info = info,
    warning = warning,
    danger = danger,
    light = light,
    dark = dark
  ),
  bs4dash_layout(
    font_size_root = NULL,
    sidebar_width = NULL,
    sidebar_padding_x = NULL,
    sidebar_padding_y = NULL,
    sidebar_mini_width = NULL,
    control_sidebar_width = NULL,
    boxed_layout_max_width = NULL,
    screen_header_collapse = NULL,
    main_bg = main_bg,
    content_padding_x = NULL,
    content_padding_y = NULL
  )
)
