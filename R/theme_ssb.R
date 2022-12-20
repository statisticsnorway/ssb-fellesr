#' Get SSB colours from KLASS
#'
#' @return List of names with vector of colours
ssb_colours <- function(){
  ssb <- klassR::GetKlass(614, output_style = "wide")
  code3_n<- nchar(ssb$code3)
  ssb$shade <- substring(ssb$code3, code3_n, code3_n)
  list(
    main = ssb$name3[c(8, 26, 19, 13, 3, 29, 22)],
    blues = ssb$name3[ssb$code2 == "B.2"],
    greens = ssb$name3[ssb$code2 == "A.1"],
    primary = ssb$name3[ssb$code1 == "A"],
    secondary = ssb$name3[ssb$code1 == "B"],
    shade3 = ssb$name3[ssb$shade == "3"],
    shade2 = ssb$name3[ssb$shade == "2"],
    shade4 = ssb$name3[ssb$shade == "4"],
    shade1 = ssb$name3[ssb$shade == "1"],
    shade5 = ssb$name3[ssb$shade == "5"]
  )
}


#' Create SSB palette
#'
#' @param name Name of the palette
#' @param n Number of colours
#' @param type Type of data. Can be "discrete" or "continuous"
#'
#' @return Vector of colours
ssb_palettes <- function(name, n,
                         type = c("discrete", "continuous")) {
  palette = ssb_colours()[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}


#' Create discete SSB colours
#'
#' @param name Name of the palette to use. Default is 'main'. Choose between:\cr
#'  'main', greens', 'blues', 'primary', 'secondary', 'shade1', 'shade2','shade3', 'shade4', 'shade5'
#'
#' @return ggproto object for using in ggplot to change discrete colours
#' @export
scale_fill_ssb <- function(name="main") {
  ggplot2::scale_fill_manual(values = ssb_palettes(name,
                                                   type = "discrete"))
}


#' Create continuous SSB colours
#'
#' @param name Name of the palette to use. Default is 'main'. Choose between:\cr
#'  'main', greens', 'blues', 'primary', 'secondary', 'shade1', 'shade2','shade3', 'shade4', 'shade5'
#' @aliases scale_colour_ssb, scale_color_ssb
#' @return ggproto object for using in ggplot to change continuus colours
#' @export
scale_colour_ssb <- function(name="main") {
  ggplot2::scale_colour_gradientn(colours = ssb_palettes(name = name,
                                                         type = "continuous"))
}

scale_color_ssb <- scale_colour_ssb


#' SSB theme for ggplot
#'
#' @return theme object
#' @export
#'
#' @examples
#' ggplot(mtcars, aes(x=cyl, fill=as.factor(cyl))) +
#' geom_bar( ) +
#' theme_ssb()
theme_ssb <- function(){
  font <- "sans"   #assign font family up front

  theme_bw() %+replace%    #replace elements we want to change

    theme(

      #grid elements
      panel.grid.major = element_line(colour = "grey", size = rel(0.5)),
      panel.grid.minor = element_line(colour = "grey", size = rel(0.5)),
      axis.ticks = element_blank(),          #strip axis ticks
      panel.border = element_rect(fill = NA,
                                  colour = "grey"),
      #since theme_minimal() already strips axis lines,
      #we don't need to do that again

      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 20,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly

      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 14),               #font size

      plot.caption = element_text( #caption
        family = font,             #font family
        size = 9,                  #font size
        hjust = 1),                #right align

      axis.title = element_text( #axis titles
        family = font,           #font family
        size = 8),               #font size

      axis.text = element_text(   #axis text
        family = font,            #axis famuly
        size = 8),                #font size

      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),

      legend.text = element_text(
        family = font,
        size = 8
      ),

      legend.title = element_text(
        family = font,
        size = 10
      )

    )
}
