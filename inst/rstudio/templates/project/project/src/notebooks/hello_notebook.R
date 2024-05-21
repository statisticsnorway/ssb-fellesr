# This is an example R fil that can be opened as a notebook

source("../functions/hello_world.R")
hello_world()

# Example of using fellesr package with ggplot example
library(fellesr)

ggplot(mtcars, aes(x=cyl, fill=as.factor(cyl))) +
geom_bar( ) +
theme_ssb()
