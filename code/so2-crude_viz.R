# cargar paquetes ---------------------------------------------------------

if( !require( "pacman" ) ) {
  install.packages( "pacman" )
}

library("pacman")

p_load("vroom", "glue", "scales", "ggpubr", "ggsci", "tidyverse")
