
# maybe propose the use to install packages?
required_packages <- c("plotly","shiny","DT")
packages_to_install <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(packages_to_install)) install.packages(packages_to_install)

# load Shiny
library(shiny)

# set up parameters
#options(shiny.host = "0.0.0.0", shiny.port = 55555)

# run the application
runApp("plot-evolution")


