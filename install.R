################################################################################
#     Run this script to download the dependcies the Lotka-Volterra
#       Predator-Prey Shiny App requires
#
#     By: Nathan Justice
#     Last edited: 23July2015
################################################################################

install.packages("shiny")
install.packages("shinyapps")
install.packages("shinythemes")
install.packages("shinyAce")
install.packages("deSolve")
install.packages("breakpoint")
install.packages("earlywarnings")
install.packages("plotrix")

shiny::runApp("~/Desktop/summer2015/lvpredprey")
