################################################################################
#     Run this script to download the dependcies the Lotka-Volterra
#       Predator-Prey Shiny App requires
#
#     By: Nathan Justice
#     Last edited: 23July2015
################################################################################

install.packages("shiny")
install.packages("shinythemes")
install.packages("shinyAce")
install.packages("deSolve")
install.packages("cpm")
#install.packages("earlywarnings")
install.packages("moments")
install.packages("plotrix")

shiny::runApp("~/Desktop/summer2015/lvpredpreyApp")
