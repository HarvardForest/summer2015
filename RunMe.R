################################################################################
#     Run this script to launch the Lotka-Volterra Predator-Prey Shiny App
#     By: Nathan Justice
#     Last edited: 22July2015
################################################################################

install.packages("shiny")
install.packages("shinyapps")
install.packages("shinythemes")
install.packages("shinyAce")
install.packages("deSolve")
install.packages("breakpoint")
install.packages("earlywarnings")
install.packages("plotrix")

runApp("lvpredprey")
