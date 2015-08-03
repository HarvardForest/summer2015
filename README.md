# About:

## Harvard Forest Summer Research Program in Ecology

### REU Student: [Nathan Justice](mailto:n.justice@outlook.com)

#### Dates: May 26, 2015 - August 7, 2015

# View the apps:
### Lotka-Volterra Predator Prey:
https://ecoapps-lvpredatorprey.herokuapp.com/ or https://nthnjustice.shinyapps.io/ecoapps-lvpredatorprey

### Pitcher Plant:
http://ecoapps-pitcherplant.herokuapp.com/ or https://nthnjustice.shinyapps.io/ecoapps-pitcherplant

#### Goal: 
A computational tool used to investigate tipping points and early warning signals in simulations of ecosystem dynamics

#### Method: 
Multiple [Shiny](http://shiny.rstudio.com/) web applications, where each app simulates a different ecological model

# Instructions for launching locally:

1) [Download R](https://www.r-project.org/)

2) [Download Git](https://git-scm.com/downloads)

3) Clone this repository to your desktop using the terminal commands:

`cd ~/Desktop`

`git clone https://github.com/HarvardForest/EcoApps.git`

4) Open the desired web application by navigating into the appropriate branch:

`cd ~/Desktop/EcoApps`

`git branch` - view all availble branches

`git checkout nameOfAppBranch`

5) Open and run the script "install.R" to download the required
  dependencies and launch the app locally
  
(If "install.R" has already run on your machine, use the "runMe.R"
script to launch the app without reinstalling the dependencies)
