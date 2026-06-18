# shARed mInotAur Database exploratioN Environment : ARIADNE
# Version: 4.1.1
# Author:
# Francesco Vitali §
# Mocali Stefano §
# Sara Del Duca §
# Elena Tondini §
# Vivianne Yayende +
# Antonio Bispo +
# Mario Adam +
# Rajasekaran Murugan @
#
# Author e-mail:
# francesco.vitali@crea.gov.it
# stefano.mocali@crea.gov.it
# sara.delduca@crea.gov.it
# elena.tondini@crea.gov.it
# vivianne.koyao-yayende@inrae.fr
# antonio.bispo@inrae.fr
# mario.adam@inrae.fr
# rajasekaran.murugan@boku.ac.at
#
# Author Affiliation:
# § Research Centre for Agriculture and Environment, Council for Agricultural Research and Economics (CREA-AA)
# + National Research Institute for Agriculture, Food and Environment (INRAE)
# @ BOKU University (BOKU)


source("global.R")
source("ui.R")
source("server.R")


shinyApp(
  ui = ui,
  server = server
)
