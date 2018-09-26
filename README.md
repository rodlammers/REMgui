# REMgui

This is an R Shiny application that serves as a simple Graphical User Interface (GUI) for the River Erosion Model (REM). In R, you must have the "shiny" package and several other helper packages installed.

``install.packages(c("shiny", "shinyBS", "rhandsontable", "dataRetrieval", "dplyr", "devtools"))``

``devtools::install_github("bceaton/stochasim")``

``devtools::install_github("rodlammers/REMvisualizer")``

Then, simply run the following line of code to launch the REMgui app:

``shiny::runGitHub("REMgui", "rodlammers")``

And that's it! You specify a folder on your computer to save model inputs and outputs. The REMgui app helps you set up relatively simple model runs. All inputs can be further customized (see the REM [User Guide](https://github.com/rodlammers/REM) which can also be downloaded from within the REMgui app). To run REM, you must download the REM [executable file](https://github.com/rodlammers/REM) and place it in the same directory in which you are saving model inputs.
