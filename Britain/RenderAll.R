library(quarto)

quarto_render("Britain/01-DataProcessing.qmd")
quarto_render("Britain/02-eBird.qmd")
quarto_render("Britain/03-BirdTrack.qmd")
quarto_render("Britain/04-Comparison.qmd")
quarto_render("Britain/05-OccupancyModelling.qmd")

quarto_render("Britain/06a-OccupancyModelling-Plots.qmd")
quarto_render("Britain/06b-OccupancyModelling-LinearDifferences.qmd")
quarto_render("Britain/06c-OccupancyModelling-LogitDifferences.qmd")