# Set options here
options(golem.app.prod = FALSE, shiny.maxRequestSize = 50*1024^2) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
datacomparator::run_app()
