## code to prepare `package_data` dataset goes here

comp_colors <- c(addition = "#1FC46F",
                 removal = "#F94444",
                 unchanged_cell = "#7C8AA4",
                 unchanged_row = "#7C8AA4")

usethis::use_data(comp_colors, overwrite = TRUE)
