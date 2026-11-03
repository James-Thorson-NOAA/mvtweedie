## code to prepare `DATASET` dataset goes here

data_dir =  R'(C:\Users\James.Thorson\Desktop\Git\mvtweedie\data-raw)'

# Load and format
southeast_alaska_wolf = read.csv( file.path(data_dir,"Wolf.csv") )

# Export
setwd( R'(C:\Users\James.Thorson\Desktop\Git\mvtweedie)' )
usethis::use_data( southeast_alaska_wolf, overwrite=TRUE )
