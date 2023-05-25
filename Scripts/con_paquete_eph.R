library(eph)

ind_4_22 <- get_microdata(
  year = 2022, 
  trimester = 4, 
  type = 'individual'
)

hog_4_22 <- get_microdata(
  year = 2022, 
  trimester = 4, 
  type = 'hogar'
)

ind_4_22 <- organize_labels(
  df = ind_4_22, 
  type='individual'
)

hog_4_22 <- organize_labels(
  df = hog_4_22, 
  type='hogar'
)






