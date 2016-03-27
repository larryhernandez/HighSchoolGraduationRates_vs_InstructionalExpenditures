process_special_rate_codes <- function(rate_as_char){

#  switch(rate_as_char, "LE1" = 1, "LE5"= 5, "LE10" = 10, "LE20" = 20, "LT50" = 50, "GE50" = 50, "GE80" = 80,
#  "GE90" = 90, "GE95" = 95, "GE99" = 99, "PS" = 0, -1)
  
  switch(rate_as_char, "LE1" = 1, "LE5"= 2.5, "LE10" = 5, "LE20" = 10, "LT50" = 25, "GE50" = 75, "GE80" = 90,
  "GE90" = 95, "GE95" = 97, "GE99" = 99, "PS" = 0, -1)
#   return(-1)
}