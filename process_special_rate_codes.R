process_special_rate_codes <- function(rate_as_char){
# DESCRIPTION: Maps the specialized input character variable rate_as_char into numeric variables using a switch() statement
#              Returns -1 for inputs that are not identified by the mapping defined within the switch() statement.
#
# EXAMPLE:     process_special_rate_codes('LE20') returns 10
#
# INPUTS:
#   rate_as_char    character variable that is converted to a pre-defined numerical value

# LE:= Less than or equal to
# GE:= Greater than or equal to
# LT:= Less than or equal to
# PS:= Privacy of Students maintained. Value reported as 0.

  switch(rate_as_char, "LE1" = 0.5, "LE5"= 2.5, "LE10" = 5, "LE20" = 10, "LT50" = 25, "GE50" = 75, "GE80" = 90,
  "GE90" = 95, "GE95" = 97.5, "GE99" = 99.5, "PS" = -1, -1)
  
#  switch(rate_as_char, "LE1" = runif(n=1,min=0,max=1), "LE5"= runif(n=1,min=0,max=5), "LE10" = runif(n=1,min=0,max=10), "LE20" = runif(n=1,min=0,max=20), "LT50" = runif(n=1,min=0,max=50), "GE50" = runif(n=1,min=50,max=100), "GE80" = runif(n=1,min=80,max=100),
#         "GE90" = runif(n=1,min=90,max=100), "GE95" = runif(n=1,min=95,max=100), "GE99" = runif(n=1,min=99,max=100), "PS" = -1, -1)
}
