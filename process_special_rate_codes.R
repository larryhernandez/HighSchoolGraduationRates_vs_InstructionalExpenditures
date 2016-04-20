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

  switch(rate_as_char, "LE1" = 1, "LE5"= 2.5, "LE10" = 5, "LE20" = 10, "LT50" = 25, "GE50" = 75, "GE80" = 90,
  "GE90" = 95, "GE95" = 97, "GE99" = 99, "PS" = 0, -1)

}
