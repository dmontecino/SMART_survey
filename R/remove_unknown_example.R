# ---------------------------------------------------------------------- #
#  Remove the rows where the PAs are unknown and also remove the example #
# ---------------------------------------------------------------------- #

# The respondent provided a person's name instead of the protected area name
dat_modified<-dat_modified[-which(map_vec(dat_modified$protected_area, \(x) all(x$protected_area==c("Unknown protected area")))),]

dat_modified<-dat_modified[-which(names(dat_modified$protected_area)=='14'),] # unknown