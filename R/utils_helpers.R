# Helpful conversion list
com_id  <-
  data.frame(
    "SCAP" = "Alhambra/Monterey Park",
    "SCBB" = "Big Bear Lake",
    "SCEM" = "El Monte",
    "SCIV" = "Imperial Valley",
    "SCNP" = "Nipomo",
    "SCPR" = "Paso Robles",
    "SCSJ" = "San Jacinto",
    "SCSB" = "Seal Beach",
    "SCAH" = "Oakland",
    "SCAN" = "Richmond",
    "SCUV" = "West Los Angeles",
    "SCSG" = "South Gate",
    "SCHS" = "Sycamore Canyon",
    "SCTV" = "Temescal Valley"
  )

# 4 Digit code to Community name
id2com <- function(X) {
  unlist(lapply(X, function(x) {ifelse(is.null(com_id[[x]]), x, levels(com_id[[x]]))}))
}

# Community name to 4 digit code
com2id <- function(X) {
  unlist(lapply(X, function(x) {i<-which(com_id == x); ifelse(length(i)!=0, names(com_id[i]), x)}))
}
