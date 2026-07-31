a <- renv::dependencies()

lapply(1:length(a$Package), function(i) {
  
  usethis::use_package(package = a$Package[i])
  
})
