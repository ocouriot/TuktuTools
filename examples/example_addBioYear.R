data("caribou")

head(caribou)

# bioStart is yday 152 (01 June)

caribou <- addBioYear(caribou, bioStart = 152)
head(caribou)
