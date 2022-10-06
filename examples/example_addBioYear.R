data("barrenground")

head(barrenground)

# bioStart is yday 152 (01 June)

barrenground <- addBioYear(barrenground, bioStart = 152)
