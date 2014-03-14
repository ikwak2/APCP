doc:
	R -e 'library(devtools);document(roclets=c("namespace", "rd"))'

build:
	R -e 'library(devtools);build("../APCP")'