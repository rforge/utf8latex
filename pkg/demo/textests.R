library(utf8latex)

# List of European cities from Wikipedia
fileIn<-system.file('extdata/EuropeanCities.txt', package='utf8latex')
u2tex( inputFile=fileIn, outputFile= 'EuropeanCities.tex', keepUTF8=TRUE)
u2tex( inputFile=fileIn, outputFile= 'EuropeanCities2.tex', keepUTF8=FALSE)

# List of European rivers from Wikipedia
fileIn<-system.file('extdata/EuropeanRivers.txt', package='utf8latex')
u2tex( inputFile=fileIn, outputFile= 'EuropeanRivers.tex', keepUTF8=TRUE)
u2tex( inputFile=fileIn, outputFile= 'EuropeanRivers2.tex', keepUTF8=FALSE)

# 
fileIn<-system.file('extdata/EnglishArmenian.txt', package='utf8latex')
u2tex( inputFile=fileIn, outputFile= 'EnglishArmenian.tex', keepUTF8=TRUE)
u2tex( inputFile=fileIn, outputFile= 'EnglishArmenian2.tex', keepUTF8=FALSE)

# Old assignment from course "Matematiska programpaket"
fileIn<-system.file('extdata/ovn1.txt', package='utf8latex')
u2tex( inputFile=fileIn, outputFile= 'ovn1auto.tex', keepUTF8=FALSE)

#utf8texc(inputString)





