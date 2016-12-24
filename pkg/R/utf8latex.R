
u2tex<-function(inputString=NA, inputFile=NA, outputFile=NA, keepUTF8=TRUE){
# create a LaTeX file from UTF-8 input with languages grouped automatically
if ((is.na(inputString)) && (is.na(inputFile))) stop('Error: either parameter <inputString> or <inputFile> should be character')
if (!is.character(inputString) && (!is.character(inputFile)) ) stop('Error: either parameter <inputString> or <inputFile> should be character')
if ((is.na(outputFile)) && (is.na(inputFile))) outputFile<-'u2texout.tex'
if (is.na(outputFile)) outputFile<-paste(inputFile,'.tex',sep='',collapse='\n')
if (is.na(inputString)){
con <- file(inputFile, 'r', blocking = FALSE)
inputString<-readLines(con)
close(con)
inputString<-paste(inputString,sep='',collapse='\n')
}
z<-mapLangID(langString=inputString)
z1<-groupWordsByLang(z)
z2<-groupPosWordsByLang(z1,TRUE)
langFontEnc<-getFontencAndLang(inputString)
l<-nchar(inputString)
r<-''
for(n in 1:l)
{
if (any(n == z2[,1])) r<-paste(r,z2[which(z2[,1]==n),2],sep='')
rtemp<-substr(inputString,n,n)
if (keepUTF8==FALSE) rtemp<-utf8texc(rtemp)
r<-paste(r,(rtemp),sep='')
}
if (is.Armenian(inputString)) otherEnc<-'\n\n\\usepackage[latin]{armtex}\n' else  otherEnc<-''# ArmTeX is not included in babel
if (file.exists(outputFile)) file.remove(outputFile)
cat('\\documentclass[11pt]{article}
\\usepackage{amsmath,amssymb,mathrsfs,amsthm,textcomp}
\\usepackage[' , langFontEnc['fontEnc'] , ']{fontenc}%
\\usepackage[utf8]{inputenc}
\\usepackage[' , langFontEnc['languageBabel'] , ']{babel}',otherEnc,'
\\title{Fileconverted by utf8latex}
\\begin{document}',file=outputFile,append=TRUE)
cat(r,file=outputFile,append=TRUE)
cat('\n\\end{document}',file=outputFile,append=TRUE)
}

utf8texc<-function(utf8char) 
{# converts a string from UTF-8 to LaTeX
if (!is.character(utf8char)) stop('Argument <<utf8char>> should be a character')
nX<-nchar(utf8char)
r<-''
for (n in 1:nX) r<-paste(r,.Call("UTF8Latex",substr(utf8char,n,n)),sep='')
r
}

utf8tex<-function(utf8var) {# converts vector, matrix and dataframe from UTF-8 to LaTeX
dx1<-dim(utf8var)[1]
dx2<-dim(utf8var)[2]
nX<-length(utf8var)
if (is.vector(utf8var)){#vector
if (nX==1) r<-utf8texc(utf8var) else r<-sapply(utf8var,utf8texc)
names(r)<-NULL
} else{#matrix or dataframe
if (is.matrix(utf8var)) r<-apply(utf8var,1:2,utf8texc) else { r<-as.matrix(utf8var);for (n in 1:dx1) for (m in 1:dx2)  r[n,m]<-utf8texc(r[n,m]) }
}
r
}

from2colTo3 <- function(file2cols, file3cols){ # reads a file with 2 columns (hex, tag) and saves it as 3 columns (hex, utf8, tag)
t2<-read.delim(file2cols, stringsAsFactors=FALSE, header=FALSE,quote='')
t3<-cbind(t2[,1],'',t2[,2])
for (n in 1:dim(t3)[1]) t3[n,2]<-intToUtf8(as.integer(paste('0x', t3[n,1],sep='')))
write(t(t3),file3cols, sep='\t',ncolumns =3)
}

joinListOfTags<-function(files3colsUTFlatex, fileCombined){ # creates a file with 3 columns (UTF8 code points, UTF8 character and LaTeX tag) from a list of files
t3<-NULL
for (tag in files3colsUTFlatex)
{
t2<-read.delim(tag, stringsAsFactors=FALSE, header=FALSE,quote='')
if (dim(t2)[2] != 3) stop(paste('Error!',tag,'doesn\'t have 3 colums of data.'))
t2<-as.matrix(t2,ncols=3)
if (any(nchar(t2[,1])!=5)) stop()
t2[,1]<-toupper(t2[,1])
if (is.null(t3)) t3<-t2 else t3<-rbind(t3,t2)
}
t4<-t3[order(t3[,1], t3[,3]),]
for (n in 2:dim(t4)[1]) if ((t4[n,1]==t4[n-1,1]) & (t4[n,3]==t4[n-1,3]) ) t4[n-1,1]<-''
t4<-t4[-which(t4[,1]==''),]
t4<-t4[which(unique(t4[,1]) %in% t4[,1]),]
write(t(t4),fileCombined, sep='\t',ncolumns =3)
}

langID<-function(utf8char){ # returns a character indicating the language of a UTF8 point
if (is.African(utf8char)) return('F')
if (is.Thai(utf8char)) return('T')
if (is.Vietnamese(utf8char)) return('V')
if (is.Cyrillic(utf8char)) return('R')
if (is.Greek(utf8char)) return('G')
if (is.Hebrew(utf8char)) return('H')
if (is.Arabic(utf8char)) return('A')
if (is.Armenian(utf8char)) return('N')
if (is.Georgian(utf8char)) return('E')
if (is.Latin(utf8char)) return('L')
return(' ')
}

mapLangID<-function(langString=NA, langFile=NA, mapFile=NA){ # returns a file with each character mapped to an identifier for several languages
if ((is.na(langString)) && (is.na(langFile))) stop('Error: either parameter <langString> or <langFile> should be character')
if (!is.character(langString) && (!is.character(langFile)) ) stop('Error: either parameter <langString> or <langFile> should be character')
if (is.character(langString)) r<- langString else {
con <- file(langFile, 'r', blocking = FALSE)
r<-readLines(con)
close(con)
}
m<-paste(r,sep='',collapse='\n')
s<-''
lenr<-nchar(m)
for(n in 1:lenr)
{
x<-substr(m,n,n)
if (x %in% c('\t','\n')) s<-paste(s, x,sep='') else s<-paste(s, langID(x),sep='')
}
if (is.character(mapFile)) cat(s,file=mapFile) else return (s)
}

getFontencAndLang<-function(mLangString=NA, mLangFile=NA)# determine fontenc and [language]babel
{
if ((is.na(mLangString)) && (is.na(mLangFile))) stop('Error: either parameter <mLangString> or <mLangFile> should be character')
if (!is.character(mLangString) && (!is.character(mLangFile)) ) stop('Error: either parameter <mLangString> or <mLangFile> should be character')
if (is.character(mLangString)) s<- mLangString else s<-mapLangID(langFile=mLangFile)
lText<-unlist(strsplit(s,''))
uniqLang<-unique(lText)
fontEnc<-''
languageBabel<-''
if ('E' %in% uniqLang) { fontEnc<-paste(fontEnc,'T8M,T8K,',sep=''); languageBabel<-paste(languageBabel,'georgian,',sep='') }
#if ('N' %in% uniqLang) {fontEnc<-paste(fontEnc,'OT6,',sep=''); languageBabel<-paste(languageBabel,'armenian,',sep='') }
if ('A' %in% uniqLang) {fontEnc<-paste(fontEnc,'LAE,LFE,',sep=''); languageBabel<-paste(languageBabel,'arabic,farsi,',sep='') }
if ('H' %in% uniqLang) {fontEnc<-paste(fontEnc,'Hx8,',sep=''); languageBabel<-paste(languageBabel,'hebrew,',sep='') }
if ('G' %in% uniqLang) {fontEnc<-paste(fontEnc,'LGRx,LGR,',sep=''); languageBabel<-paste(languageBabel,'greek,',sep='') }
if ('R' %in% uniqLang) {fontEnc<-paste(fontEnc,'T2A,',sep=''); languageBabel<-paste(languageBabel,'russian,',sep='') }
if ('V' %in% uniqLang) {fontEnc<-paste(fontEnc,'T5,',sep=''); languageBabel<-paste(languageBabel,'vietnamese,',sep='') }
if ('F' %in% uniqLang) fontEnc<-paste(fontEnc,'T4,',sep='')
if ('T' %in% uniqLang) languageBabel<-paste(languageBabel,'thai,',sep='')
fontEnc<-paste(fontEnc,'T1',sep='')
languageBabel<-paste(languageBabel,'english',sep='')
c(fontEnc=fontEnc,languageBabel=languageBabel)
}

# group words by language
groupWordsByLang<-function(mLangString=NA, mLangFile=NA)
{
if ((is.na(mLangString)) && (is.na(mLangFile))) stop('Error: either parameter <mLangString> or <mLangFile> should be character')
if (!is.character(mLangString) && (!is.character(mLangFile)) ) stop('Error: either parameter <mLangString> or <mLangFile> should be character')
if (is.character(mLangString)) s<- mLangString else s<-mapLangID(langFile=mLangFile)
s3<-gsub('F','L',mLangString)
s3<-gsub('V','L',s3)# Vietnamese and African Latin = Latin
s<-''
p<-''
lenr<-nchar(s3)
for(n in 1:lenr)
{
x<-substr(s3,n,n)
if (x %in% c('\n','\t')) s<-paste(s,x,sep='') else { if ((x == ' ') | (x==p) ) s<-paste(s,p,sep='') else { p<-x;s<-paste(s,x,sep='') }  }
}
while (grepl('\\s$',s3)) s3<-gsub('([A-Z])\\s\\s*$','\\1\\1',s3) # fill it to the end
s4<-s3
while (grepl('\\s',s4)) s4<-gsub('\\s([A-Z])','\\1\\1',s4)
s4
}

groupPosWordsByLang<-function(gLangString=NA, wTags=FALSE, vTags=NA, vEndTags=NA){
# return the positions for groups of words by language
if (wTags) if (is.na(vTags)) {
vTags<-matrix(c('L', '\\selectlanguage{english}','T', '\\selectlanguage{thai}','N', '\\artm','E', '\\selectlanguage{georgian}',
'A', '\\selectlanguage{arabic}','H', '\\selectlanguage{hebrew}','G', '\\selectlanguage{greek}','R', '\\selectlanguage{russian}'), ncol=2, byrow=TRUE)
}
if (is.na(vEndTags)) vEndTags <- matrix(c('L', '','T', '','N', '\\aroff','E', '',
'A', '','H', '','G', '','R', ''), ncol=2, byrow=TRUE)
langP<-''
langDataFrame<-c(pos=0,lang='')
for (n in 1:nchar(gLangString)){
s1<-substr(gLangString,n,n)
if (!(s1 %in% c(' ','\t','\n'))){
if (s1 %in% c('V','F')) s1<-'L'
if (!wTags) if (s1 != langP) { langDataFrame<-rbind(langDataFrame,c(n-1,langP),c(n,s1));langP<-s1 }
if (wTags) if (s1 != langP) { langDataFrame<-rbind(langDataFrame,c(n-1,vTags[which(vTags[,1]==langP),2]),c(n,
paste(  vEndTags[which(vEndTags[,1]==langP),2], vTags[which(vTags[,1]==s1),2], sep='' )  ));langP<-s1 }
}
}
d<-dim(langDataFrame)[1]
if (wTags) langDataFrame<-langDataFrame[-2*(1:(d %/% 2)),]
langDataFrame[-(1:2),]
}


utf8enc<-function(utf8var){# encodes UTF8 characters to avoid the Warning: found non-ASCII string(s)
if ((is.null(dim(utf8var))) | (is.matrix(utf8var))) 
{ 
if (is.null(dim(utf8var))) l<-1 else l<-dim(utf8var)[1]*dim(utf8var)[2]
x2<-utf8var
for (n in 1:l) 
{
k<-utf8var[n]
if (is.character(k)) if (nchar(k)>0)
{
s2<-''
for (n2 in 1:nchar(k))
{
s<-substr(k,n2,n2)
Ucode<-utf8ToInt(s)
if (Ucode>128) s2<-paste(s2,iconv(iconv(s,toRaw=T),'UTF8'),sep='') else  s2<-paste(s2,s,sep='') 
}
x2[n]<-s2
}
}
return(x2)
} else { 
x2<-utf8var
for (n in 1:dim(utf8var)[1]) for (m in 1:dim(utf8var)[2])
{
k<-utf8var[n,m]
if (is.character(k)) if (nchar(k)>0)
{ 
s2<-''
for (n2 in 1:nchar(k))
{
s<-substr(k,n2,n2)
Ucode<-utf8ToInt(s)
if (Ucode>128) s2<-paste(s2,iconv(iconv(s,toRaw=T),'UTF8'),sep='') else  s2<-paste(s2,s,sep='') 
}
x2[n,m]<-s2
}
}
return(x2)
}
}

idEncoding<-function(filename){
# detect the encoding of a text file
# more info:  http://en.wikipedia.org/wiki/Byte_order_mark
# idEncoding('textUTF16')
conn<-file(filename, 'rb')
BOM<-readBin(conn, integer(), n=5,size=1, signed=FALSE )
close(conn)
if (all(BOM[1:2]==c(0xFE, 0xFF))) return('UTF-16BE')
if (all(BOM[1:3]==c(0xEF, 0xBB, 0xBF))) return('UTF-8')
if (all(BOM[1:2]==c(0xFF, 0xFE))) return('UTF-16LE')
if (all(BOM[1:4]==c(0xFF, 0xFE, 0x00, 0x00))) return('UTF-32LE')
if (all(BOM[1:4]==c(0x00, 0x00, 0xFE, 0xFF))) return('UTF-32BE')
if (all(BOM[1:4]==c(0x2B, 0x2F, 0x76, 0x38))) return('UTF-7')
if (all(BOM[1:4]==c(0x2B, 0x2F, 0x76, 0x39))) return('UTF-7')
if (all(BOM[1:4]==c(0x2B, 0x2F, 0x76, 0x2B))) return('UTF-7')
if (all(BOM[1:4]==c(0x2B, 0x2F, 0x76, 0x2F))) return('UTF-7')
if (all(BOM[1:5]==c(0x2B, 0x2F, 0x76, 0x38, 0x2D))) return('UTF-7')
if (all(BOM[1:3]==c(0xF7, 0x64, 0x4C))) return('UTF-1')
if (all(BOM[1:4]==c(0xDD, 0x73, 0x66, 0x73))) return('UTF-EBCDIC')
if (all(BOM[1:3]==c(0x0E, 0xFE, 0xFF))) return('SCSU')
if (all(BOM[1:3]==c(0xFB, 0xEE, 0x28))) return('BOCU-1')
if (all(BOM[1:4]==c(0x84, 0x31, 0x95, 0x33))) return('GB-18030')
print('Unknown format')
}

utf8ToHex<-function(utf8char,tZeros=TRUE, sLen=5) # Convert from UTF-8-encoded Character to hexadecimal
{
if (sLen>0){
if (tZeros) s<-paste('%0',sLen,'X',sep='') else s<-paste('% ',sLen,'X',sep='')
} else s<-'%X';
x<-utf8ToInt(utf8char)
if (length(x)>1) x<-x[length(x)]
sprintf(s,x)
}

utf8ToOct<-function(utf8char,tZeros=TRUE, sLen=5) # Convert from UTF-8-encoded Character to octal
{
if (sLen>0){
if (tZeros) s<-paste('%0',sLen,'o',sep='') else s<-paste('% ',sLen,'o',sep='')
} else s<-'%o';
x<-utf8ToInt(utf8char)
if (length(x)>1) x<-x[length(x)]
sprintf(s,x)
}

is.ChineseTrad<-function(utf8char){# returns TRUE if the input is UTF-8 Chinese traditional
#code translated from Python, original author: Mark Baker http://stackoverflow.com/questions/4083038/recognizing-text-as-simplified-vs-traditional-chinese
test1 <- iconv(utf8char,"UTF-8", "big5//TRANSLIT")
test2 <- iconv(utf8char,"UTF-8", "big5//IGNORE")
if (is.na(test1) | is.na(test2)) return(FALSE)
if (identical(test1, test2)) return(TRUE)
test3 <- iconv(utf8char,"UTF-8", "gb18030//TRANSLIT")
test4 <- iconv(utf8char,"UTF-8", "gb18030//IGNORE")
if (is.na(test3) | is.na(test4)) return(FALSE)
if (identical(test3, test4)) return(TRUE)
return(FALSE)
}


is.Hangul<-function(utf8char) #returns TRUE if utf8char is within the Hangul Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x1100:0x11FF) return (TRUE)
if (v %in% 0x3130:0x318F) return (TRUE)
if (v %in% 0xA960:0xA97F) return (TRUE)
if (v %in% 0xAC00:0xD7AF) return (TRUE)
if (v %in% 0xD7B0:0xD7FF) return (TRUE)
FALSE
}
is.Arabic<-function(utf8char) #returns TRUE if utf8char is within the Arabic Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x0600: 0x06FF) return (TRUE)
if (v %in% 0x0750: 0x077F) return (TRUE)
if (v %in% 0x08A0: 0x08FF) return (TRUE)
if (v %in% 0xFB50: 0xFDFF) return (TRUE)
if (v %in% 0xFE70: 0xFEFF) return (TRUE)
if (v %in% 0x1EE00:0x1EEFF) return (TRUE)
FALSE
}
is.Hebrew<-function(utf8char) #returns TRUE if utf8char is within the Hebrew Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x0590:0x05FF) return (TRUE)
FALSE
}
is.Cyrillic<-function(utf8char) #returns TRUE if utf8char is within the Cyrillic Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x0400:0x04FF) return (TRUE)
if (v %in% 0x0500:0x052F) return (TRUE)
if (v %in% 0x2DE0:0x2DFF) return (TRUE)
if (v %in% 0xA640:0xA69F) return (TRUE)
FALSE
}
is.Katakana<-function(utf8char) #returns TRUE if utf8char is within the Katakana Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x30A0:0x30FF) return (TRUE)
if (v %in% 0x31F0:0x31FF) return (TRUE)
FALSE
}
is.Hiragana<-function(utf8char) #returns TRUE if utf8char is within the Hiragana Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x3040:0x309F) return (TRUE)
FALSE
}
is.JapanesePunctuation<-function(utf8char) #returns TRUE if utf8char is within the Japanese Punctuation Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x3000:0x303F) return (TRUE)
FALSE
}
is.JapaneseLatinAndHalfWidthKatakana<-function(utf8char) #returns TRUE if utf8char is within the Japanese Punctuation Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0xff00:0xffeF) return (TRUE)
FALSE
}
is.JapaneseCJK<-function(utf8char) #returns TRUE if utf8char is within the Japanese Punctuation Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% c(0x4e00:0x9faF, 0x3400:0x4dbF)) return (TRUE)
FALSE
}

is.Thai<-function(utf8char) #returns TRUE if utf8char is within the Thai Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x0E00:0x0E7F) return (TRUE)
FALSE
}
is.Greek<-function(utf8char) #returns TRUE if utf8char is within the Greek Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x0370:0x03FF) return (TRUE)
if (v %in% 0x1F00:0x1FFF) return (TRUE)
if (v %in% 0x10140:0x1018F) return (TRUE)
if (v %in% 0x1D200:0x1D24F) return (TRUE)
FALSE
}
is.CJK<-function(utf8char) #returns TRUE if utf8char is within the CJK Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x2E80:0x2EFF) return (TRUE)
if (v %in% 0x3000:0x303F) return (TRUE)
if (v %in% 0x31C0:0x31EF) return (TRUE)
if (v %in% 0x3200:0x32FF) return (TRUE)
if (v %in% 0x3300:0x33FF) return (TRUE)
if (v %in% 0x3400:0x4DBF) return (TRUE)
if (v %in% 0x4E00:0x9FFF) return (TRUE)
if (v %in% 0xF900:0xFAFF) return (TRUE)
if (v %in% 0xFE30:0xFE4F) return (TRUE)
if (v %in% 0x20000:0x2A6DF) return (TRUE)
if (v %in% 0x2A700:0x2B73F) return (TRUE)
if (v %in% 0x2B740:0x2B81F) return (TRUE)
if (v %in% 0x2F800:0x2FA1F) return (TRUE)
FALSE
}
is.Armenian<-function(utf8char) #returns TRUE if utf8char is within the Armenian Unicode ranges
{
v<-utf8ToInt(utf8char)
if (any(v %in%  0x0530:0x058F)) return (TRUE)
FALSE
}
is.African<-function(utf8char) #returns TRUE if utf8char is within the African Latin Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in%  c(0x0190,0x018e,0x0191,0x011a,0x0194,0x0126,0x0198,0x019d,0x0186,0x0134,0x01a9,0x014a,
0x01b2,0x01b3,0x0187,0x01a4,0x1e44,0x1e48,0x1e62,0x0167,0x0166,0x1eb8,0x01ac,0x01ae,0x0111,
0x030f,0x0253,0x0257,0x0258,0x01dd,0x0192,0x0263,0x0127,0x0199,0x0272,0x0254,0x0144)) return (TRUE)
FALSE
}
is.Georgian<-function(utf8char) #returns TRUE if utf8char is within the Armenian Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x10A0:0x10FF) return (TRUE)
if (v %in% 0x2D00:0x2D2F) return (TRUE)
FALSE
}
is.greekmath<-function(utf8char) #returns TRUE if utf8char is on the range of the Greek characters from the math packages
{
v<-utf8ToInt(utf8char)
if (v %in% 0x0391:0x03DB) return (TRUE)
FALSE
}
is.Vietnamese<-function(utf8char) #returns TRUE if utf8char is on the range of the Vietnamese characters
{
v<-utf8ToInt(utf8char)
if (v %in% c(0x01A0, 0x01A1, 0x01AF, 0x01B0, 0x1EA0:0x1EF7)) return (TRUE)
FALSE
}
is.Latin<-function(utf8char) #returns TRUE if utf8char is on the range of the Latin characters
{
v<-utf8ToInt(utf8char)
if (v %in% c(0x0041:0x005A, 0x0061:0x007A, 0x0180:0x024F, 0x0250:0x02AF, 0x1E00:0x1EFF, 0x2C60:0x2C7F, 0xA720:0xA7FF, 0x0100:0x017F)) return (TRUE)
FALSE
}
is.LaTeX.reserved<-function(utf8char) {#returns TRUE if utf8char is one of the LaTeX reserved words # $ % ^ & _ { } ~ \
if (utf8char %in% c('#', '$', '%', '^', '&', '_', '{', '}', '~', '\\')) return(TRUE) else return(FALSE)
}
is.Bengali<-function(utf8char) #returns TRUE if utf8char is within the Bengali Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x0980:0x09FF) return (TRUE)
FALSE
}
is.Brahmi<-function(utf8char) #returns TRUE if utf8char is within the Brahmi Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x11000:0x1107F) return (TRUE)
FALSE
}
is.Chakma<-function(utf8char) #returns TRUE if utf8char is within the Chakma Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x11100:0x1114F) return (TRUE)
FALSE
}
is.Devanagari<-function(utf8char) #returns TRUE if utf8char is within the Devanagari Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x0900:0x097F) return (TRUE)
FALSE
}
is.Gujarati<-function(utf8char) #returns TRUE if utf8char is within the Gujarati Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x0A80:0x0AFF) return (TRUE)
FALSE
}
is.Kannada<-function(utf8char) #returns TRUE if utf8char is within the Kannada Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x0C80:0x0CFF) return (TRUE)
FALSE
}
is.Tamil<-function(utf8char) #returns TRUE if utf8char is within the Tamil Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x0B80:0x0BFF) return (TRUE)
FALSE
}
is.Telugu<-function(utf8char) #returns TRUE if utf8char is within the Telugu Unicode ranges
{
v<-utf8ToInt(utf8char)
if (v %in% 0x0C00:0x0C7F) return (TRUE)
FALSE
}

# wrapper functions from ctype.h
# wrapper function for isalnum from ctype.h
is.alnum<-function(utf8char) .Call("is_alnum",as.character(utf8char),PACKAGE="utf8latex") 
is.alpha<-function(utf8char) .Call("is_alpha",as.character(utf8char),PACKAGE="utf8latex")
is.ascii<-function(utf8char) .Call("is_ascii",as.character(utf8char),PACKAGE="utf8latex")
is.blank<-function(utf8char) .Call("is_blank",as.character(utf8char),PACKAGE="utf8latex")
is.cntrl<-function(utf8char) .Call("is_cntrl",as.character(utf8char),PACKAGE="utf8latex")
is.digit<-function(utf8char) .Call("is_digit",as.character(utf8char),PACKAGE="utf8latex")
is.graph<-function(utf8char) .Call("is_graph",as.character(utf8char),PACKAGE="utf8latex")
is.lower<-function(utf8char) .Call("is_lower",as.character(utf8char),PACKAGE="utf8latex")
is.print<-function(utf8char) .Call("is_print",as.character(utf8char),PACKAGE="utf8latex")
is.punct<-function(utf8char) .Call("is_punct",as.character(utf8char),PACKAGE="utf8latex")
is.space<-function(utf8char) .Call("is_space",as.character(utf8char),PACKAGE="utf8latex")
is.upper<-function(utf8char) .Call("is_upper",as.character(utf8char),PACKAGE="utf8latex")
is.xdigit<-function(utf8char) .Call("is_xdigit",as.character(utf8char),PACKAGE="utf8latex")

