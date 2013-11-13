#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <locale.h>
#include <ctype.h>
#include <wchar.h>
#include <wctype.h>
#include "utf8latex.h"

static int binsearch_uint(unsigned int data[], unsigned int value) {
 /* binsearch, array must be sorted */
int max=sizeof(UnicodeCodePoint) / sizeof(unsigned int);
 int position=0;
 int begin = 0;
 int end = max - 1;
 while(begin <= end) {
  position = (begin + end) / 2;
  if(data[position]==value)
   return position;
  else if(data[position]<value)
   begin = position + 1;
  else
   end = position - 1;
 }
 return -1;//0
}

SEXP UTF8Latex(SEXP mychar) {
//const char *c = CHAR(STRING_ELT(mychar, 0));
//char resev[3]="\\  ";
wchar_t *w = Calloc(6+1, wchar_t);//CHAR(STRING_ELT(mychar, 0));
int t2;
unsigned int t;
//PROTECT(mychar = allocVector(STRSXP, 1));
mbstowcs(w, translateChar(STRING_ELT(mychar, 0)), 6+1);
t=(unsigned int)*w;
t2=binsearch_uint(UnicodeCodePoint, t);//(unsigned int **)&
/*
Rprintf("t=%u \n", t); // t gets the right value
Rprintf("t2=%d \n", t2);
Rprintf("%s \n", c);
if (t2 != 0) printf("%s %s\n", c,Latex[t2]);
printf("hello %ls\n", w);//
*/
//UNPROTECT(1);
if (t2<0) return(mychar); else return(mkString(Latex[t2]));
}

void UTF8LatexFile(SEXP SfromFile,SEXP StoFile){// convert some UTF-8 characters to Latex
const char *fromFile = CHAR(STRING_ELT(SfromFile, 0));
const char *toFile = CHAR(STRING_ELT(StoFile, 0));
   FILE *file;
   FILE *write;
    wchar_t x;
    unsigned int t;
    int t2;
setlocale(LC_CTYPE, "C.UTF-8");
   file = fopen ( fromFile, "r, ccs=UTF-8" );
   write = fopen (toFile, "w" );
  if ( file != NULL )
   {
      while ( (x=fgetwc (file )) != EOF )
      {
   t=(unsigned int)x;
   t2=binsearch_uint(UnicodeCodePoint, t);
if (t2 == -1) {
   if (t<128){ // 0x7F = 127
   fprintf (write ,"%c", (char)t);
   }
   else {
fprintf (write ,"%lc", t); // put the UTF-8 back unchanged
}
   }
   else fprintf(write,"%s", Latex[t2]);
      }
      fclose (write);
      fclose ( file );
   }
   else
   {
      Rprintf("File error");
   }
}

SEXP is_alnum(SEXP v) { // wrapper function for isalnum from ctype.h
SEXP ans;
int i, len = Rf_length(v);
wchar_t *w = Calloc(6+1, wchar_t);
unsigned int t;
PROTECT(ans = Rf_allocVector(LGLSXP, len));
for (i = 0; i < len; i++){
mbstowcs(w, translateChar(STRING_ELT(v, i)), 6+1);
t=(unsigned int)*w;
LOGICAL(ans)[i] = isalnum(t);
}
UNPROTECT(1);
return ans;
}

SEXP is_alpha(SEXP v) { // wrapper function for isalpha from ctype.h
SEXP ans;
int i, len = Rf_length(v);
wchar_t *w = Calloc(6+1, wchar_t);
unsigned int t;
PROTECT(ans = Rf_allocVector(LGLSXP, len));
for (i = 0; i < len; i++){
mbstowcs(w, translateChar(STRING_ELT(v, i)), 6+1);
t=(unsigned int)*w;
LOGICAL(ans)[i] = isalpha(t);
}
UNPROTECT(1);
return ans;
}

SEXP is_ascii(SEXP v) { // wrapper function for isascii from ctype.h
SEXP ans;
int i, len = Rf_length(v);
wchar_t *w = Calloc(6+1, wchar_t);
unsigned int t;
PROTECT(ans = Rf_allocVector(LGLSXP, len));
for (i = 0; i < len; i++){
mbstowcs(w, translateChar(STRING_ELT(v, i)), 6+1);
t=(unsigned int)*w;
LOGICAL(ans)[i] = isascii(t);
}
UNPROTECT(1);
return ans;
}

SEXP is_blank(SEXP v) { // wrapper function for isblank from ctype.h
SEXP ans;
int i, len = Rf_length(v);
wchar_t *w = Calloc(6+1, wchar_t);
unsigned int t;
PROTECT(ans = Rf_allocVector(LGLSXP, len));
for (i = 0; i < len; i++){
mbstowcs(w, translateChar(STRING_ELT(v, i)), 6+1);
t=(unsigned int)*w;
LOGICAL(ans)[i] = isblank(t);
}
UNPROTECT(1);
return ans;
}

SEXP is_cntrl(SEXP v) { // wrapper function for iscntrl from ctype.h
SEXP ans;
int i, len = Rf_length(v);
wchar_t *w = Calloc(6+1, wchar_t);
unsigned int t;
PROTECT(ans = Rf_allocVector(LGLSXP, len));
for (i = 0; i < len; i++){
mbstowcs(w, translateChar(STRING_ELT(v, i)), 6+1);
t=(unsigned int)*w;
LOGICAL(ans)[i] = iscntrl(t);
}
UNPROTECT(1);
return ans;
}

SEXP is_digit(SEXP v) { // wrapper function for isdigit from ctype.h
SEXP ans;
int i, len = Rf_length(v);
wchar_t *w = Calloc(6+1, wchar_t);
unsigned int t;
PROTECT(ans = Rf_allocVector(LGLSXP, len));
for (i = 0; i < len; i++){
mbstowcs(w, translateChar(STRING_ELT(v, i)), 6+1);
t=(unsigned int)*w;
LOGICAL(ans)[i] = isdigit(t);
}
UNPROTECT(1);
return ans;
}

SEXP is_graph(SEXP v) { // wrapper function for isgraph from ctype.h
SEXP ans;
int i, len = Rf_length(v);
wchar_t *w = Calloc(6+1, wchar_t);
unsigned int t;
PROTECT(ans = Rf_allocVector(LGLSXP, len));
for (i = 0; i < len; i++){
mbstowcs(w, translateChar(STRING_ELT(v, i)), 6+1);
t=(unsigned int)*w;
LOGICAL(ans)[i] = isgraph(t);
}
UNPROTECT(1);
return ans;
}

SEXP is_lower(SEXP v) { // wrapper function for islower from ctype.h
SEXP ans;
int i, len = Rf_length(v);
wchar_t *w = Calloc(6+1, wchar_t);
unsigned int t;
PROTECT(ans = Rf_allocVector(LGLSXP, len));
for (i = 0; i < len; i++){
mbstowcs(w, translateChar(STRING_ELT(v, i)), 6+1);
t=(unsigned int)*w;
LOGICAL(ans)[i] = islower(t);
}
UNPROTECT(1);
return ans;
}

SEXP is_print(SEXP v) { // wrapper function for isprint from ctype.h
SEXP ans;
int i, len = Rf_length(v);
wchar_t *w = Calloc(6+1, wchar_t);
unsigned int t;
PROTECT(ans = Rf_allocVector(LGLSXP, len));
for (i = 0; i < len; i++){
mbstowcs(w, translateChar(STRING_ELT(v, i)), 6+1);
t=(unsigned int)*w;
LOGICAL(ans)[i] = isprint(t);
}
UNPROTECT(1);
return ans;
}

SEXP is_punct(SEXP v) { // wrapper function for ispunct from ctype.h
SEXP ans;
int i, len = Rf_length(v);
wchar_t *w = Calloc(6+1, wchar_t);
unsigned int t;
PROTECT(ans = Rf_allocVector(LGLSXP, len));
for (i = 0; i < len; i++){
mbstowcs(w, translateChar(STRING_ELT(v, i)), 6+1);
t=(unsigned int)*w;
LOGICAL(ans)[i] = ispunct(t);
}
UNPROTECT(1);
return ans;
}

SEXP is_space(SEXP v) { // wrapper function for isspace from ctype.h
SEXP ans;
int i, len = Rf_length(v);
wchar_t *w = Calloc(6+1, wchar_t);
unsigned int t;
PROTECT(ans = Rf_allocVector(LGLSXP, len));
for (i = 0; i < len; i++){
mbstowcs(w, translateChar(STRING_ELT(v, i)), 6+1);
t=(unsigned int)*w;
LOGICAL(ans)[i] = isspace(t);
}
UNPROTECT(1);
return ans;
}

SEXP is_upper(SEXP v) { // wrapper function for isupper from ctype.h
SEXP ans;
int i, len = Rf_length(v);
wchar_t *w = Calloc(6+1, wchar_t);
unsigned int t;
PROTECT(ans = Rf_allocVector(LGLSXP, len));
for (i = 0; i < len; i++){
mbstowcs(w, translateChar(STRING_ELT(v, i)), 6+1);
t=(unsigned int)*w;
LOGICAL(ans)[i] = isupper(t);
}
UNPROTECT(1);
return ans;
}

SEXP is_xdigit(SEXP v) { // wrapper function for isxdigit from ctype.h
SEXP ans;
int i, len = Rf_length(v);
wchar_t *w = Calloc(6+1, wchar_t);
unsigned int t;
PROTECT(ans = Rf_allocVector(LGLSXP, len));
for (i = 0; i < len; i++){
mbstowcs(w, translateChar(STRING_ELT(v, i)), 6+1);
t=(unsigned int)*w;
LOGICAL(ans)[i] = isxdigit(t);
}
UNPROTECT(1);
return ans;
}

/*
isalnum,   isalpha,  isascii,  isblank,  iscntrl,  isdigit,  isgraph,
islower, isprint, ispunct, isspace,  isupper,  isxdigit  -  character
classification routines

isalnum() checks for an alphanumeric character

isalpha() checks for an alphabetic character

isascii() checks whether c is a 7-bit unsigned char value that fits into the ASCII character set.

isblank() checks for a blank character; that is, a space or a tab.

iscntrl() checks for a control character.

isdigit() checks for a digit (0 through 9).

isgraph() checks for any printable character except space.

islower() checks for a lower-case character.

isprint() checks for any printable character including space.

ispunct() checks for any printable character which is not a space or an alphanumeric character.

isspace() checks for white-space characters.

isupper() checks for an uppercase letter.

isxdigit() checks for a hexadecimal digits

*/

