.\"
.\"	$Id: fontcap.m,v 1.1.1.1 1992-04-17 22:30:06 ncargd Exp $
.\"
.\" @(#)f77.1 1.4 86/07/31 SMI; 
.TH FONTCAP 5NCARG "NOVEMBER 1989" NCAR "NCAR GRAPHICS"
.SH NAME
fontcap \- NCAR Graphics font definition file
.SH DESCRIPTION
.LP
.I font1
This fontcap defines the default font for the NCAR CGM translators.
.LP
.I font2
Character fontcap for: HERSHEY:CARTOGRAPHIC_ROMAN
.LP
.I font3
Character fontcap for: HERSHEY:CARTOGRAPHIC_GREEK
.LP
.I font4
Character fontcap for: HERSHEY:SIMPLEX_ROMAN
.LP
.I font5
Character fontcap for: HERSHEY:SIMPLEX_GREEK
.LP
.I font6
Character fontcap for: HERSHEY:SIMPLEX_SCRIPT
.LP
.I font7
Character fontcap for: HERSHEY:COMPLEX_ROMAN
.LP
.I font8
Character fontcap for: HERSHEY:COMPLEX_GREEK
.LP
.I font9
Character fontcap for: HERSHEY:COMPLEX_SCRIPT
.LP
.I font10
Character fontcap for: HERSHEY:COMPLEX_ITALIC
.LP
.I font11
Character fontcap for: HERSHEY:COMPLEX_CYRILLIC
.LP
.I font12
Character fontcap for: HERSHEY:DUPLEX_ROMAN
.LP
.I font13
Character fontcap for: HERSHEY:TRIPLEX_ROMAN
.LP
.I font14
Character fontcap for: HERSHEY:TRIPLEX_ITALIC
.LP
.I font15
Character fontcap for: HERSHEY:GOTHIC_GERMAN
.LP
.I font16
Character fontcap for: HERSHEY:GOTHIC_ENGLISH
.LP
.I font17
Character fontcap for: HERSHEY:GOTHIC_ITALIAN
.LP
.I font18
Character fontcap for: HERSHEY:MATH_SYMBOLS
.LP
.I font19
Character fontcap for: HERSHEY:SYMBOL_SET1
.LP
.I font20
Character fontcap for: HERSHEY:SYMBOL_SET2
.LP
It is possible to set the desired font using the 
.I FONTCAP
environment variable, but this is currently not recommended
because each translator selects its appropriate font.
.SH "SEE ALSO"
.I "NCAR Graphics Version 3.00 - UNIX Release"
.LP
.I "NCAR Graphics User's Guide"
