.\"
.\"	$Id: colconv.m,v 1.1.1.1 1992-04-17 22:30:34 ncargd Exp $
.\"
.TH COLCONV 3NCARG "NOVEMBER 1989" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE3
.dsNA " COLCONV - Converts color values from one color space to another
.dsS1 " CALL HLSRGB ( H, L, S, R, G, B ) Convert from HLS to RGB
.dsS2 " CALL HSVRGB ( H, S, V, R, G, B ) Convert from HSV to RGB
.dsS3 " CALL RGBHLS ( R, G, B, H, L, S ) Convert from RGB to HLS
.dsS4 " CALL RGBHSV ( R, G, B, H, S, V ) Convert from RGB to HSV
.dsS5 " CALL RGBYIQ ( R, G, B, Y, I, Q ) Convert from RGB to YIQ
.dsS6 " CALL YIQRGB ( Y, I, Q, R, G, B ) Convert from YIQ to RGB
.nrsN 6
.ds f. man/colconv.l
./" revised 10/3/89: utility Intro top-page formatting macros
./" revised 9/20/89 to add index macro definition
./" revised 9/18/89 w/new headers, footers, L1 heads
./" revised 6/13/89 to include tab macro .tA
./" revised 6/27/89 to include correct headers & footers
./" revised 6/29/89 to include macro for right-adjusted headings (cg)
.\" Index Entry Macro Definition
.de *X
.if \\nJ .tm .IE\tENTRY\t\\$1\t\\$2\t\\$3\t\\$4\t\\$5\t\\$6\t\\n%
..
.tr ~
.pn 18
.PH ""
.PF ""
.EF "@\s122-%  \s9Color\fR@@NCAR Graphics Guide to New Utilities@"
.OF "@\s9Version 3.00, October 1989@@\s9Color~~\s122-%\s9@"
.EH "@\s9COLCONV@@@"
./"  Headers & footers: replace chapter name, utility, and digit before the %
./"  Put odd header command after first page of text so it won't print on pg1;
./"  Odd header:  .OH "@@@\s9TITLE@"
.ll 6.5i
.\"   heading macros for Graphics, Version 3.00, written by Nancy Dawson, 5/89
.EQ         \"  Makes all equations be 12 point, Roman font
.S 99
.EN
.de tA
.sp
.ta 2i-1n   \"  tab setup for Type:  Default: line
Type:  \\$1\tDefault:  \\$2
.sp
..
.de >>       \"  display for indented lines
.sp
.nf
..           \"  subsequent lines are indented
.de <<       \"  end of display for indented lines
.fi
.sp
..           \"  subsequent lines are not indented
.de sF       \"  begin FORTRAN (constant spacing font)
.ps 10
.vs 12
.nf
.ft L
..
.de eF       \"  end FORTRAN (resume previous font & prev. size & font)
.ft
.fi
.ps
.vs
..
\&
.ft B
.ps 16    
.*i "COLCONV"
.ft R
.sp 4    
.ps 12
.L1 "COLCONV Introduction"
A color model is a specification of a three-dimensional color
coordinate system within which each displayable color is represented
by a point in that coordinate system.  There are three common color
models that are hardware oriented:  the RGB (Red, Green, Blue) model
used by most color graphics display devices; the CMY (Cyan, Magenta,
Yellow) model used by most color printing devices; and the YIQ model used
by broadcast television.  Unfortunately, none of these
hardware-oriented models is very easy to use by the graphics programmer,
since the models do not closely relate to our intuitive notions of color.
For example, suppose someone asked you to specify RGB values for a
pale violet.  Unless you were extremely used to dealing with RGB
values, you would most likely have to do some experimentation to get
what you wanted.
.sp
Many more intuitive color models have been invented,
and two of the most common are the HSV (Hue, Saturation, Value) model,
and the HLS (Hue, Lightness, Saturation) model.  Example 1 shows how
an HSV color wheel might be created.  A color wheel can help you
select the hue, saturation, and value coordinates to use when trying
to create a particular color.
.sp
The perception and use of color in computer graphics is a complex
combination of physics, human physiology, and aesthetics.  The
literature on the subject is extensive.  The interested reader is
directed to the references mentioned at the end of the COLCONV
documentation for further reading.
.sp
The \fBCOLCONV\fR package
contains six user entries for converting among the color models HSV,
HLS, and YIQ.  All of these color models are discussed in the
\fIFundamentals of Interactive Computer Graphics\fR, by
James Foley and Andries van Dam.
The COLCONV subroutines have the following functions:
.sp
\fBHLSRGB:\fR
HLSRGB converts a color specification given as
the Hue, Lightness, and Saturation (HLS) values to
Red, Green, and Blue (RGB) intensity values in the RGB color
space.  
.sp
\fBHSVRGB:\fR
HSVRGB converts a color specification given in
the Hue, Saturation, and Value (HSV) color space to
color values in the RGB color
space.
.sp
\fBRGBHLS:\fR
RGBHLS converts a color specification given 
in the RGB color space to color values in the HLS color space.
\fBRGBHSV:\fR
RGBHSV converts a color specification given in the
RGB color space to color values in the HSV 
color space.
.sp
\fBRGBYIQ:\fR
RGBYIQ converts a color specification given in the RGB
color space to a color specification 
in the YIQ color space.
.sp
\fBYIQRGB:\fR
YIQRGB converts a color specification given in
the YIQ coordinate system to the equivalent color
specification in the RGB coordinate system.
.OH "@@@\s9COLCONV@"
.L1 "COLCONV Calls"
Note that throughout this chapter brackets indicate that the
value is inside the range, and parentheses indicate that the
value is the end point of the range but is not within the
range.  For example, [0., 360.) indicates that 0. is the
first value inside the range, and 360. is not included in
the range but is the end point of the range.
.sp
.ne 6
.in 0
.ft B           
.S 14
HLSRGB
.S 12
.L2 Purpose
HLSRBG converts color values given in the 
Hue, Lightness, Saturation (HLS) color space to color values
in the Red, Green, and Blue (RGB) color space.
The input value H is a floating-point number in the
range [0.,360.); a value of H=0. in the input space
will result in a full blue (red and green values of 0.)
in the RGB space.  The input value L is in the range
[0.,100.]; lightness is used to signify the amount of light \(em
L=0. will result in R=G=B=0., and L=1. will result in
R=G=B=1. regardless of H and S.  The input value S is
in the range [0.,100.];  saturation is a measure of the
mixture of white light with a pure, fully saturated hue \(em
S=0. will result in R=G=B=L.  The output values
R, G, and B are red, green, and blue intensities 
in the range 0. to 1.
.sp
The idea of the algorithm can be understood if you were to
plot the red output values vs. hue for S=100. and L=50.  The red
values so plotted form a piecewise linear curve.  The blue
and green values plotted in the same way form the same 
piecewise linear curve, except shifted.  Decreasing the
saturation pushes the curves toward the lightness value,
and varying the lightness introduces an amplitude scale
factor and vertical offset.  See Example 2.
.sp
.in -.5i
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \w\fB\s11CALL HLSRGB\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB   Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w   (H,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    L,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    S,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    R,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    G,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    B)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 176 file man/colconv.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB   Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL HLSRGB\fR\s12\h'|\n(41u'   (H,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    L,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    S,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    R,\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    G,\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    B)\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-11
.in +.5i
.VL 1.2i
.LI "\fBH\fR"
A real variable in the range
[0.,360.) that represents the hue of the input color
in HLS color space.  H=0. corresponds to blue.
.LI "\fBL\fR"
A real variable in the range
[0.,100.] that represents the lightness value of the
input color in HLS color space.  Lightness is a measure
of the quantity of light \(em a lightness of 0. is black,
and a lightness of 100. gives white.  The pure hues occur
at lightness value 50.
.LI "\fBS\fR"
A real variable in the range
[0.,100.] that represents the saturation value of the
input color in HLS color space.  Saturation is a measure 
of how much white light is mixed with the color.  Colors
having a saturation value of 0. represent grays
with a gray intensity value equal to the lightness L.
Colors with a saturation value of 100. are
fully saturated colors.  The hue is undefined
when S=0.  The fully saturated pure hues occur when S=100. 
and L=50.
.LI "\fBR\fR"
A real variable in the range
[0.,1.] that represents the red intensity component
of the output color in RGB color space.
.LI "\fBG\fR"
A real variable in the range
[0.,1.] that represents the green intensity component
of the output color in RGB color space.
.LI "\fBB\fR"
A real variable in the range
[0.,1.] that represents the blue intensity component
of the output color in RGB color space.
.LE
.ne 6
.in 0
.ft B           
.S 14
HSVRGB
.S 12
.L2 "Purpose"
HSVRGB converts color values given in the 
Hue, Saturation, and Value (HSV) color space to color values
in the Red, Green, and Blue (RGB) color space.
The input value H is a floating-point number in the
range [0.,360.); a value of H=0. in the input space
will result in a full red (green and blue values of 0.)
in the RGB space.  The input value V is in the range
[0.,1.];  the input value S is
in the range [0.,1.];  saturation is a measure of the
mixture of white light with a pure, fully saturated hue \(em
S=0. will result in R=G=B=V.  The output values
R, G, and B are red, green, and blue intensities 
in the range [0.,1.].
.sp
The idea of the algorithm can be understood if you were to
plot the red output values vs. hue with S=1. and V=1.  The red
values so plotted form a piecewise linear curve.  The blue
and green values plotted in the same way form the same 
piecewise linear curve, except shifted.  Decreasing the
saturation pushes the curves toward the input value V,
and varying the value, V, introduces an amplitude scale
factor.  Drawing plots of the red, green, and blue output
values vs. hue for selected values of S and V should
provide an intuitive feel for the algorithm.  See Example 3.
.sp
.in -.5i
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \w\fB\s11CALL HSVRGB\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB   Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w   (H,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    S,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    V,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    R,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    G,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    B)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 258 file man/colconv.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB   Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL HSVRGB\fR\s12\h'|\n(41u'   (H,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    S,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    V,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    R,\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    G,\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    B)\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-11
.in +.5i
.VL 1.2i
.LI "\fBH\fR"
A real variable in the range
[0.,360.) that represents the hue of the input color
in HSV color space. 
.LI "\fBS\fR"
A real variable in the range
[0.,1.] that represents the saturation value of the
input color in HSV color space.  Saturation is a measure
of how much white light is mixed with the color. Saturation
values of 0. represent grays (with a gray value equal to
the value V).  Saturation values of 1. are fully
saturated colors.  The hue is undefined when S=0.  The
fully saturated pure hues occur when S=1. and V=1.
.LI "\fBV\fR"
A real variable in the range
[0.,1.] that represents the value of the
input color in HSV color space. 
.LI "\fBR\fR"
A real variable in the range
[0.,1.] that represents the red intensity component
of the output color in RGB color space.
.LI "\fBG\fR"
A real variable in the range
[0.,1.] that represents the green intensity component
of the output color in RGB color space.
.LI "\fBB\fR"
A real variable in the range
[0.,1.] that represents the blue intensity component
of the output color in RGB color space.
.LE
.ne 6
.in 0
.ft B           
.S 14
RGBHLS
.S 12
.L2 "Purpose"
RGBHLS converts color values given in the Red, Green, and Blue (RGB)
color space to color values in the Hue, Lightness, and Saturation
(HLS) color space.  The input Red, Green, and Blue values are
intensities in the range [0.,1.]; the output value H is returned as a
floating-point number in the range [0.,360.); full blue in the input
space (red and green values of 0.)  corresponds to a hue of 0. in the
output space; the output value L is returned as a floating-point
number in the range [0.,100.]; the output value S is returned as a
floating-point number in the range [0.,100.].  See the code
for Example 4.
.sp
The hues returned can be made to put 0 degrees at red instead of blue
by changing the offsets to the computation of H in the code.  For
compatibility with the Tektronix color model and compatibility with
the NCAR Fortran CGM translator GRAPHCAPs, we chose
hue 0. representing blue, and S and L values ranging from 0. to 100.
instead of 0. to 1. 
.sp
.in -.5i
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \w\fB\s11CALL RGBHLS\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB   Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w   (R,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    G,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    B,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    H,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    L,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    S)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 328 file man/colconv.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB   Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL RGBHLS\fR\s12\h'|\n(41u'   (R,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    G,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    B,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    H,\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    L,\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    S)\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-11
.in +.5i
.VL 1.2i
.LI "\fBR\fR"
A real variable in the range
[0.,1.] that represents the red intensity component
of the input point in RGB color space.
.LI "\fBG\fR"
A real variable in the range
[0.,1.] that represents the green intensity component
of the input point in RGB color space.
.LI "\fBB\fR"
A real variable in the range
[0.,1.] that represents the blue intensity component
of the input point in RGB color space.
.LI "\fBH\fR"
A real variable in the range
[0.,360.) that represents the hue of the input point
in HLS color space.  A value of (0.,0.,B) in the input
space will result in a hue of 0. in the output space.
.LI "\fBL\fR"
A real variable in the range
[0.,100.] that represents the lightness value of the
input point in HLS color space.  Lightness is a measure
of the quantity of light \(em a lightness of 0. is black,
and a lightness of 100. gives white.  The pure hues occur
at lightness value 50.  The lightness should be thought
of as a percentage.
.br
.ne 8
.LI "\fBS\fR"
A real variable in the range
[0.,100.] that represents the saturation value of the
input point in HLS color space.  Saturation is a measure
of how much white light is mixed with the color.  Saturation
values of 0. represent grays (with a gray value equal to
the lightness value L).  Saturation values of 100. are fully
saturated colors.  The hue is undefined when S=0.  The
fully saturated pure hues occur when S=100. and L=50.
The saturation value should be thought of as a percentage.
.LE
.ne 6
.in 0
.ft B           
.S 14
RGBHSV
.S 12
.L2 "Purpose"
RGBHSV converts color values given in the Red, Green, and Blue (RGB)
color space to color values in the Hue, Saturation, and Value (HSV)
color space.  The input Red, Green, and Blue values are intensities in
the range [0.,1.]; the output value H is returned as a floating-point
number in the range [0.,360.).  Full red (green and blue values of 0.)
in the input space corresponds to a hue of 0. in the output space; the
output value V is returned as a floating-point number in the range
[0.,1.]; the output value S is returned as a floating-point number in
the range [0.,1.].  See the code for Example 5.
.sp
The hues returned can be made to put 0 degrees at blue instead
of red by changing the offsets to the computation of H in the
code.  
.sp
.in -.5i
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \w\fB\s11CALL RGBHSV\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB   Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w   (R,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    G,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    B,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    H,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    S,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    V)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 402 file man/colconv.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB   Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL RGBHSV\fR\s12\h'|\n(41u'   (R,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    G,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    B,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    H,\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    S,\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    V)\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-11
.in +.5i
.VL 1.2i
.LI "\fBR\fR"
A real variable in the range
[0.,1.] that represents the red intensity component
of the input point in RGB color space.
.LI "\fBG\fR"
A real variable in the range
[0.,1.] that represents the green intensity component
of the input point in RGB color space.
.LI "\fBB\fR"
A real variable in the range
[0.,1.] that represents the blue intensity component
of the input point in RGB color space.
.LI "\fBH\fR"
A real variable in the range
[0.,360.) that represents the hue of the input point
in HSV color space.  A value of (R,0.,0.) in the input
space will result in a hue of 0. in the output space.
.LI "\fBS\fR"
A real variable in the range
[0.,1.] that represents the saturation value of the
input point in HSV color space.  Saturation is a measure
of how much white light is mixed with the color. Saturation
values of 0. represent grays (with a gray value equal to
V).  Saturation values of 1. are fully saturated colors.  
The hue is technically undefined when S=0.; the code leaves
H at its previous value when S=0. (0. initially).  The
fully saturated pure hues occur when S=1. and V=1.
.LI "\fBV\fR"
A real variable in the range
[0.,1.] that represents the value in HSV space.  
.LE
.ne 6
.in 0
.ft B           
.S 14
RGBYIQ
.S 12
.L2 "Purpose"
RGBYIQ converts color values given in the 
Red, Green, and Blue (RGB) color space to color values
in the YIQ color space.  The input Red, Green, 
and Blue values are intensities in the range [0.,1.].  The Y in the
YIQ model is known as \fIluminance\fR and contains the brightness content
of the RGB triple.  In fact a color image can be converted to black
and white by picking off the Y value after conversion to YIQ.  This is
exactly what a black-and-white television displays from a color image.
See the code for Example 6.
.sp
.in -.5i
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \w\fB\s11CALL RGBYIQ\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB   Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w   (R,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    G,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    B,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    Y,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    I,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    Q)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 465 file man/colconv.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB   Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL RGBYIQ\fR\s12\h'|\n(41u'   (R,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    G,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    B,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    Y,\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    I,\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    Q)\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-11
.in +.5i
.VL 1.2i
.LI "\fBR\fR"
A real variable in the range
[0.,1.] that represents the red intensity component
of the input point in RGB color space.
.LI "\fBG\fR"
A real variable in the range
[0.,1.] that represents the green intensity component
of the input point in RGB color space.
.LI "\fBB\fR"
A real variable in the range
[0.,1.] that represents the blue intensity component
of the input point in RGB color space.
.LI "\fBY\fR"
A real variable in the range
[0.,1.].  Y is the color component of a television
signal that is shown on black-and-white televisions;
Y minimizes the effect of two colors appearing different
to the human eye but mapping to similar monochrome 
intensities.
.LI "\fBI\fR"
A real variable in the range
[-.6,.6].  I attains its maximum when the input triple
is (1.,0.,0.); I attains its minimum when the input triple
is (0.,1.,1.).
.LI "\fBQ\fR"
A real variable in the range
[-.52,.52].  Q attains its maximum when the input triple
is (1.,0.,1.); Q attains its minimum when the input triple
is (0.,1.,0.).
.LE
.ne 6
.in 0
.ft B           
.S 14
YIQRGB
.S 12
.L2 "Purpose"
YIQRGB converts color values given in the 
YIQ coordinate system to colors in the Red, Green, and Blue (RGB) coordinate
system.  The input value Y is in the range [0.,1.];
I is in the range [-.6,.6]; Q is in the range [-.52,.52].
The output values R, G, and B are red, green, and blue 
intensities in the range [0.,1.].  See the code for Example 7.
.sp
.in -.5i
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \w\fB\s11CALL YIQRGB\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB   Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w   (Y,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    I,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    Q,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    R,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    G,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w    B)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 524 file man/colconv.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB   Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL YIQRGB\fR\s12\h'|\n(41u'   (Y,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    I,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    Q,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    R,\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    G,\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'    B)\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-11
.in +.5i
.VL 1.2i
.LI "\fBY\fR"
A real variable in the range
[0.,1.].  Y is the color component of a television
signal that is shown on black-and-white televisions;
Y minimizes the effect of two colors appearing different
to the human eye but mapping to similar monochrome 
intensities.
.LI "\fBI\fR"
A real variable in the range
[-.6,.6].  
.LI "\fBQ\fR"
A real variable in the range
[-.52,.52].  
.LI "\fBR\fR"
A real variable in the range
[0.,1.] that represents the red intensity component
of the output color in RGB color space.
.LI "\fBG\fR"
A real variable in the range
[0.,1.] that represents the green intensity component
of the output color in RGB color space.
.LI "\fBB\fR"
A real variable in the range
[0.,1.] that represents the blue intensity component
of the output color in RGB color space.
.LE
./".bp
./".L1 "COLCONV Examples \(em Output"
./".sp -2
./".sp 20
./"\s18\fBReplace this page with a color plate\s12\fR
./".bp
./".sp 20
./"\s18\fBReplace this page with a color plate\s12\fR
./".bp
./".SK l       
.in 0
.L1 "COLCONV Examples \(em Code"
.L2 "Example 1"
.in -.5i
.sF
      PROGRAM CWHEEL
C
C Create color wheels in HSV space.  Each wheel is produced with
C a different value of V (the value parameter in HSV space).  Each
C wheel is composed of 16 wedges.  The value for the hue remains
C the same within each wedge, and the value for the saturation
C varies linearly from the center to the outer rim within each wedge.
C The hues vary from 0. to 360. around the color wheel starting
C at pure red, and returning to pure red.
C
C This program requires PLOTCHAR for drawing its characters.
C
C  Define the number of values, the number of hues, the number 
C  of saturations, and the number of points bounding a color box.
C
      PARAMETER(NW=3, NCOL=16, NSAT=4, NPTS = 4)
      DIMENSION X(NPTS),Y(NPTS)
      DIMENSION IASF(13)
      DIMENSION VAL(NW),ST(NSAT)
      CHARACTER*80 TITLE
C
      DATA IASF / 13*1 /
      DATA PI   /  3.14159265/
C
C  Define the values to be used.
C
      DATA VAL  / 0.60, 0.80, 1.00 /
C
C  Y-coordinates for the saturation labels.
C
      DATA ST(1),ST(2),ST(3),ST(4)/-.1, -.375, -.625, -.850/
C
      CALL OPNGKS
      CALL GSCLIP(0)
      CALL GSASF(IASF)
      CALL GSFAIS(1)
C
C  Background and foreground colors.
C
      CALL GSCR(1,0,0.0,0.0,0.0)
      CALL GSCR(1,1,1.0,1.0,1.0)
C
C  Loop on the values.
C
      DO 30 IL = 1,NW
        INDEX = 2
        CALL SET(.1,.9,.1,.9,-1.2,1.2,-1.2,1.2,1)
        RINC = 2 * PI / FLOAT(NCOL)
        HINC = 360.0  / FLOAT(NCOL)
        SINC =  1.00  / FLOAT(NSAT - 1)
C
C  Loop on the hues.
C
        DO 20 IHUE = 0,NCOL - 1
          HUE    = FLOAT(IHUE)     * HINC
          THETA1 = FLOAT(IHUE -.5) * RINC
          THETA2 = FLOAT(IHUE +.5) * RINC
          X(1) = 0.0
          X(4) = 0.0
          Y(1) = 0.0
          Y(4) = 0.0
C
C  Loop on the saturations
C
          DO 10 ISAT = 1,NSAT
            SAT = (FLOAT(ISAT - 1) * SINC)
            CALL HSVRGB(HUE,SAT,VAL(IL),R,G,B)
            CALL GSCR(1,INDEX,R,G,B)
            CALL GSFACI(INDEX)
            RLEN = FLOAT(ISAT) / FLOAT(NSAT)
            X(2) = COS(THETA1) * RLEN
            Y(2) = SIN(THETA1) * RLEN
            X(3) = COS(THETA2) * RLEN
            Y(3) = SIN(THETA2) * RLEN
            CALL GFA(4,X,Y)
            X(1) = X(2)
            X(4) = X(3)
            Y(1) = Y(2)
            Y(4) = Y(3)
            INDEX = INDEX+1
 10       CONTINUE
 20     CONTINUE
C
C  Label the plots.
C
        CALL PCSETI('QU  - QUALITY FLAG',0)
        CALL PCSETI('CD  - SELECT DUPLEX DATA SET',1)
        WRITE(TITLE,700) VAL(IL)
        CALL GSPLCI(1)
        CALL PLCHHQ(0.0,1.25,TITLE(1:12),21.0,0.0,0.0)
        DO 40 L=2,NSAT
          SAT = (FLOAT(L-1)*SINC)
          WRITE(TITLE,710) SAT
          CALL PLCHHQ(0.0,ST(L),TITLE(1:6),15.0,0.0,0.0)
   40   CONTINUE
        CALL PLOTIF(0.,0.,2)
        CALL GSPLCI(1)
        CALL LINE(.98,0.,1.03,0.)
        CALL PLCHHQ(1.08,0.,'Hue=0.',15.0,0.0,-1.0)
        CALL LINE( .700,.700, .750,.740)
        CALL PLCHHQ( .80,.740,'Hue=45.',15.0,0.0,-1.0)
        CALL LINE(-.700,.700,-.750,.740)
        CALL PLCHHQ(-.80,.740,'Hue=135.',15.0,0.0,1.0)
        CALL LINE(-.700,-.700,-.750,-.740)
        CALL PLCHHQ(-.80,-.740,'Hue=225.',15.0,0.0,1.0)
        CALL LINE( .700,-.700, .750,-.740)
        CALL PLCHHQ( .80,-.740,'Hue=315.',15.0,0.0,-1.0)
        CALL FRAME
 30   CONTINUE
      CALL CLSGKS
C
 700  FORMAT('VALUE = \&',F4.2)
 710  FORMAT('S=',F4.2)
C
      END   
.eF
.sp
.ne 6
.in 0
.L2 "Example 2"
.ft R
.in -.5i
Pure hue to red.
.sp
.sF
      SUBROUTINE HLS2
      REAL    R,G,B,EPS
      PARAMETER (EPS=0.00001)
      CALL HLSRGB(120.,50.,100.,R,G,B)
      IF ( (ABS(R-1.) .GT. EPS) .OR. (ABS(G) .GT. EPS) .OR.
      *     (ABS(B) .GT. EPS) ) THEN
      IUN = I1MACH(4)
      WRITE(IUN,10)
      ENDIF
      RETURN
   10 FORMAT(' HLS2 returned incorrect values')
      END
.eF
.sp
.in 0
.L2 "Example 3"
.ft R
.in -.5i
Pure hue in HSV to green in RGB.
.sp
.sF
      SUBROUTINE HSV2
      REAL    R,G,B,EPS
      PARAMETER (EPS=0.00001)
      CALL HSVRGB(120.,1.,1.,R,G,B)
      IF ( (ABS(R-0.) .GT. EPS) .OR. (ABS(G-1.) .GT. EPS) .OR.
      *     (ABS(B-0.) .GT. EPS) ) THEN
      IUN = I1MACH(4)
      WRITE(IUN,10)
      ENDIF
      RETURN
   10 FORMAT(' HSV2 returned incorrect values')
      END
.eF
.sp
.ne 6
.in 0
.L2 "Example 4"
.in -.5i
Pure red at full intensity to HLS.
.sp
.sF
      SUBROUTINE HLS1
      REAL    H,L,S,EPS
      PARAMETER (EPS=0.00001)
      CALL RGBHLS(1.,0.,0.,H,L,S)
      IF ( (ABS(H-120.) .GT. EPS) .OR. (ABS(L-50.) .GT. EPS) .OR.
      *     (ABS(S-100.) .GT. EPS) ) THEN
      IUN = I1MACH(4)
      WRITE(IUN,10)
      ENDIF
      RETURN
   10 FORMAT(' HLS1 returned incorrect values')
      END
.eF
.sp
.in 0
.L2 "Example 5"
.in -.5i
Blue to HSV.
.sp
.sF
      SUBROUTINE THSV1
      REAL    H,S,V,EPS
      PARAMETER (EPS=0.00001)
      CALL RGBHSV(0.,0.,1.,H,S,V)
      IF ( (ABS(H-240.) .GT. EPS) .OR. (ABS(S-1.) .GT. EPS) .OR.
      *     (ABS(V-1.) .GT. EPS) ) THEN
      IUN = I1MACH(4)
      WRITE(IUN,10)
      ENDIF
      RETURN
   10 FORMAT(' THSV1 returned incorrect values')
      END
.eF
.sp
.in 0
.L2 "Example 6"
.in -.5i
Full white to YIQ.
.sp
.sF
      SUBROUTINE TYIQ1
      REAL    Y,I,Q,EPS
      PARAMETER (EPS=0.01)
      CALL RGBYIQ(1.,1.,1.,Y,I,Q)
      IF ( (ABS(Y-1.) .GT. EPS) .OR. (ABS(I) .GT. EPS) .OR.
      *     (ABS(Q) .GT. EPS) ) THEN
      IUN = I1MACH(4)
      WRITE(IUN,10)
      ENDIF
      RETURN
   10 FORMAT(' TYIQ1 returned incorrect values')
      END
.eF
.sp
.in 0
.L2 "Example 7"
.in -.5i
Convert green from YIQ to RGB.
.sp
.sF
      SUBROUTINE TYIQ2
      REAL    R,G,B,EPS
      PARAMETER (EPS=0.01)
      CALL YIQRGB(.59,-.28,-.52,R,G,B)
      IF ( (ABS(R-0.) .GT. EPS) .OR. (ABS(G-1.) .GT. EPS) .OR.
      *     (ABS(B-0.) .GT. EPS) ) THEN
      IUN = I1MACH(4)
      WRITE(IUN,10)
      ENDIF
      RETURN
   10 FORMAT(' TYIQ2 returned incorrect values')
      END
.eF
.L1 "COLCONV Errors"
When errors are detected, HLSRGB, HSVRGB, and RGBHLS call
the subroutine SETER in the error-handling package ERPRT77.  (See
Appendix A for more information about the NCAR Graphics Error Handling
Package.)  Possible error messages and their meanings are as follows:
.sp
HLSRGB - L out of range
Lightness is less than 0. or greater than 100.
.sp
HLSRGB - S out of range
Saturation is less than 0. or greater than 100.
.sp
HSVRGB - S out of range
Saturation is less than 0. or greater than 1.
.sp
HSVRGB - V out of range
Value of input color is less than 0. or greater than 1.
.sp
RGBHLS - R out of range
Value of red intensity component is less than 0. or greater than 1.
.sp
RGBHLS - G out of range
Value of green intensity component is less than 0. or greater than 1.
.sp
RGBHLS - B out of range
Value of blue intensity component is less than 0. or greater than 1.
.sp
RGBHSV - R out of range
Value of red intensity component is less than 0. or greater than 1.
.sp
RGBHSV - G out of range
Value of green intensity component is less than 0. or greater than 1.
.sp
RGBHSV - B out of range
Value of blue intensity component is less than 0. or greater than 1.
.sp
These errors are fatal by default; however, you can make them
recoverable by a call to ENTSR in the error-handling package.
.L1 "COLCONV Reference Material"
.L2 "References"
[1] Foley, James D., and van Dam, Andries, \fIFundamentals of Interactive
Computer Graphics,\fR Addison-Wesley Publishing Company, 1982.
.sp
[2] Judd, Deane B., and Wyszecki, Gunter, \fIColor in Business, Science
and Industry,\fR John Wiley & Sons, 1975.
.sp
[3] Pritchard, D. H., "U.S. Color Television Fundamentals \(em
.hw Trans-action
A Review," IEEE Transactions on Consumer Electronics, Vol. CE-23, No. 4,
November 1977, p. 467-478.
.sp
[4] Robertson, Philip K., "Visualizing Color Gamuts:  A User Interface
for the Effective Use of Perceptual Color Spaces in Data Displays,"
IEEE Computer Graphics & Applications, 8(5), September 1988, p. 50-64.
.sp
[5] Rogers, David F., \fIProcedural Elements for Computer Graphics,\fR
McGraw-Hill Book Co., 1985.
.sp
[6] Smith, Alvy R., "Color Gamut Transform Pairs," SIGGRAPH \&'78
Proceedings of the Fifth Annual Conference on Computer
Graphics and Interactive Techniques, published as Computer Graphics, A
Quarterly Report of SIGGRAPH-ACM, 12(3), August 1978, p. 12-19.












