.TH Dashpack_params 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Dashpack_params - This document briefly describes all Dashpack parameters.
.SH DESCRIPTION
Dashpack has twenty internal parameters that affect what it does.  The
current value of a parameter may be retrieved by calling one of the routines
DPGETC, DPGETI or DPGETR.  A parameter may be given a new value by calling
one of the routines DPSETC, DPSETI, or DPSETR.
.sp
The Dashpack parameter descriptions appear below in alphabetical
order. Each description begins with a line giving the parameter name 
and the intrinsic FORTRAN type of the parameter.
.IP "\&'CRB' - Character"
.sp
Character Representing a Break: the single character that is to be used in a
label string in a character dash pattern to represent a break point in the
label string. (Each piece of the label string is written using a separate
call to PLCHHQ, PLCHMQ, or PLCHLQ; the effect is to make the label "bend"
with the curve that it labels.)
.sp
The default value of 'CRB' is a vertical bar.
.IP "\&'CRG' - Character"
.sp
Character Representing a Gap: the single character that is to be used in a
character dash pattern to represent a gap (of width 'WOG') in the curve.
.sp
The default value of 'CRG' is an underscore.
.IP "\&'CRS' - Character"
.sp
Character Representing a Solid: the single character that is to be used in a
character dash pattern to represent a solid section (of width 'WOS') of the
curve.
.sp
The default value of 'CRS' is a dollar sign.
.IP "\&'DPL' - Integer"
.sp
Dash Pattern Length: the length of the character-string value of the
internal parameter 'DPT', as set by
DPSETC or by a subsequent call to DPSETI or DPSETR.  Calling DPSETC with
first argument 'DPT' sets both 'DPT', which is the character dash pattern,
and 'DPL', which is its length; 'DPL' can later be reset (normally to a
smaller value) by a subsequent call to DPSETI or DPSETR with first
argument 'DPL', thus requesting the use of a different number of characters
of the specified dash pattern.
.sp
The value of 'DPL' must be between 1 and 256.
.sp
The default value is 16.
.IP "\&'DPS' - Integer"
.sp
Dash Pattern Selector: selects the dash pattern to be used.
.sp
A negative value
of 'DPS' says that the integer dash pattern (as specified by a call to DPSETI
or DPSETR with first argument 'DPT') is to be used.  If the absolute value
is "n", then the low-order "n" bits of the integer are used as the dash
pattern.  No more than 32 bits of an integer dash pattern may be used; using
more than 24 may be a problem on some systems.
.sp
A positive value of 'DPS'
says that the current character dash pattern (as specified by a call to
DPSETC with first argument 'DPT') is to be used; 'DPS' = 0 says to use the
first 'DPL' characters of the dash pattern, while 'DPS' > 0 says to use the
first 'DPS' characters of the dash pattern.  No more than 256 characters of a
character dash pattern may be used.
.sp
The value of 'DPS' must be between -32 and +256.
.sp
The default value is 0.
.IP "\&'DPT' - Character or Integer"
.sp
Dash PaTtern: one of the current dash patterns. (In a call to DPSETC or
DPGETC, the name 'DPT' refers to the current character-string dash pattern,
but in a call to DPSETI, DPGETI, DPSETR, or DPGETR, it refers to the current
integer dash pattern.  Which of these is actually in use at a given time is
specified by the value of the internal parameter 'DPS'.)
.sp
In an integer dash pattern, 1 bits represent solids and 0 bits represent
gaps.  No more than 32 bits of an integer dash pattern may be used; using
more than 24 bits may be a problem on some systems.
.sp
A character dash pattern is a string of 256 or
fewer characters; in such a string, occurrences of the characters specified
by the values of 'CRG' and 'CRS' specify gaps and solids, respectively. Other
characters in the dash pattern form label strings to be written along a curve.
Within each complete label string of a character dash pattern, the character
specified by the value of 'CRB' may be used to specify "break points" at
which the label string may be broken into smaller substrings. Alternatively,
the single-character flag 'SCF' may be set non-zero to say that the label
string may be broken into single-character substrings. Since each such
substring is written by a separate call to PLCHHQ, PLCHMQ, or PLCHLQ, the
effect of breaking up a label string is to make the label "bend" with the
curve.
.sp
The default character dash pattern consists of sixteen dollar signs, and the
default integer dash pattern is "65535", which has sixteen low-order 1s.
.IP "\&'EPS' - Real"
.sp
EPSilon: says how far apart two points have to be (in X or Y, in the fractional
coordinate system) in order to be considered separate points by the smoothing
routine DPSMTH.
.sp
The value of 'EPS' must be greater than or equal to zero.
.sp
The default value is .000001.
.IP "\&'LS1' - Real"
.sp
Label Spacing parameter 1: specifies how much extra gap space to leave at the
beginning and end of a label. Giving 'LS1' a non-zero value helps to ensure
that there will be a sufficiently large gap to (for example) prevent a leading
minus sign from appearing to be part of the line.
.sp
The value of 'LS1' is given as a multiple of the value of the parameter 'WOC'
(the width of a character); it must not be less than zero nor greater than
10.
.sp
The default value is .5.
.IP "\&'LS2' - Real"
.sp
Label Spacing parameter 2: specifies how much extra gap space to leave for
each piece of a broken label. When break characters are used or the
single-character flag 'SCF' is turned on, 'LS2' determines the spacing of the
characters along the line.
.sp
The value of 'LS2' is given as a multiple of the value of the parameter 'WOC'
(the width of a character); it must not be less than zero nor greater than
10.
.sp
The default value is 0.
.IP "\&'LTL' - Integer"
.sp
Line Through Label: a flag that says whether or not each label substring
specified by a character dash pattern is to be written in a gap ('LTL' = 0)
or just on top of the curve ('LTL' = 1). The latter is most effective if the
line is one color and the labels are another color.
.sp
The value of 'LTL' must be either 0 or 1.
.sp
The default value is 0.
.IP "\&'MFS' - Real"
.sp
Multiplier for First Solid: a real multiplier for the length of an initial
solid portion of a curve drawn by DASHPACK. The object of using this is to
make it possible to slightly offset labels on curves that are very nearly
parallel to one another (as can happen, for example, when drawing contour
lines).
.sp
The value of 'MFS' must be greater than or equal to zero.
.sp
The default value is 1.
.IP "\&'PCF' - Integer"
.sp
PlotChar Flag: says which PLOTCHAR routine is to be called to draw character
strings. The value 0 says to call PLCHHQ, the value 1 says to call PLCHMQ,
and the value 2 says to call PLCHLQ.
.sp
The value of 'PCF" must be either 0, 1, or 2.
.sp
The default value is 0.
.IP "\&'SAF' - Real"
.sp
String Angle Flag: says how labels are to be oriented.
.sp
If 'SAF' = 0, labels are written along a curve in the direction in which the
curve is being drawn.
.sp
If 'SAF' is negative, labels are written in the direction ABS('SAF') degrees,
but this is done only if 'LTL' is non-zero; otherwise, the code behaves as
if 'SAF' were zero: labels are written in the direction of the curve.
.sp
If 'SAF' is greater than zero, labels are written along the curve, but the
angle is adjusted by adding multiples of 180 degrees so that the resulting
angle lies in the range from 'SAF'-90 to 'SAF'+90 degrees.
.sp
If a label string is broken into substrings (either because there are "break"
characters in it or because 'SCF' is non-zero), a negative value of 'SAF' will
be treated as a zero value; a value greater than zero may cause the entire
label to be written in the opposite direction along the curve if that ensures
that more characters of the label will be written at angles between 'SAF'-90
and 'SAF'+90 degrees.
.sp
Generally, when 'SAF' is non-zero, it is either -360 or +360, which has the
effect of making the labels as nearly upright as possible on the frame.
.sp
The value of 'SAF" must be in the range from -360 to +360.
.sp
The default value is 360.
.IP "\&'SBF' - Integer"
.sp
String Buffering Flag: a flag that says whether output of labels is to be
buffered or not.
.sp
When 'SBF' is non-zero, buffering is done.  This ensures
that, if the end of a curve occurs anywhere within a particular label, no
part of the label is written; instead, that part of the curve is drawn using
only the gap and solid elements of the dash pattern.  Buffering is also
important when 'SCF' is non-zero or there are "break" characters in a
label string and 'SAF' is greater than zero; in this case, the buffering
makes it possible to reorient the label as needed to make most of it
upright.
.sp
When 'SBF' is zero, buffering is turned off. There may be gaps
at the ends of curves.  If, in addition, 'SCF' is non-zero or there
are "break" characters in label strings, there may be partial labels at
the ends of curves.
.sp
The value of 'SBF' must be either 0 or 1.
.sp
The default value is 1.
.IP "\&'SCF' - Integer"
.sp
Single Character Flag: When 'SCF' is non-zero, it says that the label-string
portions of character dash patterns are to be broken into single-character
pieces, each of which is to be written by a separate call to PLCHHQ, PLCHMQ,
or PLCHLQ.
.sp
If 'SCF' = 0, label strings are broken into pieces only at the
break points indicated by the use of "break" characters in the strings.
.sp
It is not appropriate to use 'SCF' non-zero when PLCHHQ is being used and a
label string in the dash pattern contains function codes that are meaningful
to PLCHHQ; in that case, one should leave 'SCF' = 0 and use the "break"
character 'CRB' in the label string to tell DASHPACK where it can be broken.
.sp
The value of 'SCF" must be either 0 or 1.
.sp
The default value is 0.
.IP "\&'SSL' - Real"
.sp
Smoothed Segment Length: specifies how far apart the points used to draw a
smoothed curve should be.
.sp
The value of 'SSL' is given in the fractional coordinate system and must be
in the range from .000001 to 1.
.sp
The default value is .01.
.IP "\&'TCS' - Real"
.sp
Tension on Cubic Splines: a value which, if negative, turns smoothing off,
and which, if non-negative, turns smoothing on and, if greater than zero,
specifies the desired tension to be used on the cubic splines used to do
the smoothing.
.sp
Note that only the routines DPCURV, DPFRST, DPVECT, DPLAST,
and DPSMTH are affected by the value of 'TCS'; the routines DPLINE and DPDRAW
never smooth. The routine DPSMTH always smooths: if 'TCS' is less than or
equal to zero, simple cubic splines are used, and, if 'TCS' is greater than
zero, splines under tension are used, in which case 'TCS' specifies the
desired tension.
.sp
It's a bad idea to use values of 'TCS' much bigger than
about 15, as this can cause overflows in the smoothing routines.
.sp
The default value is -1.
.IP "\&'WOC' - Real"
.sp
Width Of Character: the character width to be used in writing labels.
.sp
The value of 'WOC' is given in the fractional coordinate system and must be
in the range from .000001 to 1.
.sp
The default value is .01.
.IP "\&'WOG' - Real"
.sp
Width of Gap: the width of each gap in the dashed line.
.sp
The value of 'WOG' is given in the fractional coordinate system and must be
in the range from .000001 to 1.
.sp
The default value is .005.
.IP "\&'WOS' - Real"
.sp
Width of Solid: the width of each solid in the dashed line.
.sp
The value of 'WOS' is given in the fractional coordinate system and must be
in the range from .000001 to 1.
.sp
The default value is .005.
.SH SEE ALSO
Online:
dashpack,
dpcurv,
dpdraw,
dpfrst,
dpgetc,
dpgeti,
dpgetr,
dplast,
dpline,
dpsetc,
dpseti,
dpsetr,
dpsmth,
dpvect,
.sp
Hardcopy:
None.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
