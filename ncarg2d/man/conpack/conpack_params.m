'\" t
.TH Conpack_params 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Conpack_params - This document briefly describes all Conpack
internal parameters.
.SH DESCRIPTION 
Parameter descriptions follow, in alphabetical order. Each 
description begins with a line giving the three-character 
mnemonic name of the parameter, the phrase for which the 
mnemonic stands, the intrinsic type of the parameter, and 
an indication of whether or not it is an array.
.IP "'AIA' - Area Identifier Above - Integer Array"
Each element of the parameter array 'AIA' is an area 
identifier for the area above the contour level specified 
by the corresponding element of the parameter array 'CLV'. 
The corresponding element of the parameter array 'AIB' is 
an area identifier for the area below that level. If, for a 
particular contour level, both 'AIA' and 'AIB' are zero, 
that level is ignored by the routine CPCLAM; otherwise, 
contour lines at that level are added to the area map and 
the given values of 'AIA' and 'AIB' are used as right and
left area identifiers, respectively.  (Note that contour
lines are traced with greater values to the right and lesser
values to the left.)
.sp
There are three special elements in the parameter array 
\&'AIA', corresponding to 'PAI' values of -1, -2, and -3; the 
first specifies an area identifier for the area outside the 
edge of the grid, the second an area identifier for any 
area filled with special values, and the third an area 
identifier for any area in which the mapping routine CPMPXY 
returns the "out of range" value 'ORV'.
.sp
If contour levels are chosen automatically, rather than 
being supplied by the user, the value supplied for the Ith 
element of 'AIA' is I+1 and the value supplied for the Ith 
element of 'AIB' is I.
.sp
The default value of element "-1" of 'AIA' is 0 and the 
default values of elements "-2" and "-3" are -1's.
.IP "'AIB' - Area Identifier Below - Integer Array"
See the description of 'AIA', above.
.IP "'CAF' - Cell Array Flag - Integer"
The parameter 'CAF' determines the way in which the routine 
CPCICA (which is called to store color indices in a user 
cell array), gets from an area identifier associated with a 
particular cell to a color index for that cell. Let IAID be 
the area identifier that is associated with a given cell. 
Then:
.RS
.IP \(bu
If 'CAF' is negative, the routine CPSCAE is called. The 
default version of CPSCAE behaves as follows: if the value 
of IAID is greater than or equal to zero, it itself is 
stored in the cell array as a color index; otherwise, the 
cell array is unchanged. A user version of CPSCAE may be 
supplied to do something more complicated; since the value 
of 'CAF' is one of the arguments of CPSCAE, its value may 
be used to select one of a number of color-setting schemes 
built into the routine.
.IP \(bu
If 'CAF' is positive, the value "'CAF'+IAID" is computed. 
If that value is positive, it is stored in the cell array 
as a color index; otherwise, the cell array is unchanged. 
In this case, 'CAF' acts as an offset, mapping the area 
identifiers to a unique portion of the color table.
.RE
.IP ""
The default value of 'CAF' is 0.
.IP "'CFA' - Constant-Field Label Angle - Real"
The parameter 'CFA' specifies the angle (in degrees 
counterclockwise from a vector pointing to the right) at 
which a constant-field label is to be written.
.sp
The default value of 'CFA' is 0.
.IP "'CFB' - Constant-Field Label Box Flag - Integer"
If 'CFB' is zero, the constant-field label will not be 
boxed at all. The value 1 implies that the perimeter of the 
box is to be drawn (in the same color as the label) after 
the label is drawn. The value 2 implies that the box is to 
be filled (in the color specified by 'LBC') before the 
label is drawn. The value 3 implies both of the above.
.sp
The default value of 'CFB' is 0.
.IP "'CFC' - Constant-Field Label Color Index - Integer"
If 'CFC' is less than zero, the constant-field label and 
the box, if any, around it, will be drawn in the color 
specified by the current text color index; if 'CFC' is 
greater than or equal to zero, then it specifies the 
desired color index for the label and the box. If a box is 
drawn around the label, it is made the same color as the 
label itself.
.sp
The default value of 'CFC' is -1.
.IP "'CFF' - Constant-Field-Found Flag - Integer"
The parameter 'CFF' may not be set by the user; its 
retrieved value will be non-zero if and only if CPRECT, 
CPSPS1, or CPSPS2 detected a constant field.
.sp
The default value of 'CFF' (prior to any call to CPRECT, 
CPSPS1, or CPSPS2) is zero.
.IP "'CFL' - Constant-Field Label Line Width - Real"
If 'CFL' is less than or equal to zero, line width will not 
be set before drawing a box around the constant-field 
label. If 'CFL' is greater than zero, it specifies the 
desired width, as a multiple of the "normal" line width.
.sp
The default value of 'CFL' is 0.
.IP "'CFP' - Constant-Field Label Positioning Flag - Integer"
The parameter 'CFP' says how the constant-field label is to 
be positioned. There are nine possible values, each of 
which specifies a point of the label box which is to lie on 
the point defined by 'CFX' and 'CFY': the value -4 implies 
the lower left-hand corner of the label box, -3 implies the 
center of the bottom of the box, -2 the lower right-hand 
corner of the box, -1 the center of the left edge of the 
box, 0 the center of the box, +1 the center of the right 
edge of the box, +2 the upper left-hand corner of the box, 
+3 the center of the top edge of the box, and +4 the upper 
right-hand corner of the box. Left, right, bottom, and top 
are defined from the viewpoint of someone viewing the label 
right-side up.
.sp
The default value of 'CFP' is 0, so the constant-field 
label will be centered on the point whose coordinates are 
\&'CFX' and 'CFY'.
.IP "'CFS' - Constant-Field Label Size - Real"
The parameter 'CFS' specifies the nominal size (width) of a 
character in the constant-field label, as a fraction of the 
width of the viewport. This nominal size is multiplied by 
\&'CWM'.
.sp
The default value of 'CFS' is 0.012.
.IP "'CFT' - Constant-Field Label Text String - Character"
The parameter 'CFT' specifies the text of the constant-field
label, which is written when a constant data field is 
detected; it is a character string of at most 40 
characters. The embedded string '$ZDV$' will be replaced by 
the numeric value of the field.
.sp
If 'CFT' is given the value ' ' (a single blank), the 
constant-field label will not be written.
.sp
The default value of 'CFT' is 'CONSTANT FIELD - VALUE IS 
$ZDV$'.
.IP "'CFW' - Constant-Field Label White Space Width - Real"
The parameter 'CFW' specifies the nominal width of white 
space to be left around the constant-field label, as a 
fraction of the width of the viewport. This nominal width 
is multiplied by 'CWM'.
.sp
The default value of 'CFW' is 0.005.
.IP "'CFX' - Constant-Field Label X Coordinate - Real"
The parameter 'CFX' specifies the X coordinate of the 
basepoint of the constant-field label. The given value is 
mapped linearly onto the viewport; 0 refers to the left 
edge of the viewport and 1 to the right edge of the 
viewport. Values less than 0 or greater than 1 may be used.
.sp
The default value of 'CFX' is 0.5, so the constant-field 
label is centered horizontally in the viewport.
.IP "'CFY' - Constant-Field Label Y Coordinate - Real"
The parameter 'CFY' specifies the Y coordinate of the 
basepoint of the constant-field label. The given value is 
mapped linearly onto the viewport; 0 refers to the bottom 
edge of the viewport and 1 to the top edge of the viewport. 
Values less than 0 or greater than 1 may be used.
.sp
The default value of 'CFY' is 0.5, so the constant-field 
label is centered vertically in the viewport.
.IP "'CIS' - Contour Interval Specifier - Real"
See the description of 'CLS', below. When 'CLS' is greater 
than zero, 'CIS' is used. A value of 'CIS' less than or 
equal to zero indicates that Conpack is to choose the 
contour interval (see the descriptions of the parameter 
arrays 'CIT' and 'LIT'). A value of 'CIS' which is greater 
than zero is the actual contour interval to be used; in 
this case, 'LIS' may be given a non-zero value "n" to 
specify that every "nth" contour level should be labeled. 
See also the descriptions of 'CMN' and 'CMX', below.
.sp
The default value of 'CIS' is 0.
.IP "'CIT' - Contour Interval Table - Real Array"
Each non-zero element of the ten-element parameter array 
\&'CIT' is a real number greater than or equal to 1 and less 
than 10; when Conpack picks the contour interval, it 
chooses a number of the form "c x 10**k", where "c" is one of 
the elements of 'CIT' and "k" is an integer. The non-zero 
elements of 'CIT' must be sorted in ascending order and 
appear at the beginning of the array.
.sp
The corresponding elements of the parameter array 'LIT' 
specify which contours are to be labeled. A contour at 
"nc x 10**k" is labeled if "n" is a multiple of "l", where 
"l" is the element of 'LIT' corresponding to the element 
"c" of 'CIT'. For example, if the first element of 'CIT' is 
"1." and the first element of 'LIT' is "5", then Conpack is 
allowed to use contour levels 1., 2., 3., 4., etc., with 
labels at 5., 10., 15., etc. (5 x 1., 5 x 2., 5 x 3., etc.).
.sp
The default contents of 'CIT' and 'LIT' are as follows:
.sp
.TS
tab(/);
c c c c c.
\&'PAI'/'CIT'/'LIT'/LEVELS ALLOWED/LABEL INTERVAL
-----/-----/-----/--------------/--------------
1/1.0/5/1.0 x 10 ** K/EVERY 5TH ONE
2/2.0/5/2.0 x 10 ** K/EVERY 5TH ONE
3/2.5/4/2.5 x 10 ** K/EVERY 4TH ONE
4/4.0/5/4.0 x 10 ** K/EVERY 5TH ONE
5/5.0/5/5.0 x 10 ** K/EVERY 5TH ONE
6/0.0/0/FILLER TO END/FILLER TO END
7/0.0/0/FILLER TO END/FILLER TO END
8/0.0/0/FILLER TO END/FILLER TO END
9/0.0/0/FILLER TO END/FILLER TO END
10/0.0/0/FILLER TO END/FILLER TO END
.TE
.IP "'CIU' - Contour Interval Used - Real"
The parameter 'CIU' is normally intended for retrieval 
only. When the selection of the contour interval is left up 
to Conpack, 'CIU' is given the value chosen. When contour 
levels are completely set by the user, the value of 'CIU' 
may need to be set as well, for two reasons: 1) to make the 
desired value appear in an informational label (in place of 
the embedded string '$CIU$') and 2) so that it may be used 
by the penalty scheme for positioning labels. The setting 
of 'CIU' must be done after setting the contour levels 
(because, as a side effect of the setting of element 1 of 
\&'CLV', 'CIU' is zeroed). If the user supplies contour 
levels, but supplies no value of 'CIU', and the penalty 
scheme is used to position labels, the required contour 
interval is estimated; in certain situations, this can lead 
to problems (if, for example, the same contour level 
appears twice in 'CLV', once to force lines at that level 
to be drawn and once to force that level to be used as the 
boundary for a shaded area).
.sp
The default value of 'CIU' is 0.
.IP "'CLC' - Contour Line Color Index - Integer Array"
Each element of the parameter array 'CLC', if greater than 
or equal to zero, is a color index for contour lines at the 
level specified by the associated element of 'CLV'. A value 
less than zero implies that the lines will be drawn in the 
color specified by the current polyline color index.
.sp
There are three special elements in the parameter array 
\&'CLC', corresponding to 'PAI' values of -1, -2, and -3; the 
first specifies a color index for the edge of the grid, the 
second a color index for the edge of any area filled with 
special values, and the third a color index for the edge of 
any area in which the mapping routine CPMPXY returns the 
"out of range" value.
.sp
The default value of each element of 'CLC' is -1.
.IP "'CLD' - Contour Line Dash Pattern - Character Array"
Each element of the parameter array 'CLD' is a dash pattern 
(as expected by the package Dashline) to be used (when 
\&'DPU' is non-zero) to draw contour lines at the level 
specified by the associated element of the contour level 
array 'CLV'. Elements of 'CLD' may be set using a call to 
CPSETI, with a sixteen-bit integer as the second argument, 
or using a call to CPSETC, with a character string of 32 or 
fewer characters as the second argument. In either case, 
the result will be a character string internally; a sixteen-bit
integer will be converted to a sixteen-character string 
by mapping 0-bits into apostrophes and 1-bits into dollar 
signs.
.sp
There are three special elements in the parameter array 
\&'CLD', corresponding to 'PAI' values of -1, -2, and -3; the 
first specifies a dash pattern for the edge of the grid, 
the second a dash pattern for the edge of any area filled 
with special values, and the third a dash pattern for the 
edge of any area in which the mapping routine CPMPXY 
returns the "out of range" value.
.sp
When Conpack picks the contour levels, the default value 
supplied for each associated dash pattern is the character 
constant '$$$$$$$$$$$$$$$$'. This is also the default value 
for each of the three special elements.
.IP "'CLL' - Contour Line Line Width - Real Array"
Each element of the parameter array 'CLL' specifies the 
line width used to draw contour lines at the level 
specified by the associated element of the contour level 
array 'CLV'. Each is expressed as a multiple of the 
"normal" line width; values less than or equal to zero 
imply that line width should not be set.
.sp
There are three special elements in the parameter array 
\&'CLL', corresponding to 'PAI' values of -1, -2, and -3; the 
first specifies a line width for the edge of the grid, the 
second a line width for the edge of any area filled with 
special values, and the third a line width for the edge of 
any area in which the mapping routine CPMPXY returns the 
"out of range" value.
.sp
When Conpack picks the contour levels, the default value 
supplied for each associated line width is 0. This is also 
the default value for each of the three special elements.
.IP "'CLS' - Contour Level Selection Flag - Integer"
This parameter must be set prior to the call to CPRECT, 
CPSPS1, or CPSPS2 which initiates the process of drawing a 
particular contour plot; it specifies how contour levels 
are to be selected, as follows:
.RS
.IP \(bu
If 'CLS' has the value "0", Conpack will not pick contour 
levels at all; the current values of the parameters 'NCL', 
\&'CLV', and associated arrays will not be changed. They will 
thus retain the values chosen by Conpack during a previous 
call or the values supplied by the user.
.IP \(bu
If 'CLS' has a negative value of the form "-n", Conpack 
will generate "n" contour levels, splitting the range from 
the minimum field value to the maximum field value into 
"n+1" equal intervals.
.IP \(bu
If 'CLS' has a positive value of the form "+n" and 'CIS' is 
less than or equal to zero, Conpack will use values of the 
form "bk", where "b" is a "base" value chosen by Conpack 
and "k" is an integer. The base value "b" will be a "nice" 
value (as defined by the contents of the parameter array 
\&'CIT'), chosen in such a way as to give at least "n" 
contour levels (with the default contents of the array 
\&'CIT', you may get as many as "2n" levels).
.IP \(bu
If 'CLS' has a positive value of the form "+n" and 'CIS' is 
greater than zero and 'CMN' is greater than 'CMX', Conpack 
will use values of the form "'CIS'*k", where "k" is an 
integer.
.IP \(bu
If 'CLS' has a positive value of the form "+n" and 'CIS' is 
greater than zero and 'CMN' is less than or equal to 'CMX', 
Conpack will use values of the form "'CMN'+'CIS'*k" which 
are greater than or equal to 'CMN' and less than or equal 
to 'CMX', where "k" is an integer.
.RE
.IP ""
The default value of 'CLS' is 16.
.IP "'CLU' - Contour Level Use Flags - Integer Array"
Each element of the parameter array 'CLU' indicates how the 
associated contour level, in the parameter array 'CLV', is 
to be used. The value 0 means that no contour line is to be 
drawn at the associated level, the value 1 that the line is 
to be drawn without labels, the value 2 that the labels are 
to be drawn, but not the line, and the value 3 that both 
the line and the labels are to be drawn.
.sp
There are three special elements in the parameter array 
\&'CLU', corresponding to 'PAI' values of -1, -2, and -3; the 
first specifies a flag for the edge of the grid, the second 
a flag for the edge of any area filled with special values, 
and the third a flag for the edge of any area in which the 
mapping routine CPMPXY returns the "out of range" value. In 
each case, if the flag is zero, the associated edge is not 
drawn; otherwise, the associated edge is drawn.
.sp
When Conpack chooses the contour levels, the associated 
elements of 'CLU' are given one of the two values 1 or 3, 
depending on whether the line is to be labeled or not. The 
default values of the special elements are all zeroes.
.IP "'CLV' - Contour Level Values - Real Array"
Each of the first 'NCL' elements of the parameter array 
\&'CLV' is a contour level for which something is to be done 
(the drawing of contour lines, the drawing of contour 
labels, and/or the addition of contour lines to an area 
map).
.sp
Only elements 1 through 'NCL' may be accessed via the 
parameter-setting routines. Thus, code to set the contour 
levels and associated quantities must begin with a call to 
set 'NCL'.
.sp
A side effect of setting the element numbered 'PAI' of 
\&'CLV' is that the associated element number 'PAI' in each 
of the parameter arrays 'AIA', 'AIB', 'CLC', 'CLD', 'CLL', 
\&'CLU', 'LLC', and 'LLT' is also given a default value, 
as follows:
.RS 10 
.IP\&'AIA' 12
\'PAI'+1
.IP\&'AIB' 12
\'PAI'
.IP\&'CLC' 12    
-1
.IP 'CLD' 12    
\'$$$$$$$$$$$$$$$$'
.IP 'CLL' 12     
0
.IP 'CLU' 12     
1
.IP 'LLC' 12    
-1
.IP 'LLT' 12    
\' ' (a single blank)
.RE
.IP ""
Thus, in code to set contour levels and associated 
quantities, each contour level must be set before the 
quantities associated with it.
.sp
A side effect of setting element number 1 of 'CLV' is that 
the parameter 'CIU', which indicates what contour interval 
was used, is zeroed. It is assumed that this will only 
happen when the user is providing all the contour levels, 
in which case the concept of the "contour interval" may or 
not be well defined. See the description of 'CIU' for more 
information.
.IP "'CMN' - Contour Minimum - Real"
When 'CLS' is greater than zero and 'CIS' is also greater 
than zero, if 'CMN' is less than or equal to 'CMX', then 
the contour levels used will be of the form 'CMN', 
\&'CMN'+'CIS', 'CMN'+2*'CIS', ... , 'CMN'+n*'CIS', where "n" 
is the largest integer such that 'CMN'+n*'CIS' is less than 
or equal to 'CMX'. The labeled levels will be those for 
which "n" is a multiple of 'LIS'.
.sp
The default values of 'CMN' and 'CMX' are 1 and 0, 
respectively.
.IP "'CMX' - Contour Maximum - Real"
See the description of 'CMN', above.
.IP "'CTM' - Character Temporary - Character"
The parameter name 'CTM' refers to a temporary character 
buffer in Conpack; the name may be used in the routine 
CPCHCL to get the dash pattern for the current line and in 
the routines CPCHHL, CPCHIL, and CPCHLL to get the text of 
the label being written or to change it.
.sp
The parameter 'CTM' has no meaningful default value.
.IP "'CWM' - Character Width Multiplier - Real"
All character size parameters are multiplied by 'CWM'; this 
makes it easy to scale the sizes up or down simultaneously. 
Parameters affected by this are 'CFS', 'CFW', 'DPS', 'DPV', 
\&'HLS', 'HLW', 'ILS', 'ILW', 'LLS', and 'LLW'.
.sp
The default value of 'CWM' is 1.
.IP "'DPS' - Dash Pattern Size - Real"
The parameter 'DPS' specifies the nominal size (width) of a 
character in a dash pattern, as a fraction of the width of 
the viewport. This nominal size is multiplied by 'CWM'.
.sp
The default value of 'DPS' is 0.010.
.IP "'DPU' - Dash Pattern Use Flag - Integer"
If 'DPU' is less than or equal to zero, it means that no 
dash patterns are to be used and that contour lines are to 
be drawn using calls to CURVE. If 'DPU' is non-zero, it 
means that dash patterns are to be used and that contour 
lines are to be drawn using calls to CURVED. When the label 
positioning flag ABS('LLP') = 1, contour lines are caused 
to be labeled by using a dash pattern formed by 
concatenating "n" repetitions of the appropriate element of 
\&'CLD' (the nominal dash pattern for the line) and the 
appropriate element of 'LLT' (the numeric label for the 
line); in this case, 'DPU' specifies the value of "n".
.sp
The default value of 'DPU' is 3.
.IP "'DPV' - Dash Pattern Vector Length - Real"
The parameter 'DPV' specifies the nominal length of the 
solid vector generated by a dollar sign, or the gap vector 
generated by an apostrophe, in a dash pattern, as a 
fraction of the width of the viewport. This nominal length 
is multiplied by 'CWM'.
.sp
The default value of 'DPV' is .005.
.IP "'GIC' - Group Identifier for Contour Lines - Integer"
The parameter 'GIC' specifies the group identifier for 
contour lines added to an area map by the routine CPCLAM.
.sp
The default value of 'GIC' is 3.
.IP "'GIL' - Group Identifier for Label Boxes - Integer"
The parameter 'GIL' specifies the group identifier for 
label boxes added to an area map by the routine CPLBAM.
.sp
The default value of 'GIL' is 3.
.IP "'GIS' - Group Identifier for Strips - Integer"
The parameter 'GIS' specifies the group identifier for a 
group of edges added to an area map by the routine CPCLAM 
to create a set of vertical strips. This is done only if 
the parameter 'NVS' (which is described later in this 
section) is non-zero.
.sp
The default value of 'GIS' is 4.
.IP "'HCL' - Hachure Length - Real"
The parameter 'HCL' specifies the hachure length, stated as 
a fraction of the width of the viewport. A positive value 
implies the use of hachures on the downslope side of the 
contour. A negative value implies the use of hachures on 
the upslope side of the contour.
.sp
The default value of 'HCL' is .004.
.IP "'HCS' - Hachure Spacing - Real"
The parameter 'HCS' specifies the hachure spacing, stated 
as a fraction of the width of the viewport. This is the 
distance between one hachure and the next along a contour 
line.
.sp
The default value of 'HCS' is .01.
.IP "'HCF' - Hachuring Flag - Integer"
The parameter 'HCF' is the hachuring flag, with one of the 
following values:
.RS
.IP \(bu
If 'HCF' = 0, hachuring is turned off (the default).
.IP \(bu
If 'HCF' = 1, all contours will be hachured.
.IP \(bu
If 'HCF' = 2, closed contours will be hachured only if the 
interior of the contour is "downhill". Open contours will 
be unconditionally hachured.
.IP \(bu
If 'HCF' = 3, closed contours will be hachured only if the 
interior of the contour is "downhill". Open contours will 
be unconditionally unhachured.
.IP \(bu
If 'HCF' = 4, closed contours will be hachured only if the 
interior of the contour is "downhill". Open contours will 
be hachured only if the "interior" of the contour is 
"downhill", where "interior" is defined by computing the 
total change in direction along the contour. If that total 
is positive, the "interior" is to the left; if it is 
negative, the "interior" is to the right.
.RE
.IP ""
Negative values of 'HCF' may also be used. Each has the 
same effect as the corresponding positive value, except 
that the word "downhill" in the definition above is 
replaced by the word "uphill".
.sp
The default value of 'HCF' is 0.
.sp
The user will be expected to increase the value of the 
internal parameter 'RWC' ("Real Workspace for Contours") 
from its default 100 to a value large enough to accommodate 
any contour that can arise from his/her data field; this 
may require increasing the size of the real workspace 
array. (This is so that the hachuring routine is assured of 
seeing all of each contour line. Otherwise, it can't tell 
which way the "inside" and the "outside" of the closed 
contours are.)
.sp
Closed contours that are broken into pieces that appear to 
be open contours (either because portions of the closed 
contour disappear as a result of the mapping implied by non-zero
values of the parameter 'MAP' and the out-of-range 
value 'ORV', or because they run through special-value 
areas, as defined by the internal parameter 'SPV', or 
because the user has forgotten to increase the value of 
\&'RWC', as described in the previous paragraph), will be 
hachured. The rationale for this is that no real confusion 
can arise from hachuring a contour line which shouldn't be, 
only from not hachuring one that should be.
.IP "'HIC' - High Label Color Index - Integer"
The parameter 'HIC' is used in determining the color index 
for high labels. See the description of 'HLC', below.
.IP "'HIT' - High Label Text String - Character"
The parameter 'HIT' specifies the text string to be used in 
labeling a high. See the description of 'HLT', below.
.IP "'HLA' - High/Low Label Angle - Real"
The parameter 'HLA' specifies the angle (in degrees 
counterclockwise from a vector pointing to the right) at 
which high and low labels are to be written.
.sp
The default value of 'HLA' is 0.
.IP "'HLB' - High/Low Label Box Flag - Integer"
If 'HLB' is zero, high and low labels will not be boxed at 
all. The value 1 implies that the perimeter of the box is 
to be drawn (in the same color as the label) after the 
label is drawn. The value 2 implies that the box is to be 
filled (in the color specified by 'LBC') before the label 
is drawn. The value 3 implies both of the above.
.sp
The default value of 'HLB' is 0.
.IP "'HLC' - High/Low Label Color Index - Integer"
The parameter 'HLC' is used in determining the color index 
for high and low labels.
.sp
The color index for high labels is determined in this 
manner: If 'HIC' is greater than or equal to zero, 'HIC' is 
used as the color index. If 'HIC' is less than zero, but 
\&'HLC' is greater than or equal to zero, 'HLC' is used as 
the color index. If both 'HIC' and 'HLC' are less than 
zero, the current text color index is used. If a box is 
drawn around the label, it is made the same color as the 
label itself.
.sp
The color index for low labels is determined similarly: If 
\&'LOC' is greater than or equal to zero, 'LOC' is used as 
the color index. If 'LOC' is less than zero, but 'HLC' is 
greater than or equal to zero, 'HLC' is used as the color 
index. If both 'LOC' and 'HLC' are less than zero, the 
current text color index is used. If a box is drawn around 
the label, it is made the same color as the label itself.
.sp
To set the color index of all high and low labels, simply 
supply the desired value for 'HLC'. To have highs and low 
labels which are colored differently, set 'HIC' and 'LOC'.
.sp
The default values of 'HLC', 'HIC', and 'LOC' are all -1's.
.IP "'HLE' - High/Low Equal-Value Search - Integer"
If 'HLE' has the value zero, only the "normal" search for highs and lows, as
described in the programmer document for CONPACK (in the section "Searching
for Highs and Lows") is performed.
.sp
If 'HLE' has the value one, and if the "normal" search for highs and lows sees
evidence that an additional search should be performed, then the additional
search is performed.  See the section "Extended High/Low Search Algorithm", in
the programmer document for CONPACK.
.sp
If 'HLE' has the value two or greater, and if the "normal" search for highs and
lows sees evidence that an additional search should be performed, then the
additional search is performed, but the candidate regions considered are
limited to those containing no more than 'HLE' grid points.
.sp
Whenever 'HLE' is given a non-zero value, care should be taken to provide
enough space in the integer work array to hold an additional M*N elements,
where M and N are the dimensions of the array being contoured.  This space
will be needed during a call to CPHLLB.
.sp
The default value of 'HLE' is zero.
.IP "'HLL' - High/Low Line Width - Real"
If 'HLL' has a value less than or equal to zero, line width 
will not be set before drawing boxes around high and low 
labels. If 'HLL' has a value greater than zero, it 
specifies the desired width, as a multiple of the "normal" 
line width.
.sp
The default value of 'HLL' is 0.
.IP "'HLO' - High/Low Label Overlap Flag - Integer"
The value of 'HLO' says what is to be done about the 
problem of high and low labels overlapping other objects. 
The value 0 implies that the problem will be ignored; high 
and low labels will not be checked for overlap with 
anything else. Adding 1 to the value of 'HLO' implies the 
omission of high and low labels which overlap the 
informational label. Adding 2 implies the omission of high 
and low labels which overlap other high and low labels 
found before it. Adding 4 implies the omission of high and 
low labels which overlap the edges of the viewport, while 
adding 8 implies that high and low labels which overlap the 
edges of the viewport should be moved inward by just enough 
to take care of the problem. If you add both 4 and 8, the 
effect will be as if you had added 4 alone.
.sp
The default value of 'HLO' is 3 (1 + 2).
.IP "'HLS' - High/Low Label Size - Real"
The parameter 'HLS' specifies the nominal size (width) of a 
character in a high or low label, as a fraction of the 
width of the viewport. This nominal size is multiplied by 
\&'CWM'.
.sp
The default value of 'HLS' is .012.
.IP "'HLT' - High/Low Label Text Strings - Character"
The character strings used to label highs and lows may be 
specified individually, by setting 'HIT' and 'LOT', or 
together, by setting 'HLT'. If 'HLT' is set, and there are 
no apostrophes in the given character string, both 'HIT' 
and 'LOT' will be set equal to it and it will therefore be 
used as the label for both highs and lows. If there are 
apostrophes in the string, what precedes the first one will 
be used as the value of 'HIT' (the label for a high) and 
what follows it will be used as the value of 'LOT' (the 
label for a low).
.sp
Remember that, in FORTRAN, an apostrophe in a string which 
is delimited by apostrophes is represented by two 
apostrophes.
.sp
The substring $ZDV$ may be used to represent the numeric 
value of the high or the low, divided by the current scale 
factor; the substring $ZDVU$ may be used to represent the 
unscaled value.
.sp
Some examples:
.sp
.TS
tab(/);
c c c 
l l l.
FORTRAN STRING/HIGH LABEL/LOW LABEL
------------------------/-------------/-------------
\&'H''L'/H/L
.sp
\&'HI''LO'/HI/LO
.sp
\&'$ZDV$'/1.362/0.764
.sp
\&'H($ZDV$)''L($ZDV$)'/H(1.362)/L(0.764)
.sp
\&'H:B:$ZDV$:E:''L:B:$ZDV$:E:'/H/L
.br
 / 1.362/ 0.764
.TE
.IP ""
Note that, in the final example, the subscripting 
capability of the utility Plotchar is used. The terminating 
function code "E" ensures that the "H" or the "L" will be 
centered on the high or low; to center the whole thing, 
either remove the 'E's or change them to 'N's.
.sp
Neither of the character strings 'HIT' and 'LOT' may 
contain more than 20 characters.
.sp
If 'HIT' is blank, highs will not be labeled. If 'LOT' is 
blank, lows will not be labeled.
.sp
The default value for 'HIT' is 'H:B:$ZDV$:E:' and the 
default value of 'LOT' is 'H:B:$ZDV$:E:', as shown in the 
final example above.
.IP "'HLW' - High/Low Label White Space Width - Real"
The parameter 'HLW' specifies the nominal width of white 
space to be left around a high or low label, as a fraction 
of the width of the viewport. This nominal width is 
multiplied by 'CWM'.
.sp
The default value of 'HLW' is 0.005.
.IP "'HLX' - High/Low Search Radius in X - Integer"
If 'HLX' is greater than zero, it specifies the half-width 
of the index-value neighborhood used in searching the 
contour field for highs and lows. If 'HLX' is less than or 
equal to zero, Conpack picks a reasonable value to use 
(approximately 1/8 of 'ZDM', but not less than 2 nor 
greater than 15).
.sp
As an example, if 'HLX' = 3 and 'HLY' = 4, then the values 
in ZDAT examined to determine if (I,J) is a high or a low 
are those having indices (K,L), where either K is not equal 
to I or L is not equal to J, K is between MAX(1,I-3) and 
MIN('ZDM',I+3), inclusive, and L is between MAX(1,J-4) and 
MIN('ZDN',J+4), inclusive.
.sp
The default value of 'HLX' is 0.
.IP "'HLY' - High/Low Search Radius in Y - Integer"
If 'HLY' is greater than zero, it specifies the half-height 
of the index-value neighborhood used in searching the 
contour field for highs and lows. If 'HLY' is less than or 
equal to zero, Conpack picks a reasonable value to use 
(approximately 1/8 of 'ZDN', but not less than 2 nor 
greater than 15).
.sp
For an example, see the description of 'HLX', above.
.sp
The default value of 'HLY' is 0.
.IP "'ILA' - Informational Label Angle - Real"
The parameter 'ILA' specifies the angle (in degrees 
counterclockwise from a vector pointing to the right) at 
which the informational label is to be written.
.sp
The default value of 'ILA' is 0.
.sp
.IP "'ILB' - Informational Label Box Flag - Integer"
If 'ILB' is zero, the informational label will not be boxed 
at all. The value 1 implies that the perimeter of the box 
is to be drawn (in the same color as the label) after the 
label is drawn. The value 2 implies that the box is to be 
filled (in the color specified by 'LBC') before the label 
is drawn. The value 3 implies both of the above.
.sp
The default value of 'ILB' is 0.
.sp
.IP "'ILC' - Informational Label Color Index - Integer"
.sp
If 'ILC' is less than zero, the informational label and the 
box, if any, around it, will be drawn in the color 
specified by the current text color index; if 'ILC' is 
greater than or equal to zero, then it specifies the 
desired color index for the label and the box. If a box is 
drawn around the label, it is made the same color as the 
label itself.
.sp
The default value of 'ILC' is -1.
.sp
.IP "'ILL' - Informational Label Line Width - Real"
If 'ILL' has a value less than or equal to zero, line width 
will not be set before drawing a box around the 
informational label. If 'ILL' has a value greater than 
zero, it specifies the desired width, as a multiple of the 
"normal" line width.
.sp
The default value of 'ILL' is 0.
.IP "'ILP' - Informational Label Positioning Flag - Integer"
The parameter 'ILP' says how the informational label is to 
be positioned. There are nine possible values, each of 
which specifies a point of the label box which is to lie on 
the point defined by 'ILX' and 'ILY': the value -4 implies 
the lower left-hand corner of the label box, -3 implies the 
center of the bottom of the box, -2 the lower right-hand 
corner of the box, -1 the center of the left edge of the 
box, 0 the center of the box, +1 the center of the right 
edge of the box, +2 the upper left-hand corner of the box, 
+3 the center of the top edge of the box, and +4 the upper 
right-hand corner of the box. Left, right, bottom, and top 
are defined from the viewpoint of someone reading the label 
right-side up.
.sp
The default value of 'ILP' is 4, so the upper right-hand 
corner of the box will be placed on the point
('ILX','ILY').
.sp
.IP "'ILS' - Informational Label Size - Real"
The parameter 'ILS' specifies the nominal size (width) of a 
character in the informational label, as a fraction of the 
width of the viewport. This nominal size is multiplied by 
\&'CWM'.
.sp
The default value of 'ILS' is 0.012.
.IP "'ILT' - Informational Label Text String - Character"
The parameter 'ILT' is a string of 100 or fewer characters, 
specifying the text of the informational label. The 
following substrings will be replaced by a numeric value:
.RS 10
.sp
$CIU$ - THE CONTOUR INTERVAL USED.
.br
$CMN$ - THE MINIMUM CONTOUR LEVEL.
.br
$CMX$ - THE MAXIMUM CONTOUR LEVEL.
.br
$SFU$ - THE CURRENT SCALE FACTOR.
.br
$ZMN$ - THE MINIMUM VALUE IN THE DATA ARRAY.
.br
$ZMX$ - THE MAXIMUM VALUE IN THE DATA ARRAY.
.br
.RE
.IP ""
In each case except $SFU$, the given value will have been 
divided by the current scale factor. A "U" may be inserted 
just before the final "$" (as in '$CIUU$', '$CMNU$', etc.) 
to request the use of an unscaled value.
.sp
The value with which $CIU$ is replaced will only be correct 
if Conpack itself has chosen the contour levels; otherwise, 
it may be necessary for the user to set the value of 'CIU' 
(which see, above).
.sp
If 'ILT' is given the value '  ' (a single blank), there 
will be no informational label.
.sp
The default value of 'ILT' is 'CONTOUR FROM $CMN$ TO $CMX$ 
BY $CIU$'.
.IP "'ILW' - Informational Label White Space Width - Real"
The parameter 'ILW' specifies the nominal width of white 
space to be left around the informational label, as a 
fraction of the width of the viewport. This nominal width 
is multiplied by 'CWM'.
.sp
The default value of 'ILW' is 0.005.
.IP "'ILX' - Informational Label X Coordinate - Real"
The parameter 'ILX' specifies the X coordinate of the 
basepoint of the informational label. The given value is 
mapped linearly onto the viewport; 0 refers to the left 
edge of the viewport and 1 to the right edge of the 
viewport. Values less than 0 or greater than 1 may be used.
.sp
The default value of 'ILX' is 0.98.
.IP "'ILY' - Informational Label Y Coordinate - Real"
The parameter 'ILY' specifies the Y coordinate of the 
basepoint of the informational label. The given value is 
mapped linearly onto the viewport; 0 refers to the bottom 
edge of the viewport and 1 to the top edge of the viewport. 
Values less than 0 or greater than 1 may be used.
.sp
The default value of 'ILY' is -.02.
.IP "'IWM' - Integer Workspace for Masking - Integer"
The parameter 'IWM' specifies the amount of integer 
workspace to be allotted for use by CPCLDM, which draws 
contour lines masked by an area map, in calls to the 
routine ARDRLN, in the package Areas. Assume a parameter 
value "n"; the space used will be "2n" ("n" for the array 
IAI and "n" for the array IAG, in calls to ARDRLN). The 
value "n" must be greater than or equal to the number of 
group identifiers used in generating the area map.
.sp
The default value of 'IWM' is 10.
.IP "'IWU' - Integer Workspace Usage - Integer"
The parameter 'IWU' is intended for retrieval only. It is 
zeroed by the call to CPRECT, CPSPS1, or CPSPS2. Therefore, by 
retrieving its value after an entire plot has been 
constructed, one may find out how large an integer 
workspace was actually required.
.IP "'LBC' - Label Box Color Index - Integer"
If label boxes (of whatever type) are filled, the filling 
is done using the color index specified by 'LBC'. If 'LBC' 
is less than zero, the current fill area color index is 
used.
.sp
The default value of 'LBC' is 0, which specifies the 
background color.
.IP "'LBX' - Label Box X Coordinate - Real"
Not to be set by the user. The value may be retrieved in 
one of the routines CPCHCF, CPCHHL, CPCHIL, or CPCHLL. It 
specifies the X coordinate (in the current user coordinate 
system) of the center of the box surrounding the label that 
has caused the routine to be called.
.sp
The default value of 'LBX' is 0.
.IP "'LBY' - Label Box Y Coordinate - Real"
Not to be set by the user. The value may be retrieved in 
one of the routines CPCHCF, CPCHHL, CPCHIL, or CPCHLL. It 
specifies the Y coordinate (in the current user coordinate 
system) of the center of the box surrounding the label that 
has caused the routine to be called.
.sp
The default value of 'LBY' is 0.
.IP "'LIS' - Label Interval Specifier - Integer"
When 'CLS' is given a positive value, indicating that 
Conpack is to choose contour levels at intervals of the 
form "bk", where "b" is a base value and "k" is an integer, 
and 'CIS' is given a positive value, indicating that it is 
the desired value of "b", then 'LIS' must be set to specify 
the interval between labeled contour levels.
.sp
See the descriptions of the parameters 'CLS' and 'CIS'.
.sp
As an example, one might specify 'CLS'=1, 'CIS'=1/3 and 
\&'LIS'=3 in order to get contours at values like 1/3, 2/3, 3/
3, 4/3, etc., with labels at values like 1, 2, 3, etc.
.sp
The default value of 'LIS' is 5.
.IP "'LIT' - Label Interval Table - Integer Array"
See the description of the parameter 'CIT'.
.IP "'LIU' - Label Interval Used - Integer"
The parameter 'LIU' is for retrieval only. When Conpack 
chooses the contour interval and decides that every "nth" 
one should be labeled, it sets 'LIU' to "n".
.IP "'LLA' - Line Label Angle - Real"
The parameter 'LLA' specifies the angle (in degrees 
counterclockwise from a vector pointing to the right) at 
which contour line labels are to be written when ABS('LLP') 
is greater than or equal to 2 and 'LLO' is 0.
.sp
The default value of 'LLA' is 0.
.IP "'LLB' - Line Label Box Flag - Integer"
If 'LLB' is zero, contour line labels drawn by CPLBDR will 
not be boxed at all. The value 1 implies that the perimeter 
of the box is to be drawn (in the same color as the label) 
after the label is drawn. The value 2 implies that the box 
is to be filled (in the color specified by 'LBC') before 
the label is drawn. The value 3 implies both of the above.
.sp
The default value of 'LLB' is 0.
.IP "'LLC' - Line Label Color Index - Integer Array"
Each element of the parameter array 'LLC', if greater than 
or equal to zero, is the color index for labels on contour 
lines at the level specified by the associated element of 
\&'CLV'. A value less than zero implies that the current text 
color index is to be used.
.sp
This parameter only affects line labels when ABS('LLP') = 2 
or 3 and the labels are therefore drawn by CPLBDR. It does 
not affect line labels when ABS('LLP') = 1 and the line 
labels are therefore drawn by the dash package, as called 
by CPCLDM or CPCLDR.
.sp
The default values of the elements of 'LLC' are all -1's.
.IP "'LLL' - Line Label Line Width - Real"
If 'LLL' has a value less than or equal to zero, line width 
will not be set before drawing boxes around contour line 
labels. If 'LLL' has a value greater than zero, it 
specifies the desired width, as a multiple of the "normal" 
line width.
.sp
The default value of 'LLL' is 0.
.IP "'LLO' - Line Label Orientation - Integer"
The parameter 'LLO' only has effect when ABS('LLP') is 
greater than or equal to 2, specifying use of either the 
regular scheme or the penalty scheme for positioning labels 
on contour lines. If 'LLO' is zero, the labels are written 
at the angle specified by 'LLA'. If 'LLO' is non-zero, the 
labels are written in the local direction of the contour 
line.
.sp
The default value of 'LLO' is 0.
.IP "'LLP' - Line Label Positioning - Integer"
The parameter 'LLP' says whether or not labels should be 
produced and, if so, how, as follows:
.RS
.IP \(bu
If 'LLP' is 0, it says that no line labels should be 
produced.
.IP \(bu
If ABS('LLP') is 1, it says that labels should be 
positioned along contour lines by using the old Conrec_family 
scheme of setting up a character dash pattern including the 
label and using the software dash package to draw the 
labels (which requires having 'DPU' set non-zero). This 
scheme has the disadvantages that one cannot control the 
orientation and one cannot shield the labels from having 
contour lines drawn through them.
.IP \(bu
If ABS('LLP') is 2, it says that labels should be 
positioned at regular intervals along the line. See the 
descriptions of the parameters 'RC1', 'RC2', and 'RC3'.
.IP \(bu
If ABS('LLP') is 3, it says that labels should be 
positioned using a penalty scheme, which gives much better 
results than either of the others.
.RE
.IP ""
When 'LLP' is 2 or 3, the 2D smoothing, if any, implied by 
the value of 'T2D' is suspended during label positioning, 
so that fewer label positions will be considered; this is 
quite a bit faster and the results are nearly as good as if 
the smoothing were done. To force smoothing, use 'LLP' = -2 
or -3.
.sp
The default value of 'LLP' is 1.
.IP "'LLS' - Line Label Size - Real"
The parameter 'LLS' specifies the nominal size (width) of a 
character in a contour line label, as a fraction of the 
width of the viewport. This nominal size is multiplied by 
\&'CWM'.
.sp
The default value of 'LLS' is 0.010.
.IP "'LLT' - Line Label Text String - Character"
For each I from 1 to 'NCL', element I of the parameter 
array 'LLT' is a string of twenty or fewer characters, to 
be used as a label for the contour level specified by the 
Ith element of 'CLV'. Since the character string will be 
plotted using the routine PLCHHQ, in the package Plotchar, 
it may contain colon-enclosed "function codes" to do things 
like create superscripts.
.sp
If the elements of this array are not supplied by the user, 
they will be filled in by Conpack itself.
.IP "'LLW' - Line Label White Space - Real"
The parameter 'LLW' specifies the nominal width of white 
space to be left around a contour line label, as a fraction 
of the width of the viewport. This nominal width is 
multiplied by 'CWM'.
.sp
The default value of 'LLW' is 0.005.
.IP "'LOC' - Low Label Color Index - Integer"
The parameter 'LOC' is used in determining the color index 
for low labels. See the description of 'HLC', above.
.IP "'LOT' - Low Label Text String - Character"
The parameter 'LOT' specifies the text string to be used in 
labeling a low. See 'HLT', above.
.IP "'MAP' - Mapping Flag - Integer"
If 'MAP' is zero, it says that the x and y coordinates used 
to create the contour map are not to be transformed by the 
user-replaceable subroutine CPMPXY. If 'MAP' is non-zero, 
it says that x and y coordinates are to be so transformed.
The default version of CPMPXY provides two useful mappings:
.sp
If the first subscript of the data array is a linear 
function of the longitude and the second is a linear 
function of the latitude, then one can transform all 
graphics output onto a map background created by calls to 
routines in the utility package Ezmap just by setting 'MAP' 
= 1, 'XC1' = minimum longitude, 'XCM' = maximum longitude, 
\&'YC1' = minimum latitude, and 'YCN' = maximum latitude.
Also, the parameter 'SET' must be given the value 0 in 
order to prevent Conpack from calling SET and thereby 
overriding the call done by Ezmap.
.sp
If the first subscript of the data array is a linear 
function of rho and the second is a linear function of 
theta, where rho and theta are polar coordinates, then to 
map all graphics output properly, one may set 'MAP' = 2, 
\&'XC1' = minimum rho, 'XCM' = maximum rho, 'YC1' = minimum 
theta, and 'YCN' = maximum theta. In this case, one must 
either use 'SET' = 0 and do an appropriate SET call or use 
\&'SET' = 1 and give the parameters 'WDB', 'WDL', 'WDR', and 
\&'WDT' values consistent with the mapped values of X and Y, 
which will all be of the form "rho*cos(theta)" and 
"rho*sin(theta)", respectively.
.sp
Using any other non-zero value of 'MAP' will result in the 
identity mapping.
.sp
Of course, one can replace the routine CPMPXY and build as 
many different mappings into it as desired. See the 
description of CPMPXY.
.IP "'NCL' - Number of Contour Levels - Integer"
If Conpack is to pick contour levels (see the description 
of the parameter 'CLS') then the initial call to CPRECT, 
CPSPS1, or CPSPS2 causes 'NCL' to be zeroed. Subsequently, 
during the first call to a Conpack routine requiring 
contour levels to have been chosen, 'NCL' will be set as 
part of the process of choosing them. If the user elects to 
choose the contour levels, the first parameter which must 
be set to do this is 'NCL'.
.sp
The parameter 'NCL' has no meaningful default value.
.IP "'NEL' - Numeric Exponent Length - Integer"
Giving 'NEL' a value less than or equal to zero says that 
exponents in numeric labels should be written in the 
shortest possible form; plus signs are omitted and the 
exponent magnitude is written with no leading zeroes. A 
value "n" which is greater than zero indicates that all 
exponents should be written with a sign (+ or -) and that 
the exponent magnitude should be padded with leading zeroes 
to a length of n characters.
.sp
The default value of 'NEL' is 0.
.IP "'NET' - Numeric Exponent Type - Integer"
The parameter 'NET' says what characters are to be used 
between the mantissa of a numeric label and the exponent. 
The value 0 implies the use of an E, as in FORTRAN "E 
format", the value 1 implies the use of function codes, as 
expected by the utility Plotchar, to generate "x 10**n", 
where n is a superscript exponent, and the value 2 implies 
the use of "x10**".
.sp
The default value of 'NET' is 1.
.IP "'NEU' - Numeric Exponent Use Flag - Integer"
Giving 'NEU' a value less than or equal to zero forces the 
use of the exponential form in all numeric labels. A 
positive value "n" indicates that the form without an 
exponent should be used as long as it requires no more than 
n characters; otherwise the form requiring the fewest 
characters should be used.
.sp
The default value of 'NEU' is 5.
.IP "'NLS' - Numeric Leftmost Significant Digit Flag - Integer"
Giving 'NLS' the value zero says that the leftmost non-zero 
digit of a number represented by a numeric label is to be 
considered its first significant digit. A non-zero value 
says that the digit in the same digit position as the 
leftmost non-zero digit of the largest number (in absolute 
value) in the data field being contoured is to be 
considered the leftmost significant digit. This tends to 
make the numeric labels more nearly consistent with one 
another. Consider the following example, using three 
significant digits:
.sp
.RS 10
USING 'NLS'=0: .500 1.00 1.50 ... 9.50 10.5 ...
.br
USING 'NLS'=1: .5   1.0  1.5  ... 9.5  10.5 ...
.RE
.IP ""
The default value of 'NLS' is 1.
.IP "'NLZ' - Numeric Leading Zero Flag - Integer"
Giving 'NLZ' a non-zero value says that a zero is to placed 
before any numeric label which would otherwise begin with a 
decimal point (use "0.345", rather than ".345").
.sp
The default value of 'NLZ' is 0.
.IP "'NOF' - Numeric Omission Flags - Integer"
The parameter 'NOF' says what parts of a numeric label may 
be omitted. The value 0 says that no part may be omitted. 
Add a 4 to indicate that a leading "1" or "1." which is 
unnecessary (as in "1 x 10**13") may be omitted, a 2 to 
indicate that a trailing decimal point (as in "13.") may be 
omitted, and a 1 to indicate that trailing zeroes (as in 
"46.200") may be omitted.
.sp
Contour line labels generated by Conpack and values in the 
informational label which are known to have been rounded to 
"nice" values (like '$CIU$', '$CMN$', and '$CMX$') will 
have trailing zeroes trimmed in any case, no matter what 
the value of 'NOF' is.
.sp
The default value of 'NOF' is 6 (4 + 2).
.IP "'NSD' - Number of Significant Digits - Integer"
The parameter 'NSD' specifies the maximum number of 
significant digits to be used in numeric labels 
representing contour field values. A negative value "-n" 
indicates that n significant digits should be used. A 
positive value "n" indicates that "m+n" digits should be 
used, where "m" is the number of digits that are the same 
for all values in the contour field. (For example, if the 
minimum value is "1123.6" and the maximum value is 
"1125.9", then the value of "m" is 3.)
.sp
The default value of 'NSD' is 4.
.IP "'NVS' - Number of Vertical Strips - Integer"
When the parameter 'NVS' is non-zero, an extra group of 
edges, with group identifier 'GIS', is added to the area 
map by the routine CPCLAM. These edges include the boundary 
of the viewport and enough vertical lines to break the area 
occupied by the viewport up into 'NVS' vertical strips. The 
object of this is to break up the contour bands which are 
to be filled into smaller and simpler pieces; this may be 
necessary if the graphics device in use limits the number 
of points which may be used to define a polygon to be 
filled. The area identifier for the outside of the viewport 
is -1; all other area identifiers used are 0's.
.sp
The default value of 'NVS' is 1.
.IP "'ORV' - Out-of-Range Value - Real"
If 'ORV' is non-zero, it specifies an out-of-range value, 
to be used as the value of X and Y coordinates returned by 
the mapping routine CPMPXY to say that a point is out-of-range 
(invisible) under the current mapping.
.sp
The default value of 'ORV' is 0.
.IP "'PAI' - Parameter Array Index - Integer"
The value of 'PAI' must be set before calling CPGETC, 
CPGETI, CPGETR, CPSETC, CPSETI, or CPSETR to access any 
parameter which is an array; it indicates which element of 
the array is meant. For example, to set the 10th contour 
level to zero, use code like this:
.RS 10
.sp
CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',10)
.br
CALL CPSETR ('CLV - CONTOUR LEVEL',0.)
.RE
.IP ""
The default value of 'PAI' is 0.
.IP "'PC1' - Penalty Scheme Constant 1 - Real"
The parameter 'PC1' is one of the constants used in the 
penalty scheme for positioning labels. The largest gradient 
allowed at the position of a label will be GRAV + 'PC1' * 
GRSD, where GRAV is the average gradient and GRSD is the 
standard deviation of the gradients.
.sp
The default value of 'PC1' is 1.
.IP "'PC2' - Penalty Scheme Constant 2 - Real"
The parameter 'PC2' is one of the constants used in the 
penalty scheme for positioning labels. It specifies the 
maximum (estimated) number of contour bands allowed to 
cross a label.
.sp
The default value of 'PC2' is 5.
.IP "'PC3' - Penalty Scheme Constant 3 - Real"
The parameter 'PC3' is one of the constants used in the 
penalty scheme for positioning labels. It specifies, in 
degrees, the maximum cumulative change in direction to be 
allowed along that portion of the contour line covered by a 
circle centered on a label and having a radius equal to 
half the width of the label.
.sp
The default value of 'PC3' is 60.
.IP "'PC4' - Penalty Scheme Constant 4 - Real"
The parameter 'PC4' is one of the constants used in the 
penalty scheme for positioning labels. It specifies the 
"optimal distance" in the term in the penalty function 
which attempts to force labels to be at an optimal distance 
from each other.
.sp
The default value of 'PC4' is 0.05.
.IP "'PC5' - Penalty Scheme Constant 5 - Real"
The parameter 'PC5' is one of the constants used in the 
penalty scheme for positioning labels. It specifies the 
"folding distance" in the term in the penalty function 
which attempts to force labels to be at an optimal distance 
from each other.
.sp
The default value of 'PC5' is 0.15.
.IP "'PC6' - Penalty Scheme Constant 6 - Real"
The parameter 'PC6' is one of the constants used in the 
penalty scheme for positioning labels. It specifies the 
minimum distance to be allowed between any two labels on 
the same contour line, as a fraction of the width of the 
viewport.
.sp
The default value of 'PC6' is 0.30.
.IP "'PIC' - Point Interpolation Flag for Contours - Integer"
The parameter 'PIC' specifies the number of points to 
interpolate between each pair of points defining a segment 
of a contour line, prior to any mapping implied by the 
parameter 'MAP'. It is intended that a non-zero value 
should normally be used only if 'MAP' is non-zero, which 
turns mapping on, and 'T2D' is zero, which turns the 2D 
smoother off; the intent is to map straight-line segments 
of contour lines more nearly correctly into curved-line 
segments on a background (one drawn by Ezmap, for example).
If the 2D smoother is turned on, the additional points will 
be used and the smoothed curve will be constrained to pass 
through them; this may be useful.
.sp
A negative value of 'PIC' causes ABS('PIC') points to be 
interpolated, but the interpolated points are not, in 
general, used to draw the line segment; the object, in this 
case, is simply to do a finer search for changes in 
visibility (out-of-range state, as defined by values of 
\&'ORV' returned by the routine CPMPXY) along the segment.
.sp
The default value of 'PIC' is 0.
.IP "'PIE' - Point Interpolation Flag for Edges - Integer"
The parameter 'PIE' specifies the number of points to 
interpolate between each pair of points defining a segment 
of an "edge" (the edge of the grid, the edge of a 
special-value area, or the edge of an out-of-range area). It is 
intended that a non-zero value should normally be used only 
if 'MAP' is non-zero, which turns mapping on; the intent is 
to map straight-line segments of edge lines more nearly 
correctly into curved-line segments on a background (one 
drawn by Ezmap, for example).
.sp
A negative value of 'PIE' causes ABS('PIE') points to be 
interpolated, but the interpolated points are not, in 
general, used to draw the line segment; the object, in this 
case, is simply to do a finer search for changes in 
out-of-range state (visibility) along the segment. (The edges of 
out-of-range areas, however, are drawn using all such 
interpolated points.)
.sp
Using too large an (absolute) value of 'PIE' will cause the 
tracing of the edges of out-of-range areas to be very 
time-consuming, because the number of points to be examined is 
\&'ZDM' x 'ZDN' x 'PIE' x 'PIE'.
.sp
The default value of 'PIE' is 0.
.IP "'PW1' - Penalty Scheme Weight 1 - Real"
The parameter 'PW1' specifies the weight for the gradient 
term in the penalty function.
.sp
The default value of 'PW1' is 2.
.IP "'PW2' - Penalty Scheme Weight 2 - Real"
The parameter 'PW2' specifies the weight for the 
number-of-contours term in the penalty function.
.sp
The default value of 'PW2' is 0.
.IP "'PW3' - Penalty Scheme Weight 3 - Real"
The parameter 'PW3' specifies the weight for the 
change-in-direction term in the penalty function.
.sp
The default value of 'PW3' is 1.
.IP "'PW4' - Penalty Scheme Weight 4 - Real"
The parameter 'PW4' specifies the weight for the 
optimum-distance term in the penalty function.
.sp
The default value of 'PW4' is 1.
.IP "'RC1' - Regular Scheme Constant 1 - Real"
The parameter 'RC1' specifies the desired distance from the 
beginning of a contour line to the first label on that line 
when they are positioned using the "regular" scheme. The 
nth label on each labeled contour line will be at a 
distance 'RC1' + 'RC2' x (n-1) + 'RC3' x Rn units (in the 
fractional coordinate system) from the beginning of the 
line, where "Rn" is a random number between -1 and 1.
.sp
The default value of 'RC1' is 0.25.
.IP "'RC2' - Regular Scheme Constant 2 - Real"
The parameter 'RC2' specifies the desired nominal distance 
between labels when they are positioned using the "regular" 
scheme. See the description of 'RC1', above.
.sp
The default value of 'RC2' is 0.25.
.IP "'RC3' - Regular Scheme Constant 3 - Real"
The parameter 'RC3' specifies the desired maximum variation 
in the distance between labels when they are positioned 
using the regular scheme. See the description of 'RC1', 
above.
.sp
The default value of 'RC3' is 0.05.
.IP "'RWC' - Real Workspace for Contours - Integer"
The parameter 'RWC' specifies the amount of real workspace 
to be allotted to hold X coordinates of points defining 
contour lines. Assume a parameter value "n". If no 2D 
smoothing is requested, the total space used will be "2n" 
("n" for X coordinates and another "n" for Y coordinates). 
If 2D smoothing is requested, the total space used will be 
"7n" ("n" for X coordinates, "n" for Y coordinates, and 
"5n" for scratch arrays).
.sp
Normally, the value of 'RWC' is of no particular interest 
to the user, since the same contour lines are produced with 
a small value as would be produced with a larger value. 
There are two situations in which it becomes of more 
interest: 1) When the penalty scheme is used to position 
labels, the length of the portion of the contour line over 
which the penalty function is evaluated is limited by the 
value of 'RWC'. If 'RWC' is set too small, too many labels 
may be put on a given contour line and some of them may be 
too close to each other. 2) When hachuring has been 
activated (by setting the value of 'HCF' non-zero), it is 
important that the internal routine that does the hachuring 
see entire contours at once, so that it may properly decide 
whether a contour is open or closed and, in the latter 
case, where the interior of the closed contour is. In both 
of these cases, the solution is to increase the value of 
\&'RWC'.
.sp
The default value of 'RWC' is 100.
.IP "'RWG' - Real Workspace for Gradients - Integer"
The parameter 'RWG' specifies the amount of real workspace 
to be allotted to hold gradients which are to be computed 
and used in positioning labels using the penalty scheme. 
Using a larger value provides for a more accurate 
representation of the gradient field, up to the point at 
which it exceeds 'ZDM' x 'ZDN'.
.sp
The default value of 'RWG' is 1000.
.IP "'RWM' - Real Workspace for Masking - Integer"
The parameter 'RWM' specifies the amount of real workspace 
to be allotted for use by CPCLDM, which draws contour lines 
masked by an area map, in calls to the routine ARDRLN, in 
the package Areas. Assume a parameter value "n"; the space 
used will be "2n" ("n" for the X-coordinate array XCS and 
"n" for the Y-coordinate array YCS, in calls to ARDRLN). 
Any value of "n" greater than or equal to 2 will work; 
smaller values will cause the generation of more calls to 
the user routine RTPL (one of the arguments of CPCLDM).
.sp
The default value of 'RWM' is 100.
.IP "'RWU' - Real Workspace Usage - Integer"
The parameter 'RWU' is intended for retrieval only. It is 
zeroed by the call to CPRECT, CPSPS1, or CPSPS2. 
Thereafter, as Conpack routines are called, the value of 
\&'RWU' is updated to reflect the largest number of words of 
real workspace needed at any one time. Therefore, by 
retrieving its value after an entire plot has been 
constructed, one may find out how large a real workspace 
was actually required.
.IP "'SET' - Do-SET-Call Flag - Integer
Giving 'SET' the value 0 says that no SET call is to be 
done by Conpack; the value 1 says that it is to be done. In 
the latter case, the call is done by CPRECT, CPSPS1, or 
CPSPS2.
.sp
Arguments 5-8 of a SET call done by the user must be 
consistent with the ranges of the X and Y coordinates being 
used by Conpack, as specified by the values of the 
parameters 'XC1', 'XCM', 'YC1', 'YCN', and 'MAP'. See the 
descriptions of those parameters.
.sp
The default value of 'SET' is 1.
.IP "'SFS' - Scale Factor Selector - Real"
The scale factor is that value (usually, but not 
necessarily, a power of 10) by which the actual values of 
contour field values are to be divided to get the value of 
a numeric label. If 'SFS' is given a value greater than 
zero, that value is the scale factor to be used. If 'SFS' 
is given a value less than or equal to zero, it is 
truncated to form an integer directing Conpack to select a 
scale factor, as follows:
.RS
.IP "0" 
Implies that the scale factor should be selected in such 
a way as to reduce the ZDAT element having the largest 
absolute value to the range from 0.1 to 0.999...
.IP "-1"
Implies that the scale factor should be selected in such 
a way as to reduce the ZDAT element having the largest 
absolute value to the range from 1. to 9.999...
.IP "-2"
Implies that the scale factor should be selected in such
a way as to place the decimal point in the ZDAT element 
having the largest absolute value after the rightmost 
significant digit of that value (as defined by the values 
of 'NSD' and 'NLS').
.IP "-3" 
Implies that the scale factor should be selected in such 
a way as to remove extra zeroes from the ends of the ZDAT 
element having the largest absolute value. For example, if 
that element were 0.000136, the scale factor would be 10 to 
the power -3; if that element were 136000 (assuming three 
significant digits are desired), the scale factor would be 
10 to the power 3. If there are no extra zeroes on either 
end of the ZDAT element having the largest absolute value, 
the scale factor will be 1.
.IP "-4 or less" 
Implies that the scale factor should be selected 
in such a way as to reduce all contour labels to integers.
.sp
The default value of 'SFS' is 1.
.IP "'SFU' - Scale Factor Used - Real"
The parameter 'SFU' is intended for retrieval only; it 
gives the value of the scale factor selected for use by 
Conpack.
.IP "'SPV' - Special Value - Real"
If 'SPV' is non-zero, it specifies a "special value", which 
may be used in data fields to signal missing data. No 
contour lines will be drawn within any grid cell with a 
special value at one or more of its four corners.
.sp
The default value of 'SPV' is 0.
.IP "'SSL' - Smoothed Segment Length - Real"
The parameter 'SSL' specifies the distance between points 
used to draw the curves generated by 2D smoothing; it is 
expressed as a fraction of the width of the window in the 
coordinate system in which the smoothing is being done.
.sp
The default value of 'SSL' is 0.01.
.IP "'T2D' - Tension on 2-Dimensional Splines - Real"
A non-zero value of 'T2D' says that 2D smoothing (using 
cubic splines under tension) should be done; the absolute 
value of 'T2D' is the desired tension. If 'T2D' is 
negative, smoothing will be done before the mapping, if 
any, requested by the flag 'MAP'; if 'T2D' is positive, 
smoothing will be done after the mapping.
.sp
The default value of 'T2D' is 0.
.IP "'T3D' - Tension on 3-Dimensional Splines - Real"
The parameter 'T3D' specifies the tension on the 3D 
(bicubic) splines used by CPSPS1 or CPSPS2 to smooth the 
data being contoured.
.sp
The default value of 'T3D' is 1.
.IP "'VPB' - Viewport Bottom - Real"
The parameter 'VPB' is only used when 'SET' is non-zero, 
saying that Conpack should do the call to SET; it specifies 
the position of the bottom edge of the area in which the 
viewport is to be placed, expressed as a fraction between 0 
(the bottom edge of the plotter frame) and 1 (the top edge 
of the plotter frame). See also the description of 'VPS'.
.sp
The default value of 'VPB' is 0.05.
.IP "'VPL' - Viewport Left - Real"
The parameter 'VPL' is only used when 'SET' is non-zero, 
saying that Conpack should do the call to SET; it specifies 
the position of the left edge of the area in which the 
viewport is to be placed, expressed as a fraction between 0 
(the left edge of the plotter frame) and 1 (the right edge 
of the plotter frame). See also the description of 'VPS'.
.sp
The default value of 'VPL' is 0.05.
.IP "'VPR' - Viewport Right - Real"
The parameter 'VPR' is only used when 'SET' is non-zero, 
saying that Conpack should do the call to SET; it specifies 
the position of the right edge of the area in which the 
viewport is to be placed, expressed as a fraction between 0 
(the left edge of the plotter frame) and 1 (the right edge 
of the plotter frame). See also the description of 'VPS'.
.sp
The default value of 'VPR' is 0.95.
.IP "'VPS' - Viewport Shape - Real"
The parameter 'VPS' is only used when 'SET' is non-zero, 
saying that Conpack should do the call to SET; it specifies 
the desired viewport shape, as follows:
.RS
.IP \(bu
A negative value specifies the exact shape of the viewport; 
the absolute value is the ratio of the width of the 
viewport to its height.
.IP \(bu
The value 0 specifies a viewport completely filling the 
area specified by 'VPL', 'VPR', 'VPB', and 'VPT'.
.IP \(bu
A value "s" between 0 and 1 specifies a plot of the shape 
determined by the values of 'XC1', 'XCM', 'YC1', and 'YCN', 
reverting to the shape specified by 'VPL', 'VPR', 'VPB', 
and 'VPT' if the ratio of the shorter side to the longer 
side would be less than "s".
.IP \(bu
A value "s" greater than or equal to 1 specifies a plot of 
the shape determined by the values of 'XC1', 'XCM', YC1', 
and 'YCN', reverting to a square if the ratio of the longer 
side to the shorter side would be greater than "s".
.RE
.IP ""
The viewport, whatever its final shape, is centered in, and 
made as large as possible in, the area specified by the 
parameters 'VPB', 'VPL', 'VPR', and 'VPT'.
.sp
The default value of 'VPS' is 0.25.
.IP "'VPT' - Viewport Top - Real"
The parameter 'VPT' is only used when 'SET' is non-zero, 
saying that Conpack should do the call to SET; it specifies 
the position of the top edge of the area in which the 
viewport is to be placed, expressed as a fraction between 0 
(the bottom edge of the plotter frame) and 1 (the top edge 
of the plotter frame). See also the description of 'VPS'.
.sp
The default value of 'VPT' is 0.95.
.IP "'WDB' - Window Bottom - Real"
When Conpack does the call to 'SET', the parameter 'WDB' is 
used to determine argument number 7, the user Y coordinate 
at the bottom of the window. If 'WDB' is not equal to 
\&'WDT', 'WDB' is used. If 'WDB' is equal to 'WDT', but 'YC1' 
is not equal to 'YCN', then 'YC1' is used. Otherwise, the 
value 1 is used.
.sp
The default value of 'WDB' is 0.
.IP "'WDL' - Window Left - Real"
When Conpack does the call to 'SET', the parameter 'WDL' is 
used to determine argument number 5, the user X coordinate 
at the left edge of the window. If 'WDL' is not equal to 
\&'WDR', 'WDL' is used. If 'WDL' is equal to 'WDR', but 'XC1' 
is not equal to 'XCM', then 'XC1' is used. Otherwise, the 
value 1 is used.
.sp
The default value of 'WDL' is 0.
.IP "'WDR' - Window Right - Real"
When Conpack does the call to 'SET', the parameter 'WDR' is 
used to determine argument number 6, the user X coordinate 
at the right edge of the window. If 'WDR' is not equal to 
\&'WDL', 'WDR' is used. If 'WDR' is equal to 'WDL', but 'XCM' 
is not equal to 'XC1', then 'XCM' is used. Otherwise, the 
value REAL('ZDM') is used.
.sp
The default value of 'WDR' is 0.
.IP "'WDT' - Window Top - Real"
When Conpack does the call to 'SET', the parameter 'WDB' is 
used to determine argument number 8, the user Y coordinate 
at the top of the window. If 'WDT' is not equal to 'WDB', 
\&'WDT' is used. If 'WDT' is equal to 'WDB', but 'YCN' is not 
equal to 'YC1', then 'YCN' is used. Otherwise, the value 
REAL('ZDN') is used.
.sp
The default value of 'WDT' is 0.
.IP "'WSO' - Workspace Overflow Flag - Integer"
The parameter 'WSO' says what to do when a real or integer 
workspace overflow occurs, as follows:
.RS
.IP \(bu
The value 0 indicates that execution will terminate with a 
fatal-error call to SETER.
.IP \(bu
The value 1 indicates that an error message will be written 
to the error file, after which execution will continue.
.IP \(bu
The value 2 indicates that no error message will be 
written, and that execution will continue.
.RE
.IP ""
When execution continues, the resulting plot will be 
incomplete. The values of 'IWU' and 'RWU' may be retrieved 
to find out how much workspace would have been used if the 
call on which the workspace overflow occurred had 
succeeded; note that, if these amounts are provided on a 
subsequent run, one is not assured that the workspace
overflow will be averted.
.sp 
The default value of 'WSO' is 1.
.IP "'XC1' - X Coordinate at Index 1 - Real"
The parameter 'XC1' specifies the X coordinate value which 
corresponds to a value of 1 for the first subscript of the 
data array, prior to any mapping implied by a non-zero 
value of 'MAP'. If 'XC1' is equal to 'XCM', 1 will be used.
.sp
The default value of 'XC1' is 0.
.IP "'XCM' - X Coordinate at Index M - Real"
The parameter 'XCM' specifies the X coordinate value which 
corresponds to a value of 'ZDM' for the first subscript of 
the data array, prior to any mapping implied by a non-zero 
value of 'MAP'. If 'XC1' is equal to 'XCM', REAL('ZDM') 
will be used.
.sp
The default value of 'XCM' is 0.
.IP "'YC1' - Y Coordinate at Index 1 - Real"
The parameter 'YC1' specifies the Y coordinate value which 
corresponds to a value of 1 for the second subscript of the 
data array, prior to any mapping implied by a non-zero 
value of 'MAP'. If 'YC1' is equal to 'YCM', 1 will be used.
.sp
The default value of 'YC1' is 0.
.IP "'YCN' - Y Coordinate at Index N - Real"
The parameter 'YCN' specifies the Y coordinate value which 
corresponds to a value of 'ZDN' for the second subscript of 
the data array, prior to any mapping implied by a non-zero 
value of 'MAP'. If 'YC1' is equal to 'YCN', REAL('ZDN') 
will be used.
.sp
The default value of 'YCN' is 0.
.IP "'ZD1' - ZDAT 1st Dimension - Integer"
The parameter 'ZD1' specifies the first dimension of the 
array ZDAT, which contains the data to be contoured. If 
CPRECT is called, it sets 'ZD1' (the argument KZDT is the 
desired value). If CPSPS1 or CPSPS2 is called, it either 
picks a value of 'ZD1' (if 'ZDS' is non-zero) or expects 
the user to have done so (if 'ZDS' is zero).
.sp
The default value of 'ZD1' is 1.
.IP "'ZDM' - Z Data Array Dimension M - Integer"
The parameter 'ZDM' specifies the first dimension of the 
array of data to be contoured. Its value will be less than 
or equal to the value of 'ZD1'. If CPRECT is called, it 
sets 'ZDM' (the argument MZDT is the desired value). If 
CPSPS1 or CPSPS2 is called, it either picks a value of 
\&'ZDM' (if 'ZDS' is non-zero) or expects the user to have 
done so (if 'ZDS' is zero).
.sp
The default value of 'ZDM' is 1.
.IP "'ZDN' - Z Data Array Dimension N - Integer"
The parameter 'ZDN' specifies the second dimension of the 
array of data to be contoured. If CPRECT is called, it sets 
\&'ZDN' (the argument NZDT is the desired value). If CPSPS1 
or CPSPS2 is called, it either picks a value of 'ZDN' (if 
\&'ZDS' is non-zero) or expects the user to have done so (if 
\&'ZDS' is zero).
.sp
The default value of 'ZDN' is 1.
.IP "'ZDS' - ZDAT Dimension Selector - Integer"
If 'ZDS' is non-zero, CPSPS1 or CPSPS2 will select values 
for 'ZD1', 'ZDM', and 'ZDN'; otherwise, they will be 
expected to have been set by the user. Note that, if the 
size of the dense array is not a product of the size of the 
sparse array and some perfect square, the aspect ratio of 
the dense grid may be slightly different from that of the 
sparse grid.
.sp
The default value of 'ZDS' is 1.
.IP "'ZDU' - Z Data Value, Unscaled - Real"
The parameter 'ZDU' is just like 'ZDV' (which see, below), 
but the value is unscaled.
.IP "'ZDV' - Z Data Value - Real"
The parameter 'ZDV' is mostly for output. Its value may be 
retrieved in a user version of CPCHHL to retrieve the value 
of the high or low which is being labeled. If a character 
string representing the value is desired, CPGETC may be 
used to obtain it (as modified by the current scale 
factor); thus, to obtain the character representation of an 
arbitrary value in a form consistent with the other values 
on a contour plot, set 'ZDV' with a call to CPSETR and 
retrieve the value of 'ZDV' with a call to CPGETC; if an 
unscaled value is desired, use the parameter name 'ZDU' in 
the call to CPGETC.
.IP "'ZMN' - Z Minimum Value - Real"
The minimum value in the field, as found by CPRECT, CPSPS1, 
or CPSPS2. For output only.
.IP "'ZMX' - Z Maximum Value - Real"
The maximum value in the field, as found by CPRECT, CPSPS1, 
or CPSPS2. For output only.
.SH SEE ALSO
Online:
cpgetc,
cpgeti,
cpgetr,
cprset,
cpsetc,
cpseti,
cpsetr
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
