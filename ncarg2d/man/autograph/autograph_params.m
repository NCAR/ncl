'\" t
.TH Autograph_params 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Autograph_params - This document briefly describes all Autograph
internal parameters.
.SH DESCRIPTION 
The Autograph control parameters reside in the labeled 
common block AGCONP. There are currently 485 of them, of 
which 336 are "primary" and 149 are "secondary". Primary 
control parameters have default values and are subject to 
change by a user program to produce some desired effect on 
the behavior of Autograph and/or on the nature of a graph 
being drawn. Secondary control parameters are computed by 
Autograph itself and are not normally subject to change by 
a user program.
.sp
User access to these parameters is provided by the routines 
AGGETC, AGGETF, AGGETI, AGGETP, AGGETR, AGSETC, AGSETF, 
AGSETI, AGSETP, and AGSETR. 
The first argument in a 
call to one of these routines is a character string naming 
a group of parameters (perhaps containing only a single 
parameter) which the user wishes to "set" or "get". Each 
such string has the form \'k1/k2/k3/ . . . kn.\', where k1 is 
a keyword identifying a major group of parameters, k2 is a 
keyword identifying a subgroup of that major group, k3 is a 
keyword identifying a further subgroup of that subgroup, 
and so on. Only the first three characters of k1 and the 
first two characters of the others need be used; also, 
certain keywords may be omitted.
.sp
Because of certain portability considerations, all of the 
parameters have real values. The routine AGSETP may be used 
to set the real values of the parameters in any group, and 
the routine AGGETP to retrieve those values. Either of the 
routines AGSETF or AGSETR may be used to set the real value 
of a single parameter and either of the routines AGGETF or 
AGGETR may be used to get the real value of a single 
parameter.
.sp
Some parameters may only take on discrete integral values 
(like "0.", "1.", "-6.", or "65535.") and are used in roles 
for which integers would normally be used. The routine 
AGSETI may be used to set the integer value of a single 
parameter of this type and the routine AGGETI may be used 
to get the integer value of a single parameter of this type.
.sp
Other parameters intrinsically represent character strings; 
the real value of the parameter is an identifier, generated 
when the character string is passed to Autograph and 
enabling the character string to be retrieved from 
Autograph\'s character storage space when it is needed. The 
routine AGSETC must be used to set the character-string 
value associated with a single parameter of this type and 
the routine AGGETC must be used to get the character-string 
value associated with a single parameter of this type.
.sp
Many parameters have a limited range of acceptable values. 
What generally happens when a parameter is given an out-of-range
value is that Autograph (usually the routine AGSTUP) 
resets that value to the value at the nearer end of the 
acceptable range.
.sp
Setting certain parameters (individually, rather than as 
part of a multi-parameter group) implies, as a side effect, 
"special action" by the routine AGSETP (which may be called 
directly by the user or indirectly by way of a user call to 
AGSETC, AGSETF, AGSETI, or AGSETR). For example, setting 
the parameter \'BACKGROUND.\' to request a particular 
background type causes a number of other parameters to be 
changed in order to achieve the desired result.
.sp
Each of the named parameter groups is described below. 
Square brackets are used to mark portions of a name which 
may be omitted; the notation \'k1/k2/ . . . [ki/]...kn.\' 
indicates that the keyword ki and the following slash may 
be omitted. In each description, the simplest form of the 
name is given. If a multi-parameter group is named, its 
subgroups are listed, in the order in which they occur in 
the group. If a single parameter is named, the default 
value of that parameter is given and any "special action" 
by AGSETP is described.
.IP \'PRIMARY.\'
Simplest form of name: \'PRI.\'
.sp
This group consists of all 336 primary control parameters, 
in the order in which they appear below. It was originally 
provided to give users the capability of saving and 
restoring the state of Autograph. The routines AGSAVE and 
AGRSTR (which see) should now be used for this purpose.
.IP \'FRAME.\'
Simplest form of name: \'FRA.\'
.sp
An integral real number specifying when a frame advance is 
to be done by the routines EZY, EZXY, EZMY, and EZMXY and 
having one of three possible values:
.RS
.IP \(bu 4
The value "1." specifies a frame advance after drawing a 
graph.
.IP \(bu 4
The value "2." specifies no frame advance at all.
.IP \(bu 4
The value "3." specifies a frame advance before drawing a 
graph.
.RE
.IP ""
Default value: "1." (frame advance after drawing graph).
.IP \'SET.\'
Simplest form of name: \'SET.\'
.sp
An integral real number specifying whether or not the 
arguments of the last call to "SET" are used to determine 
the linear/log nature of the current graph, the position of 
the grid window and/or the X/Y minimum/maximum values.
.sp
(Note: The routine SET is part of the package called SPPS. 
Its first four arguments specify a portion of the plotter 
frame, its next four arguments specify the minimum and 
maximum X and Y coordinate values to be mapped to that 
portion, and its ninth argument specifies the linear/log 
nature of the mapping. The routine GETSET, which is also a 
part of SPPS, is used to retrieve the arguments of the last 
call to SET.)
.sp
Giving \'SET.\' a value (individually, rather than as part of 
a group) has both an immediate effect and a delayed effect. 
The immediate effect, which occurs in the routine AGSETP, 
is to return most of the parameters in the groups \'GRID.\', 
\'X.\', and \'Y.\' to their default values. (Exceptions are 
\'X/LOGARITHMIC.\' and \'Y/LOGARITHMIC.\', which may have values 
making them immune to such resetting.)  The delayed effect, 
which occurs in the routine AGSTUP, depends on the value 
given to \'SET.\'.
.sp
There are eight acceptable values of \'SET.\', four of which 
are just the negatives of the other four. Using a negated 
value suppresses the drawing of curves by the routines EZY, 
EZXY, EZMY, and EZMXY. Acceptable absolute values of \'SET.\' 
are as follows:
.RS
.IP \(bu
The value "1." means that the arguments of the last SET 
call are not to be used by AGSTUP.
.IP \(bu
The value "2." means that, in AGSTUP, \'X/LOGARITHMIC.\' and 
\'Y/LOGARITHMIC.\' are to be given values ("0." or "-1.") 
consistent with the ninth argument of the last SET call and 
that parameters in the group \'GRID.\' are to be given values 
consistent with the first four arguments of the last SET 
call.
.IP \(bu
The value "3." means that, in AGSTUP, \'X/LOGARITHMIC.\' and 
\'Y/LOGARITHMIC.\' are to be given values ("0." or "-1.") 
consistent with the ninth argument of the last SET call and 
that the other parameters in the groups \'X.\' and \'Y.\' are 
to be given values consistent with the fifth through eighth 
arguments of the last SET call.
.IP \(bu
The value "4." implies a combination of the actions 
specified by the values "2." and "3.".
.RE
.IP ""
Default value: "1." (no arguments of last SET call used).
.sp
Special action by AGSETP: As described above, if \'SET.\' is 
set (individually, rather than as part of a group) to any 
value by an AGSETP call, the parameters in the groups 
\'GRID.\', \'X.\', and \'Y.\' are reset to their default values. 
The parameter \'X/LOGARITHMIC.\' is reset to its default 
value ("0.") only if it has the value "+1."; a value of "-1."
is not changed; \'Y/LOGARITHMIC.\' is treated similarly.
.IP \'ROW.\'
Simplest form of name: \'ROW.\'
.sp
An integral real number specifying the assumed dimensioning 
of X and Y coordinate data arrays used in calls to the 
routines EZMY and EZMXY. There are four possibilities:
.RS
.IP \(bu
The value "-2." means that both X and Y arrays are 
subscripted by curve number and point number, in that order.
.IP \(bu
The value "-1." means that Y arrays are subscripted by 
curve number and point number, in that order, but that X 
arrays are subscripted by point number only. (The same 
X-coordinate data is used for all the curves.)
.IP \(bu
Either of the values "0." or "1." means that Y arrays are 
subscripted by point number and curve number, in that 
order, but that X arrays are subscripted by point number 
only. (The same X-coordinate data is used for all of the 
curves.)
.IP \(bu
The value "+2." means that both X and Y arrays are 
subscripted by point number and curve number, in that order.
.RE
.IP ""
Default value: "1." (Y by point and curve numbers, X by 
point number only).
.IP \'INVERT.\'
Simplest form of name: \'INV.\'
.sp
An integral real number having the value "0." or "1."; 
giving it the value "1." causes the routines AGSTUP and 
AGCURV to behave as if arguments defining X-coordinate data 
had been interchanged with arguments defining Y-coordinate 
data, thus, in some sense, allowing one to graph "X as a 
function of Y". This parameter is principally intended for 
users of the routines EZY, EZXY, EZMY, and EZMXY.
.sp
Default value: "0." (no inversion of X and Y arguments).
.IP \'WINDOW.\'
Simplest form of name: \'WIN.\'
.sp
An integral real number having the value "0." or "1."; 
giving it the value "1." causes the routine AGCURV to use 
the subroutine AGQURV, rather than AGKURV, for drawing 
curves. The result is that curve portions falling outside 
the grid window are omitted. See the AGCURV man page.  
.sp
Default value: "0." (no windowing of curves).
.IP \'BACKGROUND.\'
Simplest form of name: \'BAC.\'
.sp
An integral real number specifying the type of background 
to be drawn by AGBACK. There are four acceptable values:
.RS
.IP \(bu
The value "1." specifies a "perimeter" background.
.IP \(bu
The value "2." specifies a "grid" background.
.IP \(bu
The value "3." specifies a "half-axis" background.
.IP \(bu
The value "4." specifies no background at all.
.RE
.IP ""
Default value: "1." (a "perimeter" background).
.sp
Special action by AGSETP: If \'BACKGROUND.\' is set 
(individually, rather than as part of a group) by a call to 
AGSETP, the desired background is created by changing the 
following parameters:
.RS 10
.sp
\'[AXIS/]s/CONTROL.\'
.br
\'[AXIS/]s/[TICKS/]MAJOR/[LENGTH/]INWARD.\'
.br
\'[AXIS/]s/[TICKS/]MINOR/[LENGTH/]INWARD.\'
.br
\'LABEL/CONTROL.\'
.RE
.IP ""
where "s" stands for "LEFT", "RIGHT", "BOTTOM", and "TOP". 
This determines which of the axes are plotted, how long the 
inward-pointing portions of major and minor tick marks are 
to be, and whether or not informational labels are to be 
plotted. Values used are as follows:
.sp
The value "1." (perimeter background) sets:
.RS 9
.sp
\'s/CONTROL.\' to "4." for all s;
.br
\'s/MAJOR/INWARD.\' to ".015" for all s;
.br
\'s/MINOR/INWARD.\' to ".010" for all s;
.br
\'LABEL/CONTROL.\' to "2.".
.RE
.IP ""
The value "2." (grid background) sets:
.RS 9
.sp
\'s/CONTROL.\' to "4." for "s" = "LEFT" and "BOTTOM",
.br
\'s/CONTROL.\' to "-1." for "s" = "RIGHT" and "TOP";
.br
\'s/MAJOR/INWARD.\' to "1." for all s;
.br
\'s/MINOR/INWARD.\' to "1." for all s;
.br
\'LABEL/CONTROL.\' to "2.".
.RE
.IP ""
The value "3." (half-axis background) sets:
.RS 9
.sp
\'s/CONTROL.\' to "4." for "s" = "LEFT" and "BOTTOM",
.br
\'s/CONTROL.\' to "-1." for "s" = "RIGHT" and "TOP";
.br
\'s/MAJOR/INWARD.\' to ".015" for all s;
.br
\'s/MINOR/INWARD.\' to ".010" for all s;
.br
\'LABEL/CONTROL.\' to "2.".
.RE
.IP ""
The value "4." (no background) sets:
.RS 9
.sp
\'s/CONTROL.\' to "0." for all s;
.br
\'s/MAJOR/INWARD.\' to ".015 for all s;
.br
\'s/MINOR/INWARD.\' to ".010" for all s;
.br
\'LABEL/CONTROL.\' to "0.".
.RE
.IP ""
The default values of these thirteen parameters correspond 
to the default value of \'BACKGROUND.\'. Note that, if they 
are changed directly, the value of \'BACKGROUND.\' may not 
reflect the actual nature of the background defined by them.
.IP \'NULL.\'
Simplest form of name: \'NUL.\'
.sp
This group contains the two "nulls" (or "special values") 
\'NULL/1.\' and \'NULL/2.\'.
.IP \'NULL/1.\'
Simplest form of name: \'NUL/1.\'
.sp
A real number "null 1", used in the following ways by 
Autograph:
.RS
.IP \(bu
Certain parameters have by default, or may be given, the 
value "null 1", specifying that the routine AGSTUP is to 
choose values for them. The value chosen for a given 
parameter is not back-stored in place of the "null 1"; 
thus, a unique value will be chosen for each graph drawn.
.IP \(bu
If a curve point specified by the user has X and/or Y 
coordinates equal to "null 1", that curve point is ignored. 
It is not used in computing minimum and maximum values. 
Curve segments on either side of it are not drawn.
.RE
.IP ""
Default value: "1.E36" (an arbitrary value).
.sp
Special action by AGSETP: If \'NULL/1.\' is changed 
(individually, rather than as part of a group) by an AGSETP 
call, the entire list of primary parameters is scanned - 
any value equal to the old "null 1" is replaced by the new 
one.
.IP \'NULL/2.\'
Simplest form of name: \'NUL/2.\'
.sp
A real number "null 2". Certain parameters may be given the 
value "null 2", specifying that the routine AGSTUP is to 
choose values for them. The value chosen for a given 
parameter is back-stored in place of the "null 2"; thus, a 
unique value may be chosen for the first graph of a series 
and then used for all remaining graphs in the series.
.sp
Default value: "2.E36" (an arbitrary value).
.sp
Special action by AGSETP: If \'NULL/2.\' is changed 
(individually, rather than as part of a group) by an AGSETP 
call, the entire list of primary parameters is scanned - 
any value equal to the old "null 2" is replaced by the new 
one.
.IP \'GRAPH.\'
Simplest form of name: \'GRA.\'
.sp
A group of four parameters describing the position of the 
"graph window" within the plotter frame. A graph drawn by 
Autograph (including labels) is forced to lie entirely 
within this window. Subgroups and the number of parameters 
in each are as follows:
.RS 10
.sp
\'GRAPH/LEFT.\'   (1)
.br
\'GRAPH/RIGHT.\'  (1)
.br
\'GRAPH/BOTTOM.\' (1)
.br
\'GRAPH/TOP.\'    (1)
.RE
.IP \'GRAPH/LEFT.\'
Simplest form of name: \'GRA/LE.\'
.sp
A real number between "0." and "1." specifying the position 
of the left edge of the graph window as a fraction of the 
distance from the left edge to the right edge of the 
plotter frame.
.sp
Default value: "0." (left edge of plotter frame).
.IP \'GRAPH/RIGHT.\'
Simplest form of name: \'GRA/RI.\'
.sp
A real number between "0." and "1." specifying the position 
of the right edge of the graph window as a fraction of the 
distance from the left edge to the right edge of the 
plotter frame.
.sp
Default value: "1." (right edge of plotter frame).
.IP \'GRAPH/BOTTOM.\'
Simplest form of name: \'GRA/BO.\'
.sp
A real number between "0." and "1." specifying the position 
of the bottom edge of the graph window as a fraction of the 
distance from the bottom edge to the top edge of the 
plotter frame.
.sp
Default value: "0." (bottom edge of plotter frame).
.IP \'GRAPH/TOP.\'
Simplest form of name: \'GRA/TO.\'
.sp
A real number between "0." and "1." specifying the position 
of the top edge of the graph window as a fraction of the 
distance from the bottom edge to the top edge of the 
plotter frame.
.sp
Default value: "0." (top edge of plotter frame).
.IP \'GRID.\'
Simplest form of name: \'GRI.\'
.sp
A group of five parameters describing the position and 
shape of the "grid window" within the graph window. 
Subgroups and the number of parameters in each are as 
follows:
.RS 10
.sp
\'GRID/LEFT.\'   (1)
.br
\'GRID/RIGHT.\'  (1)
.br
\'GRID/BOTTOM.\' (1)
.br
\'GRID/TOP.\'    (1)
.br
\'GRID/SHAPE.\'  (1)
.RE
.IP \'GRID/LEFT.\'
Simplest form of name: \'GRI/LE.\'
.sp
A real number between "0." and "1." specifying the position 
of the left edge of the area in which the grid window is to 
be placed, stated as a fraction of the distance from the 
left edge to the right edge of the graph window.
.sp
Default value: ".15".
.IP \'GRID/RIGHT.\'
Simplest form of name: \'GRI/RI.\'
.sp
A real number between "0." and "1." specifying the position 
of the right edge of the area in which the grid window is 
to be placed, stated as a fraction of the distance from the 
left edge to the right edge of the graph window.
.sp
Default value: ".95".
.IP \'GRID/BOTTOM.\'
Simplest form of name: \'GRI/BO.\'
.sp
A real number between "0." and "1." specifying the position 
of the bottom edge of the area in which the grid window is 
to be placed, stated as a fraction of the distance from the 
bottom edge to the top edge of the graph window.
.sp
Default value: ".15".
.IP \'GRID/TOP.\'
Simplest form of name: \'GRI/TO.\'
.sp
A real number between "0." and "1." specifying the position 
of the top edge of the area in which the grid window is to 
be placed, stated as a fraction of the distance from the 
bottom edge to the top edge of the graph window.
.sp
Default value: ".95".
.IP \'GRID/SHAPE.\'
Simplest form of name: \'GRI/SH.\'
.sp
A real number specifying the shape of the grid window. The 
grid window, whatever its shape, is centered in, and made 
as large as possible in, the area specified by the first 
four parameters in the group \'GRID.\'. The value of 
\'GRID/SHAPE.\' falls in one of four possible ranges, as follows:
.RS
.IP \(bu
A value less than "0." specifies the negative of the 
desired ratio of the grid window\'s width to its height. For 
example, the value "-2." specifies a grid window which is 
twice as wide as it is high.
.IP \(bu
The value "0." specifies a grid window of exactly the same 
shape as the area specified by the first four parameters in 
the group \'GRID.\'. The grid window therefore fills that 
area completely.
.IP \(bu
A value "s" between "0." and "1." specifies a grid window 
whose shape is determined by the range of the user\'s 
coordinate data, reverting to the shape of the area 
specified by the first four arguments in the group \'GRID.\' 
if the ratio of the shorter side of the grid window to the 
longer side of the grid window would thereby be made less 
than "s". For example, if "s" were given the value ".5" and 
the user X coordinate data ranged in value from "0." to 
"10." and the user Y coordinate data ranged in value from 
"0." to "15.", the grid window would be made two-thirds as 
wide as it was high; however, if the Y coordinate data 
ranged in value from "0." to "100.", the grid window would 
not be made one-tenth as wide as it is high, but would 
instead be made to fill the entire area specified by the 
first four arguments of the group \'GRID.\'.
.IP \(bu
A value "s" greater than or equal to "1." specifies a grid 
window whose shape is determined by the range of the user\'s 
coordinate data, reverting to a square if the ratio of the 
longer side of the grid window to the shorter side of the 
grid window would thereby be made greater than "s".
.RE
.IP ""
Note that, if \'GRID/SHAPE.\' is given a value greater than 
"0.", Autograph assumes that the user\'s X and Y coordinate 
data have the same units (both in inches, for example) and 
that the outline of a real two-dimensional object is to be 
graphed without distortion. The grid window is shaped in 
such a way as to accomplish this. This feature should not 
be used when either \'X/LOGARITHMIC.\' or \'Y/LOGARITHMIC.\' 
has a non-zero value; doing so will yield strange results.
.sp
Note that either "-1." or "+1." produces a square and that 
"-1.61803398874989" produces a golden rectangle.
.sp
Default value: "0.".
.IP \'X.\'
.br
Simplest form of name: \'X.\'
.sp
A group of seven parameters specifying the mapping of the 
user\'s X-coordinate data onto the horizontal axis of the 
grid window. Subgroups and the number of parameters in each 
are as follows:
.RS 10
.sp
\'X/MINIMUM.\'     (1)
.br
\'X/MAXIMUM.\'     (1)
.br
\'X/LOGARITHMIC.\' (1) 
.br
\'X/ORDER.\'       (1)
.br
\'X/NICE.\'        (1)
.br
\'X/SMALLEST.\'    (1)
.br
\'X/LARGEST.\'     (1)
.RE
.IP ""
See also \'SET.\' and \'INVERT.\', above.
.IP \'X/MINIMUM.\'
Simplest form of name: \'X/MI.\'
.sp
A real number specifying the minimum user X coordinate to 
be considered. This parameter normally has the value "null 
1", specifying that the routine AGSTUP should examine the 
user\'s X-coordinate data and find the minimum value for 
itself.
.sp
If the value "null 2" is used, it will be replaced, the 
next time AGSTUP is called, by an actual minimum value 
computed by AGSTUP.
.sp
If a non-null value is used, AGSTUP will not examine the 
user\'s X-coordinate data; the given value will be 
considered to be the minimum.
.sp
If both \'X/MINIMUM.\' and \'X/MAXIMUM.\' are given non-null 
values, the former should have a lesser value than the 
latter.
.sp
Default value: "1.E36" ("null 1").
.IP \'X/MAXIMUM.\'
Simplest form of name: \'X/MA.\'
.sp
Analogous to \'X/MINIMUM.\', above; it specifies the way in 
which the maximum X coordinate is to be determined.
.sp
Default value: "1.E36" ("null 1").
.IP \'X/LOGARITHMIC.\'
Simplest form of name: \'X/LO.\'
.sp
An integral real number having one of the values "-1.", 
"0.", or "+1.":
.RS
.IP \(bu
The value "0." specifies that the mapping of user X 
coordinates onto the horizontal axis of the grid window is 
to be linear.
.IP \(bu
The values "-1." and "+1." specify that the mapping is to 
be logarithmic, in which case all user X-coordinate data 
must be greater than zero. 
.IP \(bu
The value "-1." is immune to change when \'SET.\' (which see, 
above) is reset; the value "+1." is not.
.RE 
.IP ""
Default value: "0." (linear X mapping).
.IP \'X/ORDER.\'
Simplest form of name: \'X/OR.\'
.sp
An integral real number having one of the values "0." or 
"1.":
.RS
.IP \(bu
The value "0." specifies that the values of user X 
coordinates mapped to the horizontal axis of the grid 
window should increase from left to right.
.IP \(bu
The value "1." specifies that user X coordinates should 
decrease from left to right.
.RE
.IP ""
Default value: "0." (increase from left to right).
.IP \'X/NICE.\'
Simplest form of name: \'X/NI.\'
.sp
An integral real number having one of the values "-1.", 
"0.", or "+1.":
.RS
.IP \(bu
The value "-1." specifies that user X-coordinate data are 
to be mapped onto the horizontal axis of the grid window in 
such a way as to force major-tick positions at the 
endpoints of the bottom X axis.
.IP \(bu
The value "+1." specifies that user X-coordinate data are 
to be mapped onto the horizontal axis of the grid window in 
such a way as to force major-tick positions at the 
endpoints of the top X axis.
.IP \(bu
The value "0." specifies that the X-coordinate data are to 
be mapped so as to range from the left edge of the grid 
window to the right edge of the grid window; major-tick 
positions are not forced at the ends of either X axis.
.RE
.IP ""
Default value: "-1." (bottom axis "nice").
.IP \'X/SMALLEST.\'
Simplest form of name: \'X/SM.\'
.sp
This parameter comes into play when AGSTUP is called upon 
to compute the minimum X coordinate (when \'X/MINIMUM.\' has 
a null value); if the value of \'X/SMALLEST.\' is non-null, 
values less than it will not be considered in the 
computation.
.sp
Default value: "1.E36" ("null 1").
.IP \'X/LARGEST.\'
Simplest form of name: \'X/LA.\'
.sp
This parameter comes into play when AGSTUP is called upon 
to compute the maximum X coordinate (when \'X/MAXIMUM.\' has 
a null value); if the value of \'X/LARGEST.\' is non-null, 
values greater than it will not be considered in the 
computation.
.sp
Default value: "1.E36" ("null 1").
.IP \'Y.\'
.br
Simplest form of name: \'Y.\'
.sp
A group of seven parameters specifying the mapping of the 
user\'s Y-coordinate data onto the vertical axis of the grid 
window. Subgroups and the number of parameters in each are 
as follows:
.RS 10
.sp
\'Y/MINIMUM.\'     (1)
.br
\'Y/MAXIMUM.\'     (1)
.br
\'Y/LOGARITHMIC.\' (1)
.br
\'Y/ORDER.\'       (1)
.br
\'Y/NICE.\'        (1)
.br
\'Y/SMALLEST.\'    (1)
.br
\'Y/LARGEST.\'     (1)
.RE
.IP ""
See also \'SET.\' and \'INVERT.\', above.
.IP \'Y/MINIMUM.\'
Simplest form of name: \'Y/MI.\'
.sp
Analogous to \'X/MINIMUM.\', above; it specifies the way in 
which the minimum Y coordinate is to be determined.
.sp
Default value: "1.E36" ("null 1").
.IP \'Y/MAXIMUM.\'
Simplest form of name: \'Y/MA.\'
.sp
Analogous to \'X/MAXIMUM.\', above; it specifies the way in 
which the maximum Y coordinate is to be determined.
.sp
Default value: "1.E36" ("null 1").
.IP \'Y/LOGARITHMIC.\'
Simplest form of name: \'Y/LO.\'
.sp
Analogous to \'X/LOGARITHMIC.\', above; it specifies whether 
the mapping of Y coordinates is linear or logarithmic.
.sp
Default value: "0." (linear Y).
.IP \'Y/ORDER.\'
Simplest form of name: \'Y/OR.\'
.sp
Analogous to \'X/ORDER.\', above; it specifies whether 
Y-coordinates increase or decrease from bottom to top.
.sp
Default value: "0." (increase from bottom to top).
.IP \'Y/NICE.\'
Simplest form of name: \'Y/NI.\'
.sp
Analogous to \'X/NICE.\', above; it specifies whether the 
left Y axis, the right Y axis, or neither, is to be "nice".
.sp
Default value: "-1." (left axis "nice").
.IP \'Y/SMALLEST.\'
Simplest form of name: \'Y/SM.\'
.sp
Analogous to \'X/SMALLEST.\', above; comes into play when 
AGSTUP is called upon to compute the minimum Y coordinate.
.sp
Default value: "1.E36" ("null 1").
.IP \'Y/LARGEST.\'
Simplest form of name: \'Y/LA.\'
.sp
Analogous to \'X/LARGEST.\', above; comes into play when 
AGSTUP is called upon to compute the maximum Y coordinate.
.sp
Default value: "1.E36" ("null 1").
.IP \'AXIS.\'
Simplest form of name: \'AXI.\'
.sp
A group of 92 parameters describing four axes: the left 
axis, the right axis, the bottom axis, and the top axis. 
Subgroups and the number of parameters in each are as 
follows:
.RS 10
.sp
\'[AXIS/]LEFT.\'   (23)
.br
\'[AXIS/]RIGHT.\'  (23)
.br
\'[AXIS/]BOTTOM.\' (23)
.br
\'[AXIS/]TOP.\'    (23)
.RE
.IP ""
The elements of the subgroups are interleaved in the group; 
that is to say, the first elements of the four subgroups 
constitute elements 1 through 4 of the group, the second 
elements of the four subgroups constitute elements 5 
through 8 of the group, and so on.
.IP \'[AXIS/]s.\'
(where "s" means "any one of the keywords LEFT, RIGHT, 
BOTTOM, or TOP".)
.sp
Simplest form of name: \'s.\'
.sp
A group of 23 parameters describing the axis specified by 
"s". Subgroups and the number of parameters in each are as 
follows:
.RS 10
.sp
\'[AXIS/]s/CONTROL.\'      (1)
.br
\'[AXIS/]s/LINE.\'         (1)
.br
\'[AXIS/]s/INTERSECTION.\' (2)
.br
\'[AXIS/]s/FUNCTION.\'     (1)
.br
\'[AXIS/]s/TICKS.\'       (10)
.br
\'[AXIS/]s/NUMERIC.\'      (8)
.RE
.IP \'[AXIS/]s/CONTROL.\'
Simplest form of name: \'s/CO.\'
.sp
An integral real number having one of the values "-1.", 
"0.", "1.", "2.", "3.", or "4." and controlling certain 
aspects of the drawing of the axis specified by "s", as 
follows:
.RS
.IP \(bu
The value "-1." specifies that only the line portion of the 
axis may be drawn; tick marks and numeric labels are 
suppressed.
.IP \(bu
The value "0." specifies that no portion of the axis may be 
drawn.
.IP \(bu
A positive value specifies that all portions of the axis 
may be drawn and specifies what actions Autograph may take 
to prevent numeric-label overlap problems, as follows:
.IP \(bu
The value "1." specifies that numeric labels may not be 
shrunk or rotated.
.IP \(bu
The value "2." specifies that numeric labels may be shrunk, 
but not rotated.
.IP \(bu
The value "3." specifies that numeric labels may be 
rotated, but not shrunk.
.IP \(bu
The value "4." specifies that numeric labels may be both 
shrunk and/or rotated.
.RE
.IP ""
Default value: "4." for all "s" (all axes drawn, numeric 
labels may be shrunk and/or rotated).
.IP \'[AXIS/]s/LINE.\'
Simplest form of name: \'s/LI.\'
.sp
An integral real number having one of the values "0." or 
"1.":
.RS
.IP \(bu
The value "0." specifies that the line portion of the axis 
specified by "s" may be drawn.
.IP \(bu
The value "1." suppresses the line portion of the axis 
specified by "s".
.RE
.IP ""
Default value: "0." for all "s" (line portions of all axes 
may be drawn).
.IP \'[AXIS/]s/INTERSECTION.\'
Simplest form of name: \'s/IN.\'
.sp
A group of two parameters
.RS 10
.sp
\'[AXIS/]s/INTERSECTION/GRID.\'
.br
\'[AXIS/]s/INTERSECTION/USER.\'
.RE
.IP ""
each having the default value "1.E36" ("null 1"). Giving 
either of them a non-null value causes the axis specified 
by "s" to be moved away from its normal position on one 
edge of the grid window. If both are given non-null values, 
\'.../USER.\' takes precedence over \'.../GRID.\'.
.sp
If the left Y axis or the right Y axis is moved, it remains 
vertical, but intersects the bottom of the grid window at a 
specified X coordinate. Similarly, if the bottom X axis or 
the top X axis is moved, it remains horizontal, but 
intersects the left edge of the grid at a specified Y 
coordinate.
.sp
No axis may be moved outside the current graph window; if 
an attempt is made to do so, the axis is moved as far as 
the edge and no farther.
.IP \'[AXIS/]s/INTERSECTION/GRID.\'
Simplest form of name: \'s/IN/GR.\'
.sp
A real number which, if not equal to the current "null 1", 
specifies, in the grid coordinate system, the X coordinate 
(if "s" = "LEFT" or "RIGHT") or the Y coordinate (if "s" = 
"BOTTOM" or "TOP") of the point of intersection of the axis 
specified by "s" with the perpendicular sides of the grid 
window.
.sp
Default value: "1.E36" ("null 1") for all "s" (axes lie on 
the edges of the grid window).
.IP \'[AXIS/]s/INTERSECTION/USER.\'
Simplest form of name: \'s/IN/US.\'
.sp
A real number which, if not equal to the current "null 1", 
specifies, in the user coordinate system, the X coordinate 
(if "s" = "LEFT" or "RIGHT") or the Y coordinate (if "s" = 
"BOTTOM" or "TOP") of the point of intersection of the axis 
specified by "s" with the perpendicular sides of the grid 
window.
.sp
Default value: "1.E36" ("null 1") for all "s" (axes lie on 
the edges of the grid window).
.IP \'[AXIS/]s/FUNCTION.\'
Simplest form of name: \'s/FU.\'
.sp
A real number, passed as an argument to the subroutine 
AGUTOL; this subroutine defines the user-system-to-label-system
mappings, and thus the label coordinate systems, for 
all the axes. The default version of AGUTOL defines the 
identity mapping for all axes; a user version may be 
substituted to define any desired set of mappings. It is 
intended that \'AXIS/s/FUNCTION.\' be used within a user 
version of AGUTOL as a function selector. It is further 
recommended that the value "0." select the identity 
mapping, thus providing a way to re-create the default 
situation.
.sp
Tick marks on the axis specified by "s" are positioned in 
the label coordinate system. Numeric labels on the axis 
give values in the label coordinate system.
.sp
See the AGUTOL man page.
.sp
Default value: "0." for all "s" (identity mapping for all 
axes).
.IP \'[AXIS/]s/TICKS.\'
Simplest form of name: \'s/TI.\'
.sp
A group of ten parameters describing the tick marks, if 
any, which are to be a part of the axis specified by "s". 
Subgroups and the number of parameters in each are as 
follows:
.RS 10
.sp
\'[AXIS/]s/[TICKS/]MAJOR.\' (6)
.br
\'[AXIS/]s/[TICKS/]MINOR.\' (4)
.RE
.IP \'[AXIS/]s/[TICKS/]MAJOR.\'
Simplest form of name: \'s/MA.\'
.sp
A group of six parameters describing the major tick marks, 
if any, which are to be a part of the axis specified by 
"s". Subgroups and the number of parameters in each are as 
follows:
.RS 10
.sp
\'[AXIS/]s/[TICKS/]MAJOR/SPACING.\' (3)
.br
\'[AXIS/]s/[TICKS/]MAJOR/PATTERN.\' (1)
.br
\'[AXIS/]s/[TICKS/]MAJOR/LENGTH.\' (2)
.RE
.IP \'[AXIS/]s/[TICKS/]MAJOR/SPACING.\'
Simplest form of name: \'s/MA/SP.\'
.sp
A group of three parameters describing the way in which 
major tick marks, if any, are to be spaced along the axis 
specified by "s". Subgroups and the number of parameters in 
each are as follows:
.RS 10
.sp
\'[AXIS/]s/[TICKS/]MAJOR/[SPACING/]TYPE.\'  (1)
.br
\'[AXIS/]s/[TICKS/]MAJOR/[SPACING/]BASE.\'  (1)
.br
\'[AXIS/]s/[TICKS/]MAJOR/[SPACING/]COUNT.\' (1)
.RE
.IP \'[AXIS/]s/[TICKS/]MAJOR/[SPACING/]TYPE.\'
Simplest form of name: \'s/MA/TY.\'
.sp
A real number specifying where major tick marks are to be 
placed along the axis specified by "s" (that is to say, at 
what values in the label coordinate system along that 
axis). Let "b" represent the value of the parameter 
\'.../BASE.\' (described next) and "k" represent an arbitrary 
integer. Then, there are six acceptable values of 
\'.../TYPE.\':
.RS
.IP \(bu
The value "0." specifies that no major tick marks are to be 
drawn on the axis.
.IP \(bu
The value "1." specifies major tick marks at values of the 
form plus or minus b times k.
.IP \(bu
The value "2." specifies major tick marks at values of the 
form plus or minus b times 10 to the power k.
.IP \(bu
The value "3." specifies major tick marks at values of the 
form plus or minus b to the power k.
.IP \(bu
The value "null 1" specifies that Autograph should use a 
value "1.", "2.", or "3." - whichever it considers best.
.IP \(bu
The value "null 2" specifies that Autograph should use a 
value "1.", "2.", or "3." - whichever it considers best - 
and replace the "null 2" by that value.
.RE
.IP ""
Notice that major tick marks on a linear axis may be spaced 
logarithmically and that major tick marks on a logarithmic 
axis may be spaced linearly; this is sometimes useful.
.sp
Default value: "1.E36" ("null 1") for all "s" (Autograph 
spaces major tick marks as it sees fit).
.IP \'[AXIS/]s/[TICKS/]MAJOR/[SPACING/]BASE.\'
Simplest form of name: \'s/MA/BA.\'
.sp
A real number which, if greater than zero and non-null, 
specifies the base value ("b", in the preceding parameter 
description) used in spacing major tick marks in the label 
coordinate system along the axis specified by "s". A 
negative or zero value suppresses major tick marks on the 
axis. A null value causes Autograph to pick an appropriate 
base value and, if the null was a "null 2", to backstore 
that value in place of the "null 2".
.sp
Default value: "1.E36" ("null 1") for all "s" (Autograph 
picks the base values).
.IP \'[AXIS/]s/[TICKS/]MAJOR/[SPACING/]COUNT.\'
Simplest form of name: \'s/MA/CO.\'
.sp
A real number, having an integral value "n" greater than or 
equal to 0. A negative value is treated as if it were a 
zero. The value n is only used when major tick marks are to 
be spaced linearly and the base value ("b", in the 
preceding parameter descriptions) is to be chosen by 
Autograph. In this case, n is a rough estimate of the 
minimum number of major tick marks to be placed on the axis 
specified by "s". The actual number used may vary between 
"n+2" and "5n/2+4" (approximately).
.sp
Default value: "6." for all "s" (somewhere between 8 and 19 
major tick marks per linear axis).
.IP \'[AXIS/]s/[TICKS/]MAJOR/PATTERN.\'
Simplest form of name: \'s/MA/PA.\'
.sp
A real number specifying the dashed-line pattern to be used 
for major tick marks on the axis specified by "s". 
Normally, its integer equivalent is a 16-bit integer in 
which "0" bits specify "pen-up" segments (gaps) 3 plotter 
units long and "1" bits specify "pen-down" segments 
(solids) 3 plotter units long. The value "0." turns off the 
major tick marks, the value "65535." (decimal) = "177777." 
(octal) makes them solid lines. If the value "null 1" is 
used, the next call to AGSTUP resets it to "65535." 
(decimal).
.sp
Default value: "1.E36" ("null 1") for all "s" (solid-line 
patterns).
.IP \'[AXIS/]s/[TICKS/]MAJOR/LENGTH.\'
Simplest form of name: \'s/MA/LE.\'
.sp
A group of two parameters determining the length of the 
outward-pointing and inward-pointing portions of the major 
tick marks on the axis specified by "s". Subgroups and the 
number of parameters in each are as follows:
.RS 10
.sp
\'[AXIS/]s/[TICKS/]MAJOR/[LENGTH/]OUTWARD.\' (1)
.br
\'[AXIS/]s/[TICKS/]MAJOR/[LENGTH/]INWARD.\'  (1)
.RE
.IP \'[AXIS/]s/[TICKS/]MAJOR/[LENGTH/]OUTWARD.\'
Simplest form of name: \'s/MA/OU.\'
.sp
A real number specifying the length of the outward-pointing 
portion of each major tick mark on the axis specified by 
"s". The value must be of the form "e", "1.+e", or "-e", 
where "e" is greater than or equal to "0." and less than 
"1." and represents a fraction of the smaller dimension of 
the grid window.
.sp
Note:  "Outward" is defined relative to the normal position 
of the axis "s", even when that axis has been moved away 
from its normal position.
.RS
.IP \(bu
When a value "e" is used, each major tick mark extends 
outward "e" units from the axis.
.IP \(bu
When a value "1.+e" is used, each major tick mark extends 
outward to the farther edge of the grid window and then "e" 
units beyond that edge. (If the axis is not moved away from 
its normal position, "1.+e" has the same effect as "e".) 
.IP \(bu
When a value "-e" is used, the first "e" units of the 
inward-pointing portion of each major tick mark are erased. 
(This can be used to create off-axis major tick marks - for 
whatever that may be worth.)
.RE
.IP ""
Default value: "0." for all "s" (all major ticks point 
inward).
.IP \'[AXIS/]s/[TICKS/]MAJOR/[LENGTH/]INWARD.\'
Simplest form of name: \'s/MA/IN.\'
.sp
A real number specifying the length of the inward-pointing 
portion of each tick mark on the axis specified by "s". The 
value must be of the form "e", "1.+e", or "-e", where e is 
greater than or equal to "0." and less than "1." and 
represents a fraction of the smaller dimension of the grid 
window.
.sp
Note:  "Inward" is defined relative to the normal position 
of the axis "s", even when that axis has been moved away 
from its normal position.
.RS
.IP \(bu
When a value "e" is used, each major tick mark extends 
inward "e" units from the axis.
.IP \(bu
When a value "1.+e" is used, each major tick mark extends 
inward to the farther edge of the grid window and then "e" 
units beyond that edge. This feature is used to create grid 
backgrounds.
.IP \(bu
When a value "-e" is used, the first "e" units of the
outward-pointing portion of each major tick mark are erased.
.RE
.IP ""
Default value: ".015" for all "s" (all major ticks point 
inward).
.IP \'[AXIS/]s/[TICKS/]MINOR.\'
Simplest form of name: \'s/MI.\'
.sp
A group of four parameters describing the minor tick marks, 
if any, which are to be a part of the axis specified by 
"s". Subgroups and the number of parameters in each are as 
follows:
.RS 10
.sp
\'[AXIS/]s/[TICKS/]MINOR/SPACING.\' (1)
.br
\'[AXIS/]s/[TICKS/]MINOR/PATTERN.\' (1)
.br
\'[AXIS/]s/[TICKS/]MINOR/LENGTH.\'  (2)
.RE
.IP \'[AXIS/]s/[TICKS/]MINOR/SPACING.\'
Simplest form of name: \'s/MI/SP.\'
.sp
A real number specifying the desired number of minor tick 
marks to be distributed between each pair of major tick 
marks on the axis specified by "s". Acceptable values are 
as follows:
.RS 10
.IP \(bu
A value less than "1." suppresses minor tick marks 
completely.
.IP \(bu
A value greater than or equal to "1." which is non-null 
should be integral; it specifies the number of minor tick 
marks directly.
.IP \(bu
The values "null 1" and "null 2" specify that Autograph is 
to choose a reasonable integral value; if a "null 2" is 
specified, it is replaced by the integral value chosen.
.RE
.IP ""
The minor tick marks, if any, are spaced linearly in the 
label coordinate system along the axis specified by "s". 
Note that the appropriate value for the usual sort of 
logarithmic axis is "8."; this causes the minor tick marks 
between two major tick marks at label-system values 10**n and 
10**n+1 to be placed at the label-system values 2*10**n, 3*10**n, 
4*10**n, . . ., 9*10**n.
.sp
Default value: "1.E36" ("null 1") for all "s" (Autograph 
chooses appropriate values).
.IP \'[AXIS/]s/[TICKS/]MINOR/PATTERN.\'
Simplest form of name: \'s/MI/PA.\'
.sp
A real number specifying the dashed-line pattern to be used 
for minor tick marks on the axis specified by "s"; 
analogous to \'[AXIS/]s/[TICKS/]MAJOR/PATTERN.\', described 
above.
.sp
Default value: "1.E36" ("null 1") for all "s" (solid-line 
patterns).
.IP \'[AXIS/]s/[TICKS/]MINOR/LENGTH.\'
Simplest form of name: \'s/MI/LE.\'
.sp
A group of two parameters determining the length of the 
outward-pointing and inward-pointing portions of the minor 
tick marks on the axis specified by "s". Subgroups and the 
number of parameters in each are as follows:
.RS 10
.sp
\'[AXIS/]s/[TICKS/]MINOR/[LENGTH/]OUTWARD.\' (1)
.br
\'[AXIS/]s/[TICKS/]MINOR/[LENGTH/]INWARD.\'  (1)
.RE
.IP \'[AXIS/]s/[TICKS/]MINOR/[LENGTH/]OUTWARD.\'
Simplest form of name: \'s/MI/OU.\'
.sp
A real number specifying the length of the outward-pointing 
portion of each minor tick mark on the axis specified by 
"s"; analogous to \'...MAJOR/[LENGTH/]OUTWARD.\', described 
above.
.sp
Default value: "0." for all "s" (all minor ticks point 
inward).
.IP \'[AXIS/]s/[TICKS/]MINOR/[LENGTH/]INWARD.\'
Simplest form of name: \'s/MI/IN.\'
.sp
A real number specifying the length of the inward-pointing 
portion of each minor tick mark on the axis specified by 
"s"; analogous to \'...MAJOR/[LENGTH/]INWARD.\', described 
above.
.sp
Default value: ".010" for all "s" (all minor ticks point 
inward).
.IP \'[AXIS/]s/NUMERIC.\'
Simplest form of name: \'s/NU.\'
.sp
A group of eight parameters describing the numeric labels, 
if any, which are to be a part of the axis specified by 
"s". Subgroups and the number of parameters in each are as 
follows:
.RS 10
.sp
\'[AXIS/]s/[NUMERIC/]TYPE.\'     (1)
.br
\'[AXIS/]s/[NUMERIC/]EXPONENT.\' (1)
.br
\'[AXIS/]s/[NUMERIC/]FRACTION.\' (1)
.br
\'[AXIS/]s/[NUMERIC/]ANGLE.\'    (2)
.br
\'[AXIS/]s/[NUMERIC/]OFFSET.\'   (1)
.br
\'[AXIS/]s/[NUMERIC/]WIDTH.\'    (2)
.RE
.IP \'[AXIS/]s/[NUMERIC/]TYPE.\'
Simplest form of name: \'s/TY.\'
.sp
The three parameters
.RS 10
.sp
\'[AXIS/]s/[NUMERIC/]TYPE.\'
.br
\'[AXIS/]s/[NUMERIC/]EXPONENT.\'
.br
\'[AXIS/]s/[NUMERIC/]FRACTION.\'
.RE
.IP
will be described together, because they are so closely 
interdependent. They specify the type of numeric labels to 
be used (at major-tick positions) on the axis specified by 
"s". A fourth parameter,
.RS 10
.sp
\'[AXIS/]s/[TICKS/]MAJOR/[SPACING/]TYPE.\',
.RE
.IP ""
described above, also affects the type of numeric labels to 
be used. I shall refer to these four parameters in the 
ensuing discussion using short forms of their names (\'s/
TYPE.\', \'s/EXPO.\', \'s/FRAC.\', and \'s/MAJOR/TYPE.\', 
respectively).
.sp
All four have the default value "null 1" (except for the 
first, which has the default value "0." for "s" = "RIGHT" 
and "TOP"), leaving Autograph free to choose values which 
are consistent with each other and with other parameters 
describing the axis specified by "s". Any one or more of 
them may be given the value "null 2" (in which case an 
actual value chosen by Autograph is backstored over the 
"null 2") or an actual integral real value. 
.sp
Setting \'s/TYPE.\' to "0.":
.sp
This turns off the numeric labels on the axis specified by 
"s". The other three parameters are then ignored.
.sp
Setting \'s/TYPE.\' to "1.":
.sp
This selects "scientific" notation. Each numeric label is 
written in the form
.RS 10
.sp
[-] [i] [.] [f] x 10 e
.RE
.IP ""
where brackets enclose portions which may be independently
present or absent and "e" is a superscript exponent.
.sp
The parameter \'s/EXPO.\' specifies the length of "i" (the 
number of characters), thus also specifying the value of 
the exponent "e". If \'s/EXPO.\' has a value less than or 
equal to zero, "i" is omitted. If \'s/EXPO.\' is less than 
zero and has the integral absolute value "n", the fraction 
"f" is forced to have "n" leading zeroes.
.sp
The parameter \'s/FRAC.\' specifies the length of "f" (the 
number of characters). If \'s/FRAC.\' is less than or equal 
to zero, "f" is omitted. If \'s/FRAC.\' is less than zero, 
the decimal point is omitted.
.sp
If "[i] [.] [f]" has the value "1.", the first part of the 
label is omitted, leaving only "10 e".
.sp
If the entire label has the value "0.", the single 
character "0" is used.
.sp
The value of \'s/MAJOR/TYPE.\' is immaterial.
.sp
Setting \'s/TYPE.\' to "2.":
.sp
This selects "exponential" notation, the exact nature of 
which depends on the value of \'s/MAJOR/TYPE.\', as follows:
.RS 
.IP \(bu
If \'s/MAJOR/TYPE.\' has the value "1." (all major ticks at 
values of the form plus or minus b times k), each numeric 
label is written in the form
.in +10
.sp
   [-] [i] [.] [f] x 10 e
.in -10
.sp
where brackets enclose portions which may be independently 
present or absent and "e" is a superscript exponent.
.sp
The parameter \'s/EXPO.\' specifies the integral value of the 
exponent "e".
.sp
The parameter \'s/FRAC.\' specifies the length of "f" (the 
number of characters). If \'s/FRAC.\' is less than or equal 
to zero, f is omitted. If \'s/FRAC.\' is less than zero, the 
decimal point is omitted.
.sp
If the label value is exactly zero, the single character 
"0" is used.
.IP \(bu
If \'s/MAJOR/TYPE.\' has the value "2." (all major ticks at 
values of the form plus or minus b times 10 to the power 
k), each numeric label is written in the form
.sp
.in +10
   [-] [i] [.] [f] x 10 e
.sp
.in -10
where brackets enclose portions which may be independently 
present or absent and "e" is a superscript exponent.
.sp
The parameter \'s/EXPO.\' specifies the integral value of the 
exponent "e" when "k" equals "0." The value of "e" is \'s/EXPO.\'
plus "k".
.sp
The parameter \'s/FRAC.\' specifies the length of "f" (the 
number of characters). If \'s/FRAC.\' is less than or equal 
to zero, "f" is omitted. If \'s/FRAC.\' is less than zero, 
the decimal point is omitted.
.sp
If the label value is exactly zero, the single character 
"0" is used.
.IP \(bu
If \'s/MAJOR/TYPE.\' has the value "3." (all major ticks at 
values of the form plus or minus b to the power k), each 
numeric label is written in the form
.sp
.in +10
   [-] [i] [.] [f] e
.sp
.in -10
where brackets enclose portions which may be independently 
present or absent and "e" is a superscript exponent.
.sp
The parameter \'s/EXPO.\' is ignored. The value of "e" is "k".
.sp
The parameter \'s/FRAC.\' specifies the length of "f" (the 
number of characters). If \'s/FRAC.\' is less than or equal 
to zero, "f" is omitted. If \'s/FRAC.\' is less than zero, 
the decimal point is omitted.
.sp
Note that "[i] [.] [f]" expresses the value of "b".
.RE
.IP ""
Setting \'s/TYPE.\' to "3.":
.sp
This selects "no-exponent" notation, the exact nature of 
which depends on the value of \'s/MAJOR/TYPE.\', as follows:
.RS
.IP \(bu
If \'s/MAJOR/TYPE.\' has the value "1." (all major ticks at 
values of the form plus or minus b times k), each numeric 
label is written in the form
.sp
.in +10
   [-] [i] [.] [f]
.sp
.in -10
where brackets enclose portions which may be independently 
present or absent.
.sp
The parameter \'s/EXPO.\' is ignored.
.sp
The parameter \'s/FRAC.\' specifies the length of "f" (the 
number of characters). If \'s/FRAC.\' is less than or equal 
to zero, "f" is omitted. If \'s/FRAC.\' is less than zero, 
the decimal point is omitted.
.sp
If the label value is exactly zero, the single character 
"0" is used.
.IP \(bu
If \'s/MAJOR/TYPE.\' has the value "2." (all major ticks at 
values of the form plus or minus b times 10 to the power 
k), each numeric label is written in the form
.sp
.in +10
   [-] [i] [.] [f]
.sp
.in -10
where brackets enclose portions which may be independently 
present or absent.
.sp
The parameter \'s/EXPO.\' is ignored.
.sp
The length of "f" (the number of characters) is specified 
by the function
.sp
.in +10
   MAX(\'s/FRAC.\',0) - k
.sp
.in -10
if this quantity is greater than zero, and
.sp
.in +10
   MIN(\'s/FRAC.\',0)
.sp
.in -10
otherwise. This may appear somewhat formidable, but it 
produces a simple, desirable result. Suppose, for example, 
that \'s/FRAC.\' = "1.", "b" = "3.6", and "k" ranges from 
"-3" to "+3"; the labels produced are
.sp
.in +4 
\&.0036, .036, .36, 3.6, 36., 360., and 3600.
.sp
.in -4
The parameter \'s/FRAC.\' may be viewed as specifying the 
length of "f" when "k" is zero. If the function value is 
less than or equal to zero, "f" is omitted; if it is less 
than zero, the decimal point is omitted.
.IP \(bu
If \'s/MAJOR/TYPE.\' has the value "3." (all major ticks at 
values of the form plus or minus b to the power k), each 
numeric label is written in the form
.sp
.in +10
   [-] [i] [.] [f]
.sp
.in -10
if "k" is greater than or equal to zero, and in the form
.sp
.in +10
   [-] 1/ [i] [.] [f]
.sp
.in -10
if "k" is less than zero. Brackets enclose portions which 
may be independently present or absent.
.sp
The parameter \'s/EXPO.\' is ignored.
.sp
The length of "f" (the number of characters) is specified 
by the function
.sp
.in +10
   \'s/FRAC.\' * ABS(k)
.sp
.in -10
if "k" is non-zero, or
.sp
.in +10
   MIN(\'s/FRAC.\',0)
.sp
.in -10
if "k" is zero. Again, this function produces a simple 
result. Suppose that \'s/FRAC.\' = "1.", "b" = "1.1", and "k" 
ranges from "-3" to "+3"; the labels produced are
.sp
.in +4
1/1.331, 1/1.21, 1/1.1, 1., 1.1, 1.21, and 1.331
.sp
.in -4
The parameter \'s/FRAC.\' may be viewed as specifying the 
length of "f" when "k" is equal to 1. If the function value 
is less than or equal to zero, "f" is omitted; if it is 
less than zero, the decimal point is omitted.
.sp
Another example: Suppose \'s/FRAC.\' = "-1.", "b" = "2.", and 
"k" ranges from "-4" to "+4"; the labels produced are
.sp
.in +4
1/16, 1/8, 1/4, 1/2, 1, 2, 4, 8, and 16
.sp
.in -4
Default value: "1.E36" ("null 1") for all three for all "s" 
(Autograph chooses values to use), except for
.sp
.in +10
\'RIGHT/[NUMERIC/]TYPE.\' and
.br
\'TOP/[NUMERIC/]TYPE.\',
.sp
.in -10
which are zeroed to suppress the numeric labels on the 
right and top axes.
.RE
.IP \'[AXIS/]s/[NUMERIC/]EXPONENT.\'
Simplest form of name: \'s/EX.\'
.sp
See the discussion of \'[AXIS/]s/[NUMERIC/]TYPE.\', above.
.IP \'[AXIS/]s/[NUMERIC/]FRACTION.\'
Simplest form of name: \'s/FR.\'
.sp
See the discussion of \'[AXIS/]s/[NUMERIC/]TYPE.\', above.
.IP \'[AXIS/]s/[NUMERIC/]ANGLE.\'
Simplest form of name: \'s/AN.\'
.sp
A group of two integral real numbers specifying the 
orientation angle of numeric labels on the axis specified 
by "s". Subgroups and the number of parameters in each are 
as follows:
.RS 10
.sp
\'[AXIS/]s/[NUMERIC/]ANGLE/1ST.\' (1)
.br
\'[AXIS/]s/[NUMERIC/]ANGLE/2ND.\' (1)
.RE
.IP \'[AXIS/]s/[NUMERIC/]ANGLE/1ST.\'
Simplest form of name: \'s/AN/1S.\'
.sp
An integral real number having one of the values "0.", 
"90.", "180.", or "270." (plus or minus a small multiple of 
"360."), specifying the user\'s first choice for the 
orientation angle of numeric labels on the axis specified 
by "s". The value is stated in degrees counter-clockwise 
from a left-to-right horizontal vector.
.sp
The routine AGSTUP decides whether the first choice or the 
second choice is to be used. The second choice is used only 
when the first choice leads to overlap problems and the 
current value of \'[AXIS/]s/CONTROL.\' is a "3." or a "4." 
and the second choice works out better than the first. If 
AGSTUP decides to use the first choice, it leaves the first-choice
parameter with a positive value; if it decides to 
use the second choice, it leaves the first-choice parameter 
with a negative value. Values are made positive or negative 
by adding and subtracting multiples of "360.".
.sp
Default value: "0." for all "s" (horizontal labels 
preferred on all axes).
.IP \'[AXIS/]s/[NUMERIC/]ANGLE/2ND.\'
Simplest form of name: \'s/AN/2N.\'
.sp
An integral real number having one of the values "0.", 
"90.", "180.", or "270." (plus or minus a small multiple of 
"360."), specifying the user\'s second choice for the 
orientation angle of numeric labels on the axis specified 
by "s". The value is stated in degrees counter-clockwise 
from a left-to-right horizontal vector. See the description 
of the preceding parameter.
.sp
Default value: "90." for all "s" (vertical labels, readable 
from the right, on all axes).
.IP \'[AXIS/]s/[NUMERIC/]OFFSET.\'
Simplest form of name: \'s/OF.\'
.sp
A real number specifying the desired position of numeric 
labels relative to the axis specified by "s".
.sp
If the value is positive, numeric labels are to be placed 
toward the outside of the grid. If the value is negative, 
numeric labels are to be placed toward the inside of the 
grid. In either of these two cases, the magnitude of the 
value specifies the distance from the line portion of the 
axis to the nearest part of any numeric label, stated as a 
fraction of the smaller dimension of the grid window. 
Note:  "Inside" and "outside" are defined relative to the 
normal position of the axis "s", even when that axis has 
been moved away from its normal position.
.sp
If the value is exactly zero, each numeric label is 
centered on the axis. In this case, the line portion of the 
axis is suppressed and major and minor tick marks are moved 
outward so as not to overlap the numeric labels.
.sp
Default value: ".015" for all "s" (all labels outside the 
grid).
.IP \'[AXIS/]s/[NUMERIC/]WIDTH.\'
Simplest form of name: \'s/WI.\'
.sp
A group of two real parameters specifying the widths of 
characters to be used in numeric labels on the axis 
specified by "s". Subgroups and the number of parameters in 
each are as follows:
.RS 10
.sp
\'[AXIS/]s/[NUMERIC/]WIDTH/MANTISSA.\' (1)
.br
\'[AXIS/]s/[NUMERIC/]WIDTH/EXPONENT.\' (1)
.RE
.IP \'[AXIS/]s/[NUMERIC/]WIDTH/MANTISSA.\'
Simplest form of name: \'s/WI/MA.\'
.sp
A real number specifying the width of characters to be used 
in the "mantissa" of each numeric label on the axis 
specified by "s", expressed as a fraction of the smaller 
dimension of the grid window.
.sp
Default value: ".015" for all "s".
.IP \'[AXIS/]s/[NUMERIC/]WIDTH/EXPONENT.\'
Simplest form of name: \'s/WI/EX.\'
.sp
A real number specifying the width of characters to be used 
in the exponent of each numeric label on the axis specified 
by "s", expressed as a fraction of the smaller dimension of 
the grid window.
.sp
Default value: ".010" for all "s".
.IP \'DASH.\'
Simplest form of name: \'DAS.\'
.sp
A group of thirty parameters, the first of which determines 
what dashed-line patterns are to be used by the routines 
EZMY and EZMXY and the rest of which describe the "user" 
set of dashed-line patterns (as opposed to the "alphabetic" 
set, which is defined by code in the subroutine AGCURV and 
is not subject to change by the user). Subgroups and the 
number of parameters in each are as follows:
.RS 10
.sp
\'DASH/SELECTOR.\'     (1)
.br
\'DASH/LENGTH.\'       (1)
.br
\'DASH/CHARACTER.\'    (1)
.br
\'DASH/DOLLAR-QUOTE.\' (1)
.br
\'DASH/PATTERNS.\'    (26)
.RE
.IP \'DASH/SELECTOR.\'
Simplest form of name: \'DAS/SE.\'
.sp
The parameter \'DASH/SELECTOR.\' is given a negative integral 
value to specify that the routines EZMY and EZMXY should 
use the "alphabetic" set of 26 dashed-line patterns for the 
curves they draw and a positive integral value "n", less 
than or equal to 26, to specify that EZMY and EZMXY should 
use the first "n" patterns in the "user" set of dashed-line 
patterns, as defined by the current values of the remaining 
parameters in the group \'DASH.\'.
.sp
Each of the patterns in the "alphabetic" set specifies a 
solid line interrupted periodically by a letter of the 
alphabet. Each of the patterns in the "user" set is as 
defined by the user. The default "user" set produces all 
solid lines.
.sp
The routines EZY and EZXY, which draw but one curve per 
call, always use the first of the patterns in the "user" 
set; they are unaffected by the value of \'DASH/SELECTOR.\'.
.sp
The selected pattern set is used in a circular fashion. For 
example, if \'DASH/SELECTOR.\' has the value "3." and EZMY is 
used to draw nine curves, pattern 1 is used for curves 1, 
4, and 7, pattern 2 for curves 2, 5, and 8, and pattern 3 
for curves 3, 6, and 9.
.sp
Default value: "+1." (The first element of the "user" set 
of dashed-line patterns is to be used by EZMY and EZMXY.)
.IP \'DASH/LENGTH.\'
Simplest form of name: \'DAS/LE.\'
.sp
An integral real number specifying how long character-string
dashed-line patterns are expected to be. In a user 
call to ANOTAT with a positive fifth argument (implying 
that the sixth argument is an array of character-string 
dashed-line patterns) or in a user call to AGSETC setting 
\'DASH/PATTERN/n.\' (in which case the second argument is 
such a pattern), the specified character strings must be of 
the length specified by the current value of \'DASH/LENGTH.\'.
.sp
Default value: "8." (dashed-line patterns are expected to 
be eight characters long).
.IP \'DASH/CHARACTER.\'
Simplest form of name: \'DAS/CH.\'
.sp
A real number specifying the width of each character (other 
than a dollar sign or a quote) which is drawn along a curve 
as directed by a character-string dashed-line pattern 
(whether from the "alphabetic" set or from the "user" set). 
This width is expressed as a fraction of the smaller 
dimension of the grid window.
.sp
Default value: ".010"
.IP \'DASH/DOLLAR-QUOTE.\'
Simplest form of name: \'DAS/DO.\'
.sp
A real number specifying the line length corresponding to a 
dollar sign (solid) or a quote (gap) in a character-string 
dashed-line pattern, expressed as a fraction of the smaller 
dimension of the grid window.
.sp
Default value: ".010"
.IP \'DASH/PATTERNS.\'
Simplest form of name: \'DAS/PA.\'
.sp
A group of 26 parameters defining the "user" set of dashed-line
patterns. Subgroups and the number of parameters in 
each are as follows:
.RS 10
.sp
\'DASH/PATTERNS/1.\'  (1)
.br
\'DASH/PATTERNS/2.\'  (1)
.br
 .
.br
 .
.br
 .
.br
\'DASH/PATTERNS/26.\' (1)
.RE
.IP \'DASH/PATTERNS/n.\'
Simplest form of name: \'DAS/PA/n.\'
.sp
(The symbol "n" represents an integer between "1" and "26", 
inclusive.) An integral real number defining the "n"th 
dashed-line pattern in the "user" set.
.sp
If the value is positive, it must be between "0." and 
"65535.", inclusive, and is interpreted as a 16-bit binary 
pattern in which each "0" bit specifies a "pen-up" gap 
segment 3 plotter units long and each "1" bit specifies a 
"pen-down" solid segment 3 plotter units long. Such a 
pattern may be defined by a user call to AGSETF, AGSETI, or 
AGSETR.
.sp
If the value is negative, it serves as an identifier, 
allowing Autograph to retrieve, from its character storage 
space, a character string in which each single quote 
specifies a "pen-up" gap segment, each dollar sign 
specifies a "pen-down" solid segment, and each other 
character is simply to be drawn as a part of the line. Such 
a pattern may be defined by a user call to AGSETC.
.sp
Note that the function "AGDSHN" allows a user to easily 
generate the name of the "n"th dash pattern.
.sp
Default values: "65535." for all "n" (solid lines).
.IP \'LABEL.\'
Simplest form of name: \'LAB.\'
.sp
A group of 3+10n parameters, where "n" is the current value 
of \'LABEL/BUFFER/LENGTH.\' (8, by default) describing up to 
"n" informational labels. These labels are a part of the 
background drawn by a call to the routine AGBACK. Subgroups 
and the number of parameters in each are as follows:
.RS 10
.sp
\'LABEL/CONTROL.\'            (1)
.br
\'LABEL/BUFFER/LENGTH.\'      (1)
.br
\'LABEL/BUFFER/CONTENTS.\'  (10n)
.br
\'LABEL/NAME.\'               (1)
.RE
.IP \'LABEL/CONTROL.\'
Simplest form of name: \'LAB/CO.\'
.sp
An integral real number having the value "0.", "1.", or 
"2.". Values greater than "2." are changed to a "2." by the 
next AGSTUP call. Values less than "0." are changed to a 
"0." by the next AGSTUP call; negative values have a 
special use, however (see below).
.RS
.IP \(bu
The value "0." disables the drawing of informational 
labels. They remain defined, however.
.IP \(bu
The value "1." enables the drawing of informational labels 
and specifies that they may not be shrunk in response to 
overlap problems.
.IP \(bu
The value "2." enables the drawing of informational labels 
and specifies that they may be shrunk in response to 
overlap problems.
.RE
.IP ""
Default value: "2." (labels enabled, shrinkable).
.sp
Special action by AGSETP: An AGSETP call which sets this 
parameter (individually, rather than as part of a group) to 
a negative value results in the deletion of all currently 
defined labels. Note that the negative value is changed to 
a zero by the next AGSTUP call; thus, the drawing of 
informational labels is disabled until re-enabled by the 
user.
.IP \'LABEL/BUFFER.\'
Simplest form of name: \'LAB/BU.\'
.sp
A group of 1+10n parameters, where "n" is the current value 
of \'LABEL/BUFFER/LENGTH.\' (8, by default). Subgroups and 
the number of parameters in each are as follows:
.RS 10
.sp
\'LABEL/BUFFER/LENGTH.\'     (1)
.br
\'LABEL/BUFFER/CONTENTS.\' (10n)
.RE
.IP \'LABEL/BUFFER/LENGTH.\'
Simplest form of name: \'LAB/BU/LE.\'
.sp
An integral real number specifying the number of 10-word 
label definitions the label buffer will hold. A user 
program may need to retrieve, but must not set, the value 
of this parameter, since its value must match the second 
dimension of the label buffer.
.sp
Increasing the size of the label buffer requires modifying 
the Autograph source code. 
.sp
Default value: "8.".
.IP \'LABEL/BUFFER/CONTENTS.\'
Simplest form of name: \'LAB/BU/CO.\'
.sp
This parameter group may be thought of as an array FLLB, 
dimensioned 10 x n, containing up to n 10-word label 
definitions. For a second subscript j,
.RS
.IP \(bu 
FLLB(1,j) is either a real "0.", saying that no label is 
defined by this 10-word block, or it is non-zero, in which 
case it identifies a character string in Autograph\'s 
character-string storage area; the character string serves 
as a name for the label defined by this 10-word block. When 
FLLB(1,j) is non-zero:
.IP \(bu
FLLB(2,j) is either a "0.", to enable drawing of the label, 
or a "1.", to disable drawing of the label,
.IP \(bu
FLLB(3,j) and FLLB(4,j) are the X and Y coordinates of the 
label\'s "basepoint", in the grid coordinate system,
.IP \(bu
FLLB(5,j) and FLLB(6,j) are the X and Y components of the 
label\'s "offset vector", stated as signed fractions of the 
smaller dimension of the grid window,
.IP \(bu
FLLB(7,j) is an integral real number "0.", "90.", "180.", 
or "270.", specifying the angle at which the label\'s 
"baseline" emanates from the end of its offset vector,
.IP \(bu
FLLB(8,j) is an integral real number specifying how the 
lines of the label are to be positioned relative to the end 
of the offset vector ("-1." to line up the left ends, "0." 
to line up the centers, or "+1." to line up the right ends),
.IP \(bu
FLLB(9,j) is an integral real count of the number of lines 
belonging to the label, and
.IP \(bu
FLLB(10,j) is an integral real pointer specifying the 
second subscript (in the line buffer) of the first line of 
the label (the one having the largest line number), or, if 
no lines belong to the label, a "0.".
.RE
.IP ""
It is not recommended that a user program change the 
contents of this buffer directly. Label definitions should 
be accessed indirectly by means of the parameters \'LABEL/NAME.\'
and \'LABEL/[DEFINITION/]...\'.
.sp
Default values: The label buffer contains four pre-defined 
labels, corresponding to the four edges of the grid window. 
They are as follows:
.in +5
.sp
.TS
tab (/);
l c c c.
Label name:/\'L\'/\'R\'/\'B\'/\'T\'
.sp
Suppression flag:/0./0./0./0. 
.sp 
Basepoint X:/0./1./.5/.5
.sp
Basepoint Y:/.5/.5/0./1.
.sp
Offset X:/-.015/+.015/0./0.
.sp
Offset Y:/0./0./-.015/+.015
.sp
Baseline angle:/90./90./0./0. 
.sp
Centering option:/0./0./0./0. 
.sp
Line count:/1./1./1./1.
.sp
First-line index:/1./2./3./4. 
.TE
.sp
.in -5
The description of \'LINE/BUFFER/CONTENTS.\', below, gives 
the default values for the definitions of the lines which 
belong to these labels.
.IP \'LABEL/BUFFER/NAMES.\'
Simplest form of name: \'LAB/BU/NA.\'
.sp
This group is a subset of the previous one. It provides a 
way of retrieving the names of all currently-defined labels.
.IP \'LABEL/NAME.\'
Simplest form of name: \'LAB/NA.\'
.sp
An integral real pointer which, if non-zero, specifies a 
particular label in the label buffer - the one which is to 
be referenced by the parameter group \'LABEL/DEFINITION.\' 
(which see, below).
.sp
Setting \'LABEL/NAME.\' is the required first step in 
accessing a particular label definition.
.sp
Default value: "0." (undefined).
.sp
Special action by AGSETP: To access the definition of a 
particular label, one must first call AGSETC with \'LABEL/NAME.\'
as the first argument and the name of the label one 
wishes to access as the second argument. This causes AGSETP 
(which is called by AGSETC) to search for the definition of 
the desired label in the label buffer. If that definition 
is not found, a new one is made up and inserted in the 
label buffer. In either case, \'LABEL/NAME.\' is given a real 
value whose integer equivalent specifies the second 
subscript of the label definition in the label buffer.
.sp
The definition of a new label has the name specified by the 
user, a suppression flag "0.", a basepoint (.5,.5), an 
offset vector (0.,0.), a baseline angle "0.", a centering 
option "0.", a line count "0.", and a first-line index "0.".
.IP \'LABEL/DEFINITION.\'
Simplest form of name: \'LAB/DE.\'
.sp
A set of nine parameters defining the label specified by 
the current value of \'LABEL/NAME.\'. If \'LABEL/NAME.\' has 
the value "0.", referencing this group or a parameter in it 
causes an error exit. Subgroups and the number of 
parameters in each are as follows:
.RS 10
.sp
\'LABEL/[DEFINITION/]SUPPRESSION.\' (1)
.br
\'LABEL/[DEFINITION/]BASEPOINT.\'   (2)
.br
\'LABEL/[DEFINITION/]OFFSET.\'      (2)
.br
\'LABEL/[DEFINITION/]ANGLE.\'       (1)
.br
\'LABEL/[DEFINITION/]CENTERING.\'   (1)
.br
\'LABEL/[DEFINITION/]LINES.\'       (1)
.br
\'LABEL/[DEFINITION/]INDEX.\'       (1)
.RE
.IP \'LABEL/[DEFINITION/]SUPPRESSION.\'
Simplest form of name: \'LAB/SU.\'
.sp 
An integral real "suppression flag" having the value "0." 
or "1." and specifying whether drawing of the label 
specified by \'LABEL/NAME.\' is enabled ("0.") or disabled 
("1.").
.sp 
Default value for a new label: "0." (label enabled).
.sp 
Special action by AGSETP: If a user program attempts to set 
this parameter (individually, rather than as part of a 
group) to a negative value, the lines of the label 
specified by \'LABEL/NAME.\' are deleted and \'LINE/NUMBER.\' 
is zeroed. If the negative value is less than "-1.", the 
label is deleted as well and \'LABEL/NAME.\' is zeroed. 
(Deleting a label means that its name cell is set to "0.".)
.IP \'LABEL/[DEFINITION/]BASEPOINT.\'
Simplest form of name: \'LAB/BA.\'
.sp
A set of two parameters specifying the X and Y coordinates 
of the basepoint of the label specified by \'LABEL/NAME.\', 
in the grid coordinate system. The label is positioned 
relative to this basepoint. Subgroups and the number of 
parameters in each are as follows:
.RS 10
.sp
\'LABEL/[DEFINITION/]BASEPOINT/X.\' (1)
.br
\'LABEL/[DEFINITION/]BASEPOINT/Y.\' (1)
.RE
.IP \'LABEL/[DEFINITION/]BASEPOINT/X.\'
Simplest form of name: \'LAB/BA/X.\'
.sp
The X coordinate of the basepoint of the label specified by 
\'LABEL/NAME.\'. The value "0." refers to the left edge of 
the grid window, the value "1." to the right edge of the 
grid window.
.sp
Default value for a new label: ".5" (centered).
.IP \'LABEL/[DEFINITION/]BASEPOINT/Y.\'
Simplest form of name: \'LAB/BA/Y.\'
.sp
The Y coordinate of the basepoint of the label specified by 
\'LABEL/NAME.\'. The value "0." refers to the bottom edge of 
the grid window, the value "1." to the top edge of the grid 
window.
.sp
Default value for a new label: ".5" (centered).
.IP \'LABEL/[DEFINITION/]OFFSET.\'
Simplest form of name: \'LAB/OF.\'
.sp
A set of two parameters specifying the X and Y components 
of the offset vector of the label specified by \'LABEL/NAME.\',
as signed fractions of the smaller dimension of the 
grid window. The offset vector has its basepoint at the 
label basepoint. Subgroups and the number of parameters in 
each are as follows:
.RS 10
.sp
\'LABEL/[DEFINITION/]OFFSET/X.\' (1)
.br
\'LABEL/[DEFINITION/]OFFSET/Y.\' (1)
.RE
.IP \'LABEL/[DEFINITION/]OFFSET/X.\'
Simplest form of name: \'LAB/OF/X.\'
.sp
The X component of the offset vector of the label specified 
by \'LABEL/NAME.\' - negative toward the left edge, positive 
toward the right edge, of the grid window. The magnitude 
represents a fraction of the smaller dimension of the grid 
window.
.sp
Default value for a new label: "0." (zero-length vector).
.IP \'LABEL/[DEFINITION/]OFFSET/Y.\'
Simplest form of name: \'LAB/OF/Y.\'
.sp
The Y component of the offset vector of the label specified 
by \'LABEL/NAME.\' - negative toward the bottom edge, 
positive toward the top edge, of the grid window. The 
magnitude represents a fraction of the smaller dimension of 
the grid window.
.sp
Default value for a new label: "0." (zero-length vector).
.IP \'LABEL/[DEFINITION/]ANGLE.\'
Simplest form of name: \'LAB/AN.\'
.sp
An integral real number having one of the values "0.", 
"90.", "180.", or "270.", and specifying the direction in 
which the baseline of the label specified by \'LABEL/NAME.\' 
emanates from the end of its offset vector, measured 
counter-clockwise from a left-to-right horizontal vector. 
All the lines of a label are written parallel to its 
baseline and in the direction of the baseline.
.sp
Default value for a new label: "0." (horizontal, left to 
right).
.IP \'LABEL/[DEFINITION/]CENTERING.\'
Simplest form of name: \'LAB/CE.\'
.sp
An integral real number specifying the alignment of the 
lines of the label specified by \'LABEL/NAME.\' with the end 
of its offset vector. A negative value aligns the left 
ends, a zero value the centers, and a positive value the 
right ends, of the lines.
.sp
Default value for a new label: "0." (centers aligned).
.IP \'LABEL/[DEFINITION/]LINES.\'
Simplest form of name: \'LAB/LI.\'
.sp
An integral real number specifying the number of lines in 
the label specified by \'LABEL/NAME.\'.
.sp
This parameter is updated by Autograph as lines are added 
to or deleted from the label and should not be set by a 
user program.
.sp
Default value for a new label: "0." (no lines).
.IP \'LABEL/[DEFINITION/]INDEX.\'
Simplest form of name: \'LAB/IN.\'
.sp
An integral real number specifying the second subscript (in 
the line buffer) of the first line belonging to the label 
specified by \'LABEL/NAME.\' - a zero if no line belongs to 
the label.
.sp
This parameter is updated by Autograph as lines are added 
to or deleted from the label and should not be set by a 
user program.
.sp
Default value for a new label: "0." (no lines).
.IP \'LINE.\'
Simplest form of name: \'LIN.\'
.sp
A group of 4+6n parameters, where "n" is the current value 
of \'LINE/BUFFER/LENGTH.\' (16, by default) describing up to 
"n" lines, each of which is a part of some informational 
label. Subgroups and the number of parameters in each are 
as follows:
.RS 10
.sp
\'LINE/MAXIMUM.\'          (1)
.br
\'LINE/END.\'              (1)
.br
\'LINE/BUFFER/LENGTH.\'    (1)
.br
\'LINE/BUFFER/CONTENTS.\' (6n)
.br
\'LINE/NUMBER.\'           (1)
.RE
.IP \'LINE/MAXIMUM.\'
Simplest form of name: \'LIN/MA.\'
.sp
An integral real number specifying the assumed maximum 
length of a character string delivered to Autograph for use 
as the text of a label line. Such a character string may 
occur as the first argument of a call to ANOTAT (defining 
the text of line "100." in the label \'L\'), as the second 
argument of a call to ANOTAT (defining the text of line "-100."
in the label \'B\'), as the last argument of a call to 
one of the routines EZY, EZXY, EZMY, or EZMXY (defining the 
text of line "100." in the label \'T\'), or as the second 
argument of a call to AGSETC whose first argument is
.sp
.RS 10
\'LINE/[DEFINITION/]TEXT.\'
.RE
.IP ""
(defining the text of any line). In each of these cases, 
the character string must be of the length specified by 
\'LINE/MAXIMUM.\' or shorter. If it is shorter, its last 
character must be the character specified by \'LINE/END.\', 
described below.
.sp
This parameter may be given any desired non-negative 
integral value.
.sp
Default value: "40.".
.IP \'LINE/END.\'
Simplest form of name: \'LIN/EN.\'
.sp
A character string whose first character is the one used to 
mark the end of a character string defining the text of a 
label line (in calls to ANOTAT, EZY, EZXY, EZMY, EZMXY, and 
AGSETC), when that character string is shorter than the 
current maximum specified by \'LINE/MAXIMUM.\' (as described 
above).
.sp
The terminator character does not become a part of the text 
of the line. It is stripped off, so that only the preceding 
characters constitute the text of the line.
.sp
Default value: "\'$\'".
.IP \'LINE/BUFFER.\'
Simplest form of name: \'LIN/BU.\'
.sp
A group of 1+6n parameters, where "n" is the current value 
of \'LINE/BUFFER/LENGTH.\' (16, by default). Subgroups and 
the number of parameters in each are as follows:
.RS 10
.sp
\'LINE/BUFFER/LENGTH.\'    (1)
.br
\'LINE/BUFFER/CONTENTS.\' (6n)
.RE
.IP \'LINE/BUFFER/LENGTH.\'
Simplest form of name: \'LIN/BU/LE.\'
.sp
An integral real number specifying the number of 6-word 
line definitions the line buffer will hold. A user program 
may need to retrieve, but must not set, the value of this 
parameter, since its value must match the second dimension 
of the line buffer.
.sp
Increasing the size of the line buffer requires modifying 
the Autograph source code. 
.sp
Default value: "16.".
.IP \'LINE/BUFFER/CONTENTS.\'
Simplest form of name: \'LIN/BU/CO.\'
.sp
This group may be thought of as an array FLLN, dimensioned 
6 x n, containing up to n 6-word line definitions. For a 
second subscript j,
.RS
.IP \(bu 
FLLN(1,j) is either a real "null 1", saying that no label 
line is defined by this 6-word block, or an integral real 
"line number", saying that it does define a label line, in 
which case:
.IP \(bu
FLLN(2,j) is either "0.", to enable drawing of the line, or 
"1.", to disable drawing of the line.
.IP \(bu
FLLN(3,j) is the real width of each character of the line, 
stated as a fraction of the smaller dimension of the grid 
window.
.IP \(bu
FLLN(4,j) is an integral real number serving as the 
identifier of the character string defining the text of the 
line and enabling it to be retrieved from Autograph\'s 
internal character storage space.
.IP \(bu
FLLN(5,j) is an integral real count of the number of 
characters in the text of the line.
.IP \(bu
FLLN(6,j) is an integral real number specifying the second 
subscript (in the line buffer) of the next line of the 
label to which this line belongs (that one of the remaining 
lines in the chain with the largest line number) or, if 
there is no next line, a "0.".
.RE
.IP ""
It is not recommended that a user program change the 
contents of this buffer directly. Line definitions should 
be accessed indirectly by means of the parameters \'LINE/NUMBER.\'
and \'LINE/[DEFINITION/]...\'.
.sp
Default values: The line buffer contains four pre-defined 
lines, each of which belongs to one of the four pre-defined 
labels. They are as follows:
.sp
.RS 5
.TS
tab ($);
l l l l l.
Label name$\'L\'$\'R\'$\'B\'$\'T\'
.sp
Line number$+100.$-100.$-100.$+100.
.sp
Suppression flag$0.$0.$0.$0. 
.sp
Character width$\.015$\.015$\.015$\.020 
.sp
Text pointed to$\'Y\'$\' \' (a blank)$\'X\'$\' \' (a blank) 
.sp
Text length$1.$0.$1.$0.
.sp
Next-line index$0.$0.$0.$0. 
.RE
.TE
.IP ""
The description of \'LABEL/BUFFER/CONTENTS.\', above, gives 
default values for the definitions of the four labels which 
contain these lines.
.IP \'LINE/NUMBER.\'
Simplest form of name: \'LIN/NU.\'
.sp
An integral real pointer which, if non-zero, specifies a 
particular line in the line buffer - the one which is to be 
referenced by the parameter group \'LINE/DEFINITION.\' (which 
see, below).
.sp
Setting this parameter is the required first step in 
accessing a particular line definition.
.sp
Default value: "0." (undefined).
.sp
Special action by AGSETP: To access the definition of a 
particular line of a particular label, one must ensure that 
\'LABEL/NAME.\' (which see, above) is set. Then, one must 
call AGSETI with \'LINE/NUMBER.\' as the first argument and 
the number of the line one wishes to access as the second 
argument. This causes AGSETP (which is called by AGSETI) to 
search the line buffer for the definition of a line 
belonging to the label specified by the current value of 
\'LABEL/NAME.\' and having the desired line number. If no 
such definition is found, a new one is made up, inserted in 
the line buffer, and linked into the proper place in the 
chain of lines belonging to the label. In either case, 
\'LINE/NUMBER.\' is given an integral real value specifying 
the second subscript of the line definition in the line 
buffer.
.sp
The definition of a new line has the number specified by 
the user, a suppression flag "0.", a character width 
".015", a pointer to the text string "\' \' (a single 
blank)", and a text length of "1.".
.sp
Note: The "line numbers" are used to identify the lines of 
a label and to specify their positions relative to each 
other and to the baseline of the label. Lines having 
positive line numbers are drawn above the label baseline, 
lines having zero line numbers are drawn along the label 
baseline, and lines having negative line numbers are drawn 
below the label baseline. A line having a greater line 
number than another line is drawn above that line. ("Above" 
and "below" are used here from the viewpoint of someone 
reading the label.)  The magnitudes of the line numbers in 
no way affect inter-line spacing, which is determined by 
Autograph itself.
.IP \'LINE/DEFINITION.\'
Simplest form of name: \'LIN/DE.\'
.sp
A group of five parameters defining the line specified by 
\'LINE/NUMBER.\'. If \'LINE/NUMBER.\' has the value "0.", 
referencing a parameter in this group causes an error exit. 
Subgroups and the number of parameters in each are as 
follows:
.RS 10
.sp
\'LINE/[DEFINITION/]SUPPRESSION.\' (1)
.br
\'LINE/[DEFINITION/]CHARACTER.\'   (1)
.br
\'LINE/[DEFINITION/]TEXT.\'        (1)
.br
\'LINE/[DEFINITION/]LENGTH.\'      (1)
.br
\'LINE/[DEFINITION/]INDEX.\'       (1)
.RE
.IP \'LINE/[DEFINITION/]SUPPRESSION.\'
Simplest form of name: \'LIN/SU.\'
.sp
An integral real number having the value "0." or "1." and 
specifying whether drawing of the line specified by \'LINE/NUMBER.\'
is enabled ("0.") or disabled ("1.").
.sp
Default value for a new line: "0." (line enabled).
.sp
Special action by AGSETP: If a user program attempts to set 
this parameter (individually, rather than as a part of a 
group) to a negative value, the line specified by \'LINE/NUMBER.\'
is deleted and \'LINE/NUMBER.\' is reset to "0.". 
(Deleting a line means that it is unlinked from the chain 
of lines belonging to its label and that its number cell is 
set to "null 1".)
.IP \'LINE/[DEFINITION/]CHARACTER.\'
Simplest form of name: \'LIN/CH.\'
.sp
A real number specifying the desired width of each 
character of the line specified by \'LINE/NUMBER.\', stated 
as a fraction of the smaller dimension of the grid window.
.sp
Default value for a new line: ".015".
.IP \'LINE/[DEFINITION/]TEXT.\'
Simplest form of name: \'LIN/TE.\'
.sp
An integral real number serving as an identifier for a 
character string stored away in Autograph\'s internal 
character storage space.
.sp
Default value for a new line: "\' \' (a single blank)".
.sp
Special action by AGSETP: When this parameter is set by a 
call to AGSETC, the character string appearing as the 
second argument of AGSETC is stored in a character storage 
array inside Autograph, an identifier allowing for later 
retrieval of the string is generated, and the value of that 
identifier is stored (by AGSETP, which is called by AGSETC) 
as the parameter value. At that time, the length of the 
string is determined and \'LINE/[DEFINITION/]LENGTH.\' is 
set. See \'LINE/MAXIMUM.\' and \'LINE/TERMINATOR.\', above.
.IP \'LINE/[DEFINITION/]LENGTH.\'
Simplest form of name: \'LIN/LE.\'
.sp
An integral real count of the number of characters in the 
text of the line specified by \'LINE/NUMBER.\'. Setting this 
parameter less than or equal to zero suppresses the drawing 
of the line. See also the description of \'LINE/[DEFINITION/]TEXT.\',
above.
.sp
Default value for a new line: "1." (one character - a 
blank).
.IP \'LINE/[DEFINITION/]INDEX.\'
Simplest form of name: \'LIN/IN.\'
.sp
An integral real number specifying the second subscript (in 
the line buffer) of the next line of the label - a zero if 
there is no next line.
.sp
This parameter is updated by Autograph as lines are added 
to or deleted from the label and should not be set by a 
user program.
.IP \'SECONDARY.\'
Simplest form of name: \'SEC.\'
.sp
A group of 149 "secondary" control parameters. These are 
not normally set by a user program, but are computed by 
Autograph itself (the routine AGSTUP). Their values may be 
of use in some applications. Subgroups and the number of 
parameters in each are as follows:
.RS 10
.sp
\'SECONDARY/GRAPH.\'      (4)
.br
\'SECONDARY/USER.\'       (4)
.br
\'SECONDARY/CURVE.\'      (4)
.br
\'SECONDARY/DIMENSIONS.\' (3)
.br
\'SECONDARY/AXIS.\'      (80)
.br
\'SECONDARY/LABEL.\'     (54)
.RE
.IP \'SECONDARY/GRAPH.\'
Simplest form of name: \'SEC/GR.\'
.sp
A group of four real numbers specifying the X coordinates 
of the left and right edges of the graph window and the Y 
coordinates of the bottom and top edges of the graph 
window, in the grid coordinate system. These values are 
used by Autograph to determine whether a point whose 
coordinates are expressed in the grid coordinate system 
lies inside or outside the graph window.
.sp
If the parameters in the group \'GRID.\' have their default 
values (".15", ".95", ".15", ".95", and "0."), these four 
parameters will be given the values "-.1875", "1.0625", 
"-.1875", and "1.0625". Note that -.1875 = (0.-.15)/(.95-.15) 
and that 1.0625 = (1.-.15)/(.95-.15).
.IP \'SECONDARY/USER.\'
Simplest form of name: \'SEC/US.\'
.sp
A set of four real numbers specifying the X coordinates of 
the left and right edges of the grid window and the Y 
coordinates of the bottom and top edges of the grid window, 
in the user coordinate system. These values are used in 
mapping user curve points into the grid window. The 
routines AGSTUP, AGBACK, and AGCURV use these four numbers 
as arguments 5 through 8 in calls to the system-plot-package
routine SET.
.IP \'SECONDARY/CURVE.\'
Simplest form of name: \'SEC/CU.\'
.sp
A group of four real numbers specifying the X coordinates 
of the left and right edges of the grid (curve) window and 
the Y coordinates of the bottom and top edges of the grid 
(curve) window. The X coordinates are stated as fractions 
of the distance from left to right, and the Y coordinates 
as fractions of the distance from bottom to top, in the 
plotter frame. The routines AGSTUP, AGBACK, and AGCURV use 
these four numbers as arguments 1 through 4 in calls to the 
system-plot-package routine SET. If the parameters in the 
groups \'GRAPH.\' and \'GRID.\' have their default values, 
these four parameters are given the values ".15", ".95", 
".15", and ".95".
.IP \'SECONDARY/DIMENSIONS.\'
Simplest form of name: \'SEC/DI.\'
.sp
A group of three real numbers, the first two of which 
specify the width and height of the grid window and the 
third of which is equal to the smaller of the first two. 
Each is stated as a number of plotter units. If the 
parameters in the groups \'GRAPH.\' and \'GRID.\' have their 
default values and the plotter being used has 1024x1024 
addressable positions, then each of these three parameters 
will be given the value 818.4 = (.95-.15) * 1023.
.IP \'SECONDARY/AXIS.\'
Simplest form of name: \'SEC/AX.\'
.sp
A group of eighty parameters having to do with the drawing 
of the four axes. Subgroups and the number of parameters in 
each are as follows:
.RS 10
.sp
\'SECONDARY/[AXIS/]LEFT.\'   (20)
.br
\'SECONDARY/[AXIS/]RIGHT.\'  (20)
.br
\'SECONDARY/[AXIS/]BOTTOM.\' (20)
.br
\'SECONDARY/[AXIS/]TOP.\'    (20)
.RE
.IP ""
The parameters from the subgroups are interleaved in the 
group; that is to say, the first elements of the subgroups 
comprise elements 1 through 4 of the group, the second 
elements of the subgroups comprise elements 5 through 8 of 
the group, and so on.
.IP \'SECONDARY/[AXIS/]s.\'
Simplest form of name: \'SEC/s.\'
.sp
A group of twenty parameters having to do with the drawing 
of the axis specified by "s", where "s" is one of the 
keywords "LEFT", "RIGHT", "BOTTOM", or "TOP". Subgroups and 
the number of parameters in each are as follows:
.RS 10
.sp
\'SECONDARY/[AXIS/]s/POSITION.\' (6)
.br
\'SECONDARY/[AXIS/]s/TICKS.\'    (3)
.br
\'SECONDARY/[AXIS/]s/NUMERIC.\' (11)
.RE
.IP \'SECONDARY/[AXIS/]s/POSITION.\'
Simplest form of name: \'SEC/s/PO.\'
.sp
A group of six real numbers, the first three of which 
describe a point at the beginning of axis "s" and the last 
three of which describe a point at the end of axis "s". The 
first two numbers of each triplet are the X and Y 
coordinates of the point, in the grid coordinate system. 
The third number of each triplet is a user-system X or Y 
coordinate (an X coordinate for a horizontal axis, a Y 
coordinate for a vertical axis) of the point.
.IP \'SECONDARY/[AXIS/]s/TICKS.\'
Simplest form of name: \'SEC/s/TI.\'
.sp
A group of three real numbers, specifying the values 
Autograph has chosen to use for the primary parameters
.RS 10
.sp
\'[AXIS/]s/[TICKS/]MAJOR/[SPACING/]TYPE.\'
.br
\'[AXIS/]s/[TICKS/]MAJOR/[SPACING/]BASE.\'
.br
\'[AXIS/]s/[TICKS/]MINOR/SPACING.\'
.RE
.IP ""
(which see, above). These secondary parameters are used to 
hold the values Autograph chooses for the corresponding 
primary parameters so as not to disturb "null 1" values of 
those primary parameters.
.IP \'SECONDARY/[AXIS/]s/NUMERIC.\'
Simplest form of name: \'SEC/s/NU.\'
.sp
A group of eleven real numbers having to do with the 
generation of numeric labels on the axis specified by "s". 
The first three of these specify the values Autograph has 
chosen to use for the primary parameters
.RS 10
.sp
\'[AXIS/]s/[NUMERIC/]TYPE.\'
.br
\'[AXIS/]s/[NUMERIC/]EXPONENT.\'
.br
\'[AXIS/]s/[NUMERIC/]FRACTION.\'
.RE
.IP ""
(which see, above). The secondary parameters are used so as 
not to disturb "null 1" values of the primary parameters.
.sp
The fourth parameter is an integral real count of the 
number of characters in the longest numeric-label mantissa 
on the axis "s".
.sp
The fifth parameter is an integral real count of the number 
of characters in the longest numeric-label exponent on the 
axis "s".
.sp
The sixth parameter is the necessary multiplicative 
"reduction factor" (between "0." and "1.") to be applied to 
the sizes of numeric labels on the axis "s" in order to 
make them fit without overlap problems.
.sp
The seventh, eighth, ninth, and tenth parameters are real 
numbers specifying the width of the space required by 
numeric labels to the left (outward), to the right 
(inward), at the beginning and at the end of the axis "s" - 
each is stated as a fraction of the width or height of the 
grid window, depending on the orientation of the axis "s".
.sp
The eleventh parameter indicates the linear/log nature of 
the axis specified by "s".
.IP \'SECONDARY/LABEL.\'
Simplest form of name: \'SEC/LA.\'
.sp
A group of fifty-four parameters describing the six "label 
boxes", each of which provides a mechanism for moving and/or
shrinking a particular group of labels in attempting to 
keep any label in that group from overlapping an axis or 
extending outside the current graph window. Subgroups and 
the number of parameters in each are as follows:
.RS 10
.sp
\'SECONDARY/LABEL/LEFT.\'   (9)
.br
\'SECONDARY/LABEL/RIGHT.\'  (9)
.br
\'SECONDARY/LABEL/BOTTOM.\' (9)
.br
\'SECONDARY/LABEL/TOP.\'    (9)
.br
\'SECONDARY/LABEL/CENTER.\' (9)
.br
\'SECONDARY/LABEL/GRAPH.\'  (9)
.RE
.IP ""
The parameters of the subgroups are interleaved in the 
group. The first elements of the subgroups form elements 1 
through 6 of the group, the second elements of the 
subgroups form elements 7 through 12 of the group, and so 
on.
.IP \'SECONDARY/LABEL/b.\'
Simplest form of name: \'SEC/LA/b.\'
.sp
where the keyword "b" specifies the label box, as follows:
.RS
.IP \(bu
If "b" = "LEFT", label box 1 is specified. It contains all 
labels having a basepoint on the left edge of the grid 
window and a leftward-pointing offset vector. These labels 
are to be moved leftward as required to avoid overlapping 
any numeric labels on either Y axis.
.IP \(bu
If "b" = "RIGHT", label box 2 is specified. It contains all 
labels having a basepoint on the right edge of the grid 
window and a rightward-pointing offset vector. These labels 
are to be moved rightward as required to avoid overlapping 
any numeric labels on either Y axis.
.IP \(bu
If "b" = "BOTTOM", label box 3 is specified. It contains 
all labels having a basepoint on the bottom edge of the 
grid window and a downward-pointing offset vector. These 
labels are to be moved downward as required to avoid 
overlapping any numeric labels on either X axis.
.IP \(bu
If "b" = "TOP", label box 4 is specified. It contains all 
labels having a basepoint on the top edge of the grid 
window and an upward-pointing offset vector. These labels 
are to be moved upward as required to avoid overlapping any 
numeric labels on either X axis.
.IP \(bu
If "b" = "CENTER", label box 5 is specified. It contains 
all labels having a basepoint on some edge of the grid 
window and an inward-pointing offset vector. These labels 
are to be moved inward as required to avoid overlapping 
numeric labels on any axis.
.IP \(bu
If "b" = "GRAPH", label box 6 is specified. It contains all 
labels not specifically assigned to one of the other boxes. 
These labels are not moved, but may still be shrunk as 
required to avoid their running outside the grid window.
.RE
.IP ""
Prior to a call to AGSTUP, the nine parameters in this 
group are undefined. Following an AGSTUP call, but 
preceding an AGBACK call, they have what I shall call 
"interim" values. Following an AGBACK call, they have what 
I shall call "final" values.
.sp
The first parameter in the group is a "reduction factor" 
for the widths of characters in the labels in box "b". This 
parameter may have the interim value "0.", specifying that 
no actual value has yet been computed, or "1.", specifying 
that the user has prohibited shrinkage of labels in box "b" 
(by giving \'LABEL/CONTROL.\' the value "1."). The final 
value of the reduction factor may be "-1.", specifying that 
minimum-sized labels were used, but even they led to 
overlap problems, or a value between "0." and "1.", 
specifying the actual reduction factor applied when the 
labels were drawn.
.sp
The next four parameters in the group specify the grid-system
X coordinates of the left and right edges, and the 
grid-system Y coordinates of the bottom and top edges, of 
label box "b". The interim values specify the box in which 
the labels must be made to fit in order to avoid overlap, 
the final values the box in which the labels were actually 
made to fit.
.sp
The last four parameters in the group specify the grid-system
X coordinates of the left and right edges, and the 
grid-system Y coordinates of the bottom and top edges, of 
the label box "b" which would result if all the labels were 
reduced to minimum size. The interim values specify an 
unmoved box, the final values a (possibly) moved box.
.SH SEE ALSO
Online:
agdshn,
aggetc,
aggetf,
aggeti,
aggetp,
aggetr,
agrstr,
agsave,
agsetc,
agsetf,
agseti,
agsetp,
agsetr,
anotat,
displa
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
