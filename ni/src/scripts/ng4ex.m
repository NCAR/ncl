.\" The first line of this file must contain the '\"[e][r][t][v] line
.\" to tell man to run the appropriate filter "t" for table.
.\"
.\" $Id: ng4ex.m,v 1.2 2003-03-04 15:54:09 haley Exp $
.\"
.\"######################################################################
.\"#                                                                    #
.\"#               Copyright (C)  1993                                  #
.\"#        University Corporation for Atmospheric Research             #
.\"#               All Rights Reserved                                  #
.\"#                                                                    #
.\"######################################################################
.\"
.\"     File:		ng4ex.man
.\"
.\"     Author:		Jeff W. Boote
.\"			National Center for Atmospheric Research
.\"			PO 3000, Boulder, Colorado
.\"
.\"     Date:		Wed Apr 7 10:29:48 MDT 1993
.\"
.\"     Description:	Describes the ng4ex script and the example programs.
.\"
.TH ng4ex 1Nhl "NCARG Nhl EXAMPLES"
.SH NAME
.nh
ng4ex \- NCAR Graphics High Level Utilities Example Programs
.ny
.SH SYNOPSIS
\fBng4ex\fP 
[\fI\-A\fP]
[\fI\-C\fP]
[\fI\-Fortran\fP]
[\fI\-NCL\fP]
[\fI\-cdf\fP]
[\fI\-csagrid\fP]
[\fI\-cssgrid\fP]
[\fI\-gui\fP]
[\fI\-anno\fP]
[\fI\-gsun\fP]
[\fB\-W workstation_type\fR]
[\fI\-app\fP]
[\fI\-basic\fP]
[\fI\-contourplot\fP]
[\fI\-fitgrid\fP]
[\fI\-labelbar\fP]
[\fI\-legend\fP]
[\fI\-primitives\fP]
[\fI\-mapplot\fP]
[\fI\-natgrid\fP]
[\fI\-ngmath\fP]
[\fI\-streamplot\fP]
[\fI\-shgrid\fP]
[\fI\-textitem\fP]
[\fI\-tickmark\fP]
[\fI\-title\fP]
[\fI\-vectorplot\fP]
[\fI\-xyplot\fP]
[\fI\-list\fP]
[\fI\-clean\fP]
[\fI\-n\fP]
\fIname ...\fP
.SH DESCRIPTION
.I ng4ex
provides the user with access to several C, Fortran, and NCL examples
illustrating the use of the NCAR Graphics HLUs (High Level Utilities)
and NCL.  Please note that NCL is not available for Cray systems.
.sp
\fIng4ex\fP copies the source code for the specified
example(s) into the current directory and then compiles, links, and
executes the example (if it's a Fortran or C program) or runs NCL on
it (if it's an NCL script file).  Depending on the example, the output
may be sent to an NCGM (NCAR Graphics Metafile) file and/or displayed
to an X window on your screen.  You must be running X and have your
DISPLAY environment set correctly in order for the X window examples
to execute properly.
.sp
If the requested example creates an NCGM file, it will have the same
name as the example, suffixed with ".ncgm". An option allows you to
request that only the source code be copied to your directory, without
compiling or running the example.  Another option
allows you to request that only the NCGM file be left in your
directory and that all other files created by \fBng4ex\fP be deleted.
The argument \fIname\fP may be selected from the lists that appear
below.
.SH OPTIONS
.IP \fI-A\fP " " ""
Generate all available examples.
.sp
.IP \fI-C\fP " " ""
Generate all C examples.
.sp
.IP \fI-Fortran\fP " " ""
Generate all Fortran examples.
.sp
.IP \fI-NCL\fP " " ""
Generate all NCL examples.
.sp
.IP \fI-cdf\fP " " ""
Generate all the examples that use netCDF files.  For the C and
Fortran examples, this option requires that you have the netCDF
library installed on your system.
.sp
.IP \fI-gui\fP " " ""
Generate all GUI examples. This option requires that you have the
Motif libraries on your system.
.sp
.sp
.IP \fI-anno\fP " " ""
Generate all annotation examples.
.sp
.IP  \fI-W workstation_type\fP " " ""
Specify the workstation type.  This argument can only be specified 
as a string.  Some examples are meant to display to the X workstation,
to an NCGM file, or to a PostScript file only, in which case this option
may be ignored. You cannot specify more than one workstation
type.  If you don't specify one, then a default one will be used.
The following workstation_type strings are valid:
.IP "             ncgm" 18
-  NCGM file
.IP "             x11" 18
-  X11 window
.IP "             ps" 18
-  PostScript file
.IP "             pdf" 18
-  PDF file
.sp
.IP \fI-app\fP " " ""
Generate all the app Fortran, C, and NCL examples that show how to do
specific things with resources.
.sp
.IP \fI-basic\fP " " ""
Generate all the basic Fortran, C, and NCL examples that relate to the
basic examples discussed in the Quick Start Guide.
.sp
.IP \fI-contourplot\fP " " ""
Generate all Fortran, C, and NCL ContourPlot examples.
.sp
.IP \fI-csagrid\fP " " ""
Generate all NCL Csagrid examples.
.sp
.IP \fI-cssgrid\fP " " ""
Generate all NCL Cssgrid examples.
.sp
.IP \fI-dsgrid\fP " " ""
Generate all Fortran, C, and NCL Dsgrid examples.
.sp
.IP \fI-fitgrid\fP " " ""
Generate all NCL Natgrid examples.
.sp
.IP \fI-labelbar\fP " " ""
Generate all Fortran, C, and NCL Labelbar examples.
.sp
.IP \fI-legend\fP " " ""
Generate all Fortran, C, and NCL Legend examples.
.sp
.IP \fI-natgrid\fP " " ""
Generate all Fortran, C, and NCL Natgrid examples.
.sp
.IP \fI-primitives\fP " " ""
Generate all Fortran, C, and NCL GraphicStyle examples.
.sp
.IP \fI-mapplot\fP " " ""
Generate all Fortran, C, and NCL MapPlot examples.
.sp
.IP \fI-ngmath\fP " " ""
Generate all Fortran, C, and NCL Ngmath examples.
.sp
.IP \fI-shgrid\fP " " ""
Generate all NCL Shgrid examples.
.sp
.IP \fI-streamlineplot\fP " " ""
Generate all Fortran, C, and NCL StreamlinePlot examples.
.sp
.IP \fI-textitem\fP " " ""
Generate all Fortran, C, and NCL TextItem examples.
.sp
.IP \fI-tickmark\fP " " ""
Generate all Fortran, C, and NCL TickMark examples.
.sp
.IP \fI-title\fP " " ""
Generate all Fortran, C, and NCL Title examples.
.sp
.IP \fI-vectorplot\fP " " ""
Generate all Fortran, C, and NCL VectorPlot examples.
.sp
.IP \fI-xyplot\fP " " ""
Generate all Fortran, C, and NCL xyPlot examples.
.sp
.IP \fI-gsun\fP " " ""
Generate all the GSUN examples (NCL scripts only).
.sp
.IP \fI-list\fP " " ""
Specifies that the names of the selected examples should be echoed to the
screen instead of being copied or generated.  This option must be used
with one of the other options that selects a particular
subset of examples.  For example, if you want to see a list of all the
available examples, then use \fI-list\fP in conjunction with the \fI-A\fP
option.  If you want to see a list of all the ContourPlot examples, then
use \fI-list\fP with the \fI-contourplot\fP option (and so on).
.sp
.IP \fI-clean\fP " " ""
Remove everything but the ".ncgm" file.
.sp
.IP \fI-n\fP " " ""
Specifies that the example should just be copied, and not
linked or executed.
.sp
.IP \fIname(s)\fP " " ""
Name(s) of example programs to generate.
.SH "EXAMPLES AVAILABLE"
Please notice that the C examples end with the letter 'c', the
Fortran examples end with the letter 'f', and NCL examples with the letter 'n'.
.IP ap01c,ap01f,ap01n
Demonstrates various ways to use resource files.
.IP basic01c,basic01f,basic01n 1i
Demonstrates how to draw a contour plot using mostly defaults.  No data
is used in this example.
.IP basic02c,basic02f,basic02n 1i
Demonstrates how to set the view port and how to produce multiple plots on
a single frame.
.IP basic03c,basic03f,basic03n 1i
Demonstrates how to create a scalar data object, set resources using a
resource file, set resources during object creation, and set resources
after object creation.
.IP basic04c,basic04f,basic04n 1i
Demonstrates how to select and change the workstation device for
drawing your output to an NCGM file or an X workstation window.
.IP basic05c,basic05f,basic05n 1i
Demonstrates how to read and manipulate colormaps.
.IP basic06c,basic06f,basic06n 1i
Demonstrates how to position objects on an output device and how to change 
their sizes.
.IP basic07c,basic07f,basic07n 1i
Demonstrates creating three simultaneous workstations.
.IP basic08c,basic08f,basic08n 1i
Demonstrates use of a procedure that limits a plot, including
annotations that extend outside the plot viewport, to a pre-defined
bounding box within NDC space.
.IP basic09c,basic09f,basic09n 1i
Lists all the fonts with their number, name, and what the font looks like.
.IP cn01c,cn01f,cn01n 1i
Shows all the default settings for the ContourPlot resources using
a simple data set.
.IP cn02c,cn02f,cn02n 1i
Demonstrates basic features of the ContourPlot object.
.IP cn03c,cn03f,cn03n 1i
Demonstrates basic features of the ContourPlot object.  The first
frame emulates the contour plot drawn in cn01.
.IP cn04c,cn04f,cn04n 1i
Emulates the output of the ncargex example "cpex02".
.IP cn05c,cn05f,cn05n 1i
Demonstrates how to create a map plot animation with a contour overlay
and labelbar annotation.
.IP cn06c,cn06f,cn06n 1i
Shows how to read a netCDF file and produce a series of temperature
contour plots.
.IP cn07c,cn07f,cn07n 1i
Shows how to read a netCDF file and produce a series of
contour plots.
.IP cn08c,cn08f,cn08n 1i
Shows how to read a netCDF file and draw a vertical profiles of
temperature for longitude separated by 5 degrees.
.IP cn09c,cn09f,cn09n 1i
Shows how to read a netCDF file and produce a series of
surface pressure contour plots.
.IP cn10f,cn10c,cn10n 1i
Demonstrates how to read a netCDF file and produce three contour plots, an
xyplot, and a map plot.
.IP cn11c,cn11f 1i
Demonstrates how to combine LLU and HLU calls.
.IP cn12c,cn12f,cn12n 1i
Emulates example "cpex08" and draws a filled contour plot over a
map of Africa with a labelbar.
.IP cn13c,cn13f,cn13n 1i
Emulates example "mpex10" and draws a raster contour plot over a
map using inverse map transformations.
.IP cn14c,cn14f,cn14n 1i
Shows how to do a contour/map overlay with tick marks showing the lat/lon
locations.
.IP cn15c,cn15f,cn15n 1i
Shows how to combine contour plot and xy plot on a single frame. Also shows
how to do multiple workstation outputs, and how to position your PostScript 
output on the page.
.IP cn16c,cn16f,cn16n 1i
Shows how to combine a vector fill contour plot, a raster contour plot, and
a map plot on a single frame.
.IP cn17c,cn17f,cn17n 1i
Shows how to restrict a contour plot to a certain area over a
satellite projection, how to label the US states with AnnoManager, how
to use NhlDataPoly{line,marker} to draw lines/markers on a map
projection, and how to mix LLUs/HLUs to draw text in the map
projection. The NCL version doesn't have the LLU/HLU part.
.IP lb01c,lb01f,lb01n 1i
Shows all the default settings for the LabelBar resources.
.IP lb02c,lb02f,lb02n 1i
Shows how to modify some of the default resources to create a solid filled
set of boxes.
.IP lg01c,lg01f,lg01n 1i
Shows all the default settings for the Legend resources.
.IP lg02c,lg02f,lg02n 1i
Shows how to modify some of the default resources to create a legend of
five markers.
.IP lg03c,lg03f,lg03n 1i
Shows how to create a legend of five lines.
.IP pr01c,pr01f,pr01n 1i
Demonstrates basic GraphicStyle capabilities.
.IP pr02c,pr02f,pr02n 1i
Demonstrates GraphicStyle capabilities and how to some
GS resources.
.IP pr03c,pr03f,pr03n 1i
Demonstrates graphics primitives drawn in NDC space and how clipping works.
.IP pr04c,pr04f,pr04n 1i
Demonstrates graphics primitives drawn into an IrregularPlot object.
.IP pr05c,pr05f,pr05n 1i
Demonstrates overlaying graphics primitives on a MapPlot.
.IP mp01c,mp01f,mp01n 1i
Demonstrates basic MapPlot capabilities.
.IP mp02c,mp02f,mp02n 1i
Demonstrates individual control of MapPlot areas.
.IP mp03c,mp03f,mp03n 1i
Demonstrates MapPlot masking; loosely emulates the ncargex example "colcon".
.IP mp04c,mp04f,mp04n 1i
Illustrates the use of AnnoManager objects with MapPlot objects.
.IP mp05c,mp05f,mp05n 1i
Shows how to create the ten different map projections.
.IP mp06c,mp06f,mp06n 1i
Shows how to draw county lines in the United States.
.IP mp07c,mp07f,mp07n 1i
Shows how to use high resolution RANGS/GSHHS map database.
.IP nm01c,nm01f,nm01n 1i
Simple example of natural neighbor linear interpolation. 
.IP nm02c,nm02f,nm02n 1i
Simple example of natural neighbor linear regridding. 
.IP nm03c,nm03f,nm03n 1i
Shows how to retrieve aspects and slopes of an interpolation.
.IP nm04c,nm04f,nm04n 1i
Shows a simple 3D interpolation. 
.IP nm05c,nm05f,nm05n 1i
Shows how to vary the exponent of the distances in a simple 2D interpolation..
.IP nm06c,nm06f,nm06n 1i
Smoothing in a simple 2D interpolation.
.IP nm07n 1i
Illustrates use of ftcurv, ftcurv, and ftcurvi.
.IP nm08n 1i
Illustrates use of ftcurvp and ftcurvpi.
.IP nm09n 1i
Illustrates use of ftcurvs and ftcurvps.
.IP nm10n 1i
Illustrates use of ftkurv and ftkurvd.
.IP nm11n 1i
Illustrates use of ftkurvp and ftkurvpd.
.IP nm12n 1i
Illustrates use of ftsurf.
.IP nm13n 1i
Illustrates the use of csa1s.
.IP nm14n 1i
Illustrates the effect of weighting the input points using csa1xs.
.IP nm15n 1i
Illustrates extrapolation into data sparse regions using csa1xs.
.IP nm16n 1i
Illustrates two-dimensional approximation and second-order mixed partial using csa2s and csa2xs.
.IP nm17n,nm18n 1i
Illustrates the use of csa3s.
.IP nm19n 1i
Illustrates the use of csa2ls.
.IP nm20n 1i
Illustrates the use of triangulation and Voronoi diagram capabilities of the
cssgrid package.
.IP nm21n 1i
Illustrates the use of the interpolation capabilities of the cssgrid package.
.IP nm22n 1i
Illustrates the use of shgrid.
cssgrid package.
.IP st01c,st01f,st01n 1i
Demonstrates basic StreamlinePlot example.
.IP st02c,st02f,st02n 1i
Demonstrates line-drawn streamline arrows and some basic resources.
.IP st03c,st03f,st03n 1i
Shows how to overlay a StreamlinePlot on a map projection.
.IP st04c,st04f,st04n 1i
Shows a StreamlinePlot of wind vector data over a MapPlot.
.IP ti01c,ti01f,ti01n 1i
Shows all the default settings for the Title resources which results in a
blank plot.
.IP ti02c,ti01f,ti01n 1i
Turns on the main, x-axis, and y-axis strings to produce a plot with three
titles.
.IP ti03c,ti03f,ti03n 1i
Shows how to change resources to get high quality filled and colored titles.
.IP tm01c,tm01f,tm01n 1i
Shows all the default settings for the TickMark resources.
.IP tm02c,tm02f,tm02n 1i
Shows how to modify the resources to create tick marks on the axes.
.IP tm03c,tm03f,tm03n 1i
Shows how to modify the resources to change the scaling and the tick mark
labels.
.IP tx01c,tx01f,tx01n 1i
Shows all the default settings for the TextItem resources.
.IP tx02c,tx02f,tx02n 1i
Shows how to modify default resources to produce a text string.
.IP tx03c,tx03f,tx03n 1i
Uses the same resources as example "tx02c", except 114 frames are
produced with varying background colors.
.IP tx04c,tx04f,tx04n 1i
Shows how to use the TextItem object of the HLU library.
.IP tx05c,tx05f,tx05n 1i
Demonstrates the TextItem object with text having various heights and 
various angles.
.IP tx06c,tx06f,tx06n 1i
Demonstrates TextItem text justifications.
.IP tx07c,tx07f,tx07n 1i
Demonstrates TextItem text spacings and aspect ratios.
.IP tx08c,tx08f,tx08n
Simple Annotation example.
.IP vc01c,vc01f,vc01n 1i
Basic VectorPlot example.
.IP vc02c,vc02f,vc02n 1i
Demonstrates line-drawn vector arrows and the use of some basic
VectorPlot resources.
.IP vc03c,vc03f,vc03n 1i
Manipulates the FillArrow resources to demonstrate some of
the possible stylistic variations on the appearance of filled
vector arrows.
.IP vc04c,vc04f,vc04n 1i
Demonstrates how to rotate a VectorPlot 90 degrees while preserving
the original relationship of the data elements.
.IP vc05c,vc05f,vc05n 1i
Demonstrates how to overlay a VectorPlot on a MapPlot.
.IP vc06c,vc06f,vc06n 1i
Demonstrates how to use a scalarfield to determine the color of the
vector arrow fill.
.IP vc07c,vc07f,vc07n 1i
Emulates the LLU example "fcover"; shows how to overlay contours and vectors
on a MapPlot.
.IP vc08c,vc08f,vc08n 1i
Plots wind vectors at a grid stride of 3. Vectors are colored by wind speed.
.IP vc09c,vc09f,vc09n 1i
Plots an animation of the January 1996 snow storm using wind vectors colored by
temperature over a pressure field contour plot.
.IP xy01c,xy01f,xy01n 1i
Shows all the default resources for an xyPlot object.
.IP xy02c,xy02f,xy02n 1i
The same as example xy01c, only with some of the X and Y axis resources
tweaked.
.IP xy03c,xy03f,xy03n 1i
The same as example xy01c, but with the data modified a little, and the 
line resources tweaked.
.IP xy04c,xy04f,xy04n 1i
Demonstrates how to create an xyPlot with multiple lines.  Some of the line
resources are tweaked.
.IP xy05c,xy05f,xy05n 1i
Demonstrates how to create an XyPlot with multiple lines, using multiple
data items (except for xy05c, which uses the CoordArrTable object).
.IP xy06c,xy06f,xy06n 1i
Demonstrates how to read in netCDF files to generate various xy plots.
.IP xy07c,xy07f,xy07n 1i
Demonstrates how to draw a "scattergram".
.IP xy08c,xy08f,xy08n 1i
Shows how to use irregular points to change the transformation of your plot.
.IP xy09c,xy09f,xy09n 1i
Shows how to create different kinds of axes.
.IP xy10c,xy10f,xy10n 1i
Shows how to overlay an XyPlot on a MapPlot.
.IP xy11c,xy11f,xy11n 1i
Demonstrates how to display an xyPlot to an X Window, and then
copy it to a meta file.
.IP xy12c 1i
Demonstrates how to incorporate the HLU library with a GUI.
It uses an X/Motif interface, and also allows the user to copy frames from
the X Window to a meta file.
.IP xy13c 1i
Shows how to interactively retrieve data values from a plot by pointing
and clicking on the plot.
.IP xy14c,xy14f,xy14n 1i
Demonstrates the data manipulation capabilities of NCL.
.IP xy15c,xy15f,xy15n 1i
Demonstrates the creation of a set of four stacked XyPlots.
.IP xy16c,xy16f,xy16n 1i
Demonstrates how to draw three different xy lines on the same plot, but
with different axis scales.
.IP xy17c,xy17f,xy17n 1i
Demonstrates how to stack three xy plots.
.SH SEE ALSO
ncargex(1NCARG)
.SH COPYRIGHT
Copyright (C) 1987-2000
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
