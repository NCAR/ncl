.\"
.\"	$Id: plt.m,v 1.1.1.1 1992-04-17 22:30:30 ncargd Exp $
.\"
.\" @(#)plt.1 1.0 85/12/31 NCAR; from UCB 4.2
.TH PLT 1NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.SH NAME
plt \- user interface for \fINCAR metacode\fR translators
.SH SYNOPSIS
.B plt
[\ \fB\-C\fR\ ] 
[\ \fB\-L\fR\ ] 
[\ \fB\-P\fR\ ] 
[ \fB\-a\fR\ ] 
[ \fB\-d\ \fIdevice\fR\ ]
[ \fB\-f\ \fIframes\fR\ ]
[ \fB\-q\fR\ ] 
\fIfile\fR\ 
.SH DESCRIPTION
.LP
.I Plt
provides selective display and editing of \fINCAR metafiles\fR.
\fIPlt\fR is capable of running in batch mode, interactive mode or some
combination of both modes.
For example, the command
.ti +0.5i
plt -d t4107 file
.br
will prompt the user for frames to be displayed on the terminal (which
in this case is a Tektronix 4107).
The file must be an \fINCAR metacode\fR file.
.LP
If the device name is not specified on the command line,
.I plt
will use the value of the environment variable GRAPHCAP,
if has been set.
.LP
Some of the target device names on your system may designate a spooled device.
Normally all hardcopy devices are spooled.  When you select a spooled device
the plots will be sent to that device and not displayed on your terminal.
.LP
Sometimes the user would like to check the results on a terminal before 
sending to a hardcopy device.  This can be accomplished by selecting the
proper terminal type for display,
interactively previewing some of the frames, 
selecting the hardcopy device, and finally selecting the 
frames for hardcopy.
.LP
Command-line options are:
.LP
.IP \fB\-C\fP
The complex mode option.
This option is recognized only by the Imagen laser printer.
When rasterization errors occur on the Imagen, this option will correct the 
problem.  It activates the \fIprinteverypage\fP document control parameter.
Discretion should be used because it causes the Imagen to slow down.
.IP \fB\-L\fP
Activate landscape mode.
This option is recognized only by the Imagen laser printer.
Landscape orientation is the default arrangement.
Enough room is allocated to punch 3-ring binder holes.
.IP \fB\-P\fP
Activate portrait mode.
This option is recognized only by the Imagen laser printer.
Enough room is allocated to punch 3-ring binder holes.
.IP \fB\-a\fP
Display all frames before returning control.
.IP \fB\-d\fP\fI\ device\fP
Designate the target device for \fImetafile \fP translation.
A validation check of the device is made.  If it is invalid, \fIplt\fP
will list valid device names and prompt for a new device name.
If the device is spooled, no plotting will appear on your terminal.
.IP \fB\-f\fP\fI\ frames\fP
Plot the requested frames named by their position in the \fImetafile\fP.
In the following example frames 2, 6 and 1 from file \fIpictures\fP
are sent to the Imagen laser printer.
In this example, the Imagen is a spooled device.
.ti +0.5i
plt -d imagen -f 2 6 1 pictures
.br
Frame positions are numbered from 1.
.IP \fB\-q\fP
Quit after command line processing.  This option allows total batch execution 
of \fIplt\fP.
Batch execution is handy for spooled devices where no display is intended 
to be sent to the user's terminal.
In the following example all frames from file \fIcolors\fP 
are sent to the spooled
device, HP7510, which is a color film recorder.  All diagnostics are 
sent to file \fIdiag\fP and the process is run in background.
.ti +0.5i
plt -d hp7510 -a -q colors >&! diag &
.br
.LP
Unknown command-line options will be passed on to the spooled device.
\fR
\fPInteractive options: if \fIplt\fP is \fInot\fP in batch
mode (\fB\-q\fP on the command line),
\fIplt\fP will prompt the user for more options after the command line 
options have been processed.  When \fIplt\fP is ready for an 
interactive option it will
display the \fBPLT>\fP prompt.  The following options are available at that
time:
.IP \fIentering\ only\ a\ carriage\ return\fP
\fIPlt\fP will display the next frame in the \fImetafile\fP.
.IP \fIentering\ a\ number\fP
\fIPlt\fP will display that frame position in the \fImetafile\fP.
Numbering of frame positions starts at 1.
.IP \fB\C,\ L,\ P\fP
These options have the same meaning as when given as command line options.
.IP \fBa\fP
Display all frames before returning control.
.IP \fBc\fP\fI\ frame\ \fP[\fIfile\fP]
Copy the frame indicated by a position number.  
Only one frame may be copied per request.
If \fIfile\fP is present,
copy will append to that file.  If no filename is given, copy appends to
the default file, \fBMETACOPY\fP.
.IP \fBd\fP\ [\fIdevice\fP]
Designate the target device for \fImetafile \fP translation.
If the device name is not given, \fIplt\fP will prompt for it.
A validation check of the device is made.  If it is not valid \fIplt\fP
will list valid device names and prompt again.
.IP \fBh\fP
Help.  List the interactive command description on standard
output.
.IP \fBm\fP\fI\ frames\fP\ [\fIfile\fP]
Merge frames together,
so that the listed frame positions will display as one picture.
This operation merges frames into another file.
That file may then be selected for display.
If \fIfile\fP is provided the merged files are appended to that file.
If no filename is given, merge appends to the default file,
\fBMETAMERG\fP.
The following example merges frames 2, 5 and 1 into a single frame in file
merger.meta
.ti +0.5i
m 2 5 1 merger.meta
.br
See \fBBUGS\fP for further discussion.
.IP \fBn\fP\ \fImetafile\fP 
Select a new file for processing.
.IP \fBq\fP
Terminate \fIplt\fP.
.br
.LP
Unknown interactive options are ignored.
\fR
.SH "SEE ALSO"
.LP
lplot(1)
.SH DIAGNOSTICS
.LP
All messages generated by \fIplt\fR should be self-explanatory.
.SH BUGS
.LP
The \fINCAR metafile\fR record structure is amended for implementation of
merge frames.
The merging of frames is recognized only by the \fINCAR CGM translator\fR.
All other translators will interpret merged frames as a string of individual
frames.
.LP
The destination filename for merged frames cannot begin with a digit.
.SH AUTHOR
Donna Converse
