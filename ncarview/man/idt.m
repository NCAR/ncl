.\"
.\"	$Id: idt.m,v 1.9 1992-08-12 23:07:58 clyne Exp $
.\"
.\" NCAR View: idt.man,v 1.0 89/09/10 clyne 
.\" Revision 3.01 90/11/15 clyne
.TH IDT 1NCAR "15 November 1990"
.SH NAME
idt \- X window interactive image display tool
.SH SYNOPSIS
.B idt 
[ 
.BI \-toolkitoption " ..."
] [
.BI \-d " device"
] [
.BI \-f " font"
] [
.BI \-history
] [
.BI \-lmin " min"
] [
.BI \-lmax " max"
] [
.BI \-lscale " scale"
] [
.B \-oldidt
] [
.BI \-pal " pal_fname"
] [
.B \-soft
] [
.B \-Version
] [
.BI metafile
]
.SH DESCRIPTION
.B idt
provides a graphical user interface to the NCAR View interactive metafile
translator
.BR ictrans .
.B idt 
supports a subset of the 
.B ictrans
command interface. 
.PP
.B idt 
provides two types of command panels for interacting with imagery. The 
first type is the control panel, which you see when you initially invoke 
.BR idt . 
The control panel displays messages from the translators, provides
a metafile selection utility and is responsible for instantiating the second
type of command panel, the display panel. The display panel provides 
mechanisms for controlling translators. Each display panel manages a single
metafile translator. There can be multiple 
display panels in existence at the same time but only one control panel.
.SH OPTIONS
.B idt
accepts all of the standard X Toolkit command line options (see X(1)). 
.B idt
also accepts the following options:
.IP \fB\-d\fP\fI\ device \fP
Device name.
.B idt
will translate the metafile for the specified device. If 
.I device 
is 
.B X11
then the translated metafile will be displayed in an X window on the 
display pointed to by the DISPLAY environment variable. The DISPLAY 
environment variable MUST be set if 
.I device 
is 
.BR X11 .
For all other valid devices 
.B idt
will use the
.I Graphcap
file indicated by
.IR device . 
If the NCAR View graphcaps are not installed to their standard locations
for your system the name of the device must be preceded with a path
specification. For example, /usr/local/lib/graphcaps/t4010.
The translated metafile
in the case of a graphcap supported device is written to standard out. 
.IP
For compatibility with previous
translators, if a device is not specified
on the command line 
.B idt
will default to the device specified by
the GRAPHCAP environment variable. If the GRAPHCAP environment variable
does not exist a default device of 
.B X11
is assumed.
.IP \fB\-f\fP\fI\ fontcap \fP
Specify the file containing the font description to be used for stroking
text during metafile translation. The default font is 
.BR font1 .
.IP \fB\-history\fP
Write a record of all commands sent to the translator to the file
.BR ./.idthist .
.IP \fB\-oldidt\fP
Present the old, pre-version-3.2, graphical user interface.
.IP \fB\-Version\fP
Print the version number and then exit.
.PP
The following options are ignored by \fBidt\fR and are passed on to the
metafile translator \fBictrans\fR. See the man page for \fBictrans\fR for
a description of their use.
.IP \fB\-soft\fP
.IP \fB\-lmin\fP\fI\ linewidth \fP
.IP \fB\-lmax\fP\fI\ linewidth \fP
.IP \fB\-lscale\fP\fI\ linewidth \fP
.IP \fB\-pal\fP\fI\ pal_fname \fP
.IP \fB\-foreground\fP\fI\ color \fP
.IP \fB\-background\fP\fI\ color \fP
.IP \fB\-reverse\fP
.SH ENVIRONMENT
.IP GRAPHCAP
The GRAPHCAP environment variable is an alternative to the
.B -d
option for specifying the output device for translation. The command line 
option takes precedence over the environment variable.
.IP FONTCAP
The FONTCAP environment variable is an alternative to the
.B -f
option for specifying the name of the fontcap for text stroking. The 
command line 
option takes precedence over the environment variable.
.IP DISPLAY
This is the standard X environment variable for specifying display
name. If the translation output device is
.B X11
this variable MUST be set. 
.IP SHELL
Specifies the UNIX shell that the file selector is to use for 
expanding metacharacters. /bin/sh is used by default.
.IP XENVIRONMENT
Specifies the name of a resource file that overrides the global resources
stored in the RESOURCE_MANAGER property.
.SH "CONTROL PANEL COMMANDS"
The control panel provides a text widget for displaying messages from 
the translators and a row of command buttons. Messages are preceded with
the string "Translator[X]", where "X" is an integer id associated with the
translator sending the message. The first translator spawned 
is "Translator[0]", the second is "Translator[1]", etc.
.IP "select file"
Selects a file for translation. This button brings up a popup menu containing
a file selection box. The uppermost dialog box of the file selection box is
used for specifying a filter for searching for files. For example, the filter
"/usr/local/lib/*.cgm" finds all files in the directory /usr/local/lib whose
names end with ".cgm". The finder understands standard shell 
metacharacters. 
.IP
The bottom dialog box displays the currently selected file. The
user may enter a file name here or select one with the mouse from the list of
files displayed in the middle text widget.
.IP "display"
Popup a display panel. A display panel is popped up ready for translation
of the file most recently selected with the file selector. This command
will fail if a file has not been selected.
.IP "quit"
Quits the application and terminates all translators.
.SH "DISPLAY PANEL COMMANDS"
The display panel provides a mechanism for manipulating the translator
associated with a particular metafile. On most systems the name of the 
metafile appears in the title bar at the top of the panel. The
first row in the panel contains a scroll bar for randomly selecting
a frame to be translated. The label bar to the right displays the number
of the frame scrolled to. The panel
is intended to resemble a video tape editor. Thus, the second row
contains play back, jog back, stop, jog and play buttons, respectively. 
The third and forth rows contain more complex display commands.
.IP loop
Toggle the looping mode on or off. When loop is on pressing the play 
(playback) button causes the translator to process all the frames in the
segment sequentially (reverse sequentially) and then start over from
the beginning (end). The process continues until the "stop" button 
is selected. By default loop mode is off.
.IP dup
Sets duplication variable. If dup is set to 
.B 1 
each frame is displayed once,
if dup is set to 
.B 2
 each frame is displayed twice, etc. The default
value of dup is
.BR 1 .
.IP goto
Go to the specified frame and translate it.  
.B goto 
provides random access to the frames contained in the metafile.
.IP skip
Set the skip variable. If skip is set to 
.B 1
every other frame is displayed
during subsequent plays, if skip is set to 
.B 2
every third frame is 
displayed, etc. The default value for skip is 
.BR 0.
.IP "start segment"
Define the first frame in the segment. This button and the 
.B stop segment
button define a segment which is a subset of the frames contained 
in the metafile. When in play (playback) mode only the frames contained
within the defined segment are translated. The default value for 
.B start segment
is
.BR 1 ,
the first frame in the file.
.IP "stop segment"
Define the last frame in the segment. The default frame is the 
last frame in the file. Hence, by default the segment is the entire 
metafile.
.IP "set window"
Specify the workstation window (in the GKS sense). Four 
coordinates are specified
which define a rectangular window which is a subset of normalized VDC
rectangle with corner points (0,0) and (1.0,1.0). The specified window
is then mapped onto the entire display window. For example, if the workstation
window is defined by the corner points (0,0) and (0.5 0.5) then the lower
left quarter of a plot would be blown up to fill the entire window. 
Specification of such a window can be used for zooming and panning.
.IP done
Terminate processing of current metafile.
.IP "current frame"
Update the "Scrolled to Frame ->" label box with the number of the current
frame.
.IP print
Send the current frame to the printer. The 
.B print
command brings up a menu of all devices configured for accepting 
translator output. This list is created dynamically in response to 
changes in the spooler configuration file (see ncarv_spool(l)). 
Selecting an item from the menu causes the current frame to be sent
to that device.
.IP save
Save a frame to a file. This command allows you to write the metacode
for the current frame to a file. The result is a complete NCAR 
Computer Graphics Metafile (CGM) containing a single frame.
.IP unzoom
Reset windowing transformatations previously set by the
.B zoom
command back to their default. 
.IP zoom
Zoom in on an area of a plot. This command only works when translation
output is to an X window. 
.B zoom
provides you with a cursor with which you can rubberband an area of your X 
window. The area enclosed will be re-rendered to fit the dimensions of 
the window while preserving the aspect ratio of the viewport described by
the rubberband.
.SH RESOURCES
As with all standard X applications,
.B idt
may be customized through entries in the resource manager. 
In the following list of application resources provided by
.B idt 
the resource name is given followed by its class in parentheses.
These resources supplement the resources provided by the widgets themselves.
.IP "history (Class History)"
Specify whether a history of commands sent to the metafile translators
is recorded to the file "./.idthist" or not. The default for this resource
is "False".
.IP "fileSelectAction (Class FileSelectAction)"
Set the default action to be executed after a file has been selected
with the file selection box. Currently the only value this resource 
understands is "display". 
.IP "messageHeight (Class MessageHeight)"
Set the height in lines of text of the message display panel.
.IP "oldIdt (Class OldIdt)"
Specifies the "-oldidt" option.
.IP "translatorDevice (Class TranslatorDevice)"
Specify the output device for metafile translation. The default for this
resource is "X11".
.IP "translatorFont (Class TranslatorFont)"
Specify the name of the fontcap to use for stroking text during metafile
translation. The default value for this resource is "font1".
.IP "translatorSoft (Class TranslatorSoft)"
Specifies the \fBictrans\fR "-soft" option.
.IP "translatorLmin (Class TranslatorLmin)"
Specifies the \fBictrans\fR "-lmin" option.
.IP "translatorLmax (Class TranslatorLmax)"
Specifies the \fBictrans\fR "-lmax" option.
.IP "translatorLscale (Class TranslatorLscale)"
Specifies the \fBictrans\fR "-lscale" option.
.IP "translatorForeground (Class TranslatorForeground)"
Specifies the \fBictrans\fR "-foreground" option.
.IP "translatorBackground (Class TranslatorBackground)"
Specifies the \fBictrans\fR "-background" option.
.IP "translatorReverse (Class TranslatorReverse)"
Specifies the \fBictrans\fR "-reverse" option.
.SH ACTIONS
.B idt 
provides the following actions for use in event translation:
.IP FinderTranslation()
This action causes the file selector popup to search for files using the key
supplied in the file finder text box.
.IP OkFinderTranslation()
This action notifies the file selector that a selection has been made.
.IP SelectFileTranslation()
This action updates the text box in the file selector which displays 
the currently selected file.
.IP OkSDTranslation()
This action confirms a selection made in one of the display panel popup
dialog boxes.
.SH WIDGETS
In order to specify resources, it is useful to know the hierarchy
of the widgets which make up
.BR idt .
In the notation below, indentation indicates hierarchal structure. The
widget class name is given first, followed by the widget instance name.
.sp
.nf
Idt  idt
	Paned  paned
		Text  text
		Form  form
			Command  "select file"
			Command  display
			Command  quit

The hierarchy of the file selection box popup

	TransientShell  file
		Paned  pane
			Dialog "file finder"
				Label  label
				Command  finder
			AsciiText  textDisplay
			Dialog  selection
				Label  label
				Command  ok
				Command  cancel

The hierarchy of the display panel popup

	TopLevelShell  <??> (The toplevel is named after the metafile displayed)
		Paned  paned
			Form  form
				Scrollbar  scrollbar
				Label  "Scrolled to Frame ->"
			Form  form
				Command playback
				Command jogback
				Command stop
				Command jog
				Command play
			Form  form
				Toggle loop
				Command dup
					TransientShell  simpleDialog
						Dialog  dialog
							Label  label
							Command  ok
							Command  cancel
				Command goto
					TransientShell  simpleDialog
						Dialog  dialog
							Label  label
							Command  ok
							Command  cancel
				Command skip
					TransientShell  simpleDialog
						Dialog  dialog
							Label  label
							Command  ok
							Command  cancel
				Command "start segment"
					TransientShell  simpleDialog
						Dialog  dialog
							Label  label
							Command  ok
							Command  cancel
				Command "stop segment"
					TransientShell  simpleDialog
						Dialog  dialog
							Label  label
							Command  ok
							Command  cancel
				Command "set window"
					TransientShell  simpleDialog
						Dialog  dialog
							Label  label
							Command  ok
							Command  cancel
			Form  form
				Command done
				Command "current frame"
				MenuButton  print
					SimpleMenu  menu
						SmeBSBObject  <???>
						SmeBSBObject  <???>
						SmeBSBObject  <???>
							|
							|
						  dynamically configured
							|
							|
						SmeBSBObject  <???>
				Command  save
					TransientShell  simpleDialog
						Dialog  dialog
							Label  label
							Command  ok
							Command  cancel
				Command  zoom
				Command  unzoom
.fi
.sp
Paned
.SH EXAMPLES
.PP
The following resource specification can be used to set the default size 
of the graphics display window to 512 by 512 pixels and place it in the 
top left corner of your screen:
.sp
.ti +0.5i
	ctrans*geometry:	512x512+0+0
.ti -0.5i
.br
.PP
If you want to place the control panel in the top right corner insert the
following into your .Xdefaults file:
.sp
.ti +0.5i
	idt.geometry:        -0+0
.ti -0.5i
.br
.PP
Finally, if you want the display panel to appear slightly below the graphics
display window try setting its geometry resource as follows:
.sp
.ti +0.5i
	idt.TopLevelShell*geometry:        +0+536
.ti -0.5i
.br
.PP
Depending on how your window manager adorns its windows you may have to 
slightly adjust the 'y' parameter of this geometry request.
.SH FILES
.TP 40
\\.idthist
- Translator command history file
.TP 40
/usr/lib/X11/app-defaults/Idt
- Default resource file for 
.B idt
.TP 40
$HOME/.ncarv_spool
- User spooler configuration file
.TP 40
/usr/local/lib/ncarv_spool
- System spooler configuration file
.SH "SEE ALSO"
.BR ctrans(l),
.BR fcaps(l),
.BR gcaps(l),
.BR ictrans(l),
.BR ncarv_spool(l),
.BR X(1),
.BR xrdb(1)
.SH BUGS
Zooming in on large cell arrays causes the translator to crash.
.SH CAVEATS
The metafile file translation process is performed by spawning the
.B ictrans
translator. 
.B ictrans 
does not understand standard X Toolkit options. Hence, the DISPLAY environment 
variable must be set when translating a metafile to be displayed in a window.
.PP
You can not save "zoomed" plots to a file. More precisely, you may save a
"zoomed" plot, however, the resultant plot will not be "zoomed".
.PP
The "Scrolled to Frame" label is not continuously updated.
.PP
The current version of 
.B ictrans
must be on your search path.

