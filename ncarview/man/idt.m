.\"
.\"	$Id: idt.m,v 1.30 2008-12-23 00:04:16 haley Exp $
.\"
.\" NCAR View: idt.man,v 1.0 89/09/10 clyne 
.\" Revision 3.01 90/11/15 clyne
.TH IDT 1NCARG "January 1993" NCARG "NCAR GRAPHICS"
.SH NAME
idt \- X window interactive image display tool
.SH SYNOPSIS
.B idt 
[ 
.BI \-toolkitoption " ..."
] [
.BI \-background " color"
] [
.BI \-f " font"
] [
.BI \-foreground " color"
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
.B \-reverse
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
.TP 
.BI \-f " fontcap"
Fontcap file to be used for stroking text.
When interpreting CGM
.B TEXT
command elements use
.I fontcap
as the default font for textual translation. Note: CGMs may contain textual
descriptions which are not embedded in CGM
.B TEXT
elements. Hence they are not influenced by
.I fontcap
specifications.
Note also that a CGM may explicitly specify a named font which may override a
font provided on the command line. The environment variable FONTCAP
may also be used to specify a default fontcap.
.IP
If 
.I fontcap
is preceded by a UNIX directory path then 
.B ctrans
will look in that directory for the specified fontcap. Otherwise 
.B idt
searches the directory $NCARG_ROOT/lib/ncarg/fontcaps for the fontcap.
.IP
See 
.BR fontcap(5NCARG)
for a description of the available fontcaps. See
.BR fcap(1NCARG)
for a list of the fontcaps installed on your
system.
.IP
This option overrides the 
.B FONTCAP
environment variable.
.TP 
.B \-history
Write a record of all commands sent to the translator to the file
.BR ./.idthist .
.TP 
.B \-Version
Print the version number and then exit.
.PP
The following options are ignored by \fBidt\fR and are passed on to the
metafile translator \fBictrans\fR. See 
.BR ictrans(1NCARG)
for a description of their use.
.TP
.B \-soft\fP
.TP
.BI \-lmin " linewidth"
.TP
.BI \-lmax " linewidth"
.TP
.BI \-lscale " linewidth"
.TP
.BI \-pal " pal_fname"
.TP
.BI \-foreground " color"
.TP
.BI \-background " color"
.TP
.B \-reverse
.SH "CONTROL PANEL COMMANDS"
The control panel provides a text widget for displaying messages from 
the translators and a row of command buttons. Messages are preceded with
the string "Display[X]", where "X" is an integer id associated with the
translator sending the message. The first translator spawned 
is "Display[0]", the second is "Display[1]", etc.
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
.IP delay
Set the between-frame animation delay-time. When 
.B idt
is in animation mode you may request that
.B idt 
pause for a period of time between the display of each image.
The effect of setting a delay time is 
to govern the speed at which 
.B idt
"plays" imagery.
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
is then mapped onto the entire viewport. For example, if the workstation
window is defined by the corner points (0,0) and (0.5 0.5) then the lower
left quarter of a plot would be blown up to fill the largest rectangle
which fits in the drawing window while retaining the aspect ratio 
described by the normalized coordinates.
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
changes in the spooler configuration file (see ncarv_spool(5NCARG)). 
Selecting an item from the menu causes the current frame to be sent
to that device.
.IP save
Save a frame to a file. This command allows you to write the metacode
for the current frame to a file. The result is a complete NCAR 
Computer Graphics Metafile (CGM) containing a single frame.
.IP unzoom
Reset windowing transformations previously set by the
.B zoom
command back to their default. 
.IP zoom
Zoom in on an area of a plot. The function of this command is identical
to that of the
.B window 
command. The
.B zoom
command, however, allows you to select the area of interest interactively
with the mouse.
.IP animate
Toggle 
animation mode on or off. When 
.B idt
is put into animation mode the currently defined segment is rasterized
with the translator and loaded into the X server memory. Subsequent 
.B idt
commands operate on the memory-resident images. Not all 
.B idt 
commands are available in animation mode. By default animation is 
off.
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
            Command  select file
            Command  display
                TopLevelShell  <???>
                    Paned  paned
                        Core  canvas	/* This is the drawing canvas
                        Form  form
                            Scrollbar  scrollbar
                            Label  Scrolled to Frame -> 
                        Form  form
                            Command  playback
                            Command  jogback
                            Command  stop
                            Command  jog
                            Command  play
                        Form  form
                            Toggle  loop
                            Command  dup
                            Command  goto
                            Command  skip
                            Command  delay
                            Command  start segment
                            Command  stop segment
                            Command  set window
                        Form  form
                            Command  done
                            Command  current frame
                            MenuButton  print
                                SimpleMenu  menu
                                    SmeBSB  <???> 
                                    SmeBSB  <???> 
                                    SmeBSB  <???>
                                              |
                                              |
                                    dynamically configured
                                              |
                                              |
                                    SmeBSB  <???> 
                                    SmeBSB  <???>
                            Command  save
                            Command  zoom
                            Command  unzoom
                            Toggle  animate
                        Grip  grip
                        Grip  grip
                        Grip  grip
                        Grip  grip
                        Grip  grip
            Command  quit
        Grip  grip
        Grip  grip
.fi
.sp
Paned
.SH EXAMPLES
.PP
The following resource specification can be used to set the default size 
of the graphics display window to 200 by 200 pixels:
.sp
.ti +0.5i
.nf
	Idt*canvas.width:	200
	Idt*canvas.height:	200
.fi
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
.SH ENVIRONMENT
.TP
.B DISPLAY
This is the standard X environment variable for specifying display
name. If the translation output device is
.B X11
this variable MUST be set. 
.TP
.B FONTCAP
Default fontcap specifier.
.TP
.B NCARG_ROOT
Path to root of NCAR Graphics installation.
.TP
.B NCARG_LIB
If set this variable contains the path to the installed NCAR Graphics 
libraries. 
.B NCARG_LIB
overrides 
.BR NCARG_ROOT .
.TP
.B NCARG_TMP
If set, this environment variable contains a directory path to be used for
temporary files. On most systems the default is 
.BR /tmp .
On some systems the default is 
.BR /usr/tmp .
.TP
.B SHELL
Specifies the UNIX shell that the file selector is to use for 
expanding metacharacters. /bin/sh is used by default.
.TP
.B XENVIRONMENT
Specifies the name of a resource file that overrides the global resources
stored in the RESOURCE_MANAGER property.
.SH FILES
.TP 40
\\.idthist
- Translator command history file
.TP 40
$NCARG_ROOT/lib/ncarg/xapp/Idt
- Default resource file for 
.B idt
.TP 40
$HOME/.ncarv_spool
- User spooler configuration file
.TP 40
$NCARG_ROOT/lib/ncarg/ncarv_spool
- System spooler configuration file
.SH "SEE ALSO"
.BR ctrans(1NCARG),
.BR fcaps(1NCARG),
.BR gcaps(1NCARG),
.BR ictrans(1NCARG),
.BR ncarv_spool(5NCARG),
.BR X(1),
.BR xrdb(1)
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
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
The range with which one may zoom in on a plot with either the 
.B zoom
or 
.B window 
command is severely limited by Xlibs use of 'short' integers
for containing screen coordinate data.
.PP
IRIX 4.x users should not set use the shared memory connection to their
X server. i.e don't use the display shm:0.
.PP
Background color changes don't work when 
.B idt
is in animate mode. 
.B idt
will use whatever color was most recently set in the animation segment
for the entire segment.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
