.\"
.\"	$Id: cgm_edit.m,v 1.6 1993-01-16 00:02:14 clyne Exp $
.\"
.\" NCAR View: cgm_edit.man,v 1.0 89/09/10 clyne 
.TH CGM_EDIT 1NCARG "September 1989" NCARG "NCAR GRAPHICS"
.SH NAME
cgm_edit \- X window interface to the \fINCAR View\fR \fBcgm\fR library.
.SH SYNOPSIS
.B cgm_edit 
[-\fItoolkitoption\fR...] ... [\fImetafile\fR]
.SH DESCRIPTION
\fBcgm_edit\fR provides a window-based interactive front end to the 
\fINCAR View\fR
\fBcgm\fR library. The application provides a window consisting of the 
following four areas:
.IP "Metafile Display" 
Displays contents of the metafile in a line editor format. Each line of
the \fIdisplay\fR corresponds to the respective ordered frame in the metafile.
Within each line are contained the frame number, record count and optionally
a textual description of that frame.
.IP "Address Menu"
Each editing command requires either one or two frame addresses to operate
on. A frame address is the \fIdisplay's\fR line number which corresponds
to that frame. Addresses may be \fIselected\fR from the display
window with the mouse by highlighting the appropriate lines.
Alternatively, addresses may be supplied with the keyboard conforming
to the following format: address ::= <a> | <a><,><a+1> | <a><-><b> 
where a and b are valid 
frame numbers and b > a.
.IP "Edit Command Menu"
\fIEdit\fR commands are used to modify the contents of the metafile. All 
\fIedit\fR commands that \fIread\fR a frame require a \fIsource\fR address.
\fIEdit\fR commands that \fIwrite\fR a frame require a destination address. 
For example;
the \fBcopy\fR command will copy a frame at the given \fIsource\fR address
to the given \fIdestination\fR address.
.IP "File Command Menu"
\fIFile\fR commands allow selection of the metafile to be edited. \fIFile\fR
commands will generaly require a metafile path name which the user provides
in the \fIaddress menu\fR.
.SH EDIT COMMANDS
.IP "copy"
Copy a frame or block frames to another location within the file being
edited. The frames are obtained from \fIsource\fR and are deposited at 
\fIdestination\fR.
.IP "delete"
Delete a frame or block of frames from the metafile. The frame(s) address 
are specified as \fIsource\fR address.
.IP "move"
Like \fBcopy\fR except the frame(s) are moved.
.IP "merge"
Merge two or more frames on top of one another.  All frames are specified 
in the \fIsource\fR address block. The resulting merged frame is left
in the first \fIsource\fR address.
.IP "read"
Reads a metafile into the editor at the desired \fIdestination\fR address.
.IP "write"
Writes a frame or block of frames from \fIsource\fR to the named file.
.IP "label"
Supply a textual label to the file(s) addressed in \fIsource\fR.
.SH FILE COMMANDS
.IP "save"
Saves the contents of the current editing session to the named file.
.IP "quit"
Exits the editing session. If any changes have not been saved 
.B cgm_edit
displays a warning message and allows the user to save the file.
.IP "edit"
Loads a file into the editor. If the current file has unsaved changes
.B cgm_edit 
displays a warning message and allows the user to save
the changes.
.SH RESOURCES
\fBcgm_edit\fR excepts all the standard X Toolkit resource specifications.
The following is a list of the name and classes of widgets used by the
application:
.sp
.IP "\fBName, Class\fR" 2.0i
\fBComments\fR
.sp
.IP "cgm_edit, Cgm_edit"
The toplevel shell that encloses the application.
.IP "vpane, VPaned"
The frame that manages the geometry for the widgets.
.IP "text, AsciiString"
The widget which displays the metafile's contents.
.IP "addressMenu, Box"
The address menu widget.
.RS +0.5i
.IP "source, Dialog" 2.0i
The source address widget.
.IP "destination, Dialog" 
The destination address widget.
.IP "metafile, Dialog" 
The metafile path name widget.
.RS -0.5i
.IP "editMenu, Box" 2.0i
The edit command menu.
.IP "fileMenu, Box"
The file command menu.
.PP
Additionally, within each menu are widgets of class \fICommand\fR whose
names are displayed within the widget.
.SH "TOOLKIT OPTIONS"
\fBcgm_edit\fP also accepts the standard Toolkit command options:
"+rv",
"-background",
"-bd",
"-bg",
"-borderwidth",
"-bordercolor",
"-bw",
"-display",
"-fg",
"-fn",
"-font",
"-foreground",
"-geometry",
"-iconic",
"-name",
"-reverse",
"-rv",
"-synchronous",
"-title",
"-xrm".
Some of the options are meaningless for \fBcgm_edit\fP.
.SH FILES
.TP 40
/usr/local/lib/libcgm.a
- The \fINCAR View\fR \fBcgm\fR library
.TP 40
/usr/lib/X11/app-defaults/Cgm_edit
- Default resource file for \fBcgm_edit\fR
.SH "SEE ALSO"
X(1), xrdb(1), idt(1NCARG)
.SH BUGS
There is no \fIundo\fR command... yet.
.SH CAVEATS
A CGM may contain \fImetafile descriptor\fR elements that globally effect
the display of each frame contained the file. Thus \fIframes\fR are not
independent entities. Hence reading in frame from other files may give
surprising results. Similarly, writing a frame out to a file requires
that this boiler plate be included.  See the ANSI standard on CGM 
(ANSI X3.122 - 1986)
