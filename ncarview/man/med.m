.\" @(#)med.l 1.0 90/02/15 NCAR; from UCB 4.3 and S5R4
.tr ##
.TH MED 1NCARV  "February 1990" NCAR "NCAR View 3.01"
.SH NAME
med \- NCAR CGM metafile frame editor
.SH SYNOPSIS
.LP
.B med
[
.BI \-e " script"
] [
.BI \-f " sfilename"
] [
.BI \-l " local_dir"
] [
.IR filename ]
.SH DESCRIPTION
.LP
.B med 
is a metafile frame level editor designed to resemble 
syntactically 
.SM UNIX's 
.BR sed (1) 
and 
.BR ed(1).
.B med 
operates on a copy of 
.IR filename ,  
called  a  buffer,  and
overwrites a file only when you issue the 
.B w 
(write) command.
.B med 
provides line oriented editing commands to display or delete
frames from the buffer, to move, copy or merge frames within the buffer,
or to write frames from and read frames into the buffer. By default
.B med
reads in commands from standard input.
.SH OPTIONS
.TP
.BI \-e " script"
.I script
is a list of editing commands command for
.BR med .
Each command must be separated by a newline.
.TP
.BI \-f " sfilename"
Take the script from
.IR sfilename .
.TP
.BI \-f " local_dir"
Use 
.I local_dir 
as the tmp directory for scratch disk space instead of the default
.IR /tmp .
.SH USAGE
.SS "Command Structure"
.LP
.B med 
commands attempt to have a syntactically identical form to those of
.BR ed (1)
or 
.BR sed (1) .
commands consist of an optional
.IR address 
or two optional, comma separated, 
.IR addresses ,
followed by a 
.IR command ,
which may be abbreviated, 
possibly followed by a third
.IR address
or a
.IR command
specific 
.IR argument
list :
.IP
[ \fIaddress\fR [\fB,\fR \fIaddress\fR ] ] \fIcommand\fR 
[ \fIaddress | args\fR ]
.LP
.RE
If only one
.I address
is specified, operations are performed on that frame.  If two
.IR address es
are specified,
.B med
performs the operation on the inclusive range of frames.
If no address is specified than the
.I current
frame is used as a default. The exception to this is the
.B w
(write) command. The default address for 
.B w
is the entire buffer.
.LP
For example,
.B 1,10p
means \(lqprint (display) frame 1 through 10\(rq
(two addresses),
.B d
means \(lqdelete the current frame\(rq (no
address implies the current frame used as default), and
.B 1,5c5
means \(lqappend a copy of frames 1 through 5 at frame 5\(rq
(three addresses).  The meaning of
.I argument
varies for each operation. In 
.B med's
current state the only valid argument is a filename \(em for the 
write,  
.B w ,
command for instance, 
.I argument 
is the name of the file to write to.
.LP
Unlike its friends
.BR ed (1)
and 
.BR sed (1)
.B med
attempts to be fairly user friendly. If an invalid or ambiguous command
is given
.B med 
will tell you so. If 
.B med 
thinks it recognizes a command with invalid arguments a usage statement
for the offending command is given. If a particular command fails and 
.B med 
is smart enough to figure out why it will tell you. 
.B med 
will almost ALWAYS immediately terminate when an error occurs
while processing commands from a file or the command line. Help
is available by using the 
.B h 
command. Usage statements for a particular command may be obtained
with:
.B h 
.I command name .
.SS Addresses
.LP
Frames can be addressed in several ways:
.TP
.I nnn
By frame number.
Frames in the buffer are numbered relative to
the start of the buffer.  
.TP
.B $
The last frame of the buffer.
.TP
.B \&.
The current frame.
.B med
keeps track of the frame on which you last performed an operation.
This frame is called the
.IR "current frame" .
You can address this frame by typing a \(lqdot\(rq character.
.TP
.BI \(+- n
By relative frame number.
Address the frame number that is
.I n
frames higher, or
.I n
frames lower than the current frame.
.TP
.IB address \(+- n
An address followed by a plus sign
.RB (\^ + \^)
or a minus sign
.RB ( \- ),
followed by a decimal number, specifies
that address plus or minus the indicated number of frames.
If the address
is omitted, the current frame is used as the base.  For
example,
.RB ` 31\-3 '
addresses frame 28 in the buffer.
.LP
If you do not specify an address for a
command to operate on, a command that requires an address
supplies one by default, usually the current frame.
.LP
A pair of addresses separated by a comma signifies an inclusive
range of frames, and the current frame is not changed unless the command
changes it.
.SS Commands
.LP
Only one command may appear per line.
commands may accept zero, one or two addresses, followed by possibly
a third address or an argument. Commands that accept up to two addresses
regard a third as an error. Likewise, commands that do not accept an
argument regard one as an error. Commands may be abbreviated.
.LP
In the absence of a second address for
a two or three address command the command will regard the second address 
as the same as the first. For example,
.B 2d
is equivalent to 
.B 2,2d .
The absence of a first address, where required, will result in the current
frame being used as the default. The same is true in the absence of a 
required third address. For example,
.B c 
is equivalent to
.B .,.c. .
.LP
The commands 
.B q
(quit) and
.B e
(edit) may be followed by a '!' to override 
.B med's 
user protection.  
.LP
In the following list of 
.B med
commands, the default addresses/arguments appear in parentheses; the 
parenthesized addresses are not part of the command. Unless otherwise
noted a command does not change the current frame number.
.HP
.PD 0
.RB ( \|1 \|, \|$ \|)append
.I metafile
.br
Append buffer to a file. Append the addressed frames in the buffer to 
.I metafile. 
If no address is specified the entire buffer is written. If the file
does not exist create it.
.TP
.RB "(\|.\|,\|.\|)copy(\|.\|)"
.br
Copy frames. Duplicate the addressed frames in the buffer and append 
them after the third address. The current frame becomes the destination
of the last frame copied.
.TP
.RB ( \|. \|, \|.\| ) delete
Delete the addressed lines from the buffer. 
.B delete
accepts one or two addresses;
the default is the current frame.  The current frame is set to
the first frame after the deleted frame(s).
.TP
.BI edit " metafile"
Edit a metafile.
The current contents of the buffer, if any, are erased. The named metafile 
is read
in to the buffer. The resulting current frame is the last frame in the 
buffer. If changes have been made to the buffer since the last write
.B med 
will refuse the request unless the command is appended with a '!'. 
.B edit
prints the number of frames in the metafile. If no 
.I metafile 
is given, the current metafile, if any is used.
The current frame becomes the last frame in the file.
.TP
.BI help " command"
help. Give a usage message for 
.I command .
If no command is given, print list of command names with a short 
description of each.
.HP
.PD 0
.RB ( \|. \|, \|. \|)label
.I string
.br
Label the addressed frames with 
.I string .
The 
.B CGM
.I Begin Picture
element contained in each metafile frame allows for the encoding of 
character data. 
.B label
provides a means for accessing this data.
.TP
.RB ( \|. \|, \|.\| ) merge
Merge the contents of the second addressed frame on top of the first 
addressed frame. The first
addressed frame is thus changed. The second frame remains the same. The
current frame is set to the first addressed frame.
.TP
.RB "(\|.\|,\|.\|)move(\|.\|)"
.br
Move the addressed frames to the first frame following the third address. 
.TP
.RB ( \|. \|, \|.\| ) print
Show the contents of the buffer at the given address. What is actually
displayed is information regarding the addressed frames. This information
includes: relative frame number within the buffer, the number of records
contained in the frame, the starting record for the frame and the 
contents of the 
.B CGM element 
.I BEGIN PICTURE .
If no address is specified the current frame does not change. Otherwise
it becomes the last frame printed.
.HP
.PD 0
.B quit
.br
Quit. Terminate the editing session without saving the buffer contents. In
order to save the buffer an explicit write must be performed. If changes to
the buffer have been made since the last write
.B med
will refuse to terminate unless
.B quit
is appended with a '!'.
.HP
.PD 0
.RB ( \|.\| )read
.I metafile
.br
Read in a metafile. Read the contents of 
.I metafile 
into the buffer and append it at the given address. 
.I metafile 
must be a valid NCAR CGM. The resulting current frame is the last frame
read in.
.HP
.PD 0
.RB "(\|1\|,\|$\|)split <\|number\|>"
.I outfile
.br
Split the current metafile into 
.I number
files. The 
.B split
command attempts to create
.I number
metafiles from the addressed frames, each containing approximately n / number
frames where 'n' is the total number of addressed frames. The first file is 
named
.IR outfile 001.cgm,
the second file is named
.IR outfile 002.cgm,
and so on lexicographically. If no
.I outfile
is given,
.B med
is used as the default (output files will be called med001.cgm, med002.cgm, 
etc.). 
.HP
.PD 0
.RB ( \|1 \|, \|$ \|)write 
.I metafile
.br
Write buffer. Write the addressed frames in the buffer to 
.I metafile. 
If no address is specified the entire buffer is written. If the file
does not exist create it.
.TP
.BI ! " command"
.br
Escape to the shell and execute 
.I command.
.I command 
is a valid UNIX command.
.SH FILES
.PD 0
.TP 20
.BI /tmp/cgm_tools.#
temporary;
.I # 
is the process id.
.sp
.SH "EXAMPLES"
.LP
To concatenate the files 
.BR cgm1 ,
.BR cgm2 ,
and 
.B cgm3 
into a single file 
.B cgm123
one might use the following:
.sp
.IP
.B "% med -e 'r cgm1' -e 'r cgm2' -e 'r cgm3' -e 'w! cgm123'"
.sp
.LP
Or one could pass the following script to 
.B med
as a 
.BI \-f "scriptfile"
option or enter the commands interactively.
.sp
.IP
.fb
r cgm1
.br
r cgm2
.br
r cgm3
.br
w cgm123
.fr
.sp
.LP
To Overlay the contents of frame
.B 5
on top of frame
.B 4 
from a file 
.B cgm1
one could execute:
.IP
.sp
.B "% med -e '4,5 me' -e 'w!' cgm1
.sp
.SH "SEE ALSO"
.BR cgm(local),
.IR ed(1),
.BR sed(1V)
.sp
.SH CAVEATS 
The 
.B append, read 
and 
.B merge 
commands may produce surprising results. A CGM may contain a set of global
graphical primitive attributes that are applied to every frame in a file. 
Thus reading frames in from a file with different global attributes than the
current working file may not produce the desired effect. The same is true
when appending frames to a previously existing file.
Similarly, the CGM standard specifies that graphical attributes specified
within a metafile frame affect all succeeding primitives. Thus a frame
which is the product of the 
.B merge
command may appear differently than expected.

