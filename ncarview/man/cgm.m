.\"
.\"	$Id: cgm.m,v 1.20 2008-12-23 00:04:15 haley Exp $
.\"
.\" NCAR View cgm.man 3.00 9/13/89 Clyne
.TH CGM_TOOLS 1NCARG "January 1993" NCARG "NCAR GRAPHICS"
.SH NAME
CGM_open,
CGM_close,
CGM_lseek,
CGM_read,
CGM_write,
CGM_directory,
CGM_freeDirectory,
CGM_printDirectory,
CGM_getInstr,
CGM_flushGetInstr,
CGM_putInstr,
CGM_flushOutputInstr,
CGM_initMetaEdit,
CGM_termMetaEdit,
CGM_copyFrames,
CGM_deleteFrames,
CGM_mergeFrames 
CGM_moveFrames,
CGM_readFrames,
CGM_valid,
CGM_writeFile,
CGM_writeFrames,
CGM_appendFrames \- Computer Graphics Metafile operations
.SH SYNTAX
.nf
.B #include <cgm_tools.h>
.PP
.B Cgm_fd CGM_open(metafile, size, flags, mode)
.B char *metafile;
.B unsigned size;
.B int flags;
.B int mode;
.PP
.B int CGM_close(cgm_fd)
.B Cgm_fd cgm_fd;
.PP  
.B int CGM_lseek(cgm_fd, offset)
.B Cgm_fd cgm_fd;
.B int offset;
.PP
.B int CGM_read(cgm_fd, buf)
.B Cgm_fd cgm_fd;
.B unsigned char *buf;
.PP
.B int CGM_write(cgm_fd, buf)
.B Cgm_fd cgm_fd;
.B unsigned char *buf;
.PP
.B Directory *CGM_directory(cgm_fd)
.B Cgm_fd cgm_fd;
.PP
.B void CGM_freeDirectory(dir)
.B Directory       *dir;
.PP
.B void CGM_printDirectory(dir)
.B Directory *dir;
.PP
.B int CGM_getInstr(cgm_fd, instr)
.B Cgm_fd cgm_fd;
.B Instr *instr;
.PP
.B void CGM_flushGetInstr(cgm_fd)
.B Cgm_fd cgm_fd;
.PP
.B int CGM_putInstr(cgm_fd, instr)
.B Cgm_fd cgm_fd;
.B Instr *instr;
.PP
.B int CGM_flushOutputInstr(cgm_fd)
.B Cgm_fd cgm_fd;
.PP
.B Directory *CGM_initMetaEdit (metafile, size)
.B char *metafile;
.B unsigned int size;
.PP 
.B int CGM_termMetaEdit()
.PP 
.B Directory *CGM_copyFrames(start, num, target )
.B unsigned int start;
.B int num;
.B unsigned int target;
.PP 
.B Directory *CGM_deleteFrames(start, num)
.B unsigned int start,
.B num;
.PP 
.B Directory *CGM_mergeFrames(bottom, top)
.B unsigned bottom, top;
.PP 
.B Directory *CGM_moveFrames (start, num, target)
.B unsigned int start, num, target;
.PP
.B Directory *CGM_readFrames(metafile, start, num, target, size)
.B char *metafile;
.B unsigned int start;
.B int num;
.B unsigned int target, size;
.PP
.B int *CGM_validCGM(metafile)
.B char *metafile;
.PP 
.B int CGM_writeFile(metafile)
.B char *metafile;
.PP 
.B int CGM_writeFrames(metafile, start, num)
.B char *metafile;
.B unsigned start, num;
.PP 
.B int CGM_appendFrames(metafile, start, num)
.B char *metafile;
.B unsigned start, num;
.fi
.SH DESCRIPTION
The argument \fIcgm_fd\fR refers to a valid file descriptor created for
reading or writing, as appropriate by \fBCGM_open\fR. \fBCGM_read\fR,
\fBCGM_directory\fR,  \fBCGM_getInstr\fR and \fBCGM_flushGetInstr\fR 
require a file descriptor open for reading. \fBCGM_write\fR, \fBCGM_getInstr\fR,\fBCGM_flushGetInstr\fR and \fBCGM_flushOutputInstr\fR require a \fICgm_fd\fR
open for writing. \fBCGM_close\fR and \fBCGM_lseek\fR will accept any valid
\fICgm_fd\fR.
.PP
The \fIsize\fR argument refers to the CGM record size in bytes. 
For an NCAR CGM this value is 1440.
.PP
\fIbuf\fR is a pointer to user allocated memory of size \fIsize\fR. This 
storage will be used for buffering input and output of \fBCGM_read\fR
and \fBCGM_write\fR respectively.
.PP
The \fIdir\fR argument is a pointer to a \fIDirectory\fR structure 
created with
\fBCGM_directory\fR or \fBCGM_initMetaEdit\fR.
\fIdir\fR is a private resource 
that should \fINOT\fR be 
directly modified by the user. A set of convenience macros is provided 
for this purpose in \fIcgm_tools.h\fR.
.PP
The \fIstart\fR, \fInum\fR and \fItarget\fR arguments are used to 
address frame numbers in a metafile being edited with one of the commands:
\fBCGM_copyFrames\fR, \fBCGM_deleteFrames\fR, \fBCGM_readFrames\fR, 
\fBCGM_moveFrames\fR, \fBCGM_writeFrames\fR and \fBCGM_mergeFrames\fR.
The \fIstart\fR argument is the first frame in a sequence of \fInum\fR
frame(s) to perform the editing operation on. \fItarget\fR is similar
to \fIstart\fR and is used by commands that require two frame addresses
such as \fIcopy\fR. Addressing begins at zero.
.PP
.IP CGM_open
This command is modeled after the unix \fIopen\fR command. It will open
a CGM for reading or writing as specified by the \fIflags\fR argument
and return a \fICgm_fd\fR file descriptor. The \fIflags\fR and \fIopen\fR 
parameters
are passed directly on to the system \fIopen\fR command.
For a detailed explanation of these two arguments see open(2).
.IP CGM_close
Delete a file descriptor. The inverse of \fBCGM_open\fR. See close(2).
.IP CGM_read
\fBCGM_read\fR attempts to read \fIsize\fR bytes from the object 
referenced through
the descriptor \fIcgm_fd\fR. \fIsize\fR is set at the creation of \fIcgm_fd\fR
by \fBCGM_open\fR. \fBCGM_read\fR returns the number of bytes successfully
read. A zero is returned on EOF and a negative number implies an error occurred.
The unix system call \fIread\fR is called by \fBCGM_read\fR. See read(2).
.IP CGM_write
Attempts to write a single record of \fIsize\fR bytes from \fIbuf\fR from
the object referenced by \fIcgm_edit\fR where \fIsize\fR is the 
record size parameter provided at the creation of \fIcgm_fd\fR. \fIwrite\fR
returns the number of bytes successfully written. A negative 
return number implies an error occurred.
The unix system call \fIwrite\fR is called by \fBCGM_write\fR. See write(2).
.IP CGM_lseek
Advance the file pointer of \fIcgm_fd\fR to \fIoffset\fR bytes. Upon 
successful completion the current file pointer offset is returned. A 
negative return value is an error.
The unix system call \fIlseek\fR is called by \fBCGM_lseek\fR. See lseek(2).
.IP CGM_directory
Create a table of contents for the metafile referenced by \fIcgm_fd\fR. 
Return a pointer to this table of type \fIDirectory\fR. The contents of
the directory include number of metafiles, number of frames, record 
offset for each frame, frame length in records, optional frame
description and metafile status. These fields are meant to be read only
and should only be referenced by the convenience macros provided in 
cgm_tools.h. A NULL pointer is returned on failure.
.IP CGM_freeDirectory
Free memory allocated to a directory created by \fBCGM_directory\fR or
\fBCGM_initMetaEdit\fR. 
.IP CGM_printDirectory
Print the contents of a directory pointed to by \fIdir\fR to the standard
output.
.IP CGM_getInstr,
Fetch the next instruction in file referenced by \fIcgm_edit\fR  and 
convert it into a usable format pointed to by \fIinstr\fR.
\fBCGM_getInstr\fR provides an interface to the metafile for 
extracting CGM
elements. The user need not be concerned with the binary format of the 
metafile. The fields of the \fIInstr\fR are as described in \fIcgm_tools.h\fR.
The user should note that the maximum allowable data length returned in 
a single invocation is 32760 bytes. The CGM standard allows upto 32767 
bytes to be stored in a single instruction. But 32767 is not a nice number
to work with. Should the data length of a CGM instruction exceed 32760 bytes,
indicated by the boolean \fImore\fR flag,
the next invocation of \fBCGM_getInstr\fR will return the remaining data
upto the same limit. And so on... \fBCGMgetInstr\R requires a valid
\fICgm_fd\fR open for reading. For a description on CGM see the 
ANSI standard.
.IP CGM_flushGetInstr
Flush the input buffer used by \fBCGM_getInstr\fR. \fBCGM_getInstr\fR buffers
the contents of the CGM and only performs actual reads as necessary. If the
user desires other then sequential read access to a CGM it becomes necessary
to flush the input buffer before reading from a new location.
.IP CGM_putInstr
The analog to \fBCGM_getInstr\fR. This function buffers CGM instructions
to be written to a CGM referenced by \fIcgm_fd\fR. Again the user need 
not be concerned with the binary
format of the file. Writes are performed sequentially in record size 
\fIsize\fR as specified during the creation of \fIcgm_fd\fR. The same data 
length constraints that are placed on 
\fBCGM_getInstr\fR hold for \fBCGM_putInstr\fR. If the user wants to output
instructions with a data length greater than 32760 bytes then the data must
be broken up into blocks no greater than this size. The user must also set
the boolean \fImore\fR flag in the \fIInstr\fR. \fIcgm_fd\fR must be a 
valid file descriptor open for writing. For a description of the fields
of the \fIInstr\fR see the file \fIcgm_tools.h\fR. 
.IP CGM_flushOutputInstr
Flush the output buffer used by \fBCGM_putInstr\fR for the file referenced
by \fIcgm_fd\fR. It is necessary to explicitly flush the output buffer
used by \fBCGM_putInstr\fR before the file is closed or any random access
is performed. Otherwise not all CGM elements will actually get written.
.IP CGM_initMetaEdit
Initialize a metafile for editing. This is the initialization routine for
the higher level editing routines contained in this package: \fBCGM_copyFrames,
\fBCGM_deleteFrames\fR, \fBCGM_readFrames\fR, \fBCGM_moveFrames\fR, 
\fBCGM_writeFile\fR, \fBCGM_writeFrames\fR, and \fBCGM_mergeFrames\fR.
These routines only work on one metafile at a time (the one named in 
\fBCGM_initMetaEdit\fR. Invoking this routine for a second time without
explicitly saving any changes will have the effect of loading
a new file and discarding all changes made in the previous file.
\fBCGM_initMetaEdit\fR and all proceeding editing functions that \fBmake\fR
changes to the file return a pointer to a \fIDirectory\fR
as a convenience that allows the user to examine the state of the file.
The contents of the directory are private and should NOT be changed
by the user. A set of macros is provided in \fIcgm_tools.h\fR to be
used for retrieving the directory's contents. \fBNote\fR: no changes are 
actually made to the edit file unless it is explicitly overwritten with 
either \fBCGM_writeFile\fR or \fBCGM_writeFrames\fR.
.IP CGM_termMetaEdit
Terminate the editing session started with \fBCGM_initMetaEdit\fR. 
This routine should be called after any editing changes have been
saved, if desired to save them, and before exiting the editing session.
\fBCGM_termMetaEdit\fR frees valuable resources.
.IP CGM_copyFrames
Copy \fInum\fR frames beginning with \fIstart\fR to the frame addressed by
\fItarget\fR. If \fItarget\fR is already occupied then the source frames are 
inserted in its place while the target frame, and all proceeding frames,
are advanced. \fBCGM_copy\fR operates on the file initialized by 
\fBCGM_initMetaEdit\fR (the edit file). On successful completion a 
pointer to the 
current directory is returned. On error a NULL pointer is returned.
.IP CGM_deleteFrames
Delete \fInum\fR frames from the edit file
starting with frame \fIstart\fR. On successful completion a pointer to the 
current directory is returned. On error a NULL pointer is returned.
.IP CGM_mergeFrames 
Overwrite the contents of frame addressed \fIbottom\fR with the union
of the frame at location \fIbottom\fR and the frame at location \fItop\fR.
The effect of this command is equivalent to drawing the \fItop\fR frame
on top of the \fIbottom\fR frame. It is not a union in the true sense of 
the word.
On successful completion a pointer to the 
current directory is returned. On error a NULL pointer is returned.
.IP CGM_moveFrames
Move a block of \fInum\fR frames from the edit file starting with
with frame \fIstart\fR to the position occupied by frame \fItarget\fR 
On successful completion a pointer to the 
current directory is returned. On error a NULL pointer is returned.
.IP CGM_readFrames
Read \fInum\fR frames from metafile \fIfile\fR starting with frame \fIstart\fR.
Insert the frames at address \fItarget\fR in the edit file. 
On successful completion a pointer to the 
current directory is returned. On error a NULL pointer is returned.
.IP CGM_validCGM
Determine whether a file is a valid NCAR CGM or not. This function performs
a few simple diagnostics in an effort to determine whether a given file 
is in the NCAR CGM format. The tests performed are not rigorous and it is
conceivable that the information retrieved is incorrect.  A return of 1 
indicates a valid NCAR CGM. 
A return of 0 indicates the file is not a NCAR CGM. A return
of -1 indicates an error occurred and the global variable `errno' is 
set accordingly.
.IP CGM_writeFile
Write the entire contents of the current edit file to \fIfile\fR.
\fBCGM_writeFile\fR returns the integer one on success and a negative
number on failure.
.IP CGM_writeFrames
Write a block of \fInum\fR frames starting with frame \fIstart\fR to
\fIfile\fR. The source frames come from the edit file.
Note: CGM frames are contained in a wrapper 
made up of CGM \fIdelimiter\fR elements. The file created by 
\fBCGM_writeFrames\fR will use the \fIwrapper\fR provided by the current
edit file. Thus if a file \fIfoo\fR contains \fIn\fR frames that are
\fIread\fR into an editing session with a file \fIgoo\fR and then
these same frames are written out to a file \fIzoid\fR, \fIzoid\fR
may or may not be the same as the original \fIfoo\fR.
\fBCGM_writeFrames\fR returns the integer one on success and a negative
number on failure.
.IP CGM_appendFrames
Append a block of \fInum\fR frames starting with frame \fIstart\fR to
\fIfile\fR. \fIfile\fR must already exist and be a valid NCAR CGM. 
\fBCGM_appendFrames\fR returns the integer one on success and a negative
number on failure.
.SH "SEE ALSO"
ANSI X3.122 \fIComputer Graphics Metafile for the Storage and Transfer of 
Picture Description Information.
.SH BUGS
CGMs with more the one metafile stored in the are not guaranteed to work.
.PP
Should not have to explicitly flush the output buffer for \fBCGM_getInstr\fR.
This should be handled automatically when the file is closed.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
