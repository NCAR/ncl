.TH NCARG_GKS 1NCAR "January 1992" NCAR "NCAR GRAPHICS"
.SH NAME
NCAR Graphics GKS - Environment variables
.SH SYNOPSIS
.sp
A collection of environment variables allows you to control certain
aspects of NCAR Graphics' GKS behaviour.
.sp
\fBNCARG_GKS_OUTPUT\fP allows you to specify what file or process
CGM output is directed to.
.sp
\fBNCARG_GKS_PSOUTPUT\fP allows you to specify what file any PostScript
output from NCAR GKS is directed to (stdout is allowed).
.sp
\fBNCARG_GKS_PDFOUTPUT\fP allows you to specify what file any PDF
output from NCAR GKS is directed to (stdout is allowed).
.sp
\fBNCARG_GKS_GENCGM\fP allows you to request that the NCAR GKS
package generate binary encoded CGM rather than NCGM (the NCAR 
private encoding of binary encoded CGM).
Note that other NCAR Graphics utilities, such as the metafile 
translators, cannot read in generic CGM.
.sp
\fBNCARG_GKS_BUFSIZE\fP provides you with control over how CGM
output is buffered.
.sp
\fBNCARG_GKS_DEBUG\fP, if set, will result in debugging information
about the GKS I/O subsystem being output to standard out.
.sp
.SH \fBNCARG_GKS_OUTPUT\fP
.sp
By default, CGM output is directed to a file called "gmeta",
but the environment variable \fBNCARG_GKS_OUTPUT\fP can be used
to redirect output to a file with a different name or a translator
process.
.sp
\fIExamples\fP:
.sp
.in +1.0i
setenv NCARG_GKS_OUTPUT myfile
.in -1.0i
.sp
causes CGM output to be placed in "myfile".
.sp
.in +1.0i
setenv NCARG_GKS_OUTPUT "|ctrans"
.in -1.0i
.sp
causes the "ctrans" translator to be forked and CGM output piped to it.
.sp
.in +1.0i
setenv NCARG_GKS_OUTPUT "|"
.in -1.0i
.sp
causes a default translator to be forked and CGM output to be
piped to it. Note that not just any process can be used here because
it is assumed that the process is invoked as "translatorname -"
where the "-" indicates that the translator is to read from standard
input.
.sp
.SH \fBNCARG_GKS_PSOUTPUT\fP
.sp
By default, PostScript output is written to "gmetaNN.sfx"  where "NN" is
the NCAR GKS workstation ID used in the call to GOPWK to open the
workstation and "sfx" is "ps", "eps", or "epsi" as appropriate.  Setting
NCARG_GKS_PSOUTPUT will override all defaults and
write any PostScript output to the designated file.
.sp
\fIExamples\fP:
.sp
.in +1.0i
setenv NCARG_GKS_PSOUTPUT myfile
.in -1.0i
.sp
causes all PostScript output to be written to "myfile".
.sp
.in +1.0i
setenv NCARG_GKS_PSOUTPUT stdout
.in -1.0i
.sp
causes all PostScript output to be written to standard out.
.sp
.SH \fBNCARG_GKS_PDFOUTPUT\fP
.sp
By default, PDF output is written to "gmetaNN.pdf"  where "NN" is
the NCAR GKS workstation ID used in the call to GOPWK to open the
workstation.  Setting NCARG_GKS_PDFOUTPUT will override all defaults
and write any PDF output to the designated file.
.sp
\fIExamples\fP:
.sp
.in +1.0i
setenv NCARG_GKS_PDFOUTPUT myfile
.in -1.0i
.sp
causes all PDF output to be written to "myfile".
.sp
.in +1.0i
setenv NCARG_GKS_PDFOUTPUT stdout
.in -1.0i
.sp
causes all PDF output to be written to standard out.
.sp
.in +1.0i
.SH \fBNCARG_GKS_GENCGM\fP
.sp
If the \fBNCARG_GKS_GENCGM\fP environment variable is set, GKS
will strip off the NCAR wrapper and generate a binary encode CGM.
As mentioned above, other NCAR Graphics utilities
such as translators don't yet support generic CGM as
input. Thus, this option is useful primarily for getting CGM
output into a form appropriate for use with other software
packages that expect binary encoded CGM.
.sp
\fIExample\fP:
.sp
.in +1.0i
setenv NCARG_GKS_GENCGM
.in -1.0i
.sp
.SH \fBNCARG_GKS_BUFSIZE\fP
.sp
If output is to a standard file, buffered I/O is used and the
NCARG_GKS_BUFSIZE environment variable provides you with
some flexibility in specifying buffer size.
.sp
On Cray systems, the default buffer size is 1MB. All others
use whatever the system default is, as specified in the file
"stdio.h".
.sp
If the environment variable NCARG_GKS_BUFSIZE is set, it overrides
the defaults. If NCARG_GKS_BUFSIZE is set to \fIN,\fP behavior is
as follows:
.sp
If \fIN\fP is 0:
.sp
.in +1.0i
CGM output is \fIunbuffered\fP.
.in -1.0i
.sp
If \fIN\fP is not 0:
.sp
.in +1.0i
Buffer Size = \fIN\fP * 1024 bytes.
.in -1.0i
.sp
\fIExamples\fP
.sp
Let's suppose you're running a long multi-frame graphics job
on a Cray supercomputer and you'd like to use a very large I/O buffer.
The default for a Cray is 1MB but you'd like to make it 2MB.
.sp
.in +1.0i
setenv NCARG_GKS_BUFSIZE 2048
.in -1.0i
.sp
Now let's assume you're on the same Cray computer, but you want
to pipe you're output to a translator and view the frames
as they come up. A big buffer is a bad idea because your
CGM might not be flushed out every frame. You can make the
output unbuffered as follows:
.sp
.nf
.in +1.0i
setenv NCARG_GKS_OUTPUT "|ctrans"
setenv NCARG_GKS_BUFSIZE 0
.in -1.0i
.fi
.sp
As a final example, let's say you're running the big multi-frame
NCAR Graphics job on a Sun, which happens to have a default
buffer size of 1024 bytes (look in /usr/include/stdio.h). Specify
a 1MB buffer using:
.sp
.in +1.0i
setenv NCARG_GKS_BUFSIZE 1024
.in -1.0i
.sp
.SH CAVEATS
.sp
It's worth saying one more time: if you output binary encoded CGM rather
than NCGM (NCAR CGM), NCAR Graphics generally won't deal with it.
The one exception is that you can convert the file back to
NCGM using \fIcgm2ncgm\fP.
.SH "SEE ALSO"
.BR cgm2ncgm(1NCARG),
.BR ncargintro(5NCARG),
.BR ncarg_env(5NCARG)
.SH COPYRIGHT
Copyright (C) 1987-2002
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
