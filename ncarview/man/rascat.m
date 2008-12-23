.\"
.\"	$Id: rascat.m,v 1.23 2008-12-23 00:04:16 haley Exp $
.\"
.TH RASCAT 1NCARG "January 1993" NCARG "NCAR GRAPHICS"
.SH NAME
Rascat \- concatenate, convert raster files
.SH SYNOPSIS
.B rascat
[
.B \-help 
] [
.BI \-ifmt " format"
] [
.BI \-ira " algorithm" 
] [
.BI \-ofmt " format"
] [
.BI \-output " file"
] [
.BI \-resolution " resolution"
] [
.BI \-rgbscale " scale-factor"
] [
.BI \-scale " scale-factor"
] [
.B \-verbose
] [
.B \-Version
] [
.BI \-window " nx ny x y"
] [
.BI \-pal " palette_file"
] [
.BI - | file... 
]
.SH DESCRIPTION
.LP
.B rascat
reads each 
.I file
in sequence and copies its contents to the standard output performing
any format conversion and 
data massaging necessary as specified by the command line options.
By default, 
.B rascat
determines the format of a file by looking at the file name extension.
If there are multiple files input into rascat with varying
file formats 
.B rascat 
will perform format conversion such that 
the resulting concatenated file will be in the format
of the first file processed. 
.LP
Input files must all have the same spatial resolution and have the 
same depth; 8-bit and 24-bit files may not be intermixed. Furthermore,
if an input file contains multiple images each image in that file must
have the same spatial resolution and be of the same depth.
.LP
Currently, only 8-bit-indexed and 24-bit-direct color encodings are supported.
.LP
See 
.B ras_formats(5NCARG)
for a list of supported image formats.
.SH OPTIONS
.TP
.B \-help
Print a usage message and exit.
.TP
.BI \-ifmt " format"
Specify the input file format. 
.I format
is one of the file name extensions discussed 
in
.B ras_formats(5NCARG) 
(without the ".", e.g. 
.BR xwd). 
When this option is 
specified file name extensions are not necessary and are ignored if present.
All input files must have the same format.
.TP
.BI \-ira " algorithm" 
Specify the image resampling algorithm to be used when 
either the
.B -scale
or the
.B \-resolution
command line option is used. 
.I algorithm
may be either
.BR NN ,
indicating a "nearest neighbor"  algorithm, 
or
.BR BL ,
indicating a "bilinear interplation" algorithm. The default is to do
"nearest neighbor" interpolation.
.TP
.BI \-ofmt " format"
Specify the output file format. 
.I format
is one of the aforementioned file name extensions. If used in conjunction 
with the 
.B -output 
option the output file name requires no name extension and is 
ignored if present.
.TP
.BI \-output " file"
Specify an ouput file name and possibly an implicit output format. By 
default rascat writes to the standard
output. When this option is used output is written to 
.IR file .
If the 
.B \-ofmt
option is not specified 
.I file
must have a file name extension recognized by
.B rascat.
In which case the file name extension will determine the output format.
.TP
.BI \-resolution " resolution"
Resample the spatial resolution of input imagery to 
.IR resolution ,
where
.I resolution
is the number of pixels in the x direction, followed by an 
.BR x , 
followed
by the number of pixels in the y direction, with no intervening spaces. For
example, 512x512, specifies a 512 by 512 pixel resolution.
.IP
Warning: Aspect ratios are not preserved by this option. If the resolution
of your input imagery has a different aspect ratio then that specified
by 
.I resolution
the resultant image will be distorted.
.TP
.BI \-rgbscale " scale-factor"
Specify a floating point scaling factor,
.IR scale-factor ,
to be applied to all the color intensities contained in the input files.
This option may be fairly computationally expensive with 24-bit-direct
encoded imagery.
.TP
.BI \-scale " scale-factor"
Specify a uniform, floating-point scaling factor to be applied to the
spatial resolution of the input files. Unlike the 
.B  \-resolution 
option this option guarantees to preserve the aspect ratio of your
imagery. For example, setting 
.I scale-factor
to 
.B 0.5
causes your imagery to be resampled to one fourth of its original spatial
resolution. If its original resolution was 1024x1024 the resultant resolution
would be 512x512.
.TP
.B \-verbose
Tells rascat to operate in verbose mode.
.TP
.B \-Version
Print rascat's version number and then exit.
.TP
.BI \-window " nx ny x y"
Specify a subregion of the input imagery to be extracted. The area outside
the rectangular subregion defined by 
.IR nx , 
.IR ny , 
.IR  x 
and 
.I y
is discarded. 
.I x
and 
.I y
specify the position of the upper-left corner of the rectangle.
.I nx
and
.I ny
specify the rectangle's width and height, respectively.
.TP
.BI \-pal " palette_file"
Set the palette for the output rasterfile from \fIpalettefile\fP, which
can be either an HDF-type palette with an extension of ".pal", or
a textual palette with an extension of ".txt". See ras_palette(5NCARG)
for documentation on the formats. This option applies to indexed-color
imagery only.
.SH ENVIRONMENT
.TP
.B NCARG_TMP
If set, this environment variable contains a directory path to be used for
temporary files. On most systems the default is 
.BR /tmp .
On some systems the default is 
.BR /usr/tmp .
.SH EXAMPLES
In the following example a NRIF file, a Sun raster image file, and a
XWD file are concatenated and converted into a single NRIF file:
.sp
.IP
% rascat file1.nrif file2.sun file3.xwd > file123.nrif
.LP
In this example the same files are converted and concatenated into a sun file:
.sp
.IP
% rascat -ofmt sun file1.nrif file2.sun file3.xwd > file123.sun
.sp
.LP 
The
.B -ofmt
option is necessary in this example because the first file 
.B rascat
encounters is an NRIF file.
.LP
Finally, in this example the upper left 512 by 512 rectangle of the
file 
.B foo.sun
is extracted and written to the file
.BR foo.512x512.sun :
.sp
.IP
% rascat -window 512 512 0 0 foo.sun > foo.512x512.sun
.SH "SEE ALSO"
.BR rasgetpal(1NCARG),
.BR rasls(1NCARG),
.BR rassplit(1NCARG),
.BR rasview(1NCARG),
.BR ras_formats(5NCARG),
.BR ras_palette(5NCARG)
.br
.ne 5
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH CAVEATS
Aspect ratios are not preserved by the 
.B \-resolution
option.
.LP
Not all formats support both 8-bit and 24-bit encodings.
.LP
Indexed and direct color encodings cannot be mixed.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
