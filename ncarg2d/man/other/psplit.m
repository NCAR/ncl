.\"
.\"     $Id: psplit.m,v 1.8 2008-07-27 03:34:11 haley Exp $
.\"
.TH psplit 1NCARG "August 2001" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
psplit - a tool for splitting PostScript files with multiple frames into 
individual PostScript files, or simply determining the number of pages
in a file.
.SH SYNOPSIS
usage: psplit [-c] input_file [output_file_root]
.SH DESCRIPTION 
"psplit" takes a PostScript file as input (that may have
multiple pictures) and splits it into individual PostScript
files having a single frame each.  If the original file has a
%%BoundingBox comment in it, then that comment will be inserted
into each output file and the output files will be EPS (encapsulated
PostScript) conforming.  If the original file does not have a
%%BoundingBox comment, then the output files will not have one
either.  In that case, if you want the output files to be EPS files,
use the "ps2epsi" utility that is based on GhostScript.
If the -c option is present, then only the number of pages in
input_file is reported.
.sp
"psplit" is primarily intended to be used with PostScript files
that have been produced directly by NCAR Graphics, or produced from
NCAR Graphics by running "ctrans" with a PostScript graphcap.  However, 
"psplit" will also work on most PostScript files adhering to PostScript 
Document Structuring Conventions (DSC).
.sp
By default the output files are named in sequence as "pict0001.ps",
"pict0002.ps", and so forth.  If you want to specify a different
root name for the output files (i.e. a name different from "pict"),
you can specify that by entering the optional second argument.
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
