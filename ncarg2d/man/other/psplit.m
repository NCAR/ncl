.\"
.\"     $Id: psplit.m,v 1.2 1999-03-12 19:08:01 fred Exp $
.\"
.TH psplit 1NCARG "January 1999" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
psplit - a tool for splitting PostScript files into Encapsulated PostScript files
.SH SYNOPSIS
usage: psplit input_file [output_file_root]
.SH DESCRIPTION 
"psplit" takes a PostScript file as input (that may have
multiple pictures) and splits it into Encapsulated PostScript (EPS)
files.  By definition EPS files have a single picture in them, so
"psplit" produces a single EPS file for each picture in the original
file.
.sp
"psplit" is primarily intended to be used with PostScript files
that have been produced directly by NCAR Graphics, or produced from
NCAR Graphics by running "ctrans" with a PostScript graphcap.  However, 
"psplit" will also work on most PostScript files adhering to PostScript 
Document Structuring Conventions (DSC).
.sp
By default the output files are named in sequence as "pict0001.eps",
"pict0001.eps", and so forth.  If you want to specify a different
root name for the output files (i.e. a name different from "pict"),
you can specify that by entering the optional second argument.
