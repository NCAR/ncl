.\"
.\"     $Id: psplit.m,v 1.7 2005-04-09 00:13:54 fred Exp $
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

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.

