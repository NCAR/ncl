.\"
.\"	$Id: ncarg_gks_cbind.m,v 1.14 2008-07-27 03:34:10 haley Exp $
.\"
.TH NCARG_GKS_CBIND 3NCARG "February 1993" NCAR "NCAR GRAPHICS"
.SH NAME
NCAR Graphics GKS C-binding - Description of how to use the NCAR Graphics
GKS C-binding.
.SH SYNOPSIS
This section only briefly describes how to use the NCAR Graphics GKS
C-binding.  If you intend to use the GKS C-bindings heavily, you may
want to get a copy of the American National Standard's "Computer
Graphics - Graphical Kernel System (GKS) - Functional Description".
If you want information on how to use the C-bindings for the NCAR
Graphics utilities, please see \fBncarg_cbind(3NCARG)\fP or the man
page for any of the NCAR Graphics routines.
.sp
The NCAR Graphics GKS C-binding adheres to the ISO/IEC standard.  Only
the functions at level 0A have been implemented.  At the time,
not all of these functions are supported; only the ones that have
man pages associated with them are supported.  Below is a list
of the currently supported NCAR Graphics GKS C-bindings:
.sp
.nf
gaccum_tran_matrix
gactivate_ws
gcell_array
gclear_ws
gclose_gks
gclose_seg
gclose_ws
gcopy_seg_ws
gcreate_seg
gdeactivate_ws
gdel_seg
gescape
geval_tran_matrix
gfill_area
ginq_asfs
ginq_char_expan
ginq_char_ht
ginq_char_space
ginq_char_up_vec
ginq_clip
ginq_colr_rep
ginq_cur_norm_tran_num
ginq_fill_colr_ind
ginq_fill_int_style
ginq_fill_style_ind
ginq_line_colr_ind
ginq_linetype
ginq_linewidth
ginq_marker_colr_ind
ginq_marker_size
ginq_marker_type
ginq_max_norm_tran_num
ginq_name_open_seg
ginq_norm_tran
ginq_op_st
ginq_set_seg_names
ginq_text_align
ginq_text_colr_ind
ginq_text_font_prec
ginq_text_path
gopen_gks
gopen_ws
gpolyline
gpolymarker
gsel_norm_tran
gset_asfs
gset_char_expan
gset_char_ht
gset_char_space
gset_char_up_vec
gset_clip_ind
gset_colr_rep
gset_fill_colr_ind
gset_fill_int_style
gset_fill_style_ind
gset_line_colr_ind
gset_linetype
gset_linewidth
gset_marker_colr_ind
gset_marker_size
gset_marker_type
gset_seg_tran
gset_text_align
gset_text_colr_ind
gset_text_font_prec
gset_text_path
gset_vp
gset_win
gtext
gupd_ws
.sp
.fi
.sp
Please see the man page for any of these for more information on that
particular function.  In the future, as higher level GKS Fortran functions 
are implemented, there may be C-bindings created for them as well.
.sp
Notice that since the NCAR Graphics GKS C-binding adheres to the
standard, the names for the C-bindings are not like the Fortran names.
Instead, the functions names are more descriptive, like
\fBgset_fill_colr_ind\fP which corresponds to the Fortran GKS routine
\fBGSFACI\fP.  If you know the name of the Fortran GKS routine and you
need the C-binding name, you can look at the man page for the Fortran
routine to get the name of the C-binding routine.
.SH ARGUMENT LISTS
The argument list of each GKS C-binding closely corresponds with the
argument list of the Fortran routine, but in many cases, the arguments
are represented in the form of a C structure.  These structures are defined
in the include file <ncarg/gks.h>.
.SH FUNCTION PROTOTYPES
The GKS C-bindings are intended to be ANSI C compliant.  To get the
correct function prototypes, include <ncarg/gks.h> in your C program.
.SH COMPILING YOUR PROGRAM
To compile your NCAR Graphics C program with the GKS C-bindings, use the
application \fBncargcc\fP.  \fBncargcc\fP will take care of loading in
the necessary C/Fortran interface libraries as well as the NCAR
Graphics C and Fortran libraries.  You will either need to set the 
NCARG_ROOT or the NCARG_BIN, NCARG_LIB, and NCARG_INCLUDE environment 
variables in order to run \fBncargcc\fP.  See "man ncargintro" for more 
information.
.sp
If you do not wish to use \fBncargcc\fP, then you can just run it with 
no arguments to see what the necessary libraries are, and then put this 
in your Makefile or whatever else you are using to compile your program.
.SH EXAMPLES
An example of a C program that calls some of the NCAR Graphics GKS
C-bindings has been provided.  To copy this C program into your 
directory, and then compile, link, and run it, type \fBncargex c_gtxpac\fP.
.SH SEE ALSO
Online:
.BR ncarg_cbind(3NCARG),
.BR ncargex(1NCARG),
.BR ncargintro(5NCARG).
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2002
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
