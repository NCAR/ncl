.\"
.\"	$Id: gcsgwk.m,v 1.1 1993-03-11 16:21:58 haley Exp $
.\"
.TH GCSGWK 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GCSGWK (Copy segment to workstation) - copies a GKS segment to a GKS 
workstation.
.SH SYNOPSIS
CALL GCSGWK(WKID,SGNA)
.SH DESCRIPTION
.IP WKID 12
(Integer, Input) - Specifies the workstation identifier (as defined in
a GOPWK call) that is to receive the segment copy.
.IP SGNA 12
(Integer, Input) - Identifies the segment 
that is to be copied to the specified workstation.
SGNA must have been used in a previous call to GCRSG.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gevtm, gactm, gclsg, gcrsg, gdsg, gqopsg, gqsgus, gssgt.
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
