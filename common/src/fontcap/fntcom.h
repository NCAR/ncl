C
C	$Id: fntcom.h,v 1.3 2000-08-22 03:40:24 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C

      PARAMETER (IBDIM=15000)
      COMMON /FCAP/BUFFER(IBDIM),BSIZE
C
      COMMON /ERNO/EEOF,ERED,EINM,EORD,EXWD,EYWD,ESWD
      INTEGER      EEOF,ERED,EINM,EORD,EXWD,EYWD,ESWD
C
      COMMON /KVALC/FNAME
      CHARACTER*40 FNAME
C
      PARAMETER (NUMKYS=16)
      COMMON /KEYS/KEYLST
      CHARACTER*20 KEYLST(NUMKYS)
C
      PARAMETER (NUMFTS=12)
      COMMON /FONTNS/FNTNMS
      CHARACTER*40 FNTNMS(NUMFTS)
C
      COMMON /CLINE/LINE
      CHARACTER*80 LINE
C
      COMMON /FNTIO/UNIT  , BLNKL , NBPERI, NBYPWD, PKWID , BITPNT,
     +              VERBOS, LSTPNT, NWRDS , LSIZE
      INTEGER       UNIT  , NBPERI, NBYPWD, PKWID , BITPNT, VERBOS,
     +              LSTPNT, NWRDS
      CHARACTER*80 BLNKL
C
      PARAMETER (NUMNTR=28)
      COMMON /INTRNL/TYPFLG, CHRSTR, CHREND, FRIGHT, FTOP  , FCAPOV, 
     +               FCAP  , FXHOV , FXH   , FHALF , FBASE , FBOT  , 
     +               FCHSWD, FCVSWD, FLLX  , FLLY  , FURX  , FURY  , 
     +               FLLEX , FLLEY , FUREX , FUREY ,
     +               TABPNT, XBITWD, YBITWD, XBIAS , YBIAS , PKFLWD
      INTEGER        TYPFLG, CHRSTR, CHREND, FRIGHT, FTOP  , FCAPOV, 
     +               FCAP  , FXHOV , FXH   , FHALF , FBASE , FBOT  , 
     +               FCHSWD, FCVSWD, FLLX  , FLLY  , FURX  , FURY  , 
     +               FLLEX , FLLEY , FUREX , FUREY ,
     +               TABPNT, XBITWD, YBITWD, XBIAS , YBIAS , PKFLWD 
      INTEGER INTARR(NUMNTR)
      EQUIVALENCE (INTARR,TYPFLG)
C
      COMMON /SFLAGS/SVNUM ,
     +               COORD , ENDR  , BEGINH, ENDH  , BEZIER, ENDC  ,
     +               BEGINC, ENDL
      INTEGER        SVNUM ,
     +               COORD , ENDR  , BEGINH, ENDH  , BEZIER, ENDC  ,
     +               BEGINC, ENDL
