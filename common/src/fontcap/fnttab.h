C
C	$Id: fnttab.h,v 1.4 2008-07-27 12:23:42 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      COMMON /FNTTB1/ PART1, KEYSEP, KEYTER, FRMCOM
      COMMON /FNTTB2/ PART2, PART3 , PART4 , PART5 , PTHBTS, 
     1                IPTR
      INTEGER PD011,PD012,PD021,PD022,PD031,PD032,PD041,PD042,
     1	      PD051,PD052
      INTEGER OTHSZ, NTABLE, PARTSZ, IPTR
      PARAMETER (PD011=5,PD012=10)
      PARAMETER (PD021=3,PD022=6,PD031=6,PD032=10)
      PARAMETER (PD041=4,PD042=12,PD051=7,PD052=6)
      PARAMETER (PARTSZ=PD011*PD012+PD021*PD022+PD031*PD032+
     1		 PD041*PD042+PD051*PD052)
C
C  The number of rows in all parse tables.
C
      PARAMETER (OTHSZ=PD011+PD021+PD031+PD041+PD051)
C
C  The number of parse tables.
C
      PARAMETER (NTABLE=5)
      CHARACTER*1 KEYSEP,KEYTER
      CHARACTER*1 FRMCOM(2)
      INTEGER PART2(OTHSZ), PART3(OTHSZ), PART4(NTABLE), PART5(NTABLE*2)
      CHARACTER*1 PART1(PARTSZ)
      INTEGER PTHBTS
C
C  Size of search path.
C
      INTEGER WHSIZE
      PARAMETER (WHSIZE=3)
