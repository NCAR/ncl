C
C	$Id: fnttab.h,v 1.3 2000-08-22 03:40:25 haley Exp $
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
