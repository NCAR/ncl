C
C       $Id: ngmftc.f,v 1.2 2000-07-12 16:24:45 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE NGMFTC(WKID)
C
C Effects a temporaty close in metafile output allowing for subsequent 
C reopening for appending using NGREOP.  The metafile output may be 
C suspended at any time, and not necessarily at a picture break.
C
C ARGUMENTS
C
C ON INPUT  
C    
C   WKID  An integer argument indicating the workstation ID of the
C         workstation to temporarily close.
C
      INTEGER WKID
      CHARACTER*80 IDR(1),ODR(1)
C
      WRITE(IDR(1)(1:5),500) WKID
  500 FORMAT(I5)
      CALL SFLUSH
      CALL GESC(-1387,1,IDR,1,1,ODR)
C
      RETURN
      END
