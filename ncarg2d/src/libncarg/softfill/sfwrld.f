C
C $Id: sfwrld.f,v 1.5 2003-05-20 20:52:12 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
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
      SUBROUTINE SFWRLD (XRA,YRA,NRA,DST,NST,IND,NND)
C
C Declare the dimensions of argument arrays.
C
      DIMENSION XRA(NRA),YRA(NRA),DST(NST),IND(NND)
C
C Declare the labeled common block.
C
      COMMON /SFCOMN/ AID,DBL,ITY,LPA,RDS,IDC,LCH,LDP(8,8)
C
C Declare the block data routine external to force its loading.
C
      EXTERNAL SFBLDA
C
C Check for an uncleared prior error.
C
      IF (ICFELL('SFWRLD - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Convert the data to the proper units.
C
      DO 10001 I=1,NRA
      XRA(I)=CUFX(XRA(I))
      IF (ICFELL('SFWRLD',2).NE.0) RETURN
      YRA(I)=CUFY(YRA(I))
      IF (ICFELL('SFWRLD',3).NE.0) RETURN
10001 CONTINUE
C
C Call the routine SFNORM to finish the job.
C
      CALL SFNORM (XRA,YRA,NRA,DST,NST,IND,NND)
      IF (ICFELL('SFWRLD',4).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
