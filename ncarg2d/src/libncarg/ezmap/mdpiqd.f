C
C $Id: mdpiqd.f,v 1.3 2005-06-22 21:36:45 kennison Exp $
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
      SUBROUTINE MDPIQD
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCMP/  NPTB,XPTB(50),YPTB(50)
        INTEGER          NPTB
        REAL             XPTB,YPTB
        SAVE   /MAPCMP/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPIQD - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Flush the points buffer.
C
        IF (NPTB.GT.0) THEN
          CALL POINTS (XPTB,YPTB,NPTB,0,0)
          IF (ICFELL('MDPIQD',2).NE.0) RETURN
          NPTB=0
        END IF
C
C Flush the buffer in DASHPACK.
C
        CALL DPLAST
        IF (ICFELL('MDPIQD',3).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
