C
C       $Id: hlustmpxy.f,v 1.3 2000-07-12 16:26:06 haley Exp $
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
C These routine stand between Streamline and the user call-back routines
C in stmpxy.f  When HLUs are not in use, this version of the routine
C gets loaded, so that STMPXY, STIMXY, and STMPTA get called. 
C When the HLUs are in use, they load another version; it either does 
C the appropriate thing for the purposes of the HLUs or calls STMPXY,
C STIMXY, and STMPTA as needed.
C
      SUBROUTINE HLUSTMPXY(XDA,YDA,XUS,YUS,IST)
C
      CALL STMPXY(XDA,YDA,XUS,YUS,IST)
C
      RETURN
C
      END
C
C -------------------------------------------------------------
C
      SUBROUTINE HLUSTIMXY(XUS,YUS,XDA,YDA,IST)
C
      CALL STIMXY(XUS,YUS,XDA,YDA,IST)
C
      RETURN
C
      END
C
C -------------------------------------------------------------
C
      SUBROUTINE HLUSTMPTA(XDA,YDA,XUS,YUS,XND,YND,DU,DV,TA,IST)
C
      CALL STMPTA(XDA,YDA,XUS,YUS,XND,YND,DU,DV,TA,IST)
C
      RETURN
C
      END
