C
C	$Id: lined.f,v 1.3 2000-08-22 15:10:21 haley Exp $
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
      SUBROUTINE LINED (XA,YA,XB,YB)
C USER ENTRY POINT.
C
      DATA IDUMMY /0/
      CALL FL2INT (XA,YA,IXA,IYA)
      CALL FL2INT (XB,YB,IXB,IYB)
C
      CALL CFVLD (1,IXA,IYA)
      CALL CFVLD (2,IXB,IYB)
      CALL CFVLD (3,IDUMMY,IDUMMY)
C
      RETURN
C
C------REVISION HISTORY
C
C JUNE 1984          CONVERTED TO FORTRAN77 AND GKS
C
C DECEMBER 1979      ADDED REVISION HISTORY AND STATISTICS
C                    CALL
C
C JUNE 1988          CHANGED THE NAME OF A COMMON BLOCK TO GET RID OF A
C                    WARNING FROM SEGLDR.  (DJK)
C
C NOVEMBER 1988      REMOVED KURV1S AND KURV2S.  IMPLEMENTED CALLS TO
C                    MSKRV1 AND MSKRV2 INSTEAD.
C
C-----------------------------------------------------------------------
C
      END
