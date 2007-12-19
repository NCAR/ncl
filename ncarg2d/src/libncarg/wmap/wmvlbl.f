C
C	$Id: wmvlbl.f,v 1.1 2007-12-19 02:21:50 fred Exp $
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
      SUBROUTINE WMVLBL(X,Y)
C
C  This subroutine plots an informational label box using the
C  current settings for vactor scaling "VRS" and "VRN".  The
C  (X,Y) coordinate specifies the lower right hand corner of
C  the box position.  The settings for "VLB" and "VLF" specify
C  the background and foreground colors for the box and "VSC"
C  specifies the scale factor.
C
      include 'wmcomn.h'
C
      PARAMETER (R2D=57.2957795131)
      DIMENSION XX(5),YY(5)
      CHARACTER*8 CHRDIST
      INTEGER VCC,FN,CC
C
C  Save the current attributes.
C
      CALL GQPLCI(IER,ILCLRO)
      CALL GQFACI(IER,IFCLRO)
      CALL GQLWSC(IER,RLNWTH)
C
      CALL GSLWSC(VCLWID)
C
C  Convert X and Y to NDC and work in NDC space.
C
      CALL WMW2NX(1,X,XNDC)
      CALL WMW2NY(1,Y,YNDC)
      CALL GQCNTN(IER,NTRO)
      CALL GSELNT(0)
C
C  Draw boundary box.
C
      XX(1) = XNDC
      YY(1) = YNDC
      XX(2) = XX(1)
      YY(2) = YY(1)+0.05
      XX(3) = XX(2)-0.10
      YY(3) = YY(2)
      XX(4) = XX(3)
      YY(4) = YY(1)
      XX(5) = XX(1)
      YY(5) = YY(1)

      CALL GSFACI(VLBLBC)
      CALL GFA(5,XX,YY)
      CALL GSLWSC(1.)
      CALL GSPLCI(VLBLFC)
      CALL GPL(5,XX,YY)
C
C  Draw a vector of NDC length 0.07 using the current settings
C  for vectors.
C
      CALL WMGETR('VRS',VRS)
      CALL WMGETR('VRN',VRN)
      CALL WMGETR('VCW',VCW)
      CALL WMGETR('VCH',VCH)
      CALL WMGETI('VCC',VCC)
C
      CALL WMSETR('VRS',0.07)
      CALL WMSETR('VRN',0.07)
      CALL WMSETR('VCW',2.)
      CALL WMSETR('VCH',0.02)
      CALL WMSETI('VCC',VLBLFC)
C
      CALL WMVECT(XNDC-0.10+0.015,YNDC+0.015,0.07,0.)
      CALL WMSETR('VRS',VRS)
      CALL WMSETR('VRN',VRN)
      CALL WMSETR('VCW',VCW)
      CALL WMSETR('VCH',VCH)
      CALL WMSETI('VCC',VCC)

      REFDIST = 0.07*VCUREF/VCNREF
      WRITE(CHRDIST,501) REFDIST
  501 FORMAT(E8.2)

      CALL PCGETI('CC',CC)
      CALL PCGETI('FN',FN)

      CALL PCSETI('CC',VLBLFC)
      CALL PCSETI('FN',21)

      CALL PLCHHQ(XNDC-0.10+0.05,YNDC+0.035,CHRDIST,0.01275,0.,0.)
      CALL PCSETI('CC',CC)
      CALL PCSETI('FN',FN)
C
C  Restore the original environment.
C
      CALL GSPLCI(ILCLRO)
      CALL GSFACI(IFCLRO)
      CALL GSELNT(NTRO)
      CALL GSLWSC(RLNWTH)
C
      RETURN
      END
