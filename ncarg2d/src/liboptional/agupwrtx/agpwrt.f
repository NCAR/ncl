C
C	$Id: agpwrt.f,v 1.5 2008-07-27 00:23:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGPWRT (PX,PY,CS,NC,IS,IO,IC)
C
      CHARACTER*(*) CS
C
C This subroutine enables one to use PWRITX with AUTOGRAPH.  The results
C should be reasonably good, as long as nothing too fancy is tried.
C
C The basic notion of the routine is that calls to it with IC zeroed,
C implying that the string is to be centered on the point (PX,PY), may
C simply be passed directly to PWRITX without change and without much
C fear of problems caused by circumventing AUTOGRAPH's checks for labels
C that overlap.  Calls to it with IC non-zero, implying that the left
C or right end of the label is to be placed at (PX,PY) must be handled
C differently:  One character at a time is passed to PWRITX.
C
C What this means for the user is that function codes may appear in the
C x- and y-axis labels, in the graph label, and in any other label which
C has the centering parameter zeroed.  A label in which lots of function
C codes are used may look very long to AUTOGRAPH and cause it to shrink
C the character size for the label in order to prevent what it thinks is
C a potential overlap problem.  If this happens, see the consultants.
C
C CH holds single characters of CS.
C
      CHARACTER*1 CH
C
C If the centering option is zero, use PWRITX to do the whole string.
C
      IF (IC.EQ.0) THEN
        CALL PWRITX (PX,PY,CS,NC,IS,IO,0)
        RETURN
      END IF
C
C Otherwise, use PWRITX to produce one character at a time.
C
      CALL GETSET (F1,F2,G1,G2,X1,X2,Y1,Y2,LL)
C
      XS=REAL(IS)*COS(.017453292519943*REAL(IO))*(X2-X1)/
     +                                           REAL(KFPX(F2)-KFPX(F1))
      YS=REAL(IS)*SIN(.017453292519943*REAL(IO))*(Y2-Y1)/
     +                                           REAL(KFPY(G2)-KFPY(G1))
C
      FX=PX
      FY=PY
C
      IF (IC.GT.0) THEN
        FX=FX-REAL(NC)*XS
        FY=FY-REAL(NC)*YS
      END IF
C
      FX=FX-.5*XS
      FY=FY-.5*YS
C
         DO 101 I=1,NC
         FX=FX+XS
         FY=FY+YS
         CH=CS(I:I)
         CALL PWRITX (FX,FY,CH,1,IS,IO,0)
  101    CONTINUE
C
      RETURN
C
C REVISION HISTORY -----------------------------------------------------
C
C December, 1984        First put on XLIB.  Adapted from AGUSEPWRX.
C
C June, 1987            Modified to be GKS-compatible.
C
C August, 1987          Took out comments about XLIB.
C
C ----------------------------------------------------------------------
      END
