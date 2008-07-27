C
C $Id: set3.f,v 1.7 2008-07-27 00:17:34 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SET3 (XA,XB,YA,YB,ULO,UHI,VLO,VHI,WLO,WHI,EYE)
C
C THREE-DIMENSIONAL LINE DRAWING PACKAGE
C
C
C PURPOSE                THREED is a package of subroutines that
C                        provides line drawing capabilities in
C                        three-space.
C
C USAGE                  Each entry point in this package is
C                        described below.
C
C                        SET3 (XA,XB,YA,YB,UC,UD,VC,VD,WC,WD,EYE)
C
C                          XA, XB, YA, YB  define the portion of the
C                          plotting surface into which the user's
C                          plot will be placed.  These values should
C                          be in the range 0. to 1.  For example, if
C                          one wants the plot to occupy the maximum
C                          plotting surface, set XA=0., YA=0., XB=1.,
C                          YB=1.; if one wants the plot to appear in
C                          the lower left corner of the plotting
C                          surface, set XA=0., YA=0., XB=.5, YB=.5 .
C
C                          UC, UD, VC, VD, WC, and WD define a
C                          volume in user-coordinate space which
C                          will be transformed onto the plotting
C                          surface defined by XA, XB, YA, YB.
C
C                          EYE is an array, 3 words long, containing the
C                          U, V, and W coordinates of the EYE position.
C                          All lines in the plot are drawn as viewed
C                          from the EYE.  EYE is specified in user
C                          coordinates and should be outside the box
C                          defined by UC, UD, VC, VD, WC, and WD.
C
C                        CURVE3 (U,V,W,N)
C
C                          Draws a curve through N points.  The
C                          points are defined by the linear arrays
C                          U, V, and W which are dimensioned N or
C                          greater.
C
C                        LINE3 (UA,VA,WA,UB,VB,WB)
C
C                          Draws a line connecting the coordinates
C                          (UA,VA,WA)  and  (UB,VB,WB).
C
C                        FRST3 (U,V,W)
C
C                          Positions the pen to (U,V,W).
C
C                        VECT3 (U,V,W)
C
C                          Draws a line between the current pen
C                          position and the point (U,V,W).  The
C                          current pen position becomes (U,V,W).
C                          Note that a curve can be drawn by using
C                          a FRST3 call followed by a sequence of
C                          VECT3 calls.
C
C                        POINT3 (U,V,W)
C
C                          Plots a point at (U,V,W) .
C
C                        PERIM3 (MAGR1,MINR1,MAGR2,MINR2,IWHICH,VAR)
C
C                          Draws a perimeter with tick marks.
C
C                          IWHICH designates the normal vector to the
C                          perimeter drawn (1=U, 2=V, 3=W).
C
C                          VAR is the value on the axis specified by
C                          IWHICH where the perimeter is to be drawn.
C
C                          MAGR1  and  MAGR2  specify the
C                          number of major tick marks to be drawn in
C                          the two coordinate directions.
C
C                          MINR1  and  MINR2  specify the number
C                          of minor ticks between each major tick.
C
C                          MAGR1, MAGR2, MINR1 and MINR2
C                          are specified by the number
C                          of divisions(holes), not the number of
C                          ticks.  So if MAGR1=1, there would be no
C                          major divisions.
C
C                        TICK43 (MAGU,MINU,MAGV,MINV,MAGW,MINW)
C
C                          TICK43 allows program control of tick
C                          mark length in subroutine PERIM3.
C                          MAGU, MAGV, MAGW specify the length,
C                          in plotter address units of major
C                          division tick marks on the U, V, and W
C                          axes.  MINU, MINV, MINW specify the length,
C                          in plotter address units of minor
C                          division tick marks on the U, V, and
C                          W axes.
C
C                        FENCE3 (U,V,W,N,IOREN,BOT)
C
C                          This entry is used to draw a line in three-
C                          space as well as a "fence" between the
C                          line and a plane normal to one of the
C                          coordinate axes.
C
C                          The arguments U, V, W and N
C                          are the same as for CURVE3, described above.
C
C                          IOREN specifies the direction in which the
C                          fence lines are to be drawn (1 indicates
C                          parallel to the U-axis, 2 indicates parallel
C                          to the V-axis, and 3 indicates parallel to
C                          to the W-axis.)
C
C                          BOT specifies where the bottom of the fence
C                          is to be drawn.
C                          If the fence lines are to be drawn parallel
C                          to the W-axis, and  BOT=2., then the bottom
C                          of the fence would be the plane  W=2.
C
C ON OUTPUT              All arguments are unchanged.
C
C NOTE                   .  For drawing characters in conjunction
C                           with THREED, use the companion routine
C                           PWRZT.
C
C ENTRY POINTS          FENCE3, TRN32T, FRST3, VECT3, LIN3,
C                       POINT3, CURVE3, PSYM3, PERIM3, LINE3W,
C                       DRAWT, TICK43, TICK3, THREBD, THREBDX
C
C COMMON BLOCKS         TEMPRT, SET31, PWRZ1T, TCK31, PRM31, THRINT
C
C REQUIRED LIBRARY      PWRZ and the SPPS
C ROUTINES
C
C REQUIRED GKS LEVEL    0A
C
C HISTORY               Written and standardized in November 1973.
C I/O                   Plots lines.
C
C PRECISION             Single
C
C LANGUAGE              FORTRAN
C
C ACCURACY              + or -.5 plotter address units per call.
C                       There is no cumulative error.
C
C PORTABILITY           ANSI FORTRAN 77
C
C
C
C
C
      SAVE
C
      COMMON /TEMPRT/ RZERO
C
      DIMENSION       EYE(3)
C
      COMMON /SET31/  ISCALE     ,XMIN       ,XMAX       ,YMIN       ,
     1                YMAX       ,BIGD       ,R0         ,NLX        ,
     2                NBY        ,NRX        ,NTY
      COMMON /PWRZ1T/ UUMIN      ,UUMAX      ,VVMIN      ,VVMAX      ,
     1                WWMIN      ,WWMAX      ,DELCRT     ,EYEU       ,
     2                EYEV       ,EYEW
C
      AVE(A,B) = (A+B)*.5
C
C ARITHMETIC STATEMENT FUNCTION FOR SCALING
C
      SU(UTEMP) = UTEMP
      SV(VTEMP) = VTEMP
      SW(WTEMP) = WTEMP
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL THREBD
C
C SET UP FRAME SIZE
C
      NLX = XA*1023.+1.
      NRX = XB*1023.+1.
      NBY = YA*1023.+1.
      NTY = YB*1023.+1.
C
C CONSTANTS FOR PWRZT
C
      UUMIN = ULO
      UUMAX = UHI
      VVMIN = VLO
      VVMAX = VHI
      WWMIN = WLO
      WWMAX = WHI
      EYEU = EYE(1)
      EYEV = EYE(2)
      EYEW = EYE(3)
C
C FIND CORNERS IN 2-SPACE FOR 3-SPACE BOX CONTAINING OBJECT
C
      ISCALE = 0
      ATU = AVE(SU(UUMIN),SU(UUMAX))
      ATV = AVE(SV(VVMIN),SV(VVMAX))
      ATW = AVE(SW(WWMIN),SW(WWMAX))
      BIGD = 0.
      IF (RZERO .LE. 0.) GO TO  10
C
C RELATIVE SIZE FEATURE IN USE.  THIS SECTION OF CODE IS NEVER
C EXECUTED UNLESS RZERO IS SET POSITIVE IN THE CALLING PROGRAM
C VIA COMMON BLOCK TEMPRT.  RZERO  IS THE DISTANCE BETWEEN THE
C OBSERVER AND THE POINT LOOKED AT (CENTER OF THE BOX BY DEFAULT)
C WHEN THE INPUT BOX IS TO FILL THE SCREEN WHEN VIEWED FROM THE
C DIRECTION WHICH MAKES THE BOX BIGGEST.  RZERO  IS THUS TO
C BE USED TO DETERMINE THE SHAPE OF THE OBJECT.   THIS SECTION
C OF CODE IS TO BE USED WHEN IT IS DESIRED TO KEEP THE VIEWED
C OBJECT IN RELATIVE PERSPECTIVE ACROSS FRAMES--E.G. IN MAKING
C MOVIES.
C
      ALPHA = -(VVMIN-ATV)/(UUMIN-ATU)
      VVEYE = -RZERO/SQRT(1.+ALPHA*ALPHA)
      UUEYE = VVEYE*ALPHA
      VVEYE = VVEYE+ATV
      UUEYE = UUEYE+ATU
      WWEYE = ATW
      CALL TRN32T (ATU,ATV,ATW,UUEYE,VVEYE,WWEYE,1)
      CALL TRN32T (UUMIN,VVMIN,ATW,XMIN,DUMM,DUMM,2)
      CALL TRN32T (UUMAX,VVMIN,WWMIN,DUMM,YMIN,DUMM,2)
      CALL TRN32T (UUMAX,VVMAX,ATW,XMAX,DUMM,DUMM,2)
      CALL TRN32T (UUMAX,VVMIN,WWMAX,DUMM,YMAX,DUMM,2)
      BIGD = SQRT((UUMAX-UUMIN)**2+(VVMAX-VVMIN)**2+(WWMAX-WWMIN)**2)*.5
      R0 = RZERO
      GO TO  20
   10 CALL TRN32T (ATU,ATV,ATW,EYE(1),EYE(2),EYE(3),1)
      CALL TRN32T (SU(UUMIN),SV(VVMIN),SW(WWMIN),X1,Y1,DUM,2)
      CALL TRN32T (SU(UUMIN),SV(VVMIN),SW(WWMAX),X2,Y2,DUM,2)
      CALL TRN32T (SU(UUMIN),SV(VVMAX),SW(WWMIN),X3,Y3,DUM,2)
      CALL TRN32T (SU(UUMIN),SV(VVMAX),SW(WWMAX),X4,Y4,DUM,2)
      CALL TRN32T (SU(UUMAX),SV(VVMIN),SW(WWMIN),X5,Y5,DUM,2)
      CALL TRN32T (SU(UUMAX),SV(VVMIN),SW(WWMAX),X6,Y6,DUM,2)
      CALL TRN32T (SU(UUMAX),SV(VVMAX),SW(WWMIN),X7,Y7,DUM,2)
      CALL TRN32T (SU(UUMAX),SV(VVMAX),SW(WWMAX),X8,Y8,DUM,2)
      XMIN = MIN(X1,X2,X3,X4,X5,X6,X7,X8)
      XMAX = MAX(X1,X2,X3,X4,X5,X6,X7,X8)
      YMIN = MIN(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
      YMAX = MAX(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
C
C ADD RIGHT AMOUNT TO KEEP PICTURE SQUARE
C
   20 WIDTH = XMAX-XMIN
      HIGHT = YMAX-YMIN
      DIF = .5*(WIDTH-HIGHT)
      IF (DIF)  30, 50, 40
   30 XMIN = XMIN+DIF
      XMAX = XMAX-DIF
      GO TO  50
   40 YMIN = YMIN-DIF
      YMAX = YMAX+DIF
   50 ISCALE = 1
      CALL TRN32T (ATU,ATV,ATW,EYE(1),EYE(2),EYE(3),1)
      RETURN
      END
