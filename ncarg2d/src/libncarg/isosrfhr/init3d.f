C
C       $Id: init3d.f,v 1.5 2008-07-27 00:17:16 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE INIT3D (EYE,NU,NV,NW,ST1,LX,NY,IS2,IU,S)
C
C ISO-SURFACE PACKAGE FOR HIGH RESOLUTION 3-DIMENSIONAL ARRAYS
C
C
C DIMENSION OF           EYE(3),ST1(NV,NW,2),IS2(LX,NY),S(4),
C ARGUMENTS              IOBJS(MV,NW)
C
C PURPOSE                This package of three routines produces a
C                        perspective picture of an arbitrary object or
C                        group of objects with the hidden parts not
C                        drawn.  The objects are assumed to be stored
C                        in the format described below, a format which
C                        was chosen to facilitate the display of
C                        functions of three variables  or output from
C                        high resolution three-dimensional computer
C                        simulations.
C
C
C USAGE                  The object is defined by setting elements in a
C                        three-dimensional array to one where the object
C                        is and zero where it is not.  That is, the
C                        position in the array corresponds to a position
C                        in three-space, and the value of the array
C                        tells whether any object is present at that
C                        position or not.  Because a large array is
C                        needed to define objects with good resolution,
C                        only a part of the array is passed to the
C                        package with each call.
C
C                        There are two subroutines in the package.
C                        INIT3D is called at the beginning of a picture.
C                        This call can be skipped sometimes if certain
C                        criteria are met and certain precautions are
C                        taken.  See the 'TIMING' section for details.
C                        DANDR (Draw AND Remember) is called
C                        successively to process different parts of the
C                        three-dimensional array.  For example,
C                        one plane would be processed in the
C                        first call to DANDR, while a different plane
C                        would be processed in a subsequent call.  An
C                        example follows:
C
C                             CALL INIT3D(EYE,NU,NV,NW,ST1,LX,NY,
C                                         IS2,IU,S)
C                             DO 1 IBKWDS = 1,NU
C                             I = NU+1-IBKWDS
C                        C  FORM OR READ SLAB I OF THE 3 DIMENSIONAL
C                        C  ARRAY
C                        C  ONLY 1 OR 0 IN THE SLAB, CALLED IOBJS
C                              . . .
C                           1 CALL DANDR(NV,NW,ST1,LX,NX,NY,IS2,IU,S,
C                                        IOBJS,MV)
C
C ARGUMENTS
C
C ON INPUT               EYE
C                          An array, dimensioned 3, containing the U, V,
C                          and W coordinates of the eye position.  Objects
C                          are considered to be in a box with 2 extreme
C                          corners at (1,1,1) and (NU,NV,NW).  The eye
C                          position must have positive coordinates away
C                          from the coordinate planes U = 0, V = 0, and
C                          W = 0.  While gaining experience with the
C                          package, use EYE(1) = 5*NU, EYE(2) = 4*NV,
C                          EYE(3) = 3*NW.
C
C                        NU
C                          U direction length of the box containing the
C                          objects.
C
C                        NV
C                          V direction length of the box containing the
C                          objects.
C
C                        NW
C                          W direction length of the box containing the
C                          objects.
C
C                        ST1
C                          A scratch array at least NV*NW*2 words long.
C
C                        LX
C                          The number of words needed to hold NX bits.
C                          Also, the first dimension of IS2.  See NX
C                          and IS2.  On a 60 bit machine, LX=(NX-1)/60+1.
C
C                        NY
C                          Number of cells in the Y-direction of the
C                          model of the image plane.  Also the second
C                          dimension of IS2.
C
C                        IS2
C                          An array holding the image plane model.  It
C                          is dimensioned LX by NY.  The model is NX by
C                          NY and packed densely.  If hidden lines are
C                          drawn, decrease NX and NY (and LX if
C                          possible).  If visible lines are left out of
C                          the picture, increase NX and NY (and LX if
C                          need be).  As a guide, some examples showing
C                          successful choices are listed:
C
C                            GIVEN  NU  NV NW           RESULTING NX  NY
C                                                       FROM TESTING
C                            -------------------------------------------
C                                  100 100 60                    200 200
C                                   60  60 60                    110 110
C                                   40  40 40                     75  75
C
C                        IU
C                          Unit number of scratch file for the package.
C                          ST1 will be written NU times on this file.
C
C                        S
C                          A real array, dimensioned 4, which contains the
C                          world coordinates of the area where the picture
C                          is to be drawn.  That is, all plotting
C                          coordinates generated will be bounded as
C                          follows:  X coordinates will be between S(1)
C                          and S(2), Y coordinates will be between S(3)
C                          and S(4).  To prevent distortion, have
C                          S(2)-S(1) = S(4)-S(3).
C                          10.0 .LE. S(I) .LE. 1010.0   for I = 1,2,3,4.
C
C                        IOBJS
C                          A NV by NW array (with actual first dimension
C                          MV in the calling program) describing the
C                          object.  If this is the Ith call to DANDR,
C                          the part of the picture at U = NU+1-I is to
C                          be processed.  IOBJS defines the objects to
C                          be drawn in the following manner:
C                          IOBJS(J,K) = 1 if any object contains the
C                          point (NU+1-I,J,K) and IOBJS(J,K) = 0
C                          otherwise.
C
C                        NX
C                          This variable is an argument to DANDR.  It is
C                          the number of cells in the X-direction of a
C                          model of the image plane.  A silhouette of
C                          the parts of the picture processed so far is
C                          stored in this model.  Lines to be drawn are
C                          tested for visibility by examining the
C                          silhouette.  Lines in the silhouette are
C                          hidden.  Lines out of the silhouette are
C                          visible.  The solution is approximate
C                          because the silhouette is not formed exactly.
C                          See IS2.
C
C                        MV
C                          Actual first dimension of IOBJS in the
C                          calling program.  When plotting all of IOBJS,
C                          NV = MV.
C
C ON OUTPUT              EYE, NU, NV, NW, LX, NX, NY, IU, S, IOBJS, and
C                        MV are unchanged.  ST1 and IS2 have been
C                        written in.
C
C NOTE                   This routine is for large arrays, 40 x 40 x 30
C                        is a practical minimum.
C
C ENTRY POINTS           INIT3D, PERSPC, AND DANDR
C
C COMMON BLOCKS          None
C
C I/O                    Plots visible surfaces, uses scratch file or
C                        tape.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN 77
C
C HISTORY                Originally developed at NCAR starting in
C                        late 1970.
C
C ALGORITHM              The basic method is to contour cuts through
C                        the array starting with a cut nearest the
C                        observer.  The algorithm leaves out the hidden
C                        parts of the contours by suppressing those lines
C                        enclosed within the lines produced while processing
C                        the preceding cuts.  The technique is described
C                        in detail in the reference cited below.
C
C REQUIRED GKS LEVEL     0A
C
C REFERENCE              Wright, T.:  A One-pass Hidden-line Remover for
C                        Computer Drawn Three-space Objects.  Proc 1972
C                        Summer Computer Simulation Conference, 261-267,
C                        1972.
C
C ACCURACY               The algorithm is not exact.  However,
C                        reasonable pictures are produced.
C
C TIMING                 This routine is very time consuming.  If many
C                        pictures are produced with the same size arrays
C                        and eye position, much time can be saved by
C                        rewinding unit IU, filling IS2 with zeros, and
C                        skipping the call to INIT3D for other than the
C                        first picture.
C
C PORTABILITY            Two machine dependent constants are initialized
C                        in DANDR.  INIT3D has an entry statement for
C                        PERSPC.  In DANDR, .AND. and .OR. are used for
C                        masking operations.
C
C
C
      DIMENSION       EYE(3)     ,ST1(NV,NW,2)           ,IS2(LX,NY) ,
     1                S(4)
C
        SAVE
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
      CALL Q8QST4('GRAPHX','ISOSRFHR','INIT3D','VERSION  1')
C
C SET UP TRANSFORMATION ROUTINE FOR THIS LINE OF SIGHT.
C
      GO TO 101
C
C     ENTRY PERSPC(X,Y,Z,XT,YT,ZT)
C
      ENTRY PERSPC
      GO TO 122
  101 U = NU
      V = NV
      W = NW
      AX = U*.5
      AY = V*.5
      AZ = W*.5
      EX = EYE(1)
      EY = EYE(2)
      EZ = EYE(3)
      DX = AX-EX
      DY = AY-EY
      DZ = AZ-EZ
      D = SQRT(DX*DX+DY*DY+DZ*DZ)
      COSAL = DX/D
      COSBE = DY/D
      COSGA = DZ/D
      AL = ACOS(COSAL)
      BE = ACOS(COSBE)
      GA = ACOS(COSGA)
      SINGA = SIN(GA)
C
C TEST FOR CLOSE TO VERTICAL
C
      IF (SINGA .LT. 0.0001) GO TO 102
      R = 1./SINGA
      JUMP = 1
      GO TO 103
  102 SINBE = SIN(BE)
      R = 1./SINBE
      JUMP = 2
  103 X = 1.
      Y = 1.
      Z = W
      IBAK = 1
      GO TO 123
  104 YTS = YT
      X = U
      Y = V
      Z = 1.
      IBAK = 2
      GO TO 123
  105 YB = YT
      Y = 1.
      IBAK = 3
      GO TO 123
  106 XL = XT
      X = 1.
      Y = V
      IBAK = 4
      GO TO 123
  107 XR = XT
C
C MAKE SCREEN SQUARE
C
      DIF = (XR-XL-YTS+YB)*.5
      IF (DIF) 108,110,109
  108 XL = XL+DIF
      XR = XR-DIF
      GO TO 110
  109 YB = YB-DIF
      YTS = YTS+DIF
  110 REWIND IU
      C1 = .9*(S(2)-S(1))/(XR-XL)
      C2 = .05*(S(2)-S(1))+S(1)
      C3 = .9*(S(4)-S(3))/(YTS-YB)
      C4 = .05*(S(4)-S(3))+S(3)
      COSALR = COSAL*R*C1
      QF1 = (EX*COSAL+EY*COSBE+EZ*COSGA)/D
      RC3 = R*C3
      COSALD = COSAL/D
      COSBED = COSBE/D
      COSGAD = COSGA/D
C
C TRANSFORM GRID POINTS INTO 2-SPACE
C
      GO TO (111,115),JUMP
  111 COSBER = COSBE*R*C1
      X1F1 = (EX-AX)*COSBER+(AY-EY)*COSALR-C1*XL+C2
      X1F2 = EY*COSALR-EX*COSBER
      Y1F1 = (EZ-AZ)*RC3-C3*YB+C4
      DO 114 I=1,NU
         X = NU+1-I
         QI1 = X*COSALD-QF1
         XF1 = X*COSBER+X1F2
         DO 113 J=1,NV
            Y = J
            QI2 = Y*COSBED+QI1
            YF1 = XF1-Y*COSALR
            DO 112 K=1,NW
               Z = K
               QI3 = Z*COSGAD+QI2
               ST1(J,K,1) = X1F1+YF1/QI3
               ST1(J,K,2) = Y1F1+RC3*(Z-EZ)/QI3
  112       CONTINUE
  113    CONTINUE
         WRITE (IU) ST1
  114 CONTINUE
      GO TO 119
  115 COSGAR = COSGA*R*C1
      X2F1 = (EZ-AZ)*COSALR+(AX-EX)*COSGAR-C1*XL+C2
      X2F2 = EX*COSGAR-EZ*COSALR
      Y2F1 = (EY-AY)*RC3-C3*YB+C4
      DO 118 I=1,NU
         X = NU+1-I
         QI1 = X*COSALD-QF1
         XF1 = X2F2-X*COSGAR
         DO 117 J=1,NV
            Y = J
            QI2 = Y*COSBED+QI1
            YF1 = (Y-EY)*RC3
            DO 116 K=1,NW
               Z = K
               QI3 = Z*COSGAD+QI2
               ST1(J,K,1) = X2F1+(XF1+Z*COSALR)/QI3
               ST1(J,K,2) = Y2F1+YF1/QI3
  116       CONTINUE
  117    CONTINUE
         WRITE (IU) ST1
  118 CONTINUE
  119 REWIND IU
C
C ZERO OUT SCREEN MODEL
C
      DO 121 J=1,NY
         DO 120 I=1,LX
            IS2(I,J) = 0
  120    CONTINUE
  121 CONTINUE
      RETURN
C
C RETAIN OLD CODE FOR TRANSFORMING OUTER POINTS
C
  122 IBAK = 5
  123 Q = D/((X-EX)*COSAL+(Y-EY)*COSBE+(Z-EZ)*COSGA)
      GO TO (124,125),JUMP
  124 XT = ((EX+Q*(X-EX)-AX)*COSBE-(EY+Q*(Y-EY)-AY)*COSAL)*R
      YT = (EZ+Q*(Z-EZ)-AZ)*R
      GO TO 126
  125 XT = ((EZ+Q*(Z-EZ)-AZ)*COSAL-(EX+Q*(X-EX)-AX)*COSGA)*R
      YT = (EY+Q*(Y-EY)-AY)*R
  126 GO TO (104,105,106,107,127) , IBAK
  127 RETURN
      END
