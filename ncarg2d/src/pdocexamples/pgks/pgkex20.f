
      PROGRAM PGKEX20
C 
C  Getting a PostScript plot to fill an entire page.
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWKID=1)
      CHARACTER*3 LAB
C
C  Open GKS.
C
      CALL GOPKS (IERRF,IDUM) 
C
C  Specify the output position for the next PostScript workstation to be
C  opened.  These calls must appear before the call to open the workstation.
C
      CALL NGSETI('LX', 50)
      CALL NGSETI('LY', 50)
      CALL NGSETI('UX',742)
      CALL NGSETI('UY',742)
C
C  Open and activate a color PostScript workstation in landscape mode.
C
      CALL GOPWK (IWKID, 2, NGPSWK('PS','LAND','COLOR'))
      CALL GACWK (IWKID) 
C
C  Draw three rows and four columns of square boxes.
C
      NUM = 0
      DO 10 J=1,3
        Y = 0.25*REAL(J-1)
        DO 20 I=1,4
          NUM = NUM+1
          WRITE(LAB,'(I3)') NUM
          X = 0.25*REAL(I-1)
          CALL BOX(X, Y, 0.25, LAB)
   20   CONTINUE
   10 CONTINUE
      CALL FRAME
C
C  Close things out.
C
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
C
      STOP
      END
      SUBROUTINE BOX(X,Y,SZ,LAB)
C
C  Draw a square box with lower left corner at (X,Y) and size SZ x SZ 
C  and put the label LAB in the center.
C
      CHARACTER*3 LAB
      DIMENSION A(5),B(5)
C
C  Draw box.
C
      A(1) = X
      B(1) = Y
      A(2) = X+SZ
      B(2) = Y
      A(3) = A(2)
      B(3) = Y+SZ
      A(4) = X
      B(4) = B(3)
      A(5) = X
      B(5) = Y
      CALL GPL(5,A,B)
C
C  Write label in box.
C
      CALL GSCHH(0.25*SZ)
      CALL GSTXAL(2,3)
      CALL GTX(X+.5*SZ, Y+.5*SZ, LAB)
C
      RETURN
      END
