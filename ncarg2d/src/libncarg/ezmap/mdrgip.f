C
C $Id: mdrgip.f,v 1.8 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDRGIP (XCRA,YCRA,NCRA)
C
        DIMENSION XCRA(*),YCRA(*)
C
C This subroutine is called to expand a polygon defined in the unit
C square by interpolating points along any edge of the polygon that
C lies on the edge of the unit square.  The object of this is to
C prevent edge effects between adjacent lat/lon squares filled using
C the RANGS/GSHHS database.
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPRGD/  ICOL(5),ICSF(5),IDPF,LCRA,NILN,NILT,OLAT,OLON
        INTEGER          ICOL,ICSF,IDPF,LCRA,NILN,NILT
        REAL             OLAT,OLON
        SAVE   /MAPRGD/
C
C Do interpolation only if the values set using the EZMAP parameter
C 'II' say to do so and if the polygon has at least three points.
C
        IF ((NILN.GT.1.OR.NILT.GT.1).AND.NCRA.GT.2) THEN
C
C Compute the lengths of the segments to be created by interpolation.
C
          DELX=1./REAL(NILN)
          DELY=1./REAL(NILT)
C
C First, move all the coordinates to the end of the coordinate buffers.
C
          DO 101 I=NCRA,1,-1
            XCRA(I-NCRA+LCRA)=XCRA(I)
            YCRA(I-NCRA+LCRA)=YCRA(I)
  101     CONTINUE
C
C Now, move them back one at a time, interpolating points after each one
C as necessary.
C
          ICRA=LCRA-NCRA
          NCRA=0
C
  102     ICRA=ICRA+1
          NCRA=NCRA+1
          XCRA(NCRA)=XCRA(ICRA)
          YCRA(NCRA)=YCRA(ICRA)
          IF (ICRA.LT.LCRA) THEN
            IF      ((XCRA(ICRA).EQ.0..AND.XCRA(ICRA+1).EQ.0.).OR.
     +               (XCRA(ICRA).EQ.1..AND.XCRA(ICRA+1).EQ.1.)) THEN
              IF (NILT.GT.1) THEN
                XTMP=XCRA(ICRA)
                IF      (YCRA(ICRA).LT.YCRA(ICRA+1)) THEN
                  DO 103 I=INT(YCRA(ICRA  )/DELY)+1,
     +                     INT(YCRA(ICRA+1)/DELY)  ,+1
                    YTMP=DELY*REAL(I)
                    IF (YTMP.GT.YCRA(ICRA  ).AND.
     +                  YTMP.LT.YCRA(ICRA+1)) THEN
                      IF (NCRA.LT.ICRA) THEN
                        NCRA=NCRA+1
                        XCRA(NCRA)=XTMP
                        YCRA(NCRA)=YTMP
                      END IF
                    END IF
  103             CONTINUE
                ELSE IF (YCRA(ICRA).GT.YCRA(ICRA+1)) THEN
                  DO 104 I=INT(YCRA(ICRA  )/DELY)  ,
     +                     INT(YCRA(ICRA+1)/DELY)+1,-1
                    YTMP=DELY*REAL(I)
                    IF (YTMP.LT.YCRA(ICRA  ).AND.
     +                  YTMP.GT.YCRA(ICRA+1)) THEN
                      IF (NCRA.LT.ICRA) THEN
                        NCRA=NCRA+1
                        XCRA(NCRA)=XTMP
                        YCRA(NCRA)=YTMP
                      END IF
                    END IF
  104             CONTINUE
                END IF
              END IF
            ELSE IF ((YCRA(ICRA).EQ.0..AND.YCRA(ICRA+1).EQ.0.).OR.
     +               (YCRA(ICRA).EQ.1..AND.YCRA(ICRA+1).EQ.1.)) THEN
              IF (NILN.GT.1) THEN
                YTMP=YCRA(ICRA)
                IF      (XCRA(ICRA).LT.XCRA(ICRA+1)) THEN
                  DO 105 I=INT(XCRA(ICRA  )/DELX)+1,
     +                     INT(XCRA(ICRA+1)/DELX)  ,+1
                    XTMP=DELX*REAL(I)
                    IF (XTMP.GT.XCRA(ICRA  ).AND.
     +                  XTMP.LT.XCRA(ICRA+1)) THEN
                      IF (NCRA.LT.ICRA) THEN
                        NCRA=NCRA+1
                        XCRA(NCRA)=XTMP
                        YCRA(NCRA)=YTMP
                      END IF
                    END IF
  105             CONTINUE
                ELSE IF (XCRA(ICRA).GT.XCRA(ICRA+1)) THEN
                  DO 106 I=INT(XCRA(ICRA  )/DELX)  ,
     +                     INT(XCRA(ICRA+1)/DELX)+1,-1
                    XTMP=DELX*REAL(I)
                    IF (XTMP.LT.XCRA(ICRA  ).AND.
     +                  XTMP.GT.XCRA(ICRA+1)) THEN
                      IF (NCRA.LT.ICRA) THEN
                        NCRA=NCRA+1
                        XCRA(NCRA)=XTMP
                        YCRA(NCRA)=YTMP
                      END IF
                    END IF
  106             CONTINUE
                END IF
              END IF
            END IF
            GO TO 102
          END IF
C
        END IF
C
C Done.
C
        RETURN
C
      END
