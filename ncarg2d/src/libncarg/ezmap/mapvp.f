C
C $Id: mapvp.f,v 1.5 1998-04-16 20:21:26 kennison Exp $
C
      SUBROUTINE MAPVP (UOLD,VOLD,U,V)
C
C Plot the line segment from (UOLD,VOLD) TO (U,V), using either a solid
C line or a dotted line (depending on the value of the common variable
C IDTL).
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE /MAPCM4/
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      SAVE /MAPCMA/
      COMMON /MAPCMP/ NPTB,XPTB(50),YPTB(50)
      SAVE /MAPCMP/
C
C Select vector or dot mode.
C
      IF (IDTL.EQ.0) THEN
C
C Use a single vector.
C
        CALL VECTD (U,V)
        IF (ICFELL('MAPVP',1).NE.0) RETURN
C
      ELSE
C
C Use dots.  DELU and DELV are the u and v components of the vector
C joining (UOLD,VOLD) to (U,V) and VLEN is the length of the vector.
C
        DELU=U-UOLD
        DELV=V-VOLD
C
        VLEN=SQRT(DELU*DELU+DELV*DELV)
C
C Now distribute dots along the vector.  The first one is spaced just
C far enough along it (DATL units) to be DBTD units away from the last
C dot on the previous vector and the rest are DBTD units apart.
C
  101   IF (DATL.LT.VLEN) THEN
          IF (NPTB.GE.50) THEN
            CALL POINTS (XPTB,YPTB,NPTB,0,0)
            IF (ICFELL('MAPVP',2).NE.0) RETURN
            NPTB=0
          END IF
          NPTB=NPTB+1
          XPTB(NPTB)=UOLD+(DATL/VLEN)*DELU
          YPTB(NPTB)=VOLD+(DATL/VLEN)*DELV
          DATL=DATL+DBTD
          GO TO 101
        END IF
C
C Set DATL for the next call.
C
        DATL=DATL-VLEN
C
      END IF
C
C Done.
C
      RETURN
C
      END
