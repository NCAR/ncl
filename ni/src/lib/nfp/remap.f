C NCLFORTSTART
      SUBROUTINE DPOPREMAP(DST_ARRAY,MAP_WTS,DST_ADD,SRC_ADD,SRC_ARRAY,
     +                     NDST,NLINK,NW,NSRC,XMSG)

c written in f77 for the GNU f77 compiler for portability reasons

      IMPLICIT NONE
      INTEGER NLINK,NW,NDST,NSRC
      DOUBLE PRECISION MAP_WTS(NW,NLINK)
      DOUBLE PRECISION DST_ARRAY(NDST)
      DOUBLE PRECISION SRC_ARRAY(NSRC)
      INTEGER DST_ADD(NLINK)
      INTEGER SRC_ADD(NLINK)
      DOUBLE PRECISION XMSG
C NCLEND
      INTEGER N

      DO N = 1,NDST
c initialize
          DST_ARRAY(N) = XMSG
      END DO

      DO N = 1,NLINK
          IF (SRC_ARRAY(SRC_ADD(N)).NE.XMSG) THEN
              IF (DST_ARRAY(DST_ADD(N)).EQ.XMSG) THEN
                  DST_ARRAY(DST_ADD(N)) = SRC_ARRAY(SRC_ADD(N))*
     +                                    MAP_WTS(1,N)
              ELSE
                  DST_ARRAY(DST_ADD(N)) = DST_ARRAY(DST_ADD(N)) +
     +                                    SRC_ARRAY(SRC_ADD(N))*
     +                                    MAP_WTS(1,N)
              END IF
          END IF
      END DO

      RETURN
      END
