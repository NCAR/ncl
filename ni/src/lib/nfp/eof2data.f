C NCLFORTSTART
      SUBROUTINE DEOF2DATA(NEVAL,NPTS,NTIM,EV,EVTS,X,XMSG)
      IMPLICIT NONE
C                                                 INPUT
      INTEGER NEVAL,NPTS,NTIM
      DOUBLE PRECISION EV(NPTS,NEVAL),EVTS(NTIM,NEVAL),XMSG
C                                                 OUTPUT
      DOUBLE PRECISION X(NTIM,NPTS)
C NCLEND

C NCL: xEof = eof2data(ev,evts[*][*])

c construct a data array [x] from the ev and evts arrays
c .   if neval is the maximum number the reconstruction
c .   should be exact

      INTEGER NP,NE,NT,KNT
      DOUBLE PRECISION SUME
c
      DO NT = 1,NTIM
          DO NP = 1,NPTS
              X(NT,NP) = XMSG
              KNT = 0
              SUME = 0.D0
              DO NE = 1,NEVAL
                  IF (EVTS(NT,NE).NE.XMSG .AND. EV(NP,NE).NE.XMSG) THEN
                      KNT = KNT + 1
                      SUME = SUME + EVTS(NT,NE)*EV(NP,NE)
                  END IF
              END DO
              IF (KNT.GT.0) X(NT,NP) = SUME
          END DO
      END DO

      RETURN
      END
