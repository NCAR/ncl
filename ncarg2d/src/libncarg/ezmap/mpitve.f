C
C $Id: mpitve.f,v 1.1 1999-04-02 22:59:45 kennison Exp $
C
      SUBROUTINE MPITVE (RLTV,RLNV,PAPV,UCOV,VCOV,
     +                   RLTI,RLNI,PAPI,UCOI,VCOI,
     +                   RLTE,RLNE,PAPE,UCOE,VCOE)
C
C The routine MPITVE (MaP, Interpolate To Visible Edge), given the real
C latitude and longitude (RLTV and RLNV) of a point which is visible
C under the current projection, the value of P associated with that
C point (PAPV), and the U and V coordinates (UCOV and VCOV) of the
C point's projection, plus similar quantities (RLTI, RLNI, PAPI, UCOI,
C and VCOI) for a point which is either invisible under the current
C projection or is separated from it by a "crossover" boundary, returns
C similar quantities for an intermediate point at the very edge of the
C visible area or on the "crossover" boundary.
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM8/ P,Q,R
      SAVE   /MAPCM8/
C
C Declare a couple of one-dimensional arrays to use in a call to MAPGCI.
C Mostly, this is to prevent compilers from giving us a warning message,
C but it does also make the code comply with the FORTRAN standard.
C
      DIMENSION DLTH(1),DLNH(1)
      EQUIVALENCE (DLTH(1),RLTH),(DLNH(1),RLNH)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MPITVE - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
      RLTA=RLTV
      RLNA=RLNV
      PAPA=PAPV
      UCOA=UCOV
      VCOA=VCOV
      RLTB=RLTI
      RLNB=RLNI
      PAPB=PAPI
      UCOB=UCOI
      VCOB=VCOI
C
      ITMP=0
C
  101 CALL MAPGCI (RLTA,RLNA,RLTB,RLNB,1,DLTH,DLNH)
      IF (ABS(RLNH-RLNA).GT.180.) RLNH=RLNH+SIGN(360.,RLNA-RLNH)
      CALL MAPTRA (RLTH,RLNH,UCOH,VCOH)
      IF (ICFELL('MPITVE',2).NE.0) RETURN
      PAPH=P
      IF (UCOH.GE.1.E12) THEN
        IF (RLTH.EQ.RLTB.AND.RLNH.EQ.RLNB) GO TO 102
        RLTB=RLTH
        RLNB=RLNH
        PAPB=PAPH
        UCOB=UCOH
        VCOB=VCOH
      ELSE IF (UCOB.GE.1.E12) THEN
        IF (RLTH.EQ.RLTA.AND.RLNH.EQ.RLNA) GO TO 102
        RLTA=RLTH
        RLNA=RLNH
        PAPA=PAPH
        UCOA=UCOH
        VCOA=VCOH
      ELSE IF ((UCOH-UCOA)**2+(VCOH-VCOA)**2.LT.
     +         (UCOH-UCOB)**2+(VCOH-VCOB)**2) THEN
        IF (RLTH.EQ.RLTA.AND.RLNH.EQ.RLNA) GO TO 102
        RLTA=RLTH
        RLNA=RLNH
        PAPA=PAPH
        UCOA=UCOH
        VCOA=VCOH
      ELSE
        IF (RLTH.EQ.RLTB.AND.RLNH.EQ.RLNB) GO TO 102
        RLTB=RLTH
        RLNB=RLNH
        PAPB=PAPH
        UCOB=UCOH
        VCOB=VCOH
      END IF
C
      ITMP=ITMP+1
      IF (ITMP.LT.64) GO TO 101
C
  102 RLTE=RLTA
      RLNE=RLNA
      PAPE=PAPA
      UCOE=UCOA
      VCOE=VCOA
C
      RETURN
C
      END
