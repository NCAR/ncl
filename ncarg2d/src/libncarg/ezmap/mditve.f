C
C $Id: mditve.f,v 1.9 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDITVE (RLTV,RLNV,PAPV,UCOV,VCOV,
     +                   RLTI,RLNI,PAPI,UCOI,VCOI,
     +                   RLTE,RLNE,PAPE,UCOE,VCOE)
C
        DOUBLE PRECISION RLTV,RLNV,PAPV,UCOV,VCOV,
     +                   RLTI,RLNI,PAPI,UCOI,VCOI,
     +                   RLTE,RLNE,PAPE,UCOE,VCOE
C
C The routine MDITVE (MaP, Interpolate To Visible Edge), given the
C latitude and longitude (RLTV and RLNV) of a point which is visible
C under the current projection, the value of P associated with that
C point (PAPV), and the U and V coordinates (UCOV and VCOV) of the
C point's projection, plus similar quantities (RLTI, RLNI, PAPI, UCOI,
C and VCOI) for a point which is either invisible under the current
C projection or is separated from it by a "crossover" boundary, returns
C similar quantities for an intermediate point at the very edge of the
C visible area or on the "crossover" boundary.
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM8/  P,Q,R
        DOUBLE PRECISION P,Q,R
        SAVE   /MAPCM8/
C
C Declare local variables, including a couple of one-dimensional arrays
C to use in a call to MDPGCI.  Mostly, this is to prevent compilers from
C giving us a warning message, but it also makes the code comply with
C the FORTRAN standard.
C
        DOUBLE PRECISION DLNH(1),DLTH(1),PAPA,PAPB,PAPH,RLNA,RLNB,RLNH,
     +                   RLTA,RLTB,RLTH,UCOA,UCOB,UCOH,VCOA,VCOB,VCOH
        INTEGER          ITMP
        EQUIVALENCE      (DLTH(1),RLTH),(DLNH(1),RLNH)
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDITVE - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
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
  101   CALL MDPGCI (RLTA,RLNA,RLTB,RLNB,1,DLTH,DLNH)
        IF (ABS(RLNH-RLNA).GT.180.D0) RLNH=RLNH+SIGN(360.D0,RLNA-RLNH)
        CALL MDPTRA (RLTH,RLNH,UCOH,VCOH)
        IF (ICFELL('MDITVE',2).NE.0) RETURN
        PAPH=P
        IF (UCOH.GE.1.D12) THEN
          IF (RLTH.EQ.RLTB.AND.RLNH.EQ.RLNB) GO TO 102
          RLTB=RLTH
          RLNB=RLNH
          PAPB=PAPH
          UCOB=UCOH
          VCOB=VCOH
        ELSE IF (UCOB.GE.1.D12) THEN
          IF (RLTH.EQ.RLTA.AND.RLNH.EQ.RLNA) GO TO 102
          RLTA=RLTH
          RLNA=RLNH
          PAPA=PAPH
          UCOA=UCOH
          VCOA=VCOH
        ELSE IF ((UCOH-UCOA)**2+(VCOH-VCOA)**2.LT.
     +           (UCOH-UCOB)**2+(VCOH-VCOB)**2) THEN
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
  102   RLTE=RLTA
        RLNE=RLNA
        PAPE=PAPA
        UCOE=UCOA
        VCOE=VCOA
C
        RETURN
C
      END
