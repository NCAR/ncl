C
C $Id: pccffc.f,v 1.13 2008-07-27 00:17:19 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCCFFC (IPSS,IBNU,NFNT,IASC,CHGT,RDGU,LDGU,NDGU)
C
      DIMENSION RDGU(LDGU)
C
C The subroutine PCCFFC retrieves the digitization of a specified
C character from one of the locally-defined NCAR Graphics fontcaps.
C
C IPSS is the number of the pass being executed by PLCHHQ.
C
C IBNU is the number of a unit to be used in reading the fontcaps.
C
C NFNT is the number of the desired font.
C
C IASC is the ASCII index of the desired character.
C
C CHGT is the desired height of the character, in digitization units.
C
C RDGU is a real array in which the digitization is to be returned.
C
C LDGU is the length of RDGU.
C
C NDGU is the number of elements returned in RDGU.
C
C The digitization is to be in the form expected by PLCHHQ, in a
C coordinate system centered at the center of the character.  RDGU(1)
C must be a negative number whose magnitude is the distance from the
C center of the character to its left edge.  RDGU(2) must be a positive
C number whose magnitude is the distance from the center of the
C character to its right edge.  (RDGU(3),RDGU(4)) is the first point,
C (RDGU(5),RDGU(6)) the second point, etc.  (-2048.,-2048.) is used
C to represent a position in the coordinate string at which the "pen"
C is to be picked up and moved.
C
C Define the quantities required to read and work with the fontcaps.
C These definitions are essentially copied from the translator.
C
      COMMON /CAPFNT/ CHRSTR,CHREND,CHRWDT,CHRSCH,FNTRHT,FNTTOP,FNTCAP,
     +                FNTHLF,FNTBAS,FNTBOT,FNTTYP,FNTSCH,CORXST,CORXLN,
     +                CORYST,CORYLN,CORPST,CORPLN,PBEGST,PBEGLN,PENDST,
     +                PENDLN,CORSCH,NEWCLS,CPNTRS,CPNLST,CSTRKS,
     +                XLFT,XRGT,XCNT
      SAVE  /CAPFNT/
C
      INTEGER         CHRSTR,CHREND,CHRWDT,CHRSCH(11),FNTRHT,FNTTOP,
     +                FNTCAP,FNTHLF,FNTBAS,FNTBOT,FNTTYP,FNTSCH(9),
     +                CORXST,CORXLN,CORYST,CORYLN,CORPST,CORPLN,PBEGST,
     +                PBEGLN,PENDST,PENDLN,CORSCH(10),NEWCLS(300),
     +                CPNTRS(128),CPNLST,CSTRKS(5121)
C
      DIMENSION IFCP(5600)
      EQUIVALENCE (CHRSTR,IFCP(1))
C
      DIMENSION       XLFT(128),XRGT(128),XCNT(128)
C
C The variable NFRL, if non-zero, defines the number of the font read
C last.  Some other quantities need to be saved from call to call, as
C well.
C
      SAVE NFRL,LSHX,LSHY,LSHP,MSKX,MSKY,MSKP,FDGE
C
C NFRL starts off zeroed.
C
      DATA NFRL / 0 /
C
C Zero the count of digitization elements returned.
C
      NDGU=0
C
C If the desired font is not the one read last, read it.  If there is
C any problem with reading it, just return with NDGU zeroed to indicate
C that the desired character was not available.
C
      IF (NFRL.NE.NFNT) THEN
C
C Open the fontcap file.
C
        IBNS=IBNU
        CALL PCFOPN (IBNU,NFNT)
        IF (ICFELL('PCCFFC',1).NE.0) RETURN
C
C Read the fontcap.
C
        CALL PCFRED (IBNU,NFNT,IFCP,5600)
        IF (ICFELL('PCCFFC',2).NE.0) RETURN
C
C Close the fontcap file.
C
        CALL PCFCLS (IBNU,NFNT)
        IF (ICFELL('PCCFFC',3).NE.0) RETURN
        IBNU=IBNS
C
C Compute quantities needed to unpack X and Y coordinates and pen
C up/down indicators.
C
        LSHX=CORXLN-CORXST-1
        LSHY=CORYLN-CORYST-1
        LSHP=CORPLN-CORPST-1
        MSKX=2**CORXLN-1
        MSKY=2**CORYLN-1
        MSKP=2**CORPLN-1
C
C Calculate the character left (in font units) and the character right
C (in font units) for each character in the font.
C
        DO 101 I=1,128
          XLFT(I)=0.
          XRGT(I)=0.
          XCNT(I)=0.
  101   CONTINUE
C
        DO 102 I=CHRSTR,CHREND
          INDX=I-CHRSTR+1
          IPNT=CPNTRS(INDX)
          IF (IPNT.GT.0) THEN
            IF (FNTTYP.EQ.1.OR.FNTTYP.EQ.3) THEN
              XLFT(INDX)=REAL(IAND(ISHIFT(CSTRKS(IPNT),LSHX),MSKX))
              XRGT(INDX)=REAL(IAND(ISHIFT(CSTRKS(IPNT),LSHY),MSKY))
            ELSE IF (FNTTYP.EQ.0.OR.FNTTYP.EQ.2) THEN
              XLFT(INDX)=0.
              XRGT(INDX)=REAL(CHRWDT)
            END IF
            XCNT(INDX)=999999.
          END IF
  102   CONTINUE
C
C Set a fudge factor which will make fonts 1-20 work better with the
C PWRITX characters.
C
        IF (NFNT.GE.1.AND.NFNT.LE.20) THEN
          FDGE=.5
        ELSE
          FDGE=0.
        END IF
C
C Set the variable NFRL to reflect the number of the font last read.
C
        NFRL=NFNT
C
      END IF
C
C Now that the proper font has been read in, return the digitization of
C the desired character.
C
      IF (IASC.GE.CHRSTR.AND.IASC.LE.CHREND) THEN
C
        INDX=IASC-CHRSTR+1
C
C Calculate the start pointer for the character digitization (add 1
C to skip over character left and character right information for
C proportionally spaced fonts).
C
        IF (FNTTYP.EQ.0.OR.FNTTYP.EQ.2) THEN
          ICST=CPNTRS(INDX)
        ELSE IF (FNTTYP.EQ.1.OR.FNTTYP.EQ.3) THEN
          ICST=CPNTRS(INDX)+1
        END IF
C
        IF (ICST.GT.0) THEN
C
C Calculate the end pointer for the digitization.
C
          IF (INDX.EQ.CHREND-CHRSTR+1) THEN
            ICND=CPNLST
          ELSE
            ICND=CPNTRS(INDX+1)-1
          END IF
C
          IF (ICND.GT.ICST) THEN
C
C If it hasn't already been done for this character, find the range of
C X coordinates used in the digitization and use the midpoint of that
C range as the horizontal center of the character.
C
            IF (XCNT(INDX).EQ.999999.) THEN
              IXMN=IAND(ISHIFT(CSTRKS(ICST),LSHX),MSKX)
              IXMX=IXMN
              DO 103 I=ICST+1,ICND
                IXCD=IAND(ISHIFT(CSTRKS(I),LSHX),MSKX)
                IXMN=MIN(IXMN,IXCD)
                IXMX=MAX(IXMX,IXCD)
  103         CONTINUE
              XCNT(INDX)=.5*REAL(IXMN+IXMX)
            END IF
C
C Compute multipliers required to give the character the proper size
C and shape.
C
            HMUL=CHGT/REAL(FNTCAP-FNTBAS)
            WMUL=HMUL
C
C Compute the required offsets to the left and right edges.
C
            RDGU(1)=WMUL*(XLFT(INDX)-XCNT(INDX))
            RDGU(2)=WMUL*(XRGT(INDX)-XCNT(INDX))
            NDGU=2
C
C During pass 1, that's all that's required.
C
            IF (IPSS.EQ.1) RETURN
C
C Extract the digitization and put it in the expected form.
C
            DO 104 I=ICST,ICND
              IXCD=IAND(ISHIFT(CSTRKS(I),LSHX),MSKX)
              IYCD=IAND(ISHIFT(CSTRKS(I),LSHY),MSKY)
              IPEN=IAND(ISHIFT(CSTRKS(I),LSHP),MSKP)
              IF (IPEN.EQ.0.AND.NDGU.NE.2) THEN
                IF (NDGU.GE.LDGU) RETURN
                NDGU=NDGU+1
                RDGU(NDGU)=-2048.
                NDGU=NDGU+1
                RDGU(NDGU)=0.
              END IF
              IF (NDGU.GE.LDGU) RETURN
              NDGU=NDGU+1
              RDGU(NDGU)=WMUL*(REAL(IXCD)-XCNT(INDX))
              NDGU=NDGU+1
              RDGU(NDGU)=HMUL*(REAL(IYCD-FNTHLF)-FDGE)
  104       CONTINUE
C
            IF (NDGU.GE.LDGU) RETURN
            NDGU=NDGU+1
            RDGU(NDGU)=-2048.
            NDGU=NDGU+1
            RDGU(NDGU)=0.
C
          END IF
C
        END IF
C
      END IF
C
C Done.
C
      RETURN
C
      END
