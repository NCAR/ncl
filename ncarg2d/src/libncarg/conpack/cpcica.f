C
C $Id: cpcica.f,v 1.6 1995-06-02 00:24:01 kennison Exp $
C
      SUBROUTINE CPCICA (ZDAT,RWRK,IWRK,ICRA,ICA1,ICAM,ICAN,XCPF,YCPF,
     +                                                      XCQF,YCQF)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*),ICRA(ICA1,*)
C
C This routine adds color indices to a user's cell array.
C
C ZDAT is the user's data array.
C
C RWRK is the user's real workspace array.
C
C IWRK is the user's integer workspace array.
C
C ICRA is the user array in which the cell array is stored.
C
C ICA1 is the first dimension of the FORTRAN array ICRA.
C
C ICAM is the first dimension of the cell array.
C
C ICAN is the second dimension of the cell array.
C
C (XCPF,YCPF) is the point at that corner of the rectangular area
C into which the cell array maps that corresponds to the cell (1,1).
C The coordinates are given in the fractional coordinate system (unlike
C what is required in a call to GCA, in which the coordinates of the
C point P are in the world coordinate system).
C
C (XCQF,YCQF) is the point at that corner of the rectangular area into
C which the cell array maps that corresponds to the cell (ICAM,ICAN).
C The coordinates are given in the fractional coordinate system (unlike
C what is required in a call to GCA, in which the coordinates of the
C point Q are in the world coordinate system).
C
C Declare all of the CONPACK common blocks.
C
C
C CPCOM1 contains integer and real variables.
C
      COMMON /CPCOM1/ ANCF,ANHL,ANIL,ANLL,CDMX,CHWM,CINS,CINT(10)
      COMMON /CPCOM1/ CINU,CLDB(256),CLDL(256),CLDR(256)
      COMMON /CPCOM1/ CLDT(256),CLEV(256),CLWA(259),CXCF
      COMMON /CPCOM1/ CXIL,CYCF,CYIL,DBLF,DBLM,DBLN,DBLV,DFLD,DOPT
      COMMON /CPCOM1/ EPSI,FNCM,GRAV,GRSD,GSDM,HCHL,HCHS,IAIA(259)
      COMMON /CPCOM1/ IAIB(256),IBCF,IBHL,IBIL,IBLL,ICAF,ICCF
      COMMON /CPCOM1/ ICCL(259),ICFF,ICHI,ICHL,ICIL,ICLL(256)
      COMMON /CPCOM1/ ICLO,ICLP(256),ICLS,ICLU(259),ICLV,ICLW
      COMMON /CPCOM1/ IDUF,IGCL,IGLB,IGRM,IGRN,IGVS,IHCF,IHLX,IHLY
      COMMON /CPCOM1/ IIWS(2),IIWU,ILBC,IMPF,INCX(8),INCY(8)
      COMMON /CPCOM1/ INHL,INIL,INIT,INLL,IOCF,IOHL,IOLL,IPAI,IPCF
      COMMON /CPCOM1/ IPIC,IPIE,IPIL,IPLL,IRWS(4),IRWU,ISET,IWSO
      COMMON /CPCOM1/ IZD1,IZDM,IZDN,IZDS,JODP,JOMA,JOTZ,LCTM,LEA1
      COMMON /CPCOM1/ LEA2,LEA3,LEE1,LEE2,LEE3,LINS,LINT(10),LINU
      COMMON /CPCOM1/ LIWK,LIWM,LIWS(2),LNLG,LRWC,LRWG,LRWK
      COMMON /CPCOM1/ LRWM,LRWS(4),LSDD,LSDL,LSDM,LTCF,LTHI
      COMMON /CPCOM1/ LTIL,LTLO,MIRO,NCLB(256),NCLV,NDGL,NEXL
      COMMON /CPCOM1/ NEXT,NEXU,NLBS,NLSD,NLZF,NOMF,NOVS,NR04,NSDL
      COMMON /CPCOM1/ NSDR,OORV,PITH,SCFS,SCFU,SEGL,SVAL,T2DS,T3DS
      COMMON /CPCOM1/ UCMN,UCMX,UVPB,UVPL,UVPR,UVPS,UVPT,UWDB,UWDL
      COMMON /CPCOM1/ UWDR,UWDT,UXA1,UXAM,UYA1,UYAN,WCCF,WCHL,WCIL
      COMMON /CPCOM1/ WCLL,WLCF,WLHL,WLIL,WLLL,WOCH,WODA,WTCD,WTGR
      COMMON /CPCOM1/ WTNC,WTOD,WWCF,WWHL,WWIL,WWLL,XAT1,XATM,XLBC
      COMMON /CPCOM1/ XVPL,XVPR,XWDL,XWDR,YAT1,YATN,YLBC,YVPB,YVPT
      COMMON /CPCOM1/ YWDB,YWDT,ZDVL,ZMAX,ZMIN
      EQUIVALENCE (IIWS(1),II01),(LIWS(1),LI01)
      EQUIVALENCE (IIWS(2),II02),(LIWS(2),LI02)
      EQUIVALENCE (IRWS(1),IR01),(LRWS(1),LR01)
      EQUIVALENCE (IRWS(2),IR02),(LRWS(2),LR02)
      EQUIVALENCE (IRWS(3),IR03),(LRWS(3),LR03)
      EQUIVALENCE (IRWS(4),IR04),(LRWS(4),LR04)
      SAVE   /CPCOM1/
C
C CPCOM2 holds character parameters.
C
      COMMON /CPCOM2/ CHEX,CLBL(256),CLDP(259),CTMA,CTMB,FRMT
      COMMON /CPCOM2/ TXCF,TXHI,TXIL,TXLO
      CHARACTER*13 CHEX
      CHARACTER*64 CLBL
      CHARACTER*128 CLDP
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*64 TXCF
      CHARACTER*32 TXHI
      CHARACTER*128 TXIL
      CHARACTER*32 TXLO
      SAVE   /CPCOM2/
C
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CPCICA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CPCICA - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C If the mapping flag is turned on and CPMPXY is not capable of doing
C the inverse transformations, log an error and quit.
C
      IF (IMPF.NE.0) THEN
C
        TST1=REAL(IMPF)
        TST2=0.
C
        CALL HLUCPMPXY (0,TST1,TST2,TST3,TST4)
        IF (ICFELL('CPCICA',3).NE.0) RETURN
C
        IF (TST2.NE.2..AND.TST2.NE.3.) THEN
          CALL SETER ('CPCICA - CANNOT CONTINUE - CPMPXY DOES NOT DO INV
     +ERSE MAPPINGS',4,1)
          RETURN
        END IF
      END IF
C
C Check for errors in the arguments.
C
      IF (ICAM.LE.0.OR.ICAN.LE.0.OR.ICAM.GT.ICA1) THEN
        CALL SETER ('CPCICA - THE DIMENSIONS OF THE CELL ARRAY ARE INCOR
     +RECT',5,1)
        RETURN
      END IF
C
      IF (XCPF.LT.0..OR.XCPF.GT.1..OR.YCPF.LT.0..OR.YCPF.GT.1..OR.XCQF.L
     +T.0..OR.XCQF.GT.1..OR.YCQF.LT.0..OR.YCQF.GT.1.) THEN
        CALL SETER ('CPCICA - ONE OF THE CORNER POINTS OF THE CELL ARRAY
     + IS INCORRECT',6,1)
        RETURN
      END IF
C
C Do the proper SET call.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('CPCICA',7).NE.0) RETURN
C
C If no contour levels are defined, try to pick a set of levels.
C
      IF (NCLV.LE.0) THEN
        CALL CPPKCL (ZDAT,RWRK,IWRK)
        IF (ICFELL('CPCICA',8).NE.0) RETURN
      END IF
C
C If no levels are defined now, do nothing.
C
      IF (NCLV.LE.0) RETURN
C
C Get indices for the contour levels in ascending order.
C
      CALL CPSORT (CLEV,NCLV,ICLP)
C
C Loop through each cell in the cell array.  Find the center point of
C each cell in the fractional system (coordinates XCFS and YCFS) and
C then in the user system (coordinates XCUS and YCUS).
C
      DO 10001 I=1,ICAM
C
        XCCF=XCPF+(REAL(I)-.5)*((XCQF-XCPF)/REAL(ICAM))
        XCCU=CFUX(XCCF)
        IF (ICFELL('CPCICA',9).NE.0) RETURN
C
        DO 10002 J=1,ICAN
C
          YCCF=YCPF+(REAL(J)-.5)*((YCQF-YCPF)/REAL(ICAN))
          YCCU=CFUY(YCCF)
          IF (ICFELL('CPCICA',10).NE.0) RETURN
C
C Find the center point of each cell in the data index system.  The flag
C IOOR is set non-zero if, in the process, the point is found to be
C invisible under the current mapping or outside the coordinate ranges
C associated with the data grid; at the same time, the area identifier
C for the cell is set to the value specified for areas outside the grid.
C
          IOOR=0
C
          IF (IMPF.EQ.0) THEN
            XCCI=1.+((XCCU-XAT1)/(XATM-XAT1))*REAL(IZDM-1)
            YCCI=1.+((YCCU-YAT1)/(YATN-YAT1))*REAL(IZDN-1)
          ELSE
            CALL HLUCPMPXY (-IMPF,XCCU,YCCU,XCCD,YCCD)
            IF (ICFELL('CPCICA',11).NE.0) RETURN
            IF ((OORV.EQ.0..OR.XCCD.NE.OORV).AND.XCCD.GE.XAT1.AND.XCCD.L
     +T.XATM.AND.YCCD.GE.YAT1.AND.YCCD.LT.YATN) THEN
              XCCI=1.+((XCCD-XAT1)/(XATM-XAT1))*REAL(IZDM-1)
              YCCI=1.+((YCCD-YAT1)/(YATN-YAT1))*REAL(IZDN-1)
            ELSE
              IOOR=1
              IAID=IAIA(257)
            END IF
          END IF
C
C If the cell is positioned over a point that corresponds to a point in
C the data array, compute the appropriate indices into the data array.
C
          IF (IOOR.EQ.0) THEN
C
            INDX=INT(XCCI)
            INDY=INT(YCCI)
C
C If the indices are out of range, use the area identifier specified for
C areas outside the grid.
C
            IF (INDX.LT.1.OR.INDX.GE.IZDM.OR.INDY.LT.1.OR.INDY.GE.IZDN)
     +THEN
C
              IAID=IAIA(257)
C
C Otherwise, if the special-value feature is turned on and any of the
C corner points is a special value, use the area identifier specified
C for special-value areas.
C
            ELSE IF (SVAL.NE.0..AND.(ZDAT(INDX,INDY).EQ.SVAL.OR.ZDAT(IND
     +X,INDY+1).EQ.SVAL.OR.ZDAT(INDX+1,INDY).EQ.SVAL.OR.ZDAT(INDX+1,INDY
     ++1).EQ.SVAL)) THEN
C
              IAID=IAIA(258)
C
C Otherwise, interpolate to find a data value.
C
            ELSE
C
              ZVAL= (REAL(INDY+1)-YCCI)*
     +             ((REAL(INDX+1)-XCCI)*ZDAT(INDX  ,INDY  )+
     +              (XCCI-REAL(INDX  ))*ZDAT(INDX+1,INDY  ))+
     +              (YCCI-REAL(INDY  ))*
     +             ((REAL(INDX+1)-XCCI)*ZDAT(INDX  ,INDY+1)+
     +              (XCCI-REAL(INDX  ))*ZDAT(INDX+1,INDY+1))
C
C Given the data value, find an area identifier associated with it.
C
              CALL CPGVAI (ZVAL,IAID)
C
            END IF
C
          END IF
C
C Modify the current cell array element as directed by the value of
C the internal parameter 'CAF'.
C
          IF (ICAF.GE.0) THEN
            IF (ICAF+IAID.GT.0) ICRA(I,J)=ICAF+IAID
          ELSE
            CALL HLUCPSCAE (ICRA,ICA1,ICAM,ICAN,XCPF,YCPF,XCQF,YCQF,
     +                                                I,J,ICAF,IAID)
            IF (ICFELL('CPCICA',12).NE.0) RETURN
          END IF
C
10002   CONTINUE
C
10001 CONTINUE
C
C Done.
C
      RETURN
C
      END
