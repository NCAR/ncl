C
C $Id: dpdraw.f,v 1.13 2008-07-27 00:16:58 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DPDRAW (XCPF,YCPF,IFVL)
C
C DPDRAW is the workhorse routine of DASHPACK.  When IFVL is less than
C or equal to zero, a "first point" call is implied: (XCPF,YCPF) is a
C point in the fractional system at which a curve is to begin.  When
C IFVL is equal to one, a "vector" call is implied: (XCPF,YCPF) is a
C point in the fractional system to which the curve is to be extended.
C When IFVL is greater than or equal to two, a "last point" call is
C implied: the values of XCPF and YCPF are ignored, and all buffered
C information about the curve being drawn is flushed out.
C
C Actually, DPDRAW is more forgiving than that:  If a "first point"
C call is omitted, the first "vector" call will act instead like a
C "first point" call.  If a "last point" call is omitted, the next
C "first point" call will cause buffer-flushing before the new curve
C is started.  (Still, it is best to use a "first point" call, followed
C by a series of "vector" calls, followed by a "last point" call, to
C draw a curve.)
C
C Define the lengths of the arrays that hold the individual elements
C of the dash pattern.  (LDPA should be equal to the length of the
C character variable CHDP, in the common block DPCMCH.)
C
        PARAMETER (LDPA=256)
C
C Define the lengths of the arrays that are used to delay processing of
C contiguous character strings.  (While the gap for a character string
C is being passed over, curve points and character-string descriptors
C are saved; if it turns out that there is enough space to hold the
C character strings, they are drawn; otherwise, the saved curve points
C are used to draw the curve using nothing but gap segments and solid
C segments.  LPSB determines how many curve points can be saved and
C LSSB how many character-string elements can be saved.)
C
        PARAMETER (LPSB=256,LSSB=LDPA)
C
C Declare the character common block.
C
        COMMON /DPCMCH/ CHDP,CHRB,CHRG,CHRS
          CHARACTER*256 CHDP
          CHARACTER*1 CHRB,CHRG,CHRS
        SAVE   /DPCMCH/
C
C Declare the real/integer common block.
C
        COMMON /DPCMRI/ ANGF,DBPI,EPSI,IDPI,IDPS,ILTL,INDP,IPCF,ISBF,
     +                  ISCF,LCDP,RLS1,RLS2,RMFS,TENS,WCHR,WGAP,WSLD
        SAVE   /DPCMRI/
C
C Declare the arrays that are used to hold the descriptors of the
C constituent elements of the dash pattern.  For a value of I between
C 1 and NDPE, IDPE(I) will be -1 for a gap in which no characters will
C be written, 0 for a solid-line chunk, and greater than 0 for a gap
C in which characters will be written (in which case it contains either
C a +1, indicating the little gap that precedes or follows a label, or
C the packed indices of the character substring to be written); RDPE(I)
C is the length of line to be devoted to the element.
C
        DIMENSION IDPE(LDPA),RDPE(LDPA)
C
C Declare arrays that are used to delay processing of character strings
C (which is done to avoid outputting a partial string at the end of a
C curve).  The first two of these are used to save point-coordinate
C information and the remaining four are used to save character-string
C information.
C
        DIMENSION RSPX(LPSB),RSPY(LPSB)
C
        DIMENSION ISSP(LSSB),RSSA(LSSB),RSSX(LSSB),RSSY(LSSB)
C
C Declare a couple of character temporaries to use.
C
        CHARACTER*1 CHRL,CHRN
C
C Declare variables to receive the current window and viewport.
C
        DIMENSION WNDW(4),VPRT(4)
C
C Declare local variables to be saved from one call to the next.  The
C meanings of the mnemonics are as follows: "ICPE" stands for "Index of
C Current Pattern Element", "IDPE" for "Integer parts of Dash Pattern
C Elements", "IDST" for "Integer Drawing STatus",  "IFPF" for "Integer
C Fast-Path Flag", "ISSP" for "Integer Saved-String Pointers", "NBCS"
C for "Number of Break Characters Seen (in a particular dash pattern)",
C "NDPE" for "Number of Dash Pattern Elements", "NPSB" for "Number of
C Points in Save Buffer", "NSSB" for "Number of Strings in Save Buffer",
C "RDPE" for "Real parts of Dash Pattern Elements", "RSPX" for "Real
C Saved Point X's", "RSPY" for "Real Saved Point Y's", "RSSA" for "Real
C Saved String Angles", "RSSX" for "Real Saved String X's", "RSSY" for
C "Real Saved String Y's", "WCPE" for "Width of Current Pattern
C Element", "XCGF" for "X Coordinate at start of Gap, Fractional
C system", "XCLF" for "X Coordinate of Last point, Fractional system",
C "YCGF" for "Y Coordinate at start of Gap, Fractional system", and
C "YCLF" for "Y Coordinate of Last point, Fractional system".
C
        SAVE ICPE,IDPE,IDST,IFPF,ISSP,NBCS,NDPE,NPSB,NSSB,RDPE,
     +       RSPX,RSPY,RSSA,RSSX,RSSY,WCPE,XCGF,XCLF,YCGF,YCLF
C
C Initialize local variables that need it.  IDST is 0 when DPDRAW is
C expecting a first point, 1 when it's expecting the first continuation
C point, and 2 when it's expecting another continuation point or the
C last-point call.  NPSB and NSSB are the number of items in the
C point-save and string-save buffers, respectively.
C
        DATA IDST / 0  /
C
        DATA NPSB / 0  /
C
        DATA NSSB / 0  /
C
C Define a multiplicative constant to get from radians to degrees.
C
        DATA RTOD / 57.2957795130823 /
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL DPBLDA
C
C Check for an uncleared prior error.
C
        IF (ICFELL('DPDRAW - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If the current dash pattern has never been decomposed into its
C constituent elements (or if that has not been done since the last
C time some descriptor of the dash pattern was changed) and this call
C is not just a buffer-dump call, do that now.
C
        IF (IDPI.EQ.0.AND.IFVL.LT.2) THEN
C
C Initialize the number of dash-pattern elements found and the number
C of break characters seen.
C
          NDPE=0
          NBCS=0
C
C If IDPS is less than zero, use the integer dash pattern.
C
          IF (IDPS.LT.0) THEN
C
            NBTS=ABS(IDPS)
            LBIT=1-IAND(ISHIFT(INDP,1-NBTS),1)
C
            DO 101 I=1,NBTS
C
              IBIT=IAND(ISHIFT(INDP,I-NBTS),1)
C
              IF (IBIT.NE.LBIT) THEN
                IF (NDPE.GE.LDPA) GO TO 105
                NDPE=NDPE+1
                IDPE(NDPE)=IBIT-1
                RDPE(NDPE)=0.
              END IF
C
              IF (IBIT.EQ.0) THEN
                RDPE(NDPE)=RDPE(NDPE)+WGAP
              ELSE
                RDPE(NDPE)=RDPE(NDPE)+WSLD
              END IF
C
              LBIT=IBIT
C
  101       CONTINUE
C
C Otherwise, use the character dash pattern.
C
          ELSE
C
            NCHR=IDPS
            IF (NCHR.EQ.0) NCHR=LCDP
C
            CHRN=CHDP(1:1)
C
            CHRL=CHRG
            IF (CHRL.EQ.CHRN) CHRL=CHRS
C
C Initially, generate the descriptors of the dash-pattern elements
C assuming that PLCHMQ or PLCHLQ will be used.
C
            DO 102 I=1,NCHR
C
              IF (CHRN.EQ.CHRG) THEN
C
                IF (CHRL.NE.CHRG) THEN
                  IF (NDPE.GE.LDPA) GO TO 103
                  NDPE=NDPE+1
                  IDPE(NDPE)=-1
                  RDPE(NDPE)=0.
                END IF
C
                RDPE(NDPE)=RDPE(NDPE)+WGAP
C
              ELSE IF (CHRN.EQ.CHRS) THEN
C
                IF (CHRL.NE.CHRS) THEN
                  IF (NDPE.GE.LDPA) GO TO 103
                  NDPE=NDPE+1
                  IDPE(NDPE)=0
                  RDPE(NDPE)=0.
                END IF
C
                RDPE(NDPE)=RDPE(NDPE)+WSLD
C
              ELSE IF (CHRN.NE.CHRB) THEN
C
                IF (CHRL.EQ.CHRB.OR.CHRL.EQ.CHRG.OR.CHRL.EQ.CHRS.OR.
     +              ICHAR(CHRL).GT.127.OR.ICHAR(CHRN).GT.127) THEN
C
                  IF (CHRL.EQ.CHRG.OR.CHRL.EQ.CHRS.OR.
     +                ICHAR(CHRL).GT.127.OR.ICHAR(CHRN).GT.127) THEN
                    IF (RLS1.NE.0..OR.
     +                  ICHAR(CHRL).GT.127.OR.ICHAR(CHRN).GT.127) THEN
                      IF (NDPE.GE.LDPA) GO TO 103
                      NDPE=NDPE+1
                      IDPE(NDPE)=+1
                      RDPE(NDPE)=MAX(.01,RLS1)*WCHR
                    END IF
                  END IF
C
                  IF (NDPE.GE.LDPA) GO TO 103
                  NDPE=NDPE+1
                  IDPE(NDPE)=LDPA*I+I-1
                  RDPE(NDPE)=RLS2*WCHR
C
                END IF
C
                IDPE(NDPE)=IDPE(NDPE)+1
                RDPE(NDPE)=RDPE(NDPE)+WCHR
C
                IF (ISCF.NE.0.AND.ICHAR(CHRN).LT.128) CHRN=CHRB
C
              ELSE
C
                NBCS=NBCS+1
C
              END IF
C
              CHRL=CHRN
C
              IF (I.NE.NCHR) THEN
                CHRN=CHDP(I+1:I+1)
              ELSE
                CHRN=CHRG
              END IF
C
              IF ((CHRN.EQ.CHRG.OR.CHRN.EQ.CHRS).AND.NDPE.NE.0) THEN
C
                IF (IDPE(NDPE).GT.1.AND.RLS1.NE.0.) THEN
                  IF (NDPE.GE.LDPA) GO TO 103
                  NDPE=NDPE+1
                  IDPE(NDPE)=+1
                  RDPE(NDPE)=MAX(.01,RLS1)*WCHR
                END IF
C
              END IF
C
  102       CONTINUE
C
C If there were nothing but break characters in the dash pattern, use a
C solid line.
C
  103       IF (NDPE.EQ.0) THEN
              NDPE=1
              IDPE(1)=0
              RDPE(1)=1.
            END IF
C
C If PLCHHQ is to be used, post-process the list to compute correct gap
C lengths for character-string elements; gap lengths for symbols are not
C recomputed.
C
            IF (IPCF.EQ.0) THEN
C
              CALL PCGETI ('TE - TEXT EXTENT COMPUTATION FLAG',ITEF)
              IF (ICFELL('DPDRAW',2).NE.0) RETURN
C
              CALL PCSETI ('TE - TEXT EXTENT COMPUTATION FLAG',1)
              IF (ICFELL('DPDRAW',3).NE.0) RETURN
C
              DO 104 I=1,NDPE
C
                IF (IDPE(I).GT.1) THEN
C
                  IFCH=IDPE(I)/LDPA
                  ILCH=MOD(IDPE(I),LDPA)
C
                  IF (ICHAR(CHDP(IFCH:IFCH)).LE.127) THEN
C
                    CALL PLCHHQ (CFUX(.5),CFUY(.5),CHDP(IFCH:ILCH),
     +                                                WCHR,360.,0.)
                    IF (ICFELL('DPDRAW',4).NE.0) RETURN
C
                    CALL PCGETR ('XB - X COORDINATE AT BEGINNING',XBEG)
                    IF (ICFELL('DPDRAW',5).NE.0) RETURN
C
                    CALL PCGETR ('XE - X COORDINATE AT END',XEND)
                    IF (ICFELL('DPDRAW',6).NE.0) RETURN
C
                    RDPE(I)=RLS2*WCHR+MAX(0.,XEND-XBEG)
C
                  END IF
C
                END IF
C
  104         CONTINUE
C
              CALL PCSETI ('TE - TEXT EXTENT COMPUTATION FLAG',ITEF)
              IF (ICFELL('DPDRAW',7).NE.0) RETURN
C
            END IF
C
          END IF
C
C Set the fast-path flag to -1 if the pattern is nothing but gap, to
C +1 if the pattern is nothing but solid, and to 0 otherwise.  Also,
C in the final case, remove any "dummy" gap at the end of the list
C that duplicates one at the beginning of the list.
C
  105     IF (NDPE.EQ.1.AND.IDPE(1).LT.0) THEN
            IFPF=-1
          ELSE IF (NDPE.EQ.1.AND.IDPE(1).EQ.0) THEN
            IFPF=+1
          ELSE
            IFPF=0
            IF (IDPE(1).EQ.1.AND.IDPE(NDPE).EQ.1) NDPE=NDPE-1
          END IF
C
C Debug prints ...
C
C         IF (IDPS.GE.0) THEN
C           PRINT '(''IN DPDRAW - XCPF, YCPF, IFVL = '',2E12.4,I4)' ,
C    +            XCPF,YCPF,IFVL
C           PRINT '(I4''-ELEMENT LIST'',4X,I4,2X,A)' ,
C    +            NDPE,NCHR,CHDP(1:NCHR)
C           DO 999 I=1,NDPE
C             IF (IDPE(I).LT.0) THEN
C               PRINT '(I6,E12.4,''  GAP      '')' , I,RDPE(I)
C             ELSE IF (IDPE(I).EQ.0) THEN
C               PRINT '(I6,E12.4,''  SOLID    '')' , I,RDPE(I)
C             ELSE IF (IDPE(I).EQ.1) THEN
C               PRINT '(I6,E12.4,''  DUMMY GAP'')' , I,RDPE(I)
C             ELSE
C               IFCH=IDPE(I)/LDPA
C               ILCH=MOD(IDPE(I),LDPA)
C               LNTH=ILCH-IFCH+1
C               IF (LNTH.NE.1) THEN
C                 PRINT '(I6,E12.4,''  STRING   '',I4,2X,A)' ,
C    +                  I,RDPE(I),LNTH,CHDP(IFCH:ILCH)
C               ELSE IF (ICHAR(CHDP(IFCH:ILCH)).LT.128) THEN
C                 PRINT '(I6,E12.4,''  STRING   '',I4,2X,A)' ,
C    +                  I,RDPE(I),LNTH,CHDP(IFCH:ILCH)
C               ELSE
C                 PRINT '(I6,E12.4,''  SYMBOL   '',6X,A)' ,
C    +                  I,RDPE(I),CHAR(ICHAR(CHDP(IFCH:ILCH))-128)
C               END IF
C             END IF
C 999       CONTINUE
C         END IF
C
C Set the dash-pattern-initialized flag.
C
          IDPI=1
C
        END IF
C
C If the call does not define a continuation point or if DPDRAW is not
C in the proper state to handle such a point, ...
C
        IF (IFVL.NE.1.OR.IDST.EQ.0) THEN
C
C ... first, using only gap and solid elements of the dash pattern,
C draw any portion of a previous curve that was skipped as part of
C the gap for a character string that ended up never being drawn ...
C
          IF (NPSB.GT.1.AND.ILTL.EQ.0) THEN
C
            XCNF=RSPX(1)
            YCNF=RSPY(1)
C
            ISPE=ICPE
            WCPE=0.
C
            DO 108 I=2,NPSB
C
              XCLF=XCNF
              YCLF=YCNF
C
              XCNF=RSPX(I)
              YCNF=RSPY(I)
C
              DIST=SQRT((XCNF-XCLF)**2+(YCNF-YCLF)**2)
C
  106         IF (ICPE.EQ.ISPE.OR.IDPE(ICPE).EQ.0) THEN
                CALL PLOTIF (XCLF,YCLF,0)
                IF (ICFELL('DPDRAW',8).NE.0) RETURN
              END IF
C
              IF (DIST.LE.WCPE) THEN
C
                WCPE=WCPE-DIST
C
                IF (ICPE.EQ.ISPE.OR.IDPE(ICPE).EQ.0) THEN
                  CALL PLOTIF (XCNF,YCNF,1)
                  IF (ICFELL('DPDRAW',9).NE.0) RETURN
                END IF
C
              ELSE
C
                XCLF=XCLF+(WCPE/DIST)*(XCNF-XCLF)
                YCLF=YCLF+(WCPE/DIST)*(YCNF-YCLF)
C
                DIST=DIST-WCPE
C
                IF (ICPE.EQ.ISPE.OR.IDPE(ICPE).EQ.0) THEN
                  CALL PLOTIF (XCLF,YCLF,1)
                  IF (ICFELL('DPDRAW',10).NE.0) RETURN
                END IF
C
                ISPE=ICPE
C
  107           ICPE=MOD(ICPE,NDPE)+1
                IF (ICPE.NE.ISPE.AND.IDPE(ICPE).GT.0) GO TO 107
C
                IF (IDPE(ICPE).LE.0.AND.RDPE(ICPE).GT.0.) THEN
                  ISPE=0
                  WCPE=RDPE(ICPE)
                ELSE
                  ISPE=ICPE
                  WCPE=1.
                END IF
C
                GO TO 106
C
              END IF

  108       CONTINUE
C
          END IF
C
          NPSB=0
          NSSB=0
C
C ... and then, either flush the PLOTIF buffer ...
C
          IF (IFVL.GE.2) THEN
C
            CALL PLOTIF (0.,0.,2)
            IF (ICFELL('DPDRAW',11).NE.0) RETURN
C
            IDST=0
C
C ... or do first-point initialization, whichever is appropriate.
C
          ELSE
C
            XCLF=XCPF
            YCLF=YCPF
C
            ICPE=1
            WCPE=RDPE(1)
C
            IF (IDPE(ICPE).EQ.0) WCPE=RMFS*WCPE
C
            IF (IDPE(ICPE).GT.1) THEN
              XCGF=XCPF
              YCGF=YCPF
            END IF
C
            IF (IDPE(ICPE).GT.0.AND.ISBF.NE.0.AND.NPSB.EQ.0) THEN
              NPSB=1
              RSPX(1)=XCLF
              RSPY(1)=YCLF
            END IF
C
            IDST=1
C
          END IF
C
C Otherwise, the call defines a continuation point and DPDRAW is in the
C correct state to handle such a point, so process it.
C
        ELSE
C
          IF (IFPF.GT.0) THEN
C
            CALL PLOTIF (XCLF,YCLF,0)
            IF (ICFELL('DPDRAW',12).NE.0) RETURN
C
            CALL PLOTIF (XCPF,YCPF,1)
            IF (ICFELL('DPDRAW',13).NE.0) RETURN
C
          ELSE IF (IFPF.EQ.0) THEN
C
            DIST=SQRT((XCPF-XCLF)**2+(YCPF-YCLF)**2)
C
  109       IF (IDPE(ICPE).EQ.0.OR.(ILTL.NE.0.AND.IDPE(ICPE).GT.0)) THEN
              CALL PLOTIF (XCLF,YCLF,0)
              IF (ICFELL('DPDRAW',14).NE.0) RETURN
            END IF
C
            IF (DIST.LE.WCPE) THEN
C
              WCPE=WCPE-DIST
C
              IF (IDPE(ICPE).EQ.0.OR.
     +            (ILTL.NE.0.AND.IDPE(ICPE).GT.0)) THEN
C
                CALL PLOTIF (XCPF,YCPF,1)
                IF (ICFELL('DPDRAW',15).NE.0) RETURN
C
              END IF
C
              IF (IDPE(ICPE).GT.0.AND.ISBF.NE.0) THEN
C
                IF (NPSB.EQ.LPSB) THEN
C
                  IMIN=2
                  DMIN=SQRT((RSPX(3)-RSPX(2))**2+
     +                      (RSPY(3)-RSPY(2))**2)
C
                  DO 110 I=2,NPSB-2
                    DTMP=SQRT((RSPX(I+1)-RSPX(I))**2+
     +                        (RSPY(I+1)-RSPY(I))**2)
                    IF (DTMP.LT.DMIN) THEN
                      IMIN=I
                      DMIN=DTMP
                    END IF
  110             CONTINUE
C
                  RSPX(IMIN)=.5*(RSPX(IMIN)+RSPX(IMIN+1))
                  RSPY(IMIN)=.5*(RSPY(IMIN)+RSPY(IMIN+1))
C
                  DO 111 I=IMIN+1,NPSB-1
                    RSPX(I)=RSPX(I+1)
                    RSPY(I)=RSPY(I+1)
  111             CONTINUE
C
                  NPSB=NPSB-1
C
                END IF
C
                NPSB=NPSB+1
                RSPX(NPSB)=XCPF
                RSPY(NPSB)=YCPF
C
              END IF
C
            ELSE
C
              XCLF=XCLF+(WCPE/DIST)*(XCPF-XCLF)
              YCLF=YCLF+(WCPE/DIST)*(YCPF-YCLF)
C
              DIST=DIST-WCPE
C
              IF (IDPE(ICPE).EQ.0.OR.
     +            (ILTL.NE.0.AND.IDPE(ICPE).GT.0)) THEN
C
                CALL PLOTIF (XCLF,YCLF,1)
                IF (ICFELL('DPDRAW',16).NE.0) RETURN
C
              END IF
C
              IF (IDPE(ICPE).GT.1) THEN
C
                IF (NSSB.GE.LSSB) THEN
                  CALL SETER ('DPDRAW - IMPLEMENTATION ERROR - SEE SPECI
     +ALIST',17,1)
                  RETURN
                END IF
C
                NSSB=NSSB+1
C
                RSSX(NSSB)=.5*(XCGF+XCLF)
                RSSY(NSSB)=.5*(YCGF+YCLF)
C
                IF (XCLF.EQ.XCGF.AND.YCLF.EQ.YCGF) THEN
                  RSSA(NSSB)=0.
                ELSE
                  RSSA(NSSB)=RTOD*ATAN2(YCLF-YCGF,XCLF-XCGF)
                END IF
C
                ISSP(NSSB)=ICPE
C
              END IF
C
              ICPE=MOD(ICPE,NDPE)+1
              WCPE=RDPE(ICPE)
C
              IF (IDPE(ICPE).GT.1) THEN
                XCGF=XCLF
                YCGF=YCLF
              END IF
C
              IF (NSSB.NE.0.AND.(ISBF.EQ.0.OR.ICPE.EQ.1
     +                                    .OR.IDPE(ICPE).LE.1)) THEN
C
                IF (NSSB.GT.1.AND.NPSB.GT.0.AND.ANGF.GT.0.) THEN
C
                  NUDE=0
C
                  DO 112 I=1,NSSB
                    ANGD=MOD(RSSA(I)-ANGF+3780.,360.)-180.
                    IF (ANGD.LT.-90..OR.ANGD.GT.90.) NUDE=NUDE+1
  112             CONTINUE
C
                  IF (NUDE.GT.NSSB/2) THEN
C
                    NTMP=NPSB
C
                    XC1F=XCLF
                    YC1F=YCLF
C
                    XC2F=XCLF
                    YC2F=YCLF
C
                    XC3F=RSPX(NTMP)
                    YC3F=RSPY(NTMP)
C
                    DTMP=SQRT((XC3F-XC2F)**2+(YC3F-YC2F)**2)
C
                    DO 114 I=1,NSSB
C
                      WTMP=RDPE(ISSP(I))
C
  113                 IF (WTMP.GE.DTMP) THEN
C
                        WTMP=WTMP-DTMP
C
                        XC2F=XC3F
                        YC2F=YC3F
C
                        IF (NTMP.GT.1) THEN
                          NTMP=NTMP-1
                          XC3F=RSPX(NTMP)
                          YC3F=RSPY(NTMP)
                        ELSE
                          NTMP=MIN(-2,NTMP-1)
                          IF (-NTMP.LE.NPSB) THEN
                            XC3F=RSPX(1)-(RSPX(-NTMP)-RSPX(1))*1000.
                            YC3F=RSPY(1)-(RSPY(-NTMP)-RSPY(1))*1000.
                          ELSE
                            XC3F=RSPX(1)-(XCLF-RSPX(1))*1000.
                            YC3F=RSPY(1)-(YCLF-RSPY(1))*1000.
                          END IF
                        END IF
C
                        DTMP=SQRT((XC3F-XC2F)**2+(YC3F-YC2F)**2)
C
                        GO TO 113
C
                      END IF
C
                      XC2F=XC2F+(WTMP/DTMP)*(XC3F-XC2F)
                      YC2F=YC2F+(WTMP/DTMP)*(YC3F-YC2F)
C
                      DTMP=DTMP-WTMP
C
                      RSSX(I)=.5*(XC1F+XC2F)
                      RSSY(I)=.5*(YC1F+YC2F)
C
                      IF (XC1F.EQ.XC2F.AND.YC1F.EQ.YC2F) THEN
                        RSSA(I)=0.
                      ELSE
                        RSSA(I)=RTOD*ATAN2(YC2F-YC1F,XC2F-XC1F)
                      END IF
C
                      XC1F=XC2F
                      YC1F=YC2F
C
  114               CONTINUE
C
                  END IF
C
                END IF
C
                CALL PLOTIF (0.,0.,2)
                IF (ICFELL('DPDRAW',18).NE.0) RETURN
C
                DO 115 I=1,NSSB
C
                  XTMP=CFUX(RSSX(I))
                  IF (ICFELL('DPDRAW',19).NE.0) RETURN
C
                  YTMP=CFUY(RSSY(I))
                  IF (ICFELL('DPDRAW',20).NE.0) RETURN
C
                  IFCH=IDPE(ISSP(I))/LDPA
                  ILCH=MOD(IDPE(ISSP(I)),LDPA)
C
                  ANGD=RSSA(I)
C
                  IF (ANGF.NE.0..AND.ISCF.EQ.0.AND.NBCS.EQ.0) THEN
C
                    IF (ANGF.GT.0.) THEN
C
                      ANGD=MOD(ANGD-ANGF+3780.,360.)-180.
C
                      IF (ANGD.LT.-90.) THEN
                        ANGD=ANGD+180.
                      ELSE IF (ANGD.GT.+90.) THEN
                        ANGD=ANGD-180.
                      END IF
C
                      ANGD=MOD(ANGF+ANGD,360.)
C
                    ELSE IF (ILTL.NE.0) THEN
C
                      ANGD=MOD(ABS(ANGF),360.)
C
                    END IF
C
                  END IF
C
                  IF (ICHAR(CHDP(IFCH:IFCH)).GT.127) THEN
C
                    CALL GQCNTN (IERR,NTIU)
C
                    IF (IERR.NE.0) THEN
                      CALL SETER('DPDRAW - ERROR EXIT FROM GQCNTN',21,1)
                      RETURN
                    END IF
C
                    CALL GQNT (NTIU,IERR,WNDW,VPRT)
C
                    IF (IERR.NE.0) THEN
                      CALL SETER ('DPDRAW - ERROR EXIT FROM GQNT',22,1)
                      RETURN
                    END IF
C
                    CALL DPDSYM (WNDW(1)+(RSSX(I)-VPRT(1))/
     +                                   (VPRT(2)-VPRT(1))*
     +                                   (WNDW(2)-WNDW(1)),
     +                           WNDW(3)+(RSSY(I)-VPRT(3))/
     +                                   (VPRT(4)-VPRT(3))*
     +                                   (WNDW(4)-WNDW(3)),
     +                           CHAR(ICHAR(CHDP(IFCH:IFCH))-128),
     +                           WCHR*(WNDW(2)-WNDW(1))/
     +                                (VPRT(2)-VPRT(1)),
     +                           ANGD)
C
                  ELSE IF (IPCF.EQ.0) THEN
C
                    CALL PLCHHQ (XTMP,YTMP,CHDP(IFCH:ILCH),WCHR,ANGD,0.)
                    IF (ICFELL('DPDRAW',23).NE.0) RETURN
C
                  ELSE IF (IPCF.EQ.1) THEN
C
                    CALL PLCHMQ (XTMP,YTMP,CHDP(IFCH:ILCH),WCHR,ANGD,0.)
                    IF (ICFELL('DPDRAW',24).NE.0) RETURN
C
                  ELSE
C
                    CALL PLCHLQ (XTMP,YTMP,CHDP(IFCH:ILCH),WCHR,ANGD,0.)
                    IF (ICFELL('DPDRAW',25).NE.0) RETURN
C
                  END IF
C
  115           CONTINUE
C
                NPSB=0
                NSSB=0
C
              END IF
C
              IF (IDPE(ICPE).GT.0.AND.ISBF.NE.0) THEN
                IF (NPSB.EQ.0) THEN
                  NPSB=1
                  RSPX(1)=XCLF
                  RSPY(1)=YCLF
                END IF
              ELSE
                NPSB=0
              END IF
C
              GO TO 109
C
            END IF
C
          END IF
C
          XCLF=XCPF
          YCLF=YCPF
C
          IDST=2
C
        END IF
C
C Done.
C
        RETURN
C
      END
