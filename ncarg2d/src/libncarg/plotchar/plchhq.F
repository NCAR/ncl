C
C	$Id: plchhq.F,v 1.6 1992-10-20 22:53:39 haley Exp $
C
C***********************************************************************
C P A C K A G E   P L O T C H A R   -   I N T R O D U C T I O N
C***********************************************************************
C
C This file contains implementation instructions and the code for the
C package PLOTCHAR.  Banners like the one above delimit major sections
C of the file.  The code itself is separated into four sections: the
C character-writing routines, the parameter-access routines, the
C internal routines, and the block data routines which determine the
C default values of internal parameters.
C
C***********************************************************************
C P A C K A G E   P L O T C H A R   -   I M P L E M E N T A T I O N
C***********************************************************************
C
C The PLOTCHAR package is written in FORTRAN-77 and should be quite
C easy to implement.  To implement the full package requires four
C steps:
C
C 1.  Supply 3 machine-dependent functions for bit manipulations -
C     ISHIFT(I1,I2), IOR(I1,I2), and IAND(I1,I2) - and one function
C     to provide machine-dependent quantities - I1MACH(I).  These
C     functions are the same ones used to implement the NCAR GKS plot
C     package.
C
C 2.  Create the PWRITX binary database from the card-image files
C     PWRITXC1, PWRITXC2, PWRITXD1, and PWRITXD2.  To do this, read
C     the instructions in the file PWRITXNT and execute the program
C     given there.
C
C 3.  Create the binary fontcaps that are used by the translators,
C     in a manner described in "NCAR Graphics Generic Package
C     Installer's Guide".  This will require running the program
C     FONTC once per fontcap.
C
C 4.  Supply system-dependent versions of three routines which are
C     used to access both the PWRITX binary database and the binary
C     fontcaps.  These routines are as follows:
C
C     SUBROUTINE PCFOPN (IBNU,NFNT)
C     SUBROUTINE PCFRED (IBNU,NFNT,IBUF,LBUF)
C     SUBROUTINE PCFCLS (IBNU,NFNT)
C
C     PCFOPN is called to open a particular binary file, PCFRED is
C     called to read the next binary record from a file, and PCFCLS
C     is called to close the file.  IBNU is the number of a unit to
C     be used (if necessary) in manipulating the files.  The default
C     value of IBNU is 3, which is defined in the BLOCK DATA routine
C     PCBLDA; it may be necessary to change this value.  NFNT is the
C     number of the desired font: 0 selects the PWRITX database and
C     a non-zero value selects one of the other fonts.  IBUF is an
C     integer array, of length LBUF, into which the next record of
C     data is to be read.  On a Unix system, the following versions
C     may be used (BOFRED, BINRED, and BCLRED are support routines for
C     the FORTRAN version of the metafile translator "cgmtrans"):
C
C     SUBROUTINE PCFOPN (IBNU,NFNT)
C     CHARACTER*128 FILENM
C     DATA FILENM / ' ' /
C     IF (NFNT.EQ.0) THEN
C        CALL GNGPAT (FILENM,'database',ISTAT)
C        IF (ISTAT .NE. -1) THEN
C            DO 101 I=1,119
C               IF (FILENM(I:I).EQ.CHAR(0)) THEN
C                 FILENM(I:I+9)='/pwritdata'
C                 GO TO 104
C               ENDIF
C101          CONTINUE
C            GO TO 105
C        ELSE
C            DO 102 I=2,128
C                LENEM=I
C                IF (FILENM(I:I).EQ.CHAR(0)) GO TO 103
C102         CONTINUE
C103         PRINT * , 'PCFOPN - ',FILENM(1:LENEM-1)
C            STOP
C        ENDIF
C104     OPEN (UNIT=IBNU,FILE=FILENM,STATUS='OLD',FORM='UNFORMATTED',
C    +        ERR=105)
C        REWIND IBNU
C     ELSE IF (NFNT.GE.1.AND.NFNT.LE.20) THEN
C        CALL BOFRED (IBNU,NFNT,IOST,ISTA)
C        IF (ISTA.NE.0) THEN
C           PRINT * , 'PCFOPN - ERROR OPENING FONT ',NFNT
C           STOP
C        END IF
C     ELSE
C        PRINT * , 'PCFOPN - REQUEST FOR UNAVAILBLE FONT ',NFNT
C        STOP
C     END IF
C     RETURN
C105  PRINT * , 'PCFOPN - ERROR OPENING PWRITX DATA FILE ',FILENM
C     STOP
C     END
C
C     END
C
C     SUBROUTINE PCFRED (IBNU,NFNT,IBUF,LBUF)
C       DIMENSION IBUF(LBUF)
C       IF (NFNT.EQ.0) THEN
C         READ (IBNU,ERR=101) (IBUF(I),I=1,LBUF)
C       ELSE
C         CALL BINRED (IBNU,LBUF,IBUF,IOST,ISTA)
C         IF (ISTA.NE.0) THEN
C           PRINT * , 'PCFRED - ERROR READING FONT ',NFNT
C           STOP
C         END IF
C       END IF
C       RETURN
C 101   PRINT * , 'PCFRED - ERROR READING PWRITX DATABASE'
C       STOP
C     END
C
C     SUBROUTINE PCFCLS (IBNU,NFNT)
C       IF (NFNT.EQ.0) THEN
C         CLOSE (UNIT=IBNU,STATUS='KEEP',ERR=101)
C       ELSE
C         CALL BCLRED (IBNU,IOST,ISTA)
C         IF (ISTA.NE.0) THEN
C           PRINT * , 'PCFCLS - ERROR CLOSING FONT ',NFNT
C           STOP
C         END IF
C       END IF
C       RETURN
C 101   PRINT * , 'PCFCLS - ERROR CLOSING PWRITX DATABASE'
C       STOP
C     END
C
C It is not necessary to implement the full package in order to get a
C great deal of use out of it.  You can leave out steps 2, 3, and 4
C and change the default value of the internal parameter IQUF, in the
C BLOCK DATA routine PCBLDA, below, from a 0 to a 1.  This will leave
C the package still usable in "medium-quality" mode, as described in
C the write-up.
C
C***********************************************************************
C C O D E   -   C H A R A C T E R - W R I T I N G   R O U T I N E S
C***********************************************************************
C
      SUBROUTINE PLCHHQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C This is the high-quality character-drawing routine.
C
C
C D E C L A R A T I O N S
C
C
      CHARACTER CHRS*(*)
C
C COMMON block declarations.  PCPRMS holds user-accessible internal
C parameters.  PCSVEM holds other variables which are either used in
C more than one routine or which need to be saved from one call to the
C next.  PCPFMQ holds internal parameters for the medium-quality
C character generator PLCHMQ.  PCPFFC holds internal parameters for the
C new routine (PCCFFC) which access the fonts defined by font caps.
C
C Note that the sizes of IDDA and INDA may be reduced to match the
C values of IDDL and INDL computed below.
C
      COMMON /PCPRMS/ ADDS,CONS,DSTB,DSTL,DSTR,DSTT,HPIC(3),ICEN,IQUF,
     +                ISCR,ITEF,JCOD,NFCC,SSIC,SSPR,SUBS,VPIC(3),
     +                WPIC(3),XBEG,XCEN,XEND,XMUL(3),YBEG,YCEN,YEND,
     +                YMUL(3)
      SAVE   /PCPRMS/
C
      COMMON /PCSVEM/ IBNU,ICOD,IDDA(8625),IDDL,RDGU(300),IDPC(256),
     +                IERU,INDA(789),INDL,INIT,IVCO,IVDU,NBPW,NPPW
      SAVE   /PCSVEM/
C
      COMMON /PCPFMQ/ RHTW
      SAVE   /PCPFMQ/
C
      COMMON /PCPFFC/ NFNT
      SAVE   /PCPFFC/
C
C Declare the BLOCK DATA routine external to force it to load.
C
      EXTERNAL PCBLDA
C
C Define a dummy array for use in skipping records on IBNU.
C
      DIMENSION IDUM(1)
C
C Define arrays in which to put multipliers representing the combined
C effects of changing 'PH', 'PW', 'IH', 'IW', 'CH', 'CW', and the
C zoom factor.
C
      DIMENSION XMZM(3),YMZM(3)
C
C Define arrays implementing the subscript/superscript stack.
C
      DIMENSION NLEV(5),XCNO(5),YCNO(5),XRGO(5),YRGO(5),ICSO(5),ISZO(5),
     +          CSTO(5),SNTO(5)
C
C Define an array to hold the codes associated with 19 "special" ASCII
C characters which might occur in CHRS and for which we will attempt
C to provide a proper response.
C
      DIMENSION ISPC(19)
C
C Define a character variable in which to collect characters to be
C written by PLCHMQ or PLCHLQ if lower quality is selected.
C
      CHARACTER*100 CHBF
C
C Define a character array in which to define the characters to be
C associated with values of NDPC from 1 to 95.
C
      CHARACTER*1 CDPC(95)
C
C Define an array in which to define the ASCII decimal equivalents of
C the characters in CDPC.
C
      DIMENSION IASC(95)
C
C Define the characters to be associated with values of NDPC from 1
C to 95.
C
      DATA CDPC / 'A','B','C','D','E','F','G','H','I','J',
     +            'K','L','M','N','O','P','Q','R','S','T',
     +            'U','V','W','X','Y','Z','0','1','2','3',
     +            '4','5','6','7','8','9','+','-','*','/',
     +            '(',')','$','=',' ',',','.','a','b','c',
     +            'd','e','f','g','h','i','j','k','l','m',
     +            'n','o','p','q','r','s','t','u','v','w',
     +            'x','y','z','!','"','#','%','&',':',';',
#if defined(u370) && defined(unix)
     +            '<','>','?','@','[','\',']','{','|','}',
#else
     +            '<','>','?','@','[','\\',']','{','|','}',
#endif
#if defined(mips) && defined(ultrix)
     +            '~','\'','^','_','`'                   /
#else
     +            '~','''','^','_','`'                   /
#endif
C
C Define the ASCII decimal equivalents of the characters in CDPC.
C
      DATA IASC /  65, 66, 67, 68, 69, 70, 71, 72, 73, 74,
     +             75, 76, 77, 78, 79, 80, 81, 82, 83, 84,
     +             85, 86, 87, 88, 89, 90, 48, 49, 50, 51,
     +             52, 53, 54, 55, 56, 57, 43, 45, 42, 47,
     +             40, 41, 36, 61, 32, 44, 46, 97, 98, 99,
     +            100,101,102,103,104,105,106,107,108,109,
     +            110,111,112,113,114,115,116,117,118,119,
     +            120,121,122, 33, 34, 35, 37, 38, 58, 59,
     +             60, 62, 63, 64, 91, 92, 93,123,124,125,
     +            126, 39, 94, 95, 96                    /
C
C Define the number of card images used to represent the contents of
C the arrays INDA and IDDA.  These are used to compute the number of
C binary words to be read into these arrays.
C
      DATA ICND,ICDD / 49,575 /
C
C Define indices for font, size, and case definitions.  These indices
C are used as displacements into the array INDA.  The sum of a font
C index (Roman or Greek), a size index (principal, indexical, or
C cartographic), a case index (upper or lower), and the DPC code for
C a particular character uniquely defines the index of an entry in
C INDA pointing to the proper digitization in the array IDDA.
C
      DATA IFRO,IFGR      / 0,384     /
      DATA ISZP,ISZI,ISZC / 0,128,256 /
      DATA ICSU,ICSL      / 0, 64     /
C
C Define the codes associated with 19 "special" ASCII characters which
C might possibly appear in CHRS.
C
      DATA ISPC / 93,484,485,111,421,91,92,99,100,94,321,105,337,106,
     +            745,336,746,431,483 /
C
C
C I N I T I A L I Z A T I O N
C
C
C If it hasn't already been done, do that part of the initialization
C which is done only once per loading of the routine.
C
      IF (INIT.EQ.0) THEN
C
        INIT=1
C
C NBPW is the number of bits per word and NPPW is the number of 15-bit
C parcels per word.  INDL and IDDL are the number of words to be read
C into the arrays INDA and IDDA, respectively.
C
        NBPW=I1MACH(5)
        NPPW=NBPW/15
        INDL=(ICND*16-1)/NPPW+1
        IDDL=(ICDD*16*15-1)/NBPW+1
C
C Define the error file logical unit number.
C
        IERU=I1MACH(4)
C
C If it hasn't already been done, initialize the numeric equivalent of
C the function code character.
C
        IF (NFCC.EQ.0) NFCC=ICHAR(':')
C
C Initialize the table IDPC.  For a given character "c", IDPC(ICHAR(c))
C is the integer value of the equivalent of "c" in an augmented DPC
C character set.  (DPC was the set of six-bit character codes used on
C the CDC 7600 (the machine on which the predecessor of PLCHHQ was
C first implemented) and the digitization arrays are arranged in DPC
C order.  First, all undefined entries are zeroed.
C
        DO 101 I=1,256
          IDPC(I)=0
  101   CONTINUE
C
C Then, the entries are defined, one at a time.
C
        DO 102 NDPC=1,95
          NCOL=ICHAR(CDPC(NDPC))
          IF (NCOL.GE.1.AND.NCOL.LE.256) THEN
            IF (IDPC(NCOL).EQ.0) IDPC(NCOL)=NDPC
          END IF
  102   CONTINUE
C
C Set the flag ICOD to force loading of the dataset implied by the
C user-set or default value of JCOD.
C
        ICOD=-1
C
      END IF
C
C Get the fractional-system coordinates of the string position.
C
      XFRA=CUFX(XPOS)
      YFRA=CUFY(YPOS)
C
C Determine the resolution of the plotter, as declared by default or
C by the user.
C
      CALL GETUSV ('XF',IRSX)
      RSLN=2.**IRSX-1.
C
C Determine a multiplier for the digitized size which will make the
C characters have the size requested by the user.
C
      IF (SIZE.LE.0.) THEN
        SIZM=ABS(SIZE)/1023.
      ELSE IF (SIZE.LT.1.) THEN
        SIZM=SIZE/WPIC(1)
      ELSE
        SIZM=(SIZE/RSLN)/WPIC(1)
      END IF
C
C If high-quality characters are to be used, load the high-quality
C character dataset.
C
      IF (IQUF.EQ.0) THEN
C
C If no dataset has been read, or if the user has requested the other
C one of the two, read the proper one.  The first time a given dataset
C is read, verify that it has been done correctly.
C
        IF (ICOD.NE.JCOD) THEN
C
          ICOD=JCOD
C
          IBNS=IBNU
          CALL PCFOPN (IBNU,0)
C
          IF (ICOD.NE.0) THEN
            CALL PCFRED (IBNU,0,IDUM,1)
            CALL PCFRED (IBNU,0,IDUM,1)
          END IF
C
          CALL PCFRED (IBNU,0,INDA,INDL)
          CALL PCFRED (IBNU,0,IDDA,IDDL)
C
          CALL PCFCLS (IBNU,0)
          IBNU=IBNS
C
          IERR=0
          IF (ICOD.EQ.0.AND.IVCO.EQ.0) THEN
            CALL PCCCHK (IERR)
            IVCO=1
          END IF
          IF (ICOD.NE.0.AND.IVDU.EQ.0) THEN
            CALL PCDCHK (IERR)
            IVDU=1
          END IF
          IF (IERR.NE.0) CALL SETER
     +                  ('PLCHHQ - DATASET NOT LOADED CORRECTLY',IERR,2)
        END IF
C
C If lower-quality characters are to be used, save the PLCHMQ parameter
C specifying the ratio of character height to character width.
C
      ELSE
C
        RHWO=RHTW
C
      END IF
C
C Set the orientation angle, in radians, and determine the components
C of various vectors in the direction  of the string and at right
C angles to it.
C
      ANGR=.017453292519943*ANGD
C
      SINO=SIN(ANGR)
      COSO=COS(ANGR)
      STSO=SIZM*SINO
      STCO=SIZM*COSO
      SINM=SIN(ANGR-1.57079632679489)
      COSM=COS(ANGR-1.57079632679489)
      SINP=SIN(ANGR+1.57079632679489)
      COSP=COS(ANGR+1.57079632679489)
C
C Decide whether to do pass 1 and pass 2 or just pass 2.  (If the string
C is positioned relative to its starting point, we need only do pass 2.)
C
      IPSS=1
      IF (ICEN.EQ.0.AND.CNTR.EQ.-1.) IPSS=2
C
C For this pass, begin the string at the position of the image of the
C user's (XPOS,YPOS) in the fractional system.
C
      XBEG=XFRA
      YBEG=YFRA
C
C Save the current value of NFNT for later restoration.
C
      NFNO=NFNT
C
C Start of pass.  Make three copies of the string position for various
C purposes.  (XBOL,YBOL) is the point at the beginning of the current
C line, (XCEN,YCEN) is the point at the center of the last character,
C and (XRGT,YRGT) is the point at the right end of the last character.
C
  103 XBOL=XBEG
      YBOL=YBEG
      XCEN=XBEG
      YCEN=YBEG
      XRGT=XBEG
      YRGT=YBEG
C
C Initialize the character index and the count of characters in the
C string.
C
      ICHR=0
      NCHR=LEN(CHRS)
C
C Initialize the index offsets for font, size and case to Roman,
C principal, and upper, respectively.
C
      IFNT=IFRO
      ISZE=ISZP
      ICSE=ICSU
C
C Initialize the index which says what size the current character is.
C
      IPIC=1
C
C If lower-quality characters are being used and we are starting pass
C 2, initialize the substring position.  It is assumed that the whole
C substring needs to be shifted by 1/6 character width to the right.
C This is as required for PLCHMQ.  Also initialize the substring
C character count and the character size.
C
      IF (IQUF.NE.0.AND.IPSS.EQ.2) THEN
        XSUB=CFUX(XBEG+WPIC(1)*STCO/6.)
        YSUB=CFUY(YBEG+WPIC(1)*STSO/6.)
        NCBF=0
        SIZS=SIZM*WPIC(1)
        IF (RHTW.GE.0.) RHTW=1.5*HPIC(1)/WPIC(1)
      END IF
C
C Zero the character counts for "writing down" and for case change.
C
      NDWN=0
      NCSE=0
C
C Zero the variable defining the vertical space used by the previous
C character, which is used for vertical spacing when "writing down".
C
      VEPC=0.
C
C Zero variables which affect the spacing between characters.
C
      ADDP=0.
      SUBP=0.
C
C Set the current subscript/superscript level.
C
      NSSL=0
C
C Set the flag to indicate that we're not currently processing function
C codes.
C
      IPFC=0
C
C Initialize the multipliers representing the combined effects of
C changing 'PH', 'PW', 'IH', 'IW', 'CH', 'CW', and the zoom factor.
C
      XMZM(1)=XMUL(1)
      XMZM(2)=XMUL(2)
      XMZM(3)=XMUL(3)
      YMZM(1)=YMUL(1)
      YMZM(2)=YMUL(2)
      YMZM(3)=YMUL(3)
C
C If the distances to the edges of the string are to be computed,
C initialize the needed quantities.
C
      IF ((IPSS.EQ.1.AND.ICEN.NE.0).OR.
     +    (IPSS.EQ.2.AND.ITEF.NE.0)) THEN
        DSTL=-1.E6
        DSTR=-1.E6
        DSTB=-1.E6
        DSTT=-1.E6
      END IF
C
C
C  P R O C E S S   T H E   N E X T   C H A R A C T E R
C
C
C If there are no more characters in the string, jump to end-of-string
C processor.
C
  104 IF (ICHR.GE.NCHR) GO TO 108
C
C Get the next character from the string.  NCOL is the number of the
C character in the character collating sequence on the machine on
C which PLCHHQ is being run.
C
      ICHR=ICHR+1
      NCOL=ICHAR(CHRS(ICHR:ICHR))
C
C If the function-code signal character is encountered, flip the value
C of the function-code processing flag and go get the next character.
C
      IF (NCOL.EQ.NFCC) THEN
        IPFC=1-IPFC
        GO TO 104
      END IF
C
C NDPC is the number of the character in the DPC collating sequence.
C
      NDPC=IDPC(NCOL)
C
C If function codes are being processed, jump to the function code
C processor.
C
      IF (IPFC.NE.0) GO TO 109
C
C If the character is not one of the "standard" ones, take special
C action.  Lower-case alphabetic characters behave like upper-case
C alphabetic characters, but force the case to "lower".  "Special"
C characters yield a particular index into the PWRITX database or, in
C three cases, a blank.  As a last resort, a star is used.
C
      IF (NDPC.LT.1.OR.NDPC.GT.47) THEN
        IF (NDPC.GE.48.AND.NDPC.LE.73) THEN
          INDP=IFNT+ISZE+ICSL+NDPC-47
        ELSE IF (NDPC.GE.74.AND.NDPC.LE.92) THEN
          INDP=ISPC(NDPC-73)
        ELSE IF (NDPC.GE.93.AND.NDPC.LE.95) THEN
          INDP=45
        ELSE
          INDP=358
        END IF
        GO TO 105
      END IF
C
C Compute the index of the entry in INDA which points to the proper
C digitization in IDDA.
C
      INDP=IFNT+ISZE+ICSE+NDPC
C
C
C   D R A W   A   C H A R A C T E R
C
C
C If high-quality characters are being used, get the digitization of
C the character in the array RDGU; if no digitization is found, try a
C star instead; if that can't be found, there's a more serious error.
C In any case, define the distances from the center of the character to
C its left and right ends.
C
  105 IF (IQUF.EQ.0) THEN
        IF (NFNT.EQ.0.OR.NDPC.LT.1.OR.NDPC.GT.95.OR.INDP.EQ.95) THEN
          CALL PCEXCD (INDP,MIN(2,IPSS+ICEN),NDGU)
        ELSE
          IF (ICSE.NE.ICSL.OR.NDPC.LT.1.OR.NDPC.GT.26) THEN
            NASC=IASC(NDPC)
          ELSE
            NASC=IASC(NDPC+47)
          END IF
          CALL PCCFFC (MIN(2,IPSS+ICEN),IBNU,NFNT,NASC,IPIC,RDGU,300,
     +                                                             NDGU)
          IF (NDGU.EQ.0) THEN
            CALL PCCFFC (MIN(2,IPSS+ICEN),IBNU,1,NASC,IPIC,RDGU,300,
     +                                                             NDGU)
            IF (NDGU.EQ.0) CALL PCEXCD (INDP,MIN(2,IPSS+ICEN),NDGU)
          END IF
        END IF
        IF (NDGU.EQ.0) THEN
          CALL PCEXCD (358,MIN(2,IPSS+ICEN),NDGU)
          IF (NDGU.EQ.0) CALL SETER
     +            ('PLCHHQ - INTERNAL LOGIC ERROR - SEE CONSULTANT',1,2)
        END IF
        IF (CONS.EQ.0.) THEN
          DTLE=-XMZM(IPIC)*RDGU(1)
          DTRE= XMZM(IPIC)*RDGU(2)
        ELSE IF (CONS.LT.0.) THEN
          DTLE=-CONS
          DTRE=-CONS
        ELSE
          DTLE=CONS*WPIC(IPIC)
          DTRE=CONS*WPIC(IPIC)
        END IF
      ELSE
        DTLE=.5*WPIC(IPIC)
        DTRE=.5*WPIC(IPIC)
      END IF
C
C If this character is to be "written down" (that is to say, beneath
C the previous character), arrange for that.  Otherwise, just zero the
C "last character written down" flag.  Note that, while subscripting
C or superscripting is in progress, "writing down" is suspended.
C
      IF (NDWN.GT.0.AND.NSSL.EQ.0) THEN
        LCWD=1
        XCEN=XCEN+.5*SIZM*(VEPC+VPIC(IPIC))*COSM
        YCEN=YCEN+.5*SIZM*(VEPC+VPIC(IPIC))*SINM
        IF (ADDP.NE.0.) THEN
          IF (ADDS.LT.0.) THEN
            XCEN=XCEN+SIZM*ADDP*COSM
            YCEN=YCEN+SIZM*ADDP*SINM
          ELSE
            XCEN=XCEN+SIZM*.5*(ADDP+ADDS*WPIC(IPIC))*COSM
            YCEN=YCEN+SIZM*.5*(ADDP+ADDS*WPIC(IPIC))*SINM
          END IF
        END IF
        IF (SUBP.NE.0.) THEN
          IF (SUBS.LT.0.) THEN
            XCEN=XCEN-SIZM*SUBP*COSM
            YCEN=YCEN-SIZM*SUBP*SINM
          ELSE
            XCEN=XCEN-SIZM*.5*(SUBP+SUBS*WPIC(IPIC))*COSM
            YCEN=YCEN-SIZM*.5*(SUBP+SUBS*WPIC(IPIC))*SINM
          END IF
        END IF
        NDWN=NDWN-1
        IF (IQUF.NE.0.AND.IPSS.EQ.2) THEN
          IF (NCBF.NE.0) THEN
            CALL PCMQLQ (XSUB,YSUB,CHBF(1:NCBF),SIZS,ANGD,-1.)
            NCBF=0
          END IF
          XSUB=CFUX(XCEN-(DTLE-WPIC(IPIC)/6.)*STCO)
          YSUB=CFUY(YCEN-(DTLE-WPIC(IPIC)/6.)*STSO)
        END IF
      ELSE
        LCWD=0
      END IF
C
C Find the coordinates of the center and the right end of the new
C character.
C
      IF (LCWD.EQ.0) THEN
        XCEN=XRGT+DTLE*STCO
        YCEN=YRGT+DTLE*STSO
        IF (ADDP.NE.0.) THEN
          IF (ADDS.LT.0.) THEN
            XCEN=XCEN+ADDP*STCO
            YCEN=YCEN+ADDP*STSO
          ELSE
            XCEN=XCEN+.5*(ADDP+ADDS*WPIC(IPIC))*STCO
            YCEN=YCEN+.5*(ADDP+ADDS*WPIC(IPIC))*STSO
          END IF
        END IF
        IF (SUBP.NE.0.) THEN
          IF (SUBS.LT.0.) THEN
            XCEN=XCEN-SUBP*STCO
            YCEN=YCEN-SUBP*STSO
          ELSE
            XCEN=XCEN-.5*(SUBP+SUBS*WPIC(IPIC))*STCO
            YCEN=YCEN-.5*(SUBP+SUBS*WPIC(IPIC))*STSO
          END IF
        END IF
        IF (IQUF.NE.0.AND.IPSS.EQ.2.AND.(ADDP.NE.0..OR.SUBP.NE.0.)) THEN
          IF (NCBF.NE.0) THEN
            CALL PCMQLQ (XSUB,YSUB,CHBF(1:NCBF),SIZS,ANGD,-1.)
            NCBF=0
          END IF
          XSUB=CFUX(XCEN-(DTLE-WPIC(IPIC)/6.)*STCO)
          YSUB=CFUY(YCEN-(DTLE-WPIC(IPIC)/6.)*STSO)
        END IF
      END IF
C
      XRGT=XCEN+DTRE*STCO
      YRGT=YCEN+DTRE*STSO
C
C Draw the character and/or update the quantities from which the
C magnitudes of the text-extent vectors will be computed.
C
      IF (IQUF.EQ.0) THEN
C
        RDGU(1)=-2048.
C
        IF (IPSS.EQ.2.AND.(ITEF.EQ.0.OR.ANGD.NE.360.)) THEN
C
          DO 106 I=3,NDGU,2
            IF (RDGU(I).NE.-2048.) THEN
              XTMP=XCEN+XMZM(IPIC)*RDGU(I  )*STCO
     +                 -YMZM(IPIC)*RDGU(I+1)*STSO
              YTMP=YCEN+XMZM(IPIC)*RDGU(I  )*STSO
     +                 +YMZM(IPIC)*RDGU(I+1)*STCO
              IF (RDGU(I-2).EQ.-2048.) THEN
                IF (ISCR.EQ.0) THEN
                  CALL PLOTIF (XTMP,YTMP,0)
                ELSE
                  CALL SCPLTW (INT(XTMP*32767.),INT(YTMP*32767.),0)
                END IF
              ELSE
                IF (ISCR.EQ.0) THEN
                  CALL PLOTIF (XTMP,YTMP,1)
                ELSE
                  CALL SCPLTW (INT(XTMP*32767.),INT(YTMP*32767.),1)
                END IF
              END IF
            END IF
  106     CONTINUE
C
        END IF
C
        IF ((IPSS.EQ.1.AND.ICEN.NE.0).OR.
     +      (IPSS.EQ.2.AND.ITEF.NE.0)) THEN
C
          UCEN=+(XCEN-XFRA)*COSO+(YCEN-YFRA)*SINO
          VCEN=-(XCEN-XFRA)*SINO+(YCEN-YFRA)*COSO
C
          DO 107 I=3,NDGU,2
            IF (RDGU(I).NE.-2048.) THEN
              DSTL=MAX(DSTL,-UCEN-SIZM*XMZM(IPIC)*RDGU(I  ))
              DSTR=MAX(DSTR,+UCEN+SIZM*XMZM(IPIC)*RDGU(I  ))
              DSTB=MAX(DSTB,-VCEN-SIZM*YMZM(IPIC)*RDGU(I+1))
              DSTT=MAX(DSTT,+VCEN+SIZM*YMZM(IPIC)*RDGU(I+1))
            END IF
  107     CONTINUE
C
        END IF
C
      ELSE
C
        IF (IPSS.EQ.2.AND.(ITEF.EQ.0.OR.ANGD.NE.360.)) THEN
          IF (NCBF.EQ.100) THEN
            CALL PCMQLQ (XSUB,YSUB,CHBF(1:NCBF),SIZS,ANGD,-1.)
            XSUB=CFUX(CUFX(XSUB)+REAL(NCBF)*WPIC(IPIC)*STCO)
            YSUB=CFUY(CUFY(YSUB)+REAL(NCBF)*WPIC(IPIC)*STSO)
            NCBF=0
          END IF
          NCBF=NCBF+1
          IF (INDP.EQ.95) THEN
            CHBF(NCBF:NCBF)='x'
          ELSE IF (INDP.EQ.358) THEN
            CHBF(NCBF:NCBF)=' '
          ELSE IF (ICSE.EQ.ICSL.AND.NDPC.GE.1.AND.NDPC.LE.26) THEN
            CHBF(NCBF:NCBF)=CDPC(NDPC+47)
          ELSE
            CHBF(NCBF:NCBF)=CDPC(NDPC)
          END IF
        END IF
C
        IF ((IPSS.EQ.1.AND.ICEN.NE.0).OR.
     +      (IPSS.EQ.2.AND.ITEF.NE.0)) THEN
          UCEN=+(XCEN-XFRA)*COSO+(YCEN-YFRA)*SINO
          VCEN=-(XCEN-XFRA)*SINO+(YCEN-YFRA)*COSO
          DSTL=MAX(DSTL,-UCEN-SIZM*WPIC(IPIC)/3.,
     +                  -UCEN+SIZM*WPIC(IPIC)/3.)
          DSTR=MAX(DSTR,+UCEN-SIZM*WPIC(IPIC)/3.,
     +                  +UCEN+SIZM*WPIC(IPIC)/3.)
          DSTB=MAX(DSTB,-VCEN-SIZM*ABS(RHTW)*WPIC(IPIC)/3.,
     +                  -VCEN+SIZM*ABS(RHTW)*WPIC(IPIC)/3.)
          DSTT=MAX(DSTT,+VCEN-SIZM*ABS(RHTW)*WPIC(IPIC)/3.,
     +                  +VCEN+SIZM*ABS(RHTW)*WPIC(IPIC)/3.)
        END IF
C
      END IF
C
C Save the vertical space occupied by the current character as that of
C the previous character.
C
      VEPC=VPIC(IPIC)
C
C Set variables affecting the amount of space between characters.
C
      IF (ADDS.LT.0.) THEN
        ADDP=-ADDS
      ELSE IF (ADDS.GT.0.) THEN
        ADDP=ADDS*WPIC(IPIC)
      END IF
C
      IF (SUBS.LT.0.) THEN
        SUBP=-SUBS
      ELSE IF (SUBS.GT.0.) THEN
        SUBP=SUBS*WPIC(IPIC)
      END IF
C
C If case was set for a specified number of characters, see if all of
C them have been done and, if so, reset the case index.
C
      IF (NCSE.GT.0) THEN
        NCSE=NCSE-1
        IF (NCSE.EQ.0) ICSE=ICSP
      END IF
C
C If we are subscripting or superscripting for a specified number of
C characters, see if all of them have been done and, if so, reset all
C the required variables.
C
      IF (NSSL.NE.0) THEN
        IF (NLEV(NSSL).GT.0) THEN
          NLEV(NSSL)=NLEV(NSSL)-1
          IF (NLEV(NSSL).EQ.0) THEN
            XCEN=XCNO(NSSL)
            YCEN=YCNO(NSSL)
            XRGT=XRGO(NSSL)
            YRGT=YRGO(NSSL)
            ICSE=ICSO(NSSL)
            ISZE=ISZO(NSSL)
            IPIC=ISZE/128+1
            NSSL=NSSL-1
            IF (IQUF.NE.0.AND.IPSS.EQ.2) THEN
              IF (NCBF.NE.0) THEN
                CALL PCMQLQ (XSUB,YSUB,CHBF(1:NCBF),SIZS,ANGD,-1.)
                NCBF=0
              END IF
              XSUB=CFUX(XRGT+WPIC(IPIC)*STCO/6.)
              YSUB=CFUY(YRGT+WPIC(IPIC)*STSO/6.)
              SIZS=SIZM*WPIC(IPIC)
              IF (RHTW.GE.0.) RHTW=1.5*HPIC(IPIC)/WPIC(IPIC)
            END IF
          END IF
        END IF
      END IF
C
C Go get the next character.
C
      GO TO 104
C
C
C E N D   O F   S T R I N G   E N C O U N T E R E D
C
C
C Restore the original value of NFNT.
C
  108 NFNT=NFNO
C
C Figure out where the end of the character string fell.  This depends
C on whether the last character was "written down" or "written across".
C In the former case, the bottom of the character box is used; in the
C latter case, the center of the right edge of the character is used.
C
      IF (LCWD.NE.0) THEN
        XEND=XCEN+.5*VEPC*STSO
        YEND=YCEN-.5*VEPC*STCO
      ELSE
        XEND=XRGT
        YEND=YRGT
      END IF
C
C If pass 2 was just completed, wrap it up and quit.
C
      IF (IPSS.EQ.2) THEN
C
        IF (ITEF.EQ.0.OR.ANGD.NE.360.) THEN
          IF (IQUF.EQ.0) THEN
            CALL PLOTIF (0.,0.,2)
          ELSE
            IF (NCBF.NE.0) THEN
              CALL PCMQLQ (XSUB,YSUB,CHBF(1:NCBF),SIZS,ANGD,-1.)
            END IF
          END IF
        END IF
C
C Restore the PLCHMQ parameter specifying the ratio of character height
C to width.
C
        IF (IQUF.NE.0) RHTW=RHWO
C
C Done.
C
        RETURN
C
      ENDIF
C
C Otherwise, prepare for pass 2.
C
      IPSS=2
C
C Adjust the beginning coordinates so as to achieve the desired
C centering.
C
      IF (ICEN.EQ.0) THEN
        XBEG=XBEG-.5*(CNTR+1.)*(XEND-XBEG)
        YBEG=YBEG-.5*(CNTR+1.)*(YEND-YBEG)
      ELSE
        XBEG=XBEG-.5*(DSTR-DSTL)*COSO+.5*(DSTT-DSTB)*SINO
        YBEG=YBEG-.5*(DSTR-DSTL)*SINO-.5*(DSTT-DSTB)*COSO
      END IF
C
C Go back to do pass 2.
C
      GO TO 103
C
C
C   F U N C T I O N   C O D E   P R O C E S S I N G
C
C
C If the current character is invalid, issue an error message and go
C back for the next one.
C
  109 IF (NDPC.LE.0.OR.(NDPC.GE.35.AND.NDPC.NE.45.AND.NDPC.NE.46)) THEN
        IF (IPSS.EQ.2) WRITE (IERU,1001) ICHR,CHRS(ICHR:ICHR)
        GO TO 104
      ENDIF
C
C If the character is an octal digit, retrieve an octal number in INDP
C and jump back to process the character it points to.  NDPC is set to
C a blank so as to avoid problems when low-quality characters are used.
C
      IF (NDPC.GE.27.AND.NDPC.LE.34) THEN
        CALL PCGTPI (CHRS,NCHR,ICHR,8,INDP)
        ICHR=ICHR-1
        NDPC=45
        GO TO 105
      ENDIF
C
C Blanks and commas are just separators; ignore them.
C
      IF (NDPC.EQ.45.OR.NDPC.EQ.46) GO TO 104
C
C Jump to the appropriate section to process an alphabetic character.
C
C             A,  B,  C,  D,  E,  F,  G,  H,  I,  J,  K,  L,  M,
C             N,  O,  P,  Q,  R,  S,  T,  U,  V,  W,  X,  Y,  Z
C
      GO TO (124,118,125,123,121,128,112,126,114,110,115,117,110,
     +       122,110,113,104,111,119,110,116,127,110,129,130,131) , NDPC
C
C Log an error if it's not one of the legal ones.
C
  110 IF (IPSS.EQ.2) WRITE (IERU,1001) ICHR,CHRS(ICHR:ICHR)
      GO TO 104
C
C FONT DEFINITION
C ---------------
C
C Define index offset into INDA.
C
C R is for Roman font.
C
  111 IFNT=IFRO
      GO TO 104
C
C G is for Greek font.
C
  112 IFNT=IFGR
      GO TO 104
C
C SIZE DEFINITION
C ---------------
C
C Define index offset into INDA and set current character height and
C width.
C
C P is for Principal size.
C
  113 ISZE=ISZP
      IPIC=1
      GO TO 132
C
C I is for Indexical size.
C
  114 ISZE=ISZI
      IPIC=2
      GO TO 132
C
C K is for Cartographic size.
C
  115 ISZE=ISZC
      IPIC=3
      GO TO 132
C
C CASE DEFINITION
C ---------------
C
C Set old and current index offsets into INDA and look for character
C count.
C
C  U is for Upper case.
C
  116 ICSP=ICSL
      ICSE=ICSU
      CALL PCGTDI (CHRS,NCHR,ICHR,NCSE)
      GO TO 104
C
C L is for Lower case.
C
  117 ICSP=ICSU
      ICSE=ICSL
      CALL PCGTDI (CHRS,NCHR,ICHR,NCSE)
      GO TO 104
C
C LEVEL DEFINITION
C ----------------
C
C B is for suBscript.  Set the sine and cosine of the offset angle and
C jump to join the superscript code.
C
  118 SINT=SINM
      COST=COSM
      GO TO 120
C
C S is for Superscript.  Set the sine and cosine of the offset angle.
C
  119 SINT=SINP
      COST=COSP
C
C Increment the subscript/superscript level, but to no more than five.
C
  120 NSSL=MIN(NSSL+1,5)
C
C Retrieve the character count, if any, following the B or S.
C
      CALL PCGTDI (CHRS,NCHR,ICHR,NLEV(NSSL))
C
C Compute the size of the offset to the superscript or subscript and
C use that to modify the sine and cosine factors.
C
      OFFS=SSPR*SIZM
      IF (ISZE.NE.ISZP) OFFS=SSIC*SIZM
      COST=OFFS*COST
      SINT=OFFS*SINT
C
C Save all the variables which will be needed to undo the offset on a
C stack.
C
      XCNO(NSSL)=XCEN
      YCNO(NSSL)=YCEN
      XRGO(NSSL)=XRGT
      YRGO(NSSL)=YRGT
      ICSO(NSSL)=ICSE
      ISZO(NSSL)=ISZE
      SNTO(NSSL)=SINT
      CSTO(NSSL)=COST
C
C Reset the coordinates of the right end of the last character.
C
      XRGT=XRGT+COST
      YRGT=YRGT+SINT
C
C Reset the character case and size to values appropriate for subscripts
C and superscripts.
C
      IF (ISZE.EQ.ISZP) THEN
        ISZE=ISZI
        IPIC=2
      ELSE
        ISZE=ISZC
        IPIC=3
        ICSE=ICSU
      END IF
C
        GO TO 132
C
C E is for End of subscripting or superscripting, which is illegal if
C we're not in that mode.
C
  121 IF (NSSL.EQ.0) GO TO 110
C
C Return everything to what it was after drawing the base character.
C
      XCEN=XCNO(NSSL)
      YCEN=YCNO(NSSL)
      XRGT=XRGO(NSSL)
      YRGT=YRGO(NSSL)
      ICSE=ICSO(NSSL)
      ISZE=ISZO(NSSL)
      IPIC=ISZE/128+1
      NSSL=NSSL-1
C
      GO TO 132
C
C N is for Normal, which is illegal if we're not subscripting or
C superscripting.
C
  122 IF (NSSL.EQ.0) GO TO 110
C
C Return everything to what it was after drawing the base character
C except the position of the right edge, in which we just reverse the
C original offset.
C
      XCEN=XCNO(NSSL)
      YCEN=YCNO(NSSL)
      XRGT=XRGT-CSTO(NSSL)
      YRGT=YRGT-SNTO(NSSL)
      ICSE=ICSO(NSSL)
      ISZE=ISZO(NSSL)
      IPIC=ISZE/128+1
      NSSL=NSSL-1
C
      GO TO 132
C
C DIRECTION DEFINITION
C --------------------
C
C D is for Down.  Set the character count to the following decimal
C integer, if there is one, or to a number exceeding the number of
C characters left in the string, otherwise.  Set the flag indicating
C that the last character was "written down".
C
  123 CALL PCGTDI (CHRS,NCHR,ICHR,NDWN)
      IF (NDWN.LE.0) NDWN=NCHR
      LCWD=1
      GO TO 104
C
C A is for Across.  Clear the "down" flags.
C
  124 NDWN=0
      LCWD=0
      GO TO 104
C
C COORDINATE DEFINITION
C ---------------------
C
C C is for Carriage return.  Reset the beginning-of-line position and
C the positions of the center and right edge of the last character.
C Suppress spacing manipulations at the beginning of the new line.
C
  125 XBOL=XBOL+SIZM*VPIC(IPIC)*COSM
      YBOL=YBOL+SIZM*VPIC(IPIC)*SINM
      XCEN=XBOL
      YCEN=YBOL
      XRGT=XBOL
      YRGT=YBOL
      ADDP=0.
      SUBP=0.
      GO TO 132
C
C H is for Horizontal.
C
C Retrieve the integer which follows the H and use it to modify all of
C the current character-position variables.
C
  126 CALL PCGTDI (CHRS,NCHR,ICHR,NUPA)
      DELX=REAL(NUPA)
      IF (ICHR.LT.NCHR) THEN
        IF (CHRS(ICHR+1:ICHR+1).EQ.'Q') THEN
          ICHR=ICHR+1
          IF (NUPA.EQ.0) NUPA=1
          DELX=REAL(NUPA)*WPIC(IPIC)
        END IF
      END IF
      XCEN=XCEN+DELX*STCO
      YCEN=YCEN+DELX*STSO
      XRGT=XRGT+DELX*STCO
      YRGT=YRGT+DELX*STSO
      GO TO 132
C
C V is for Vertical.
C
C Retrieve the integer which follows the V and use it to modify all of
C the current character-position variables.
C
  127 CALL PCGTDI (CHRS,NCHR,ICHR,NUPA)
      DELY=REAL(NUPA)
      IF (ICHR.LT.NCHR) THEN
        IF (CHRS(ICHR+1:ICHR+1).EQ.'Q') THEN
          ICHR=ICHR+1
          IF (NUPA.EQ.0) NUPA=1
          DELY=REAL(NUPA)*VPIC(IPIC)
        END IF
      END IF
      XCEN=XCEN-DELY*STSO
      YCEN=YCEN+DELY*STCO
      XRGT=XRGT-DELY*STSO
      YRGT=YRGT+DELY*STCO
      GO TO 132
C
C FONT CHANGE REQUEST
C -------------------
C
C F is for Font.  The intent is to request use of one of the Hershey
C fonts which are defined by fontcaps, stroking out the characters,
C rather than leaving it to be done by the translator.
C
C Retrieve the integer which follows the F; it determines which font to
C use.  If there is no such integer, go back to the font that was in
C use when PLCHHQ was called.
C
  128 ICHS=ICHR
      CALL PCGTDI (CHRS,NCHR,ICHR,NFNT)
      IF (ICHR.EQ.ICHS) NFNT=NFNO
      GO TO 132
C
C ZOOM REQUEST
C ------------
C
C X is for X zoom, Y is for Y zoom, and Z is for Zoom.  The intent is
C to temporarily change the width, the height, or both dimensions, of
C the characters to a given percentage of normal.
C
C The integer which follows the X, Y, or Z determines the percent
C of normal to be used.  If a "Q" follows the number, it requests a
C vertical adjustment of the current center and right positions to
C make the characters look as if they were all written on the same
C base line.  This has no meaning for "X" and is just ignored.
C
C Process an "X".
C
  129 CALL PCGTDI (CHRS,NCHR,ICHR,ITMP)
      ZOOM=REAL(ITMP/100.)
      IF (ZOOM.EQ.0.) ZOOM=1.
      XMZM(1)=ZOOM*XMUL(1)
      XMZM(2)=ZOOM*XMUL(2)
      XMZM(3)=ZOOM*XMUL(3)
      IF (ICHR.LT.NCHR) THEN
        IF (CHRS(ICHR+1:ICHR+1).EQ.'Q') ICHR=ICHR+1
      END IF
      GO TO 132
C
C Process a "Y".
C
  130 CALL PCGTDI (CHRS,NCHR,ICHR,ITMP)
      ZOLD=YMZM(1)/YMUL(1)
      ZOOM=REAL(ITMP/100.)
      IF (ZOOM.EQ.0.) ZOOM=1.
      YMZM(1)=ZOOM*YMUL(1)
      YMZM(2)=ZOOM*YMUL(2)
      YMZM(3)=ZOOM*YMUL(3)
      IF (ICHR.LT.NCHR) THEN
        IF (IQUF.EQ.0.AND.CHRS(ICHR+1:ICHR+1).EQ.'Q') THEN
          ICHR=ICHR+1
          DELY=.428571428571429*HPIC(IPIC)*(ZOOM-ZOLD)
          XCEN=XCEN-DELY*STSO
          YCEN=YCEN+DELY*STCO
          XRGT=XRGT-DELY*STSO
          YRGT=YRGT+DELY*STCO
        END IF
      END IF
      GO TO 132
C
C Process a "Z".
C
  131 CALL PCGTDI (CHRS,NCHR,ICHR,ITMP)
      ZOLD=YMZM(1)/YMUL(1)
      ZOOM=REAL(ITMP/100.)
      IF (ZOOM.EQ.0.) ZOOM=1.
      XMZM(1)=ZOOM*XMUL(1)
      XMZM(2)=ZOOM*XMUL(2)
      XMZM(3)=ZOOM*XMUL(3)
      YMZM(1)=ZOOM*YMUL(1)
      YMZM(2)=ZOOM*YMUL(2)
      YMZM(3)=ZOOM*YMUL(3)
      IF (ICHR.LT.NCHR) THEN
        IF (IQUF.EQ.0.AND.CHRS(ICHR+1:ICHR+1).EQ.'Q') THEN
          ICHR=ICHR+1
          DELY=.428571428571429*HPIC(IPIC)*(ZOOM-ZOLD)
          XCEN=XCEN-DELY*STSO
          YCEN=YCEN+DELY*STCO
          XRGT=XRGT-DELY*STSO
          YRGT=YRGT+DELY*STCO
        END IF
      END IF
      GO TO 132
C
C A function code has just occurred which changes the size of the
C characters being written or implies that the next character will
C not simply be written after the previous one.  If lower-quality
C characters are in use, this implies that characters collected
C in the buffer should be dumped before going back for the next
C character in the input string.
C
  132 IF (IQUF.NE.0.AND.IPSS.EQ.2) THEN
        IF (NCBF.NE.0)CALL PCMQLQ (XSUB,YSUB,CHBF(1:NCBF),SIZS,ANGD,-1.)
        XSUB=CFUX(XRGT+WPIC(IPIC)*STCO/6.)
        YSUB=CFUY(YRGT+WPIC(IPIC)*STSO/6.)
        NCBF=0
        SIZS=SIZM*WPIC(IPIC)
        IF (RHTW.GE.0.) RHTW=1.5*HPIC(IPIC)/WPIC(IPIC)
      END IF
C
      GO TO 104
C
C Formats.
C
 1001 FORMAT(' PLCHHQ - CHARACTER NUMBER',I3,' (',A1,') IS NOT A',
     +       ' LEGAL FUNCTION CODE')
C
      END
