C
C  NGCOMI contains the values for all settable parameters:
C
C    CFILNM  -  Metafile name.
C    CPICNM  -  Picture name for CGM pictures.
C     IWKID  -  Currently applicable workstation ID.
C    IFULLB  -  Flag for full background in PostScript output.
C      IXPS  -  Flag for pause in the X11 driver.
C    ISTXMX  -  PostScript stack size limit.
C    IPTHMX  -  PostScript path size limit.
C    ILJOIN  -  PostScript line join type.
C     ILCAP  -  PostScript line cap type.
C      ILLX  -  Lower left X coordinate for PostScript page position.
C      ILLY  -  Lower left Y coordinate for PostScript page position.
C      IURX  -  Upper right X coordinate for PostScript page position.
C      IURY  -  Upper right Y coordinate for PostScript page position.
C    FILSPC  -   Spacing between fill lines in PostScript.
C    HATSPC  -  Spacing between hatch fill lines in PostScript
C    RNLSCL  -  Nominal linewidth scale factor for PostScript lines.
C    ICOSCL  -  Scale factor for PostScript coordinate representation.
C    RMITER  -  Miter limit for PostScript lines when line join type is
C               miter.
C    IERRMX  -  Maximum number of GKS errors allowed before abort.
C    ISGSAV  -  Flag for saving GKS segments at close WISS time.
C    IGKSCP  -  Flag for controlling if GKS clipping is on.
C
      COMMON /NGCOMI/  IWKID, IFULLB,   IXPS, ISTKMX, IPTHMX, 
     +                ILJOIN,  ILCAP,   ILLX,   ILLY,   IURX,
     +                  IURY, FILSPC, HATSPC, RNLSCL, ICOSCL,
     +                RMITER, IERRMX, ISGSAV, IGKSCP
      COMMON /NGCOMC/ CFILNM, CPICNM
      CHARACTER CFILNM*256, CPICNM*80
