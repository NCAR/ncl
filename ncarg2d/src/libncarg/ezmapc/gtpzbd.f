C
C $Id: gtpzbd.f,v 1.1 1999-04-02 23:05:50 kennison Exp $
C
      BLOCK DATA GTPZBD
C
        COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        SAVE   /PRINZ0/
C
        COMMON /SPCSCH/ FILE27,FILE83
          CHARACTER*128 FILE27,FILE83
        SAVE   /SPCSCH/
C
        COMMON /SPCSIR/ ISPHER,LU27,LU83,LEN
        SAVE   /SPCSIR/
C
        DATA IPEMSG /         1 /
        DATA IPELUN /         6 /
        DATA IPPARM /         1 /
        DATA IPPLUN /         6 /
        DATA FILE27 / 'NAD1927' /
        DATA FILE83 / 'NAD1983' /
        DATA ISPHER /         0 /
        DATA LU27   /        11 /
        DATA LU83   /        12 /
        DATA LEN    /       108 /
C
      END
