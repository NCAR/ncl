c -----------------------------------
      INTEGER FUNCTION greg2juli (yyyy,mm,dd) 
      implicit none
      integer  yyyy,mm,dd

c NCL: julian_day = greg2juld_int (yyyy,mm,dd)

c converts calendar date to Julian date
c .   cf Fliegel & Van Flandern, CACM 11(10):657, 1968

c nomenclature:
c .   yyyy   - year
c .   mm     - month [1 to 12]
c .   dd     - day   [1 to 31]

c A Julian day is defined as the number of days 
c .   since January 1, 4713 B.C. 
c A Julian day begins at noon Universal Time [UT; 12Z] 
c .   of the given date.

c Examples:
c .   integer gregjuli,  julday
c .   julday = greg2juli (1900,1,1)   ==>  julday=2415021

      greg2juli = 
     +       dd - 32075 + 1461*(yyyy + 4800 + (mm - 14)/12)/4 +  
     +       367*(mm - 2 - ((mm - 14)/12)*12)/12 -              
     +       3*((yyyy + 4900 + (mm - 14)/12)/100)/4

      return
      end
c -----------------------------------
      REAL*8 FUNCTION greg2juld (yyyy,mm,dd,hr) 
      implicit none
      integer yyyy,mm,dd,hr

c NCL: julian_day = greg2juld_double (yyyy,mm,dd,hr)

c converts calendar date to Julian date
c .   cf Fliegel & Van Flandern, CACM 11(10):657, 1968
c .   =====> minor modifications by Shea <===== 

c nomenclature:
c .   yyyy   - year
c .   mm     - month [1 to 12]
c .   dd     - day   [1 to 31]
c .   hr     - hour  [0 =< hour <= 23]

c A Julian day is defined as the number of days 
c .   since January 1, 4713 B.C. 
c A Julian day begins at noon Universal Time [UT; 12Z] 
c .   of the given date.

c Examples:
c .   real julday
c .   julday = greg2juld (1900,1,1,0)    ==>  julday=2415020.5
c .   julday = greg2juld (1900,1,1,3)    ==>  julday=2415020.625
c .   julday = greg2juld (1900,1,1,6)    ==>  julday=2415020.75 
c .   julday = greg2juld (1900,1,1,12)   ==>  julday=2415021.
c .   julday = greg2juld (1900,1,1,15)   ==>  julday=2415021.125
c .   julday = greg2juld (1900,1,1,18)   ==>  julday=2415021.25

      integer juld ! local  (julian day)

      juld = dd - 32075 + 1461*(yyyy + 4800 + (mm - 14)/12)/4 +  
     +       367*(mm - 2 - ((mm - 14)/12)*12)/12 -              
     +       3*((yyyy + 4900 + (mm - 14)/12)/100)/4

      greg2juld = dble(juld)  + dble(hr-12)/dble(24.)

      return
      end
c -------------------------------------------
      SUBROUTINE juld2greg (juld,yyyy,mm,dd,hr)
      implicit none

c NCL: itime = juld2greg_int (juld) where itime(4)

      REAL*8  juld            ! input
      INTEGER yyyy,mm,dd,hr   ! output
      INTEGER jd, k,n         ! local

c expands a Julian date into a calendar date
c cf Fliegel & Van Flandern, CACM 11(10):657, 1968
c .   =====> minor modifications by Shea <===== 

c A Julian day is defined as the number of days 
c .   since January 1, 4713 B.C. 
c A Julian day begins at noon Universal Time [UT; 12Z] 
c .   of the given date.

c Examples:
c .   call juld2greg (2415020.5 ,yyyy,mm,dd,hr) ==> 1900,1,1,0
c .   call juld2greg (2415020.75,yyyy,mm,dd,hr) ==> 1900,1,1,6
c .   call juld2greg (2415021.  ,yyyy,mm,dd,hr) ==> 1900,1,1,12
c .   call juld2greg (2415021.25,yyyy,mm,dd,hr) ==> 1900,1,1,18
 
      jd = int(juld+0.5)                   ! shea

      k  = jd + 68569
      n  = 4*k/146097
      k  = k - (146097*n + 3)/4
      yyyy = 4000*(k + 1)/1461001
      k  = k - 1461*yyyy/4 + 31
      mm = 80*k/2447
      dd = k - 2447*mm/80
      k  = mm/11
      mm = mm + 2 - 12*k
      yyyy = 100*(n - 49) + yyyy + k

      hr = nint(24.*sngl((juld-dble(jd))) + 12.)      ! shea

      return
      end
c -------------------------------------------
      SUBROUTINE juli2greg (juld,yyyy,mm,dd)
      implicit none

c NCL: itime = juld2greg_int (juli) where itime(3)

      INTEGER juld            ! input
      INTEGER yyyy,mm,dd      ! output
      INTEGER jd, k,n         ! local

c expands a Julian date into a calendar date
c cf Fliegel & Van Flandern, CACM 11(10):657, 1968
c .   =====> minor modifications by Shea <===== 

c A Julian day is defined as the number of days 
c .   since January 1, 4713 B.C. 
c A Julian day begins at noon Universal Time [UT; 12Z] 
c .   of the given date.

c Examples:
c .   call juli2greg (2415021,nyear,nmo,ndy,nhr) ==> 1900,1,1,12
     
      jd = juld 
      k  = jd + 68569
      n  = 4*k/146097
      k  = k - (146097*n + 3)/4
      yyyy = 4000*(k + 1)/1461001
      k  = k - 1461*yyyy/4 + 31
      mm = 80*k/2447
      dd = k - 2447*mm/80
      k  = mm/11
      mm = mm + 2 - 12*k
      yyyy = 100*(n - 49) + yyyy + k

      return
      end
