/* Date / Copyright 1994 by Kai Hofmann
******* Date/--history-- ****************************************************
*
*   NAME
*	history -- This is the development history of the Date module
*
*   VERSION
*	$VER: Date 33.088 (11.08.1994)
*
*   HISTORY
*	16.01.1994 -	Procedures: JulianLeapYear, GregorianLeapYear &
*			HeisLeapYear initiated.
*	22.01.1994 -	Procedures: JulianMonthDays, GregorianMonthDays,
*			HeisMonthDays, JulianYearDays, GregorianYearDays,
*			HeisYearDays, JulianDayDiff, GregorianDayDiff,
*			HeisDayDiff, JulianDaySmaller, GregorianDaySmaller,
*			HeisDaySmaller, JulianWeekday, GregorianWeekday,
*			HeisWeekday, JulianDaysBeforeWeekday,
*			GregorianDaysBeforeWeekday, HeisDaysBeforeWeekday,
*			JulianDaysAfterWeekday, GregorianDaysAfterWeekday,
*			HeisDaysAfterWeekday JulianDiffDate, FreeDate
*			initiated.
*			Types: Weekdays, Date, DatePtr initiated.
*			Vars of Gregorian reform initiated
*			(for changing to different countries)
*	23.01.1994 -	Procedures: JulianDiffDate finished,
*			GregorianDiffDate, HeisDiffDate, JYearToScaliger,
*			GYearToScaliger, HYearToScaliger, ScaligerYearToJ,
*			ScaligerYearToG, ScaligerYearToH, JSYearToJD,
*			GSYearToJD, HSYearToJD, JDtoMJD, MJDtoJD, JulianToJD,
*			GregorianToJD, HeisToJD, TimeToJD, JDToTime, FreeTime
*			initiated.
*			Types: Time, TimePtr initiated.
*	28.01.1994 -	Procedures: GregorianMoonAge, MoonMonthAge,
*			GregorianEaster initiated.
*	30.01.1994 -	Procedures: JulianDiffDate, GregorianDiffDate,
*			HeisDiffDate, JDtoTime, GregorianEaster edited
*			(changing return value from ptr to VAL variables).
*			Procedures: FreeDate, FreeTime deleted.
*			Types: Date, DatePtr, Time, TimePtr deleted (not
*			longer needed, because of the procedure changes).
*			Procedures: GregorianMoonAge, GregorianEaster changed
*			year parameter from CARDINAL to INTEGER (this is more
*			consistent to the rest of the library).
*			Bugs removed: GregorianWeekday, HeisWeekday
*			(before removing, the weekday for leapyears was
*			wrong)
*			Procedure: GregorianEaster finished.
*	30.01.1994 -	Ported to Oberon-2
*	31.01.1994 -	Compiled with Oberon-2 V3.11
*	12.02.1994 -	Procedures: TimeZoneFactor, LMT, TimeToSec, SecToTime
*			initiated.
*			Version-String installed :)
*	12.02.1994 -	Starting translation to SAS C 6.51
*			Date.h translated
*	13.02.1994 -	Continuation of C translation
*	17.02.1994 -	New Oberon-2 Port, because yesterday Daniel Armor
*			gives me a small hint about the SHORT command
*			(I did not know about this!)
*	17.02.1994 -	Small bug in Autodocs removed
*			making this text as Date/--history-- autodoc
*	17.02.1994 -	Continuation of C translation
*	18.02.1994 -	Finished with C translation
*	19.02.1994 -	C bugs removed (thanks to SAS for helping a C Lamer
*			like me!), some optimizations done too.
*	19.02.1994 -	Oberon-2 version compiled with V40.17 includes
*	21.02.1994 -	Starting to write Modula-II testmodule
*			Vars for the begining of Heis calculation initiated.
*			Fixed small bugs in GregorianWeekday, HeisWeekday,
*			TimeToSec, SecToTime
*			Return-value of LMT changed to LONGINT!
*			Converting testmodule to Oberon-2
*	22.02.1994 -	Converting testmodule to C
*	23.02.1994 -	I noticed, that I forgot the 3 functions
*			JulianWeek, GregorianWeek, HeisWeek
*	24.02.1994 -	Initiated the 3 forgotten functions
*	26.02.1994 -	Initiating new GregorianEastern with Gauﬂ-algorithms
*			but ONLY for 1900-2099!
*	27.02.1994 -	Bug fixed in JulianWeekday
*			Bugs fixed in JulianDayDiff, GregorianDayDiff,
*			HeisDayDiff
*			JulianDayGreater, GregorianDayGreater,
*			HeisDayGreater Initiated.
*	02.03.1994 -	Small bug fixed in HeisDayDiff
*			Bugs from 27.02. fixed in Modula-II and Oberon-2
*			versions
*			I found the way to extend Gregorian Easter!
*			Little bug fixed in JulianWeek, GregorianWeek,
*			HeisWeek (~(M2) is not !(C))
*	05.03.1994 -	Some internal bugs removed
*			New internal procedures GregorianSB,
*			GregorianJHSB, GregorianJHStartSB!
*			Extending GregorianEaster :)
*	11.03.1994 -	Things from 05.03. done in Modula-II and Oberon
*	12.03.1994 -	If __SASC is defined autoinitialization instead of
*			_DateInit will be used!
*	13.03.1994 -	After studying the SAS C Manual again I decided to
*			check for __SASC_650 instead of __SASC because of
*			the available priorities!
*			Setting the priority of _DateInit for
*			autoinitialization to 600!
*	15.03.1994 -	Making Date as library
*	16.03.1994 -	Some work on the Autodocs was done
*			eliminating OldGregorianEaster by comments
*			(ANSI: STOP bad standards like that there are NO
*			 nested comments possible in C!!!)
*	19.03.1994 -	Some work on the Autodocs was done in the M2 Code
*	20.03.1994 -	Some work on the Autodocs was done in the Oberon Code
*	22.03.1994 -	In JDtoMJD, MJD to JD an L was added to the constant
*			In GregorianWeekday(), HeisWeekday(),
*			JulianDiffDate(), GregorianDiffDate(),
*			HeisDiffDate(), JDToTime() I have inserted
*			conversions (found with Borland C++ 4.0)
*	24.03.1994 -	Making SunOS4.1.3, SunOS5.3(Solaris2.3) &
*			RS6000 AIX3.2.? binaries with gcc
*			Eliminating nested commends by inserting a space
*			between / and * (I hate this ANSI C standard
*			feature for commends :(
*	27.03.1994 -	Adding library register assignments to the autodocs
*	03.04.1994 -	Small fixes for the SAS C++ Compiler
*			Small bug fixed in the M2 version of GregorianEaster
*	04.04.1994 -	Adding some 'static' keywords
*	10.04.1994 -	Changing from Shareware to Gift Ware ;-)
*	02.08.1994 -	Small fixes in the Autodocs (thanks to Rita Reichl
*			for correcting my bad english ;-)
*	11.08.1994 -	Again small fixes in the Autodocs!
*
*****************************************************************************
*
*
*/

/*
******* Date/--background-- *************************************************
*
*   NAME
*	Date -- This module was designed to help calc. calendar dates (V33)
*
*   FUNCTION
*	I know about the date routines in the Amiga-OS(TM), but I decided
*	not to use them because of their limited functionalities and of
*	the portability of this module!
*
*   NOTES
*	A tropical year is 365.2422 days! / 365d, 5h, 48min, 46sec
*	A moon month is 29.53059 days! / 29d, 12h, 44min, 2.9 sec
*	A moon phase is 7.38265 days!
*
*	(German) Books which helped me creating this library:
*	    Kleine Naturwissenschaftliche Bibliothek, Band 23
*	    Ewige Kalender
*	    A.W. Butkewitsch & M.S. Selikson
*	    5. Auflage
*	    Teubner, Leipzig 1974
*	    ISBN 3-322-00393-0
*
*	    Tag und Woche, Monat und Jahr: eine Kulturgeschichte des
*	    Kalenders
*	    Rudolf Wendorff
*	    Westdeutscher, Opladen 1993
*	    ISBN 3-531-12417-X
*
*	    Kalender und Chronologie: Bekanntes & Unbekanntes aus der
*	    Kalenderwissenschaft
*	    Heinz Zemanek
*	    4. Auflage
*	    Oldenbourg, M¸nchen 1987
*	    ISBN 3-486-20447-5
*
*	    Meyers Handbuch
*	    ¸ber das Weltall
*	    Karl Schaifers & Gerhard Traving
*	    5. Auflage
*	    Bibliographisches Institut Mannheim 1973
*	    ISBN 3-411-00940-3
*
*	(English) Books which helped me creating this library:
*	    Mathematical Astronomy with a Pocket Calculator
*	    Aubrey Jones Fras
*	    unknown(first) Edition
*	    David & Charles Newton Abbot, London 1978
*	    ISBN 0-7153-7675-6
*
*   COPYRIGHT
*	This module is Copyright 1994 by Kai Hofmann - all rights reserved!
*	For private use, Public Domain, Gift Ware, Freeware and Shareware
*	you could use this module under following conditions:
*	- You send me a little gift (money is very welcome :)
*	    For Bank Account see below - but *ONLY* send in DM
*	    to this Bank Account!!!
*	  Other nice gifts: all Amiga hardware, and I am searching for a
*	  good old 1541 (C64 floppy)
*	- You include a notice in your product, that you use this library
*	  and that it is Copyright by Kai Hofmann!
*	If you want to redistribute this library read the following points:
*	- Redistribution warranty is given to:
*	    Fred Fish for his great Amiga-Software-Library
*	    The German SAAR AG PD-Library
*	    The German AMOK PD-Library
*	    All public accessible INTERNET servers and PHONE boxes!
*	    All others who do NOT take more than DM 5.- for one disk
*	    ALL others who do NOT take more than DM 50.- for one CD
*	For commercial use send me DM 200.-
*	But if you are Apple or Microsoft you have to send (20000.- US$)
*
*   DISCLAIMER
*
*      THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
*   APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
*   HOLDER AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY
*   OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
*   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
*   PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE
*   PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE
*   COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.
*
*      IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
*   WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY REDISTRIBUTE THE
*   PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
*   GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
*   USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS
*   OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR
*   THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER
*   PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE
*   POSSIBILITY OF SUCH DAMAGES.
*
*   ADDITIONAL INFORMATIONS
*	I have tried to make portable/useful and I hope bugfree software
*	for eternity - but this seems to be impossible (sorry!) :)
*	So I hope you will pay a fee for this.
*
*   AUTHOR
*	Kai Hofmann
*	Arberger Heerstraﬂe 92
*	28307 Bremen
*	Germany
*	EMail: i07m@zfn.uni-bremen.de
*	(no phone - I hate it!)
*
*	Bank account : 1203 7503
*	Account owner: Kai Hofmann
*	Bank code    : 290 501 01
*	Bank name    : Sparkasse in Bremen
*
*    THANX
*	Thanx are going to the following people:
*	Danial Armor		- For his hint about the Oberon-2 SHORT
*				  command
*	Heinz Zemanek		- For his great book
*	Christian Schaefer	- For spending time on this lib with his
*				  Borland C++ 4.0 compiler
*	Rita Reichl		- For correcting my bad english ;-)
*
*****************************************************************************
*
*
*/

#include "date.h"
#include <stdlib.h> /* abs()   */
#include <math.h>   /* floor() */


 /* This is only for AMIGA */
#ifndef __SASC_650
#undef __MakeLib
#endif
#ifdef __MakeLib
#include <exec/libraries.h>

   void __regargs __chkabort(void);
   void __regargs __chkabort(void)
   {}

   void __regargs _CXBRK(void);
   void __regargs _CXBRK(void)
   {}

#endif

 static unsigned short	BeforeGregorianDay, BeforeGregorianMonth,
			AfterGregorianDay, AfterGregorianMonth,
			StartHeisDay,StartHeisMonth;
 static int		BeforeGregorianYear, AfterGregorianYear,
			StartHeisYear;

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   bool __saveds __asm JulianLeapYear(register __d0 const int year)
#else
   bool JulianLeapYear(const int year)
#endif

/*
******* Date/JulianLeapYear *************************************************
*
*   NAME
*	JulianLeapYear -- Checks if a year is a leap year. (V33)
*
*   SYNOPSIS
*	leapyear = JulianLeapYear(year);
*	   d0			   d0
*
*	bool JulianLeapYear(const int year);
*
*   FUNCTION
*	JulianLeapYear checks if a year is a leap year in the julian calendar
*	For years after Chr. it checks if the year is devideable by 4.
*	For years before Chr. a leap year must have a modulo 4 value of 1
*
*   INPUTS
*	year - The year which should be checked (from -32768 to 32767)
*	    I think only values from -7 to 32767 are valid, because of
*	    the variant that was done on -8 by Augustus and other things!
*
*   RESULT
*	leapyear - TRUE if the year is a leap year, otherwise false.
*
*   EXAMPLE
*	...
*	if (JulianLeapYear(1994))
*	  printf("leap year!\n");
*	else
*	  printf("no leap year!\n");
*	...
*
*   NOTES
*	A year is 365.25 days long!
*	Use this function only for values from -7 to 1582!
*
*   BUGS
*	No known bugs.
*
*   SEE ALSO
*	GregorianLeapYear(),HeisLeapYear()
*
*****************************************************************************
*
*
*/

  {if (year <= 0)
     return((bool)(abs(year) % 4 == 1));
   else
     return((bool)(year % 4 == 0));
  }


#ifdef __MakeLib
   bool __saveds __asm GregorianLeapYear(register __d0 const int year)
#else
   bool GregorianLeapYear(const int year)
#endif

/*
******* Date/GregorianLeapYear **********************************************
*
*   NAME
*	GregorianLeapYear -- Checks if a year is a leap year. (V33)
*
*   SYNOPSIS
*	leapyear = GregorianLeapYear(year);
*	   d0			      d0
*
*	bool GregorianLeapYear(const int year);
*
*   FUNCTION
*	GregorianLeapYear checks if a year is a leap year.
*	For years after 1582 all years devideable by 4 are leap years,
*	without years devideable by 100, but years devideable by 400
*	are leap years again!
*	For years before 1582 see JulianLeapYear().
*
*   INPUTS
*	year - The year which should be checked (from -32768 to 32767)
*	    I think only values from -7 to 3200 are valid, because of
*	    the variant that was done on -8 by Augustus and other things!
*
*   RESULT
*	leapyear - TRUE if the year is a leap year, otherwise false.
*
*   EXAMPLE
*	...
*	if (GregorianLeapYear(1994))
*	  printf("leap year!\n");
*	else
*	  printf("no leap year!\n");
*	...
*
*   NOTES
*	A year is 365.2425 days long!
*	Use this function only for values from -7 to 3199!
*
*   BUGS
*	No known bugs.
*
*   SEE ALSO
*	JulianLeapYear(),HeisLeapYear()
*
*****************************************************************************
*
*
*/

  {if (year < BeforeGregorianYear)
     {/* Year of the Gregorian reform */
      return(JulianLeapYear(year));
     }
   else
     {/* AfterGregorianYear reform */
      return((bool)((year % 4 == 0) && ((year % 100 > 0) || (year % 400 == 0))));
     }
  }


#ifdef __MakeLib
   bool __saveds __asm HeisLeapYear(register __d0 const int year)
#else
   bool HeisLeapYear(const int year)
#endif

/*
******* Date/HeisLeapYear ***************************************************
*
*   NAME
*	HeisLeapYear -- Checks if a year is a leap year. (V33)
*
*   SYNOPSIS
*	leapyear = HeisLeapYear(year);
*	   d0			 d0
*
*	bool HeisLeapYear(const int year);
*
*   FUNCTION
*	HeisLeapYear checks if a year is a leap year.
*	For years after 1582 see GregorianLeapYear(),
*	The correction from N. Heis says, that all years devideable by
*	3200 are no longer leap years!
*	For years before 1582 see JulianLeapYear
*
*   INPUTS
*	year - The year which should be checked (from -32768 to 32767)
*	    I think only values from -7 to 32767 are valid, because of
*	    the variant that was done on -8 by Augustus and other things!
*
*   RESULT
*	leapyear - TRUE if the year is a leap year, otherwise false.
*
*   EXAMPLE
*	...
*	if (HeisLeapYear(1994))
*	  printf("leap year!\n");
*	else
*	  printf("no leap year!\n");
*	...
*
*   NOTES
*	A year is now 365.2421875 days!
*	Use this function only for values from -7 to 8000!
*
*   BUGS
*	No known bugs.
*
*   SEE ALSO
*	JulianLeapYear(),GregorianLeapYear()
*
*****************************************************************************
*
*
*/

  {if (year < BeforeGregorianYear)
     {/* Year of the Gregorian reform */
      return(JulianLeapYear(year));
     }
   else
     {/* year >= AfterGregorianYear */
      if (year % 3200 == 0)
        {/* Correction from N. Heis */
         return(false);
        }
      else
        {/* (no leap year all 3200 years) */
         return(GregorianLeapYear(year));
        }
     }
  }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   unsigned short __saveds __asm JulianMonthDays(register __d0 const unsigned short month, register __d1 const int year)
#else
   unsigned short JulianMonthDays(const unsigned short month, const int year)
#endif

/*
******* Date/JulianMonthDays ************************************************
*
*   NAME
*	JulianMonthDays -- Gives back the number of days of a month. (V33)
*
*   SYNOPSIS
*	days = JulianMonthDays(month,year);
*	 d0			d0    d1
*
*	unsigned short JulianMonthDays(const unsigned short month,
*	    const int year);
*
*   FUNCTION
*	JulianMonthDays gives you back the number of days a month in
*	a specified year has.
*
*   INPUTS
*	month - The month from which you want to get the number of days.
*	year  - The year in which the month is.
*
*   RESULT
*	days - The number of days the month uses, or 0 if you use
*	    a wrong month.
*
*   EXAMPLE
*	...
*	days = JulianMonthDays(1,1994);
*	printf("Days of January 1994 : %d\n",days);
*	...
*
*   NOTES
*	It is better only to use this function for years from -7 to 09.1582!
*
*   BUGS
*	No known bugs.
*
*   SEE ALSO
*	JulianLeapYear(),GregorianMonthDays(),HeisMonthDays()
*
*****************************************************************************
*
*
*/

  {switch (month)
     {case  1 :
      case  3 :
      case  5 :
      case  7 :
      case  8 :
      case 10 :
      case 12 : return(31);
      case  4 :
      case  6 :
      case  9 :
      case 11 : return(30);
      case  2 : if (JulianLeapYear(year))
                  return(29);
                else
                  if (!JulianLeapYear(year))
                    return(28);
      default : return(0);
     }
  }


#ifdef __MakeLib
   unsigned short __saveds __asm GregorianMonthDays(register __d0 const unsigned short month, register __d1 const int year)
#else
   unsigned short GregorianMonthDays(const unsigned short month, const int year)
#endif

/*
******* Date/GregorianMonthDays *********************************************
*
*   NAME
*	GregorianMonthDays -- Gives back the number of days of a month. (V33)
*
*   SYNOPSIS
*	days = GregorianMonthDays(month,year);
*	 d0			   d0    d1
*
*	unsigned short GregorianMonthDays(const unsigned short month,
*	    const int year);
*
*   FUNCTION
*	GregorianMonthDays gives you back the number of days a month in
*	a specified year has.
*	For the year 1582 and the month 10 there are only 21 days,
*	because of the Gregorian-reform 10 days are deleted from
*	the month (for more - look out for books about this!)
*
*   INPUTS
*	month - The month from which you want to get the number of days.
*	year  - The year in which the month is.
*
*   RESULT
*	days - The number of days the month uses, or 0 if you use
*	    a wrong month.
*
*   EXAMPLE
*	...
*	days = GregorianMonthDays(1,1994);
*	printf("Days of January 1994 : %d\n",days);
*	...
*
*   NOTES
*	Use this function only for years from -7 to 3199!
*
*   BUGS
*	none.
*
*   SEE ALSO
*	GregorianLeapYear(),JulianMonthDays(),HeisMonthDays()
*
*****************************************************************************
*
*
*/

  {if ((year == AfterGregorianYear) && (month == AfterGregorianMonth))
     {/* 10 days canceled by Gregor XIII
				in countries who chnaged later are more days */
      return((unsigned short)(31-((AfterGregorianDay-BeforeGregorianDay)-1)));
     }
   else
     if ((month == 2) && GregorianLeapYear(year))
       return(29);
     else
       if ((month == 2) && (!GregorianLeapYear(year)))
         return(28);
       else
         {/* use Julian function for other calcs. */
          return(JulianMonthDays(month,year));
         }
  }


#ifdef __MakeLib
   unsigned short __saveds __asm HeisMonthDays(register __d0 const unsigned short month, register __d1 const int year)
#else
   unsigned short HeisMonthDays(const unsigned short month, const int year)
#endif

/*
******* Date/HeisMonthDays **************************************************
*
*   NAME
*	HeisMonthDays -- Gives back the number of days of a month. (V33)
*
*   SYNOPSIS
*	days = HeisMonthDays(month,year);
*	 d0		      d0    d1
*
*	unsigned short HeisMonthDays(const unsigned short month,
*	    const int year);
*
*   FUNCTION
*	HeisMonthDays gives you back the number of days a month in
*	a specified year has.
*	For the year 1582 and the month 10 there are only 21 days,
*	because of the Gregorian-reform 10 days are deleted from
*	the month (for more - look out for books about this!)
*
*   INPUTS
*	month - The month from which you want to get the number of days.
*	year  - The year in which the month is.
*
*   RESULT
*	days - The number of days the month uses, or 0 if you use
*	    a wrong month.
*
*   EXAMPLE
*	...
*	days = HeisMonthDays(1,1994);
*	printf("Days of January 1994 : %d\n",days);
*	...
*
*   NOTES
*	Use this function only for years from -7 to 8000!
*
*   BUGS
*	See GregorianMonthDays!
*
*   SEE ALSO
*	HeisLeapYear(),JulianMonthDays(),GregorianMonthDays()
*
*****************************************************************************
*
*
*/

  {if ((month == 2) && HeisLeapYear(year))
     return(29);
   else
     if ((month == 2) && (!HeisLeapYear(year)))
       return(28);
     else
       {/* use Gregorian function for other calcs */
        return(GregorianMonthDays(month,year));
       }
  }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   unsigned int __saveds __asm JulianYearDays(register __d0 const int year)
#else
   unsigned int JulianYearDays(const int year)
#endif

/*
******* Date/JulianYearDays *************************************************
*
*   NAME
*	JulianYearDays -- Gives back the number of days in a year. (V33)
*
*   SYNOPSIS
*	days = JulianYearDays(year);
*	 d0		       d0
*
*	unsigned int JulianYearDays(const int year);
*
*   FUNCTION
*	JulianYearDays gives you back the number of days in
*	a specified year.
*
*   INPUTS
*	year  - The year in which to count the days.
*
*   RESULT
*	days - The number of days the year uses.
*
*   EXAMPLE
*	...
*	days = JulianYearDays(1994);
*	printf("Days of 1994 : %d\n",days);
*	...
*
*   NOTES
*	It is better only to use this function for years from -7 to 1581!
*
*   BUGS
*	No known bugs.
*
*   SEE ALSO
*	JulianMonthDays(),GregorianYearDays(),HeisYearDays()
*
*****************************************************************************
*
*
*/

 {unsigned short month;
  unsigned int days;

  days = 0;
  for (month=1; month<=12; month++)
    {/* add the days of all 12 month */
     days += JulianMonthDays(month,year);
    }
  return(days);
 }


#ifdef __MakeLib
   unsigned int __saveds __asm GregorianYearDays(register __d0 const int year)
#else
   unsigned int GregorianYearDays(const int year)
#endif

/*
******* Date/GregorianYearDays **********************************************
*
*   NAME
*	GregorianYearDays -- Gives back the number of days in a year. (V33)
*
*   SYNOPSIS
*	days = GregorianYearDays(year);
*	 d0			  d0
*
*	unsigned int GregorianYearDays(const int year);
*
*   FUNCTION
*	GregorianYearDays gives you back the number of days in
*	a specified year.
*
*   INPUTS
*	year  - The year in which to count the days.
*
*   RESULT
*	days - The number of days the year uses.
*
*   EXAMPLE
*	...
*	days = GregorianYearDays(1994);
*	printf("Days of 1994 : %d\n",days);
*	...
*
*   NOTES
*	It is better only to use this function for years from -7 to 3199!
*
*   BUGS
*	No known bugs.
*
*   SEE ALSO
*	GregorianMonthDays(),JulianYearDays(),HeisYearDays()
*
*****************************************************************************
*
*
*/

 {unsigned short month;
  unsigned int days;

  days = 0;
  for (month=1; month<=12; month++)
    {/* add the days of all 12 month */
     days += GregorianMonthDays(month,year);
    }
  return(days);
 }


#ifdef __MakeLib
   unsigned int __saveds __asm HeisYearDays(register __d0 const int year)
#else
   unsigned int HeisYearDays(const int year)
#endif

/*
******* Date/HeisYearDays ***************************************************
*
*   NAME
*	HeisYearDays -- Gives back the number of days in a year. (V33)
*
*   SYNOPSIS
*	days = HeisYearDays(year);
*	 d0		     d0
*
*	unsigned int HeisYearDays(const int year);
*
*   FUNCTION
*	HeisYearDays gives you back the number of days in
*	a specified year.
*
*   INPUTS
*	year  - The year in which to count the days.
*
*   RESULT
*	days - The number of days the year uses.
*
*   EXAMPLE
*	...
*	days = HeisYearDays(1994);
*	printf("Days of 1994 : %d\n",days);
*	...
*
*   NOTES
*	It is better only to use this function for years from -7 to 8000!
*
*   BUGS
*	No known bugs.
*
*   SEE ALSO
*	HeisMonthDays(),JulianYearDays(),GregorianYearDays()
*
*****************************************************************************
*
*
*/

 {unsigned short month;
  unsigned int days;

  days = 0;
  for (month=1; month<=12; month++)
    {/* add the days of all 12 month */
     days += HeisMonthDays(month,year);
    }
  return(days);
 }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   bool __saveds __asm JulianDaySmaller(register __d0 const unsigned short day1, register __d1 const unsigned short month1, register __d2 const int year1, register __d3 const unsigned short day2, register __d4 const unsigned short month2, register __d5 const int year2)
#else
   bool JulianDaySmaller(const unsigned short day1, const unsigned short month1, const int year1, const unsigned short day2, const unsigned short month2, const int year2)
#endif

/*
******* Date/JulianDaySmaller ***********************************************
*
*   NAME
*	JulianDaySmaller -- Checks if date1 is smaller than date2. (V33)
*
*   SYNOPSIS
*	smaller = JulianDaySmaller(day1,month1,year1,day2,month2,year2);
*	  d0			    d0    d1    d2    d3    d4    d5
*
*	bool JulianDaySmaller(const unsigned short day1,
*	    const unsigned short month1, const int year1,
*	    const unsigned short day2, const unsigned short month2,
*	    const int year2);
*
*   FUNCTION
*	JulianDaySmaller test if date1 is smaller than date2.
*
*   INPUTS
*	day1   - day of the first date
*	month1 - month of the first date
*	year1  - year of the first date
*	day2   - day of the second date
*	month2 - month of the second month
*	year2  - year of the second date
*
*   RESULT
*	smaller - This is TRUE is date1 < date2 otherwise it's FALSE.
*
*   EXAMPLE
*	...
*	if (JulianDaySmaller(18,9,1970,22,1,1994))
*	  printf("<\n");
*	else
*	  printf(">=\n");
*	...
*
*   NOTES
*	It is better only to use this function for years from -7 to 1582!
*
*   BUGS
*	No known bugs.
*
*   SEE ALSO
*	GregorianDaySmaller(),HeisDaySmaller()
*
*****************************************************************************
*
*
*/

 {if (year1 == year2)
    {if (month1 == month2)
       return((bool)(day1 < day2));
     else
       return((bool)(month1 < month2));
    }
  else
    return((bool)(year1 < year2));
 }


#ifdef __MakeLib
   bool __saveds __asm GregorianDaySmaller(register __d0 const unsigned short day1, register __d1 const unsigned short month1, register __d2 const int year1, register __d3 const unsigned short day2, register __d4 const unsigned short month2, register __d5 const int year2)
#else
   bool GregorianDaySmaller(const unsigned short day1, const unsigned short month1, const int year1, const unsigned short day2, const unsigned short month2, const int year2)
#endif

/*
******* Date/GregorianDaySmaller ********************************************
*
*   NAME
*	GregorianDaySmaller -- Checks if date1 is smaller than date2. (V33)
*
*   SYNOPSIS
*	smaller = GregorianDaySmaller(day1,month1,year1,day2,month2,year2);
*	  d0			       d0    d1    d2    d3    d4    d5
*
*	bool GregorianDaySmaller(const unsigned short day1,
*	    const unsigned short month1, const int year1,
*	    const unsigned short day2, const unsigned short month2,
*	    const int year2);
*
*   FUNCTION
*	GregorianDaySmaller test if date1 is smaller than date2.
*
*   INPUTS
*	day1   - day of the first date
*	month1 - month of the first date
*	year1  - year of the first date
*	day2   - day of the second date
*	month2 - month of the second month
*	year2  - year of the second date
*
*   RESULT
*	smaller - This is TRUE is date1 < date2 otherwise it's FALSE.
*
*   EXAMPLE
*	...
*	if (GregorianDaySmaller(18,9,1970,22,1,1994))
*	  printf("<\n");
*	else
*	  printf(">=\n");
*	...
*
*   NOTES
*	It is better only to use this function for years from -7 to 3200!
*
*   BUGS
*	No known bugs.
*
*   SEE ALSO
*	JulianDaySmaller(),HeisDaySmaller()
*
*****************************************************************************
*
*
*/

 {return(JulianDaySmaller(day1,month1,year1,day2,month2,year2));
 }


#ifdef __MakeLib
   bool __saveds __asm HeisDaySmaller(register __d0 const unsigned short day1, register __d1 const unsigned short month1, register __d2 const int year1, register __d3 const unsigned short day2, register __d4 const unsigned short month2, register __d5 const int year2)
#else
   bool HeisDaySmaller(const unsigned short day1, const unsigned short month1, const int year1, const unsigned short day2, const unsigned short month2, const int year2)
#endif

/*
******* Date/HeisDaySmaller *************************************************
*
*   NAME
*	HeisDaySmaller -- Checks if date1 is smaller than date2. (V33)
*
*   SYNOPSIS
*	smaller = HeisDaySmaller(day1,month1,year1,day2,month2,year2);
*	  d0			  d0    d1    d2    d3    d4    d5
*
*	bool HeisDaySmaller(const unsigned short day1,
*	    const unsigned short month1, const int year1,
*	    const unsigned short day2, const unsigned short month2,
*	    const int year2);
*
*   FUNCTION
*	HeisDaySmaller test if date1 is smaller than date2.
*
*   INPUTS
*	day1   - day of the first date
*	month1 - month of the first date
*	year1  - year of the first date
*	day2   - day of the second date
*	month2 - month of the second month
*	year2  - year of the second date
*
*   RESULT
*	smaller - This is TRUE is date1 < date2 otherwise it's FALSE.
*
*   EXAMPLE
*	...
*	if (HeisDaySmaller(18,9,1970,22,1,1994))
*	  printf("<\n");
*	else
*	  printf(">=\n");
*	...
*
*   NOTES
*	It is better only to use this function for years from -7 to 8000!
*
*   BUGS
*	No known bugs.
*
*   SEE ALSO
*	JulianDaySmaller,GregorianDaySmaller()
*
*****************************************************************************
*
*
*/

 {/* To avoid bugs if differences to JulianDaySmaller was found! */
  return(GregorianDaySmaller(day1,month1,year1,day2,month2,year2));
 }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   bool __saveds __asm JulianDayGreater(register __d0 const unsigned short day1, register __d1 const unsigned short month1, register __d2 const int year1, register __d3 const unsigned short day2, register __d4 const unsigned short month2, register __d5 const int year2)
#else
   bool JulianDayGreater(const unsigned short day1, const unsigned short month1, const int year1, const unsigned short day2, const unsigned short month2, const int year2)
#endif

/*
******* Date/JulianDayGreater ***********************************************
*
*   NAME
*	JulianDayGreater -- Checks if date1 is greater than date2. (V33)
*
*   SYNOPSIS
*	greater = JulianDayGreater(day1,month1,year1,day2,month2,year2);
*	  d0			    d0    d1    d2    d3    d4    d5
*
*	bool JulianDayGreater(const unsigned short day1,
*	    const unsigned short month1, const int year1,
*	    const unsigned short day2, const unsigned short month2,
*	    const int year2);
*
*   FUNCTION
*	JulianDayGreater test if date1 is greater than date2.
*
*   INPUTS
*	day1   - day of the first date
*	month1 - month of the first date
*	year1  - year of the first date
*	day2   - day of the second date
*	month2 - month of the second month
*	year2  - year of the second date
*
*   RESULT
*	greater - This is TRUE is date1 > date2 otherwise it's FALSE.
*
*   EXAMPLE
*	...
*	if (JulianDayGreater(18,9,1970,22,1,1994))
*	  printf(">\n");
*	else
*	  printf("<=\n");
*	...
*
*   NOTES
*	It is better only to use this function for years from -7 to 1582!
*
*   BUGS
*	No known bugs.
*
*   SEE ALSO
*	GregorianDayGreater(),HeisDayGreater()
*
*****************************************************************************
*
*
*/

 {if (year1 == year2)
    {if (month1 == month2)
       return((bool)(day1 > day2));
     else
       return((bool)(month1 > month2));
    }
  else
    return((bool)(year1 > year2));
 }


#ifdef __MakeLib
   bool __saveds __asm GregorianDayGreater(register __d0 const unsigned short day1, register __d1 const unsigned short month1, register __d2 const int year1, register __d3 const unsigned short day2, register __d4 const unsigned short month2, register __d5 const int year2)
#else
   bool GregorianDayGreater(const unsigned short day1, const unsigned short month1, const int year1, const unsigned short day2, const unsigned short month2, const int year2)
#endif

/*
******* Date/GregorianDayGreater ********************************************
*
*   NAME
*	GregorianDayGreater -- Checks if date1 is greater than date2. (V33)
*
*   SYNOPSIS
*	greater = GregorianDayGreater(day1,month1,year1,day2,month2,year2);
*	  d0			       d0    d1    d2    d3    d4    d5
*
*	bool GregorianDayGreater(const unsigned short day1,
*	    const unsigned short month1, const int year1,
*	    const unsigned short day2, const unsigned short month2,
*	    const int year2);
*
*   FUNCTION
*	GregorianDayGreater test if date1 is greater than date2.
*
*   INPUTS
*	day1   - day of the first date
*	month1 - month of the first date
*	year1  - year of the first date
*	day2   - day of the second date
*	month2 - month of the second month
*	year2  - year of the second date
*
*   RESULT
*	greater - This is TRUE is date1 > date2 otherwise it's FALSE.
*
*   EXAMPLE
*	...
*	if (GregorianDayGreater(18,9,1970,22,1,1994))
*	  printf("<\n");
*	else
*	  printf(">=\n");
*	...
*
*   NOTES
*	It is better only to use this function for years from -7 to 3200!
*
*   BUGS
*	No known bugs.
*
*   SEE ALSO
*	JulianDayGreater(),HeisDayGreater()
*
*****************************************************************************
*
*
*/

 {return(JulianDayGreater(day1,month1,year1,day2,month2,year2));
 }


#ifdef __MakeLib
   bool __saveds __asm HeisDayGreater(register __d0 const unsigned short day1, register __d1 const unsigned short month1, register __d2 const int year1, register __d3 const unsigned short day2, register __d4 const unsigned short month2, register __d5 const int year2)
#else
   bool HeisDayGreater(const unsigned short day1, const unsigned short month1, const int year1, const unsigned short day2, const unsigned short month2, const int year2)
#endif

/*
******* Date/HeisDayGreater *************************************************
*
*   NAME
*	HeisDayGreater -- Checks if date1 is greater than date2. (V33)
*
*   SYNOPSIS
*	greater = HeisDayGreater(day1,month1,year1,day2,month2,year2);
*	  d0			  d0    d1    d2    d3    d4    d5
*
*	bool HeisDayGreater(const unsigned short day1,
*	    const unsigned short month1, const int year1,
*	    const unsigned short day2, const unsigned short month2,
*	    const int year2);
*
*   FUNCTION
*	HeisDayGreater test if date1 is greater than date2.
*
*   INPUTS
*	day1   - day of the first date
*	month1 - month of the first date
*	year1  - year of the first date
*	day2   - day of the second date
*	month2 - month of the second month
*	year2  - year of the second date
*
*   RESULT
*	greater - This is TRUE is date1 > date2 otherwise it's FALSE.
*
*   EXAMPLE
*	...
*	if (HeisDayGreater(18,9,1970,22,1,1994))
*	  printf(">\n");
*	else
*	  printf("<=\n");
*	...
*
*   NOTES
*	It is better only to use this function for years from -7 to 8000!
*
*   BUGS
*	No known bugs.
*
*   SEE ALSO
*	JulianDayGreater(),GregorianDayGreater()
*
*****************************************************************************
*
*
*/

 {/* To avoid bugs if differences to JulianDaySmaller was found! */
  return(GregorianDayGreater(day1,month1,year1,day2,month2,year2));
 }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   long __saveds __asm JulianDayDiff(register __d0 const unsigned short day1, register __d1 unsigned short month1, register __d2 int year1, register __d3 const unsigned short day2, register __d4 unsigned short month2, register __d5 int year2)
#else
   long JulianDayDiff(const unsigned short day1, unsigned short month1, int year1, const unsigned short day2, unsigned short month2, int year2)
#endif

/*
******* Date/JulianDayDiff **************************************************
*
*   NAME
*	JulianDayDiff -- Calculates the days between 2 dates. (V33)
*
*   SYNOPSIS
*	days = JulianDayDiff(day1,month1,year1,day2,month2,year2);
*	 d0		      d0    d1    d2    d3    d4    d5
*
*	long JulianDayDiff(const unsigned short day1, unsigned short month1,
*	    int year1, const unsigned short day2, unsigned short month2,
*	    int year2);
*
*   FUNCTION
*	JulianDayDiff gives you back the number of days between
*	two specified dates.
*
*   INPUTS
*	day1   - day of the first date
*	month1 - month of the first date
*	year1  - year of the first date
*	day2   - day of the second date
*	month2 - month of the second month
*	year2  - year of the second date
*
*   RESULT
*	days - The number of days between the two dates
*	    (positive if date1 <= date2).
*
*   EXAMPLE
*	...
*	days = JulianDayDiff(18,9,1970,22,1,1994);
*	printf("Age of Kai Hofmann in days : %d\n",days);
*	...
*
*   NOTES
*	It is better only to use this function for years from -7 to 1582!
*
*   BUGS
*	No known bugs.
*
*   SEE ALSO
*	JulianLeapYear(),JulianMonthDays(),JulianYearDays(),
*	GregorianDayDiff(),HeisDayDiff()
*
*****************************************************************************
*
*
*/

 {unsigned long t1,t2;

  t1 = day1; /* set days left in the actual month */
  t2 = day2;

  while (month1 > 1)
    {/* calc days left by the gone month of the year1 */
     month1--;
     t1 += JulianMonthDays(month1,year1);
    }

  while (month2 > 1)
    {/* calc days left by the gone month of the year2 */
     month2--;
     t2 += JulianMonthDays(month2,year2);
    }

  while (year1 > year2)
    {/* calc days of diff years */
     year1--;
     t1 += JulianYearDays(year1);
    }

  while (year1 < year2)
    {/* calc days of diff years */
     year2--;
     t2 += JulianYearDays(year2);
    }

  return((long)t2-(long)t1);
 }


#ifdef __MakeLib
   long __saveds __asm GregorianDayDiff(register __d0 const unsigned short day1, register __d1 unsigned short month1, register __d2 int year1, register __d3 const unsigned short day2, register __d4 unsigned short month2, register __d5 int year2)
#else
   long GregorianDayDiff(const unsigned short day1, unsigned short month1, int year1, const unsigned short day2, unsigned short month2, int year2)
#endif

/*
******* Date/GregorianDayDiff ***********************************************
*
*   NAME
*	GregorianDayDiff -- Calculates the days between 2 dates. (V33)
*
*   SYNOPSIS
*	days = GregorianDayDiff(day1,month1,year1,day2,month2,year2);
*	 d0		         d0    d1    d2    d3    d4    d5
*
*	long GregorianDayDiff(const unsigned short day1,
*	    unsigned short month1, int year1, const unsigned short day2,
*	    unsigned short month2, int year2);
*
*   FUNCTION
*	GregorianDayDiff gives you back the number of days between
*	two specified dates.
*
*   INPUTS
*	day1   - day of the first date
*	month1 - month of the first date
*	year1  - year of the first date
*	day2   - day of the second date
*	month2 - month of the second month
*	year2  - year of the second date
*
*   RESULT
*	days - The number of days between the two dates
*	    (positive if date1 <= date2).
*
*   EXAMPLE
*	...
*	days = GregorianDayDiff(18,9,1970,22,1,1994);
*	printf("Age of Kai Hofmann in days : %d\n",days);
*	...
*
*   NOTES
*	It is better only to use this function for years from -7 to 02.3200!
*
*   BUGS
*	If you use one of the dates 5.10.1582 to 14.10.1582 you will get a
*	wrong output because these days don't exist!
*
*   SEE ALSO
*	GregorianLeapYear(),GregorianMonthDays(),GregorianYearDays(),
*	JulianDayDiff(),HeisDayDiff()
*
*****************************************************************************
*
*
*/

 {long t1,t2;

  t1 = day1; /* set days left in the actual month */
  t2 = day2;

  if ((year1 == 1582) && (month1 == 10))
   {if ((day1 < 5) && GregorianDaySmaller(day1,month1,year1,day2,month2,year2) && GregorianDaySmaller(day2,month2,year2,1,11,1582) && GregorianDayGreater(day2,month2,year2,14,10,1582))
      t2 -= 10;
    if (day1 > 14)
     {if (GregorianDaySmaller(day1,month1,year1,day2,month2,year2) && GregorianDayGreater(day2,month2,year2,31,10,1582))
        t2 += 10;
      if (GregorianDayGreater(day1,month1,year1,day2,month2,year2) && GregorianDaySmaller(day2,month2,year2,5,10,1582))
        t1 -= 10;
     }
   }
  if ((year2 == 1582) && (month2 == 10) && (day2 > 14))
   {if (GregorianDaySmaller(day2,month2,year2,day1,month1,year1) && GregorianDayGreater(day1,month1,year1,31,10,1582))
      t1 += 10;
    if (GregorianDayGreater(day2,month2,year2,day1,month1,year1) && GregorianDaySmaller(day1,month1,year1,1,10,1582))
      t2 -= 10;
   }

  while (month1 > 1)
    {/* calc days left by the gone month of the year1 */
     month1--;
     t1 += GregorianMonthDays(month1,year1);
    }

  while (month2 > 1)
    {/* calc days left by the gone month of the year2 */
     month2--;
     t2 += GregorianMonthDays(month2,year2);
    }

  while (year1 > year2)
    {/* calc days of diff years */
     year1--;
     t1 += GregorianYearDays(year1);
    }

  while (year1 < year2)
    {/* calc days of diff years */
     year2--;
     t2 += GregorianYearDays(year2);
    }

  return(t2-t1);
 }


#ifdef __MakeLib
   long __saveds __asm HeisDayDiff(register __d0 const unsigned short day1, register __d1 unsigned short month1, register __d2 int year1, register __d3 const unsigned short day2, register __d4 unsigned short month2, register __d5 int year2)
#else
   long HeisDayDiff(const unsigned short day1, unsigned short month1, int year1, const unsigned short day2, unsigned short month2, int year2)
#endif

/*
******* Date/HeisDayDiff ****************************************************
*
*   NAME
*	HeisDayDiff -- Calculates the days between 2 dates. (V33)
*
*   SYNOPSIS
*	days = HeisDayDiff(day1,month1,year1,day2,month2,year2);
*	 d0		    d0    d1    d2    d3    d4    d5
*
*	long HeisDayDiff(const unsigned short day1, unsigned short month1,
*	    int year1, const unsigned short day2, unsigned short month2,
*	    int year2);
*
*   FUNCTION
*	HeisDayDiff gives you back the number of days between
*	two specified dates.
*
*   INPUTS
*	day1   - day of the first date
*	month1 - month of the first date
*	year1  - year of the first date
*	day2   - day of the second date
*	month2 - month of the second month
*	year2  - year of the second date
*
*   RESULT
*	days - The number of days between the two dates
*	    (positive if date1 <= date2).
*
*   EXAMPLE
*	...
*	days = HeisDayDiff(18,9,1970,22,1,1994);
*	printf("Age of Kai Hofmann in days : %d\n",days);
*	...
*
*   NOTES
*	It is better only to use this function for years from -7 to 8000!
*
*   BUGS
*	If you use on of the dates 5.10.1582 to 14.10.1582 you will get
*	wrong output because these days don't exist!
*
*   SEE ALSO
*	HeisLeapYear(),HeisMonthDays(),HeisYearDays(),
*	JulianDayDiff(),GregorianDayDiff()
*
*****************************************************************************
*
*
*/

 {long t1,t2;

  t1 = day1; /* set days left in the actual month */
  t2 = day2;

  if ((year1 == 1582) && (month1 == 10))
   {if ((day1 < 5) && HeisDaySmaller(day1,month1,year1,day2,month2,year2) && HeisDaySmaller(day2,month2,year2,1,11,1582) && HeisDayGreater(day2,month2,year2,14,10,1582))
      t2 -= 10;
    if (day1 > 14)
     {if (HeisDaySmaller(day1,month1,year1,day2,month2,year2) && HeisDayGreater(day2,month2,year2,31,10,1582))
        t2 += 10;
      if (HeisDayGreater(day1,month1,year1,day2,month2,year2) && HeisDaySmaller(day2,month2,year2,5,10,1582))
        t1 -= 10;
     }
   }
  if ((year2 == 1582) && (month2 == 10) && (day2 > 14))
   {if (HeisDaySmaller(day2,month2,year2,day1,month1,year1) && HeisDayGreater(day1,month1,year1,31,10,1582))
      t1 += 10;
    if (HeisDayGreater(day2,month2,year2,day1,month1,year1) && HeisDaySmaller(day1,month1,year1,1,10,1582))
      t2 -= 10;
   }

  while (month1 > 1)
    {/* calc days left by the gone month of the year1 */
     month1--;
     t1 += HeisMonthDays(month1,year1);
    }

  while (month2 > 1)
    {/* calc days left by the gone month of the year2 */
     month2--;
     t2 += HeisMonthDays(month2,year2);
    }

  while (year1 > year2)
    {/* calc days of diff years */
     year1--;
     t1 += HeisYearDays(year1);
    }

  while (year1 < year2)
    {/* calc days of diff years */
     year2--;
     t2 += HeisYearDays(year2);
    }

  return(t2-t1);
 }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   Weekdays __saveds __asm JulianWeekday(register __d0 const unsigned short day, register __d1 unsigned short month, register __d2 int year)
#else
   Weekdays JulianWeekday(const unsigned short day, unsigned short month, int year)
#endif

/*
******* Date/JulianWeekday **************************************************
*
*   NAME
*	JulianWeekday -- Gets the weekday of a specified date. (V33)
*
*   SYNOPSIS
*	weekday = JulianWeekday(day,month,year);
*	  d0			 d0   d1   d2
*
*	Weekdays JulianWeekday(const unsigned short day,
*	    unsigned short month, int year);
*
*   FUNCTION
*	JulianWeekday gets the weekday for a specified date.
*
*   INPUTS
*	day   - day of the date
*	month - month of the date
*	year  - year of the date
*
*   RESULT
*	weekday - This result is of type:
*	    Weekdays = (dayerr,Monday,Tuesday,Wednesday,Thursday,Friday,
*	    Saturday,Sunday);
*	    dayerr will show you, that an error occurs!
*
*   EXAMPLE
*	...
*	weekday = JulianWeekday(4,10,1582);
*	if (weekday == dayerr)
*	  {
*	   ...
*	  }
*	...
*
*   NOTES
*	It is better only to use this function for years from 1 to 02.1582!
*	In this version no dayerr will occur!
*
*   BUGS
*	For years <= 0 errors could occur, or systemcrashs(?).
*
*   SEE ALSO
*	GregorianWeekday(),HeisWeekday()
*
*****************************************************************************
*
*
*/

 {unsigned short decade,wday;

  /* January and february dates must be 13 and 14 of the year before! */
  switch (month)
    {case 1 :
     case 2 : month += 12;
              year--;
    }
  decade = (unsigned short)(year - ((year / 100) * 100));
  /* Formula from Ch. Zeller in 1877 */
  wday = (unsigned short)((day + (((month+1) * 26) / 10) + decade +
			(decade / 4) + 5 - (unsigned short)(year / 100)) % 7);
  /* Convert (1-su 2-mo 3-tu 4-we 5-th 6-fr 7/0-sa) to normal days */
  if (wday == 0)
    wday = 6;
  else
    {wday--;
     if (wday == 0)
       wday = 7;
    }
  return((Weekdays)wday);
 }


#ifdef __MakeLib
   Weekdays __saveds __asm GregorianWeekday(register __d0 const unsigned short day, register __d1 unsigned short month, register __d2 int year)
#else
   Weekdays GregorianWeekday(const unsigned short day, unsigned short month, int year)
#endif

/*
******* Date/GregorianWeekday ***********************************************
*
*   NAME
*	GregorianWeekday -- Gets the weekday of a specified date. (V33)
*
*   SYNOPSIS
*	weekday = GregorianWeekday(day,month,year);
*	  d0			    d0   d1   d2
*
*	Weekdays GregorianWeekday(const unsigned short day,
*	    unsigned short month, int year);
*
*   FUNCTION
*	GregorianWeekday gets the weekday for a specified date.
*
*   INPUTS
*	day   - day of the date
*	month - month of the date
*	year  - year of the date
*
*   RESULT
*	weekday - This result is of type:
*	    Weekdays = (dayerr,Monday,Tuesday,Wednesday,Thursday,Friday,
*	    Saturday,Sunday);
*	    dayerr will show you, that an error occurs!
*
*   EXAMPLE
*	...
*	weekday = GregorianWeekday(22,1,1994);
*	if (weekday == dayerr)
*	  {
*	   ...
*	  }
*	...
*
*   NOTES
*	It is better only to use this function for years from -7 to 3200!
*	In this version dayerr will only occur for the lost days :)
*
*   BUGS
*	It's not possible to use years < 0 (for more see JulianWeekday()).
*
*   SEE ALSO
*	JulianWeekday(),HeisWeekday()
*
*****************************************************************************
*
*
*/

 {Weekdays weekday;
  unsigned int wd;

  if (GregorianDaySmaller(day,month,year,BeforeGregorianDay+1,BeforeGregorianMonth,BeforeGregorianYear))
    return(JulianWeekday(day,month,year));
  else
    {if (GregorianDaySmaller(day,month,year,AfterGregorianDay,AfterGregorianMonth,AfterGregorianYear))
       return(dayerr);
     else
      {if (year==1582)
         return((Weekdays)((unsigned short)((GregorianDayDiff(15,10,1582,day,month,year)+4)%7)+1));
       else
        {/* Formula from J. I. Perelman 1909 */
         wd = year + (year / 4) - (year / 100) + (year / 400)
				+ (int)GregorianDayDiff(1,1,year,day,month,year);
         if (GregorianLeapYear(year))
           wd--;
         weekday = (Weekdays)(wd % 7);
         if (weekday == dayerr)
           weekday = Sunday;
         return(weekday);
        }
      }
    }
 }


#ifdef __MakeLib
   Weekdays __saveds __asm HeisWeekday(register __d0 const unsigned short day, register __d1 unsigned short month, register __d2 int year)
#else
   Weekdays HeisWeekday(const unsigned short day, unsigned short month, int year)
#endif

/*
******* Date/HeisWeekday ****************************************************
*
*   NAME
*	HeisWeekday -- Gets the weekday of a specified date. (V33)
*
*   SYNOPSIS
*	weekday = HeisWeekday(day,month,year);
*	  d0		       d0   d1   d2
*
*	Weekdays HeisWeekday(const unsigned short day, unsigned short month,
*	    int year);
*
*   FUNCTION
*	HeisWeekday gets the weekday for a specified date.
*
*   INPUTS
*	day   - day of the date
*	month - month of the date
*	year  - year of the date
*
*   RESULT
*	weekday - This result is of type:
*	    Weekdays = (dayerr,Monday,Tuesday,Wednesday,Thursday,Friday,
*	    Saturday,Sunday);
*	    dayerr will show you, that an error occurs!
*
*   EXAMPLE
*	...
*	weekday = HeisWeekday(22,1,1994);
*	if (weekday == dayerr)
*	  {
*	   ...
*	  }
*	...
*
*   NOTES
*	It is better only to use this function for years from -7 to 8000!
*	In this version dayerr will only occur for the lost days :)
*
*   BUGS
*	It is not possible to use year < 0 (see JulianWeekday() for more).
*
*   SEE ALSO
*	JulianWeekday(),GregorianWeekday()
*
*****************************************************************************
*
*
*/

 {Weekdays weekday;
  unsigned int wd;

  if (HeisDaySmaller(day,month,year,StartHeisDay,StartHeisMonth,StartHeisYear))
    return(GregorianWeekday(day,month,year));
  else
    {/* Formula from J. I. Perelman 1909 - extended for N.Heis in 01.1994
	by Kai Hofmann */
     wd = year + (year / 4) - (year / 100) + (year / 400)
		- (year / 3200) + (int)HeisDayDiff(1,1,year,day,month,year);
     if (HeisLeapYear(year))
       wd--;
     weekday = (Weekdays)(wd % 7);
     if (weekday == dayerr)
       weekday = Sunday;
     return(weekday);
    }
 }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   unsigned short __saveds __asm JulianDaysBeforeWeekday(register __d0 const unsigned short day, register __d1 const unsigned short month, register __d2 const int year, register __d3 const Weekdays weekday)
#else
   unsigned short JulianDaysBeforeWeekday(const unsigned short day, const unsigned short month, const int year, const Weekdays weekday)
#endif

/*
******* Date/JulianDaysBeforeWeekday ****************************************
*
*   NAME
*	JulianDaysBeforeWeekday -- Returns the diff to the wday before. (V33)
*
*   SYNOPSIS
*	days = JulianDaysBeforeWeekday(day,month,year,weekday);
*	 d0				d0  d1    d2    d3
*
*	unsigned short JulianDaysBeforeWeekday(const unsigned short day,
*	    const unsigned short month, const int year,
*	    const Weekdays weekday);
*
*   FUNCTION
*	Returns the days to the weekday before the specified date.
*	So if you specify the 22.1.1994 (Saturday) and Thursday
*	you get back 2!
*	If you specify the 22.1.1994 and Saturday you get back 0
*	(the same day)!
*
*   INPUTS
*	day     - day of the date
*	month   - month of the date
*	year    - year of the date
*	weekday - weekday to search for building difference
*
*   RESULT
*	days - The days back to the searched weekday (0-6)
*	    If you get back 8 an error occurs!
*
*   EXAMPLE
*	...
*	days = JulianDaysBeforeWeekday(22,1,1994,Thursday);
*	...
*
*   NOTES
*	It is better to use this function only from -7 to 02.1582!
*
*   BUGS
*	See JulianWeekday()!
*
*   SEE ALSO
*	JulianWeekday(),GregorianDaysBeforeWeekday(),HeisDaysBeforeWeekday()
*
*****************************************************************************
*
*
*/

 {Weekdays wday;

  if (weekday == dayerr)
    return(8);
  else
    {wday = JulianWeekday(day,month,year);
     if (wday >= weekday)
       return((unsigned short)(wday-weekday));
     else
       {/* wday < weekday */
        return((unsigned short)(7-weekday+wday));
       }
    }
 }


#ifdef __MakeLib
   unsigned short __saveds __asm GregorianDaysBeforeWeekday(register __d0 const unsigned short day, register __d1 const unsigned short month, register __d2 const int year, register __d3 const Weekdays weekday)
#else
   unsigned short GregorianDaysBeforeWeekday(const unsigned short day, const unsigned short month, const int year, const Weekdays weekday)
#endif

/*
******* Date/GregorianDaysBeforeWeekday *************************************
*
*   NAME
*	GregorianDaysBeforeWeekday -- Returns the diff to wday before. (V33)
*
*   SYNOPSIS
*	days = GregorianDaysBeforeWeekday(day,month,year,weekday);
*	 d0				   d0  d1    d2    d3
*
*	unsigned short GregorianDaysBeforeWeekday(const unsigned short day,
*	    const unsigned short month, const int year,
*	    const Weekdays weekday);
*
*   FUNCTION
*	Returns the days to the weekday before the specified date.
*	So if you specify the 22.1.1994 (Saturday) and Thursday
*	you get back 2!
*	If you specify the 22.1.1994 and Saturday you get back 0
*	(the same day)!
*
*   INPUTS
*	day     - day of the date
*	month   - month of the date
*	year    - year of the date
*	weekday - weekday to search for building difference
*
*   RESULT
*	days - The days back to the searched weekday (1-7)
*	    If you get back 8 an error occurs!
*
*   EXAMPLE
*	...
*	days = GregorianDaysBeforeWeekday(22,1,1994,Thursday);
*	...
*
*   NOTES
*	It is better to use this function only from -7 to 3200!
*
*   BUGS
*	See GregorianWeekday()!
*
*   SEE ALSO
*	GregorianWeekday(),JulianDaysBeforeWeekday(),HeisDaysBeforeWeekday()
*
*****************************************************************************
*
*
*/

 {Weekdays wday;

  if (weekday == dayerr)
    return(8);
  else
    {wday = GregorianWeekday(day,month,year);
     if (wday >= weekday)
       return((unsigned short)(wday-weekday));
     else
       {/* wday < weekday */
        return((unsigned short)(7-weekday+wday));
       }
    }
 }


#ifdef __MakeLib
   unsigned short __saveds __asm HeisDaysBeforeWeekday(register __d0 const unsigned short day, register __d1 const unsigned short month, register __d2 const int year, register __d3 const Weekdays weekday)
#else
   unsigned short HeisDaysBeforeWeekday(const unsigned short day, const unsigned short month, const int year, const Weekdays weekday)
#endif

/*
******* Date/HeisDaysBeforeWeekday ******************************************
*
*   NAME
*	HeisDaysBeforeWeekday -- Returns the diff to wday before. (V33)
*
*   SYNOPSIS
*	days = HeisDaysBeforeWeekday(day,month,year,weekday);
*	 d0			      d0  d1    d2    d3
*
*	unsigned short HeisDaysBeforeWeekday(const unsigned short day,
*	    const unsigned short month, const int year,
*	    const Weekdays weekday);
*
*   FUNCTION
*	Returns the days to the weekday before the specified date.
*	So if you specify the 22.1.1994 (Saturday) and Thursday
*	you get back 2!
*	If you specify the 22.1.1994 and Saturday you get back 0
*	(the same day)!
*
*   INPUTS
*	day     - day of the date
*	month   - month of the date
*	year    - year of the date
*	weekday - weekday to search for building difference
*
*   RESULT
*	days - The days back to the searched weekday (1-7)
*	    If you get back 8 an error occurs!
*
*   EXAMPLE
*	...
*	days = HeisDaysBeforeWeekday(22,1,1994,Thursday);
*	...
*
*   NOTES
*	It is better to use this function only from -7 to 8000!
*
*   BUGS
*	See HeisWeekday()!
*
*   SEE ALSO
*	HeisWeekday(),JulianDaysBeforeWeekday(),GregorianDaysBeforeWeekday()
*
*****************************************************************************
*
*
*/

 {Weekdays wday;

  if (weekday == dayerr)
    return(8);
  else
    {wday = HeisWeekday(day,month,year);
     if (wday >= weekday)
       return((unsigned short)(wday-weekday));
     else
       {/* wday < weekday */
        return((unsigned short)(7-weekday+wday));
       }
    }
 }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   unsigned short __saveds __asm JulianDaysAfterWeekday(register __d0 const unsigned short day, register __d1 const unsigned short month, register __d2 const int year, register __d3 const Weekdays weekday)
#else
   unsigned short JulianDaysAfterWeekday(const unsigned short day, const unsigned short month, const int year, const Weekdays weekday)
#endif

/*
******* Date/JulianDaysAfterWeekday *****************************************
*
*   NAME
*	JulianDaysAfterWeekday -- Returns the diff to the wday after. (V33)
*
*   SYNOPSIS
*	days = JulianDaysAfterWeekday(day,month,year,weekday);
*	 d0			       d0   d1   d2     d3
*
*	unsigned short JulianDaysAfterWeekday(const unsigned short day,
*	    const unsigned short month, const int year,
*	    const Weekdays weekday);
*
*   FUNCTION
*	Returns the days to the weekday after the specified date.
*	So if you specify the 22.1.1994 (Saturday) and Thursday
*	you get back 5!
*	If you specify the 22.1.1994 and Saturday you get back 0
*	(the same day)!
*
*   INPUTS
*	day     - day of the date
*	month   - month of the date
*	year    - year of the date
*	weekday - weekday to search for building difference
*
*   RESULT
*	days - The days after to the searched weekday.
*
*   EXAMPLE
*	...
*	days = JulianDaysAfterWeekday(22,1,1994,Thursday);
*	...
*
*   NOTES
*	It is better to use this function only from -7 to 1582!
*
*   BUGS
*	See JulianWeekday()!
*
*   SEE ALSO
*	JulianWeekday(),GregorianDaysAfterWeekday(),HeisDaysAfterWeekday()
*
*****************************************************************************
*
*
*/

 {Weekdays wday;

  if (weekday == dayerr)
    return(8);
  else
    {wday = JulianWeekday(day,month,year);
     if (wday <= weekday)
       return((unsigned short)(weekday-wday));
     else
       {/* wday > weekday */
        return((unsigned short)(7-wday+weekday));
       }
    }
 }


#ifdef __MakeLib
   unsigned short __saveds __asm GregorianDaysAfterWeekday(register __d0 const unsigned short day, register __d1 const unsigned short month, register __d2 const int year, register __d3 const Weekdays weekday)
#else
   unsigned short GregorianDaysAfterWeekday(const unsigned short day, const unsigned short month, const int year, const Weekdays weekday)
#endif

/*
******* Date/GregorianDaysAfterWeekday **************************************
*
*   NAME
*	GregorianDaysAfterWeekday -- Returns the diff to wday after. (V33)
*
*   SYNOPSIS
*	days = GregorianDaysAfterWeekday(day,month,year,weekday);
*	 d0			          d0   d1   d2     d3
*
*	unsigned short GregorianDaysAfterWeekday(const unsigned short day,
*	    const unsigned short month, const int year,
*	    const Weekdays weekday);
*
*   FUNCTION
*	Returns the days to the weekday after the specified date.
*	So if you specify the 22.1.1994 (Saturday) and Thursday
*	you get back 5!
*	If you specify the 22.1.1994 and Saturday you get back 0
*	(the same day)!
*
*   INPUTS
*	day     - day of the date
*	month   - month of the date
*	year    - year of the date
*	weekday - weekday to search for building difference
*
*   RESULT
*	days - The days after to the searched weekday.
*
*   EXAMPLE
*	...
*	days = GregorianDaysAfterWeekday(22,1,1994,Thursday);
*	...
*
*   NOTES
*	It is better to use this function only from -7 to 3200!
*
*   BUGS
*	See GregorianWeekday()!
*
*   SEE ALSO
*	GregorianWeekday(),JulianDaysAfterWeekday(),HeisDaysAfterWeekday()
*
*****************************************************************************
*
*
*/

 {Weekdays wday;

  if (weekday == dayerr)
    return(8);
  else
    {wday = GregorianWeekday(day,month,year);
     if (wday <= weekday)
       return((unsigned short)(weekday-wday));
     else
       {/* wday > weekday */
        return((unsigned short)(7-wday+weekday));
       }
    }
 }


#ifdef __MakeLib
   unsigned short __saveds __asm HeisDaysAfterWeekday(register __d0 const unsigned short day, register __d1 const unsigned short month, register __d2 const int year, register __d3 const Weekdays weekday)
#else
   unsigned short HeisDaysAfterWeekday(const unsigned short day, const unsigned short month, const int year, const Weekdays weekday)
#endif

/*
******* Date/HeisDaysAfterWeekday *******************************************
*
*   NAME
*	HeisDaysAfterWeekday -- Returns the diff to the wday after. (V33)
*
*   SYNOPSIS
*	days = HeisDaysAfterWeekday(day,month,year,weekday);
*	 d0			     d0   d1   d2     d3
*
*	unsigned short HeisDaysAfterWeekday(const unsigned short day,
*	    const unsigned short month, const int year,
*	    const Weekdays weekday);
*
*   FUNCTION
*	Returns the days to the weekday after the specified date.
*	So if you specify the 22.1.1994 (Saturday) and Thursday
*	you get back 5!
*	If you specify the 22.1.1994 and Saturday you get back 0
*	(the same day)!
*
*   INPUTS
*	day     - day of the date
*	month   - month of the date
*	year    - year of the date
*	weekday - weekday to search for building difference
*
*   RESULT
*	days - The days after to the searched weekday.
*
*   EXAMPLE
*	...
*	days = HeisDaysAfterWeekday(22,1,1994,Thursday);
*	...
*
*   NOTES
*	It is better to use this function only from -7 to 8000!
*
*   BUGS
*	See HeisWeekday()!
*
*   SEE ALSO
*	HeisWeekday(),JulianDaysAfterWeekday(),GregorianDaysAfterWeekday()
*
*****************************************************************************
*
*
*/

 {Weekdays wday;

  if (weekday == dayerr)
    return(8);
  else
    {wday = HeisWeekday(day,month,year);
     if (wday <= weekday)
       return((unsigned short)(weekday-wday));
     else
       {/* wday > weekday */
        return((unsigned short)(7-wday+weekday));
       }
    }
 }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   void __saveds __asm JulianDiffDate(register __d0 const unsigned short day, register __d1 const unsigned short month, register __d2 const int year, register __d3 int days, register __a0 unsigned short *dday, register __a1 unsigned short *dmonth, register __a2 int *dyear)
#else
   void JulianDiffDate(const unsigned short day, const unsigned short month, const int year, int days, unsigned short *dday, unsigned short *dmonth, int *dyear)
#endif

/*
******* Date/JulianDiffDate *************************************************
*
*   NAME
*	JulianDiffDate -- Returns the date for a diff to another date. (V33)
*
*   SYNOPSIS
*	JulianDiffDate(day,month,year,diffdays,dday,dmonth,dyear);
*			d0   d1   d2     d3     a0    a1    a2
*
*	void JulianDiffDate(const unsigned short day,
*	    const unsigned short month, const int year, int days,
*	    unsigned short *dday, unsigned short *dmonth, int *dyear);
*
*   FUNCTION
*	Returns the date which lies diffdays before/after the specified date.
*
*   INPUTS
*	day      - day of the date
*	month    - month of the date
*	year     - year of the date
*	diffdays - difference to the date in days
*
*   RESULT
*	dday   - Destination day
*	dmonth - Destination month
*	dyear  - Destination year
*
*   EXAMPLE
*	...
*	JulianDiffDate(23,1,1994,7,&dday,&dmonth,&dyear);
*	...
*
*   NOTES
*	It is better to use this function only from -7 to 1582!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	JulianDayDiff(),JulianMonthDays(),GregorianDiffDate(),HeisDiffDate()
*
*****************************************************************************
*
*
*/

 {int ddays;

  *dday = day;
  *dmonth = month;
  *dyear = year;
  if (days >= 0)
    {/* add */
     ddays = (int)JulianDayDiff(*dday,*dmonth,*dyear,1,1,(*dyear)+1);
     while (days >= ddays)
       {/* years */
        *dday = 1;
        *dmonth = 1;
        (*dyear)++;
        days -= ddays;
        ddays = (int)JulianDayDiff(*dday,*dmonth,*dyear,1,1,(*dyear)+1);
       }
     ddays = (int)JulianDayDiff(*dday,*dmonth,*dyear,1,(*dmonth)+1,*dyear);
     while (days >= ddays)
       {/* months */
        *dday = 1;
        (*dmonth)++;
        days -= ddays;
        ddays = (int)JulianDayDiff(*dday,*dmonth,*dyear,1,(*dmonth)+1,*dyear);
       }
     if (days > 0)
       {/* days */
        *dday += (unsigned short)days;
       }
    }
  else
    {/* sub */
     ddays = (int)JulianDayDiff(*dday,*dmonth,*dyear,31,12,(*dyear)-1);
     while (days <= ddays)
       {/* years */
        *dday = 31;
        *dmonth = 12;
        (*dyear)--;
        days -= ddays;
        ddays = (int)JulianDayDiff(*dday,*dmonth,*dyear,31,12,(*dyear)-1);
       }
     ddays = (int)JulianDayDiff(*dday,*dmonth,*dyear,JulianMonthDays((*dmonth)-1,*dyear),(*dmonth)-1,*dyear);
     while (days <= ddays)
       {/* months */
        *dday = JulianMonthDays((*dmonth)-1,*dyear);
        (*dmonth)--;
        days -= ddays;
        ddays = (int)JulianDayDiff(*dday,*dmonth,*dyear,JulianMonthDays((*dmonth)-1,*dyear),(*dmonth)-1,*dyear);
       }
     if (days < 0)
       *dday -= (unsigned short)abs(days);
    }
 }


#ifdef __MakeLib
   void __saveds __asm GregorianDiffDate(register __d0 const unsigned short day, register __d1 const unsigned short month, register __d2 const int year, register __d3 int days, register __a0 unsigned short *dday, register __a1 unsigned short *dmonth, register __a2 int *dyear)
#else
   void GregorianDiffDate(const unsigned short day, const unsigned short month, const int year, int days, unsigned short *dday, unsigned short *dmonth, int *dyear)
#endif

/*
******* Date/GregorianDiffDate **********************************************
*
*   NAME
*	GregorianDiffDate -- Returns the diff date to another date. (V33)
*
*   SYNOPSIS
*	GregorianDiffDate(day,month,year,diffdays,dday,dmonth,dyear);
*			   d0   d1   d2     d3     a0    a1    a2
*
*	void GregorianDiffDate(const unsigned short day,
*	    const unsigned short month, const int year, int days,
*	    unsigned short *dday, unsigned short *dmonth, int *dyear);
*
*   FUNCTION
*	Returns the date which lies diffdays before/after the specified date.
*
*   INPUTS
*	day      - day of the date
*	month    - month of the date
*	year     - year of the date
*	diffdays - difference to the date in days
*
*   RESULT
*	dday   - Destination day
*	dmonth - Destination month
*	dyear  - Destination year
*
*   EXAMPLE
*	...
*	GregorianDiffDate(23,1,1994,7,&dday,&dmonth,&dyear);
*	...
*
*   NOTES
*	It is better to use this function only from -7 to 3200!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	GregorianDayDiff(),GregorianMonthDays(),JulianDiffDate(),
*	HeisDiffDate()
*
*****************************************************************************
*
*
*/

 {int ddays;

  *dday = day;
  *dmonth = month;
  *dyear = year;
  if (days >= 0)
    {/* add */
     ddays = (int)GregorianDayDiff(*dday,*dmonth,*dyear,1,1,(*dyear)+1);
     while (days >= ddays)
       {/* years */
        *dday = 1;
        *dmonth = 1;
        (*dyear)++;
        days -= ddays;
        ddays = (int)GregorianDayDiff(*dday,*dmonth,*dyear,1,1,(*dyear)+1);
       }
     ddays = (int)GregorianDayDiff(*dday,*dmonth,*dyear,1,(*dmonth)+1,*dyear);
     while (days >= ddays)
       {/* months */
        *dday = 1;
        (*dmonth)++;
        days -= ddays;
        ddays = (int)GregorianDayDiff(*dday,*dmonth,*dyear,1,(*dmonth)+1,*dyear);
       }
     if (days > 0)
       {/* days */
        *dday += (unsigned short)days;
       }
    }
  else
    {/* sub */
     ddays = (int)GregorianDayDiff(*dday,*dmonth,*dyear,31,12,(*dyear)-1);
     while (days <= ddays)
       {/* years */
        *dday = 31;
        *dmonth = 12;
        (*dyear)--;
        days -= ddays;
        ddays = (int)GregorianDayDiff(*dday,*dmonth,*dyear,31,12,(*dyear)-1);
       }
     ddays = (int)GregorianDayDiff(*dday,*dmonth,*dyear,GregorianMonthDays((*dmonth)-1,*dyear),(*dmonth-1),*dyear);
     while (days <= ddays)
       {/* months */
        *dday = GregorianMonthDays((*dmonth-1),*dyear);
        (*dmonth)--;
        days -= ddays;
        ddays = (int)GregorianDayDiff(*dday,*dmonth,*dyear,GregorianMonthDays((*dmonth)-1,*dyear),(*dmonth)-1,*dyear);
       }
     if (days < 0)
       *dday -= (unsigned short)abs(days);
    }
 }


#ifdef __MakeLib
   void __saveds __asm HeisDiffDate(register __d0 const unsigned short day, register __d1 const unsigned short month, register __d2 const int year, register __d3 int days, register __a0 unsigned short *dday, register __a1 unsigned short *dmonth, register __a2 int *dyear)
#else
   void HeisDiffDate(const unsigned short day, const unsigned short month, const int year, int days, unsigned short *dday, unsigned short *dmonth, int *dyear)
#endif

/*
******* Date/HeisDiffDate ***************************************************
*
*   NAME
*	HeisDiffDate -- Returns the date for a diff to another date. (V33)
*
*   SYNOPSIS
*	HeisDiffDate(day,month,year,diffdays,dday,dmonth,dyear);
*		      d0   d1   d2     d3     a0    a1    a2
*
*	void HeisDiffDate(const unsigned short day,
*	    const unsigned short month, const int year, int days,
*	    unsigned short *dday, unsigned short *dmonth, int *dyear);
*
*   FUNCTION
*	Returns the date which lies diffdays before/after the specified date.
*
*   INPUTS
*	day      - day of the date
*	month    - month of the date
*	year     - year of the date
*	diffdays - difference to the date in days
*
*   RESULT
*	dday   - Destination day
*	dmonth - Destination month
*	dyear  - Destination year
*
*   EXAMPLE
*	...
*	HeisDiffDate(23,1,1994,7,&dday,&dmonth,&dyear);
*	...
*
*   NOTES
*	It is better to use this function only from -7 to 8000!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	HeisDayDiff(),HeisMonthDays(),JulianDiffDate(),GregorianDiffDate()
*
*****************************************************************************
*
*
*/

 {int ddays;

  *dday = day;
  *dmonth = month;
  *dyear = year;
  if (days >= 0)
    {/* add */
     ddays = (int)HeisDayDiff(*dday,*dmonth,*dyear,1,1,(*dyear)+1);
     while (days >= ddays)
       {/* years */
        *dday = 1;
        *dmonth = 1;
        (*dyear)++;
        days -= ddays;
        ddays = (int)HeisDayDiff(*dday,*dmonth,*dyear,1,1,(*dyear)+1);
       }
     ddays = (int)HeisDayDiff(*dday,*dmonth,*dyear,1,(*dmonth+1),*dyear);
     while (days >= ddays)
       {/* months */
        *dday = 1;
        (*dmonth)++;
        days -= ddays;
        ddays = (int)HeisDayDiff(*dday,*dmonth,*dyear,1,(*dmonth)+1,*dyear);
       }
     if (days > 0)
       {/* days */
        *dday += (unsigned short)days;
       }
    }
  else
    {/* sub */
     ddays = (int)HeisDayDiff(*dday,*dmonth,*dyear,31,12,(*dyear)-1);
     while (days <= ddays)
       {/* years */
        *dday = 31;
        *dmonth = 12;
        (*dyear)--;
        days -= ddays;
        ddays = (int)HeisDayDiff(*dday,*dmonth,*dyear,31,12,(*dyear)-1);
       }
     ddays = (int)HeisDayDiff(*dday,*dmonth,*dyear,HeisMonthDays((*dmonth)-1,*dyear),(*dmonth)-1,*dyear);
     while (days <= ddays)
       {/* months */
        *dday = HeisMonthDays((*dmonth)-1,*dyear);
        (*dmonth)--;
        days -= ddays;
        ddays = (int)HeisDayDiff(*dday,*dmonth,*dyear,HeisMonthDays((*dmonth)-1,*dyear),(*dmonth)-1,*dyear);
       }
     if (days < 0)
       *dday -= (unsigned short)abs(days);
    }
 }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   unsigned int __saveds __asm JYearToScaliger(register __d0 const int year)
#else
   unsigned int JYearToScaliger(const int year)
#endif

/*
******* Date/JYearToScaliger ************************************************
*
*   NAME
*	JYearToScaliger -- Returns the year as Scaliger year. (V33)
*
*   SYNOPSIS
*	syear = JYearToScaliger(year);
*	 d0			 d0
*
*	unsigned int JYearToScaliger(const int year);
*
*   FUNCTION
*	Returns the Scaliger year.
*
*   INPUTS
*	year     - Julian year
*
*   RESULT
*	syear - The Scaliger year
*
*   EXAMPLE
*	...
*	syear = JYearToScaliger(1582);
*	...
*
*   NOTES
*	It is better to use this function only from -7 to 1582!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	GYearToScaliger(),HYearToScaliger()
*
*****************************************************************************
*
*
*/

 {if ((year < 0) && (year > -4714))
    return((unsigned int)(4714+year));
  else
    if ((year > 0) && (year < 3268))
      return((unsigned int)(4713+year));
    else
      return(0);
 }


#ifdef __MakeLib
   unsigned int __saveds __asm GYearToScaliger(register __d0 const int year)
#else
   unsigned int GYearToScaliger(const int year)
#endif

/*
******* Date/GYearToScaliger ************************************************
*
*   NAME
*	GYearToScaliger -- Returns the year as Scaliger year. (V33)
*
*   SYNOPSIS
*	syear = GYearToScaliger(year);
*	 d0			 d0
*
*	unsigned int GYearToScaliger(const int year);
*
*   FUNCTION
*	Returns the Scaliger year.
*
*   INPUTS
*	year     - Gregorian year
*
*   RESULT
*	syear - The Scaliger year
*
*   EXAMPLE
*	...
*	syear = GYearToScaliger(1994);
*	...
*
*   NOTES
*	It is better to use this function only from -7 to 3200!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	JYearToScaliger(),HYearToScaliger()
*
*****************************************************************************
*
*
*/

 {/* if other calcs are better use here! */
  return(JYearToScaliger(year));
 }


#ifdef __MakeLib
   unsigned int __saveds __asm HYearToScaliger(register __d0 const int year)
#else
   unsigned int HYearToScaliger(const int year)
#endif

/*
******* Date/HYearToScaliger ************************************************
*
*   NAME
*	HYearToScaliger -- Returns the year as Scaliger year. (V33)
*
*   SYNOPSIS
*	syear = HYearToScaliger(year);
*	 d0			 d0
*
*	unsigned int HYearToScaliger(const int year);
*
*   FUNCTION
*	Returns the Scaliger year.
*
*   INPUTS
*	year     - Heis year
*
*   RESULT
*	syear - The Scaliger year
*
*   EXAMPLE
*	...
*	syear = HYearToScaliger(1994);
*	...
*
*   NOTES
*	It is better to use this function only from -7 to 8000!
*
*   BUGS
*	The Scaliger period is defined to 3268!!!.
*
*   SEE ALSO
*	JYearToScaliger(),GYearToScaliger()
*
*****************************************************************************
*
*
*/

 {/* for compatiblities if GYearToScaliger will be changed */
  return(GYearToScaliger(year));
 }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   int __saveds __asm ScaligerYearToJ(register __d0 const unsigned int syear)
#else
   int ScaligerYearToJ(const unsigned int syear)
#endif

/*
******* Date/ScaligerYearToJ ************************************************
*
*   NAME
*	ScaligerYearToJ -- Returns the Scaliger year as Julian year. (V33)
*
*   SYNOPSIS
*	year = ScaligerYearToJ(syear);
*	 d0			d0
*
*	int ScaligerYearToJ(const unsigned int syear);
*
*   FUNCTION
*	Returns the Julian year of a Scaliger year.
*
*   INPUTS
*	syear     - Scaliger year
*
*   RESULT
*	year - The Julian year
*
*   EXAMPLE
*	...
*	year = ScaligerYearToJ(4800);
*	...
*
*   NOTES
*	It is better to use this function only from 4707 to 6295!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	ScaligerYearToG(),ScaligerYearToH()
*
*****************************************************************************
*
*
*/

 {if (syear < 4714)
    return((int)(4714+syear));
  else
    return((int)(syear-4713));
 }


#ifdef __MakeLib
   int __saveds __asm ScaligerYearToG(register __d0 const unsigned int syear)
#else
   int ScaligerYearToG(const unsigned int syear)
#endif

/*
******* Date/ScaligerYearToG ************************************************
*
*   NAME
*	ScaligerYearToG -- Returns the Scaliger year as Gregorian year. (V33)
*
*   SYNOPSIS
*	year = ScaligerYearToG(syear);
*	 d0			d0
*
*	int ScaligerYearToG(const unsigned int syear);
*
*   FUNCTION
*	Returns the Gregorian year of a Scaliger year.
*
*   INPUTS
*	syear     - Scaliger year
*
*   RESULT
*	year - The Gregorian year
*
*   EXAMPLE
*	...
*	year = ScaligerYearToG(6400);
*	...
*
*   NOTES
*	It is better to use this function only from 4707 to 7981!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	ScaligerYearToJ(),ScaligerYearToH()
*
*****************************************************************************
*
*
*/

 {return(ScaligerYearToJ(syear));
 }


#ifdef __MakeLib
   int __saveds __asm ScaligerYearToH(register __d0 const unsigned int syear)
#else
   int ScaligerYearToH(const unsigned int syear)
#endif

/*
******* Date/ScaligerYearToH ************************************************
*
*   NAME
*	ScaligerYearToH -- Returns the Scaliger year as Heis year. (V33)
*
*   SYNOPSIS
*	year = ScaligerYearToH(syear);
*	 d0			d0
*
*	int ScaligerYearToH(const unsigned int syear);
*
*   FUNCTION
*	Returns the Heis year of a Scaliger year.
*
*   INPUTS
*	syear     - Scaliger year
*
*   RESULT
*	year - The Heis year
*
*   EXAMPLE
*	...
*	year = ScaligerYearToH(7000);
*	...
*
*   NOTES
*	It is better to use this function only from 4707 to 7981!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	ScaligerYearToJ(),ScaligerYearToG()
*
*****************************************************************************
*
*
*/

 {/* for compatibilitie if ScaligerYearToG is changed! */
  return(ScaligerYearToG(syear));
 }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   unsigned long __saveds __asm JSYearToJD(register __d0 const unsigned int syear)
#else
   unsigned long JSYearToJD(const unsigned int syear)
#endif

/*
******* Date/JSYearToJD *****************************************************
*
*   NAME
*	JSYearToJD -- Calcs the JD from a Scaliger year. (V33)
*
*   SYNOPSIS
*	jd = JSYearToJD(syear);
*	d0		 d0
*
*	unsigned long JSYearToJD(const unsigned int syear);
*
*   FUNCTION
*	Returns the Julianday of a Scaliger year.
*
*   INPUTS
*	syear     - Scaliger year
*
*   RESULT
*	jd - The Julianday
*
*   EXAMPLE
*	...
*	jd = JSYearToJD(4800);
*	...
*
*   NOTES
*	It is better to use this function only from 4707 to 6295!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	GSYearToJD(),HSYearToJD()
*
*****************************************************************************
*
*
*/

 {return(((unsigned long)syear-1)*365+((unsigned long)syear+2) / 4);
 }


#ifdef __MakeLib
   unsigned long __saveds __asm GSYearToJD(register __d0 const unsigned int syear)
#else
   unsigned long GSYearToJD(const unsigned int syear)
#endif

/*
******* Date/GSYearToJD *****************************************************
*
*   NAME
*	GSYearToJD -- Calcs the JD from a Scaliger year. (V33)
*
*   SYNOPSIS
*	jd = GSYearToJD(syear);
*	d0		 d0
*
*	unsigned long GSYearToJD(const unsigned int syear);
*
*   FUNCTION
*	Returns the Julianday of a Scaliger year.
*
*   INPUTS
*	syear     - Scaliger year
*
*   RESULT
*	jd - The Julianday
*
*   EXAMPLE
*	...
*	jd = GSYearToJD(4800);
*	...
*
*   NOTES
*	It is better to use this function only from 4707 to 7981!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	JSYearToJD(),HSYearToJD()
*
*****************************************************************************
*
*
*/

 {if (syear < 6296)
    {/* 1583 */
     return(JSYearToJD(syear));
    }
  else
    return(JSYearToJD(6296)-10+(unsigned long)(GregorianDayDiff(1,1,1583,1,1,ScaligerYearToG(syear))));
 }


#ifdef __MakeLib
   unsigned long __saveds __asm HSYearToJD(register __d0 const unsigned int syear)
#else
   unsigned long HSYearToJD(const unsigned int syear)
#endif

/*
******* Date/HSYearToJD *****************************************************
*
*   NAME
*	HSYearToJD -- Calcs the JD from a Scaliger year. (V33)
*
*   SYNOPSIS
*	jd = HSYearToJD(syear);
*	d0		 d0
*
*	unsigned long HSYearToJD(const unsigned int syear);
*
*   FUNCTION
*	Returns the Julianday of a Scaliger year.
*
*   INPUTS
*	syear     - Scaliger year
*
*   RESULT
*	jd - The Julianday
*
*   EXAMPLE
*	...
*	jd = HSYearToJD(6700);
*	...
*
*   NOTES
*	It is better to use this function only from 4707 to 7981!
*	In this version only GSYearToJD() is called, because the
*	Scaliger period is only valid to 3268
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	JSYearToJD(),GSYearToJD()
*
*****************************************************************************
*
*
*/

 {return(GSYearToJD(syear));
 }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   unsigned long __saveds __asm JDtoMJD(register __d0 const unsigned long jd)
#else
   unsigned long JDtoMJD(const unsigned long jd)
#endif

/*
******* Date/JDtoMJD ********************************************************
*
*   NAME
*	JDtoMJD -- Switches from JD to MJD. (V33)
*
*   SYNOPSIS
*	mjd = JDtoMJD(jd);
*	d0	      d0
*
*	unsigned long JDtoMJD(const unsigned long jd);
*
*   FUNCTION
*	Returns the Modified Julianday of a Julianday.
*
*   INPUTS
*	jd - Julianday
*
*   RESULT
*	mjd - The Modified Julianday
*
*   EXAMPLE
*	...
*	mjd = JDtoMJD(2449354);
*	...
*
*   NOTES
*	none
*
*   BUGS
*	Only use this function for jd > 2400001, because mjd is only
*	defined for this, otherwise system will crash!
*
*   SEE ALSO
*	MJDtoJD()
*
*****************************************************************************
*
*
*/

 {return(jd-2400001L);
 }


#ifdef __MakeLib
   unsigned long __saveds __asm MJDtoJD(register __d0 const unsigned long mjd)
#else
   unsigned long MJDtoJD(const unsigned long mjd)
#endif

/*
******* Date/MJDtoJD ********************************************************
*
*   NAME
*	MJDtoJD -- Switches from MJD to JD. (V33)
*
*   SYNOPSIS
*	jd = MJDtoJD(mjd);
*	d0	     d0
*
*	unsigned long MJDtoJD(const unsigned long mjd);
*
*   FUNCTION
*	Returns the Julianday of a Modified Julianday.
*
*   INPUTS
*	mjd - Modified Julianday
*
*   RESULT
*	jd - The Julianday
*
*   EXAMPLE
*	...
*	jd = JDtoMJD(49353);
*	...
*
*   NOTES
*	none
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	MJDtoJD()
*
*****************************************************************************
*
*
*/

 {return(mjd+2400001L);
 }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   unsigned long __saveds __asm JulianToJD(register __d0 const unsigned short day, register __d1 const unsigned short month, register __d2 const int year)
#else
   unsigned long JulianToJD(const unsigned short day, const unsigned short month, const int year)
#endif

/*
******* Date/JulianToJD *****************************************************
*
*   NAME
*	JulianToJD -- Returns the JD for a date. (V33)
*
*   SYNOPSIS
*	jd = JulianToJD(day,month,year);
*	d0		d0   d1    d2
*
*	unsigned long JulianToJD(const unsigned short day,
*	    const unsigned short month, const int year);
*
*   FUNCTION
*	Returns the JD for a Julian date.
*
*   INPUTS
*	day      - day of the date to convert
*	month    - month of the date to convert
*	year     - year of the date to convert
*
*   RESULT
*	jd - This is the JD
*
*   EXAMPLE
*	...
*	jd = JulianToJD(23,1,1994);
*	...
*
*   NOTES
*	It is better to use this function only from -7 to 1582!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	JSYearToJD(),JYearToScaliger(),JulianDayDiff(),GregorianToJD(),
*	HeisToJD()
*
*****************************************************************************
*
*
*/

 {return(JSYearToJD(JYearToScaliger(year))+(unsigned long)(JulianDayDiff(1,1,year,day,month,year)));
 }


#ifdef __MakeLib
   unsigned long __saveds __asm GregorianToJD(register __d0 const unsigned short day, register __d1 const unsigned short month, register __d2 const int year)
#else
   unsigned long GregorianToJD(const unsigned short day, const unsigned short month, const int year)
#endif

/*
******* Date/GregorianToJD **************************************************
*
*   NAME
*	GregorianToJD -- Returns the JD for a date. (V33)
*
*   SYNOPSIS
*	jd = GregorianToJD(day,month,year);
*	d0		   d0   d1    d2
*
*	unsigned long GregorianToJD(const unsigned short day,
*	    const unsigned short month, const int year);
*
*   FUNCTION
*	Returns the JD for a Gregorian date.
*
*   INPUTS
*	day      - day of the date to convert
*	month    - month of the date to convert
*	year     - year of the date to convert
*
*   RESULT
*	jd - This is the JD
*
*   EXAMPLE
*	...
*	jd = GregorianToJD(23,1,1994);
*	...
*
*   NOTES
*	It is better to use this function only from -7 to 3200!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	GSYearToJD(),GYearToScaliger(),GregorianDayDiff(),JulianToJD(),
*	HeisToJD()
*
*****************************************************************************
*
*
*/

 {return(GSYearToJD(GYearToScaliger(year))+(unsigned long)(GregorianDayDiff(1,1,year,day,month,year)));
 }


#ifdef __MakeLib
   unsigned long __saveds __asm HeisToJD(register __d0 const unsigned short day, register __d1 const unsigned short month, register __d2 const int year)
#else
   unsigned long HeisToJD(const unsigned short day, const unsigned short month, const int year)
#endif

/*
******* Date/HeisToJD *******************************************************
*
*   NAME
*	HeisToJD -- Returns the JD for a date. (V33)
*
*   SYNOPSIS
*	jd = HeisToJD(day,month,year);
*	d0	      d0   d1    d2
*
*	unsigned long HeisToJD(const unsigned short day,
*	    const unsigned short month, const int year);
*
*   FUNCTION
*	Returns the JD for a Heis date.
*
*   INPUTS
*	day      - day of the date to convert
*	month    - month of the date to convert
*	year     - year of the date to convert
*
*   RESULT
*	jd - This is the JD
*
*   EXAMPLE
*	...
*	jd = HeisToJD(23,1,1994);
*	...
*
*   NOTES
*	It is better to use this function only from -7 to 3268!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	HSYearToJD(),HYearToScaliger(),HeisDayDiff(),JulianToJD(),HeisToJD()
*
*****************************************************************************
*
*
*/

 {return(HSYearToJD(HYearToScaliger(year))+(unsigned long)(HeisDayDiff(1,1,year,day,month,year)));
 }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   float __saveds __asm TimeToJD(register __d0 const unsigned short hour, register __d1 const unsigned short min, register __d2 const unsigned short sec)
#else
   float TimeToJD(const unsigned short hour, const unsigned short min, const unsigned short sec)
#endif

/*
******* Date/TimeToJD *******************************************************
*
*   NAME
*	TimeToJD -- Returns the JD for a time. (V33)
*
*   SYNOPSIS
*	jd = TimeToJD(hour,min,sec);
*	d0	       d0   d1  d2
*
*	float TimeToJD(const unsigned short hour, const unsigned short min,
*	    const unsigned short sec);
*
*   FUNCTION
*	Returns the JD for a specified time.
*
*   INPUTS
*	hour - hour of the time to convert
*	min  - minute of the time to convert
*	sec  - sec. of the time to convert
*
*   RESULT
*	jd - This is the JD time
*
*   EXAMPLE
*	...
*	jd = TimeToJD(16,33,0);
*	...
*
*   NOTES
*	none
*
*   BUGS
*	There is no check, if the specified time is a valid time!
*
*   SEE ALSO
*	JDToTime()
*
*****************************************************************************
*
*
*/

 {return((float)(((unsigned long)hour*3600+(unsigned int)min*60+sec) / 86400.0));
 }


#ifdef __MakeLib
   void __saveds __asm JDToTime(register __d0 float jd, register __a0 unsigned short *rhour, register __a1 unsigned short *rmin, register __a2 unsigned short *rsec)
#else
   void JDToTime(float jd, unsigned short *rhour, unsigned short *rmin, unsigned short *rsec)
#endif

/*
******* Date/JDToTime *******************************************************
*
*   NAME
*	JDToTime -- Returns the real time for a JD time. (V33)
*
*   SYNOPSIS
*	JDToTime(jd,rhour,rmin,rsec);
*		 d0  a0    a1   a2
*
*	void JDToTime(float jd, unsigned short *rhour, unsigned short *rmin,
*	    unsigned short *rsec);
*
*   FUNCTION
*	Returns the real time for a JD time.
*
*   INPUTS
*	jd - JD time
*
*   RESULT
*	rhour - 24 hour real time
*	rmin  - real minutes
*	rsec  - real seconds
*
*   EXAMPLE
*	...
*	JDToTime(0.76543,&rhour,&rmin,&rsec);
*	...
*
*   NOTES
*	none.
*
*   BUGS
*	If jd is > 0 (including days) there will be occur arithmetic bugs!
*
*   SEE ALSO
*	TimeToJD()
*
*****************************************************************************
*
*
*/

 {unsigned long sec;

  if (jd > 0.0)
    jd -= floor(jd);
  sec = (unsigned long)(jd * 86400.0);
  *rhour = (unsigned short)(sec / 3600);
  sec -= (*rhour) * 3600;
  *rmin = (unsigned short)(sec / 60);
  sec -= (*rmin) * 60;
  *rsec = (unsigned short)sec;
 }

 /* ----internal----------------------------------------------------------- */

 static unsigned short GregorianSZ(const unsigned int year)

/*
*****i* Date/GregorianSZ ****************************************************
*
*   NAME
*	GregorianSZ -- Returns the 'Sonnenzirkel' (V33)
*
*   SYNOPSIS
*	sz = GregorianSZ(year);
*
*	unsigned short GregorianSZ(const unsigned int year);
*
*   FUNCTION
*	Returns the 'Sonnenzirkel' of a year.
*
*   INPUTS
*	year     - For this year the 'Sonnenzirkel' is calculatet.
*
*   RESULT
*	sz - The 'Sonnenzirkel' for the specified year.
*
*   EXAMPLE
*	...
*	sz = GregorianSZ(1994);
*	...
*
*   NOTES
*	Use this only for 1582 to 4100!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	GYearToScaliger()
*
*****************************************************************************
*
*
*/

 {unsigned short gz;
  gz = (unsigned short)(GYearToScaliger(year) % 28);
  if (gz==0)
    gz = 28;
  return(gz);
 }


 static unsigned short GregorianGZ(const unsigned int year)

/*
*****i* Date/GregorianGZ ****************************************************
*
*   NAME
*	GregorianGZ -- Returns the 'Goldene Zahl' (golden number) (V33)
*
*   SYNOPSIS
*	gz = GregorianGZ(year);
*
*	unsigned short GregorianGZ(const unsigned int year);
*
*   FUNCTION
*	Returns the 'Goldene Zahl' of a year.
*
*   INPUTS
*	year     - For this year the 'Goldene Zahl' is calculatet.
*
*   RESULT
*	gz - The 'Goldene Zahl' for the specified year.
*
*   EXAMPLE
*	...
*	gz = GregorianGZ(1994);
*	...
*
*   NOTES
*	Use this only for 1582 to 4100!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	GYearToScaliger()
*
*****************************************************************************
*
*
*/

 {unsigned int syear;

  syear = GYearToScaliger(year) % 19;
  if (syear == 0)
    syear = 19;
  return((unsigned short)syear);
 }


 static unsigned short GEP(const unsigned int year)

/*
*****i* Date/GEP ************************************************************
*
*   NAME
*	GEP -- Internal function to help calculating the 'EP' (V33)
*
*   SYNOPSIS
*	hep = GEP(year);
*
*	unsigned short GEP(const unsigned int year);
*
*   FUNCTION
*	Internal function to help calculating the 'EP'
*
*   INPUTS
*	year - This is the year for which the help EP is to be
*	    calculatetd
*
*   RESULT
*	hep - The help value for the EP calculation.
*
*   EXAMPLE
*	...
*	hep = GEP(1994);
*	...
*
*   NOTES
*	Use this only for 1582 to 4200!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*
*
*****************************************************************************
*
*
*/

   {unsigned short	century,decade;
    int			ep;

    ep = 1; /* 1582 */
    century = (unsigned short)(year / 100);
    decade = (unsigned short)(year - century * 100);
    if (year < 1701)
      return(1);
    else
      if (year < 1800)
        return(0);
      else
        {ep -= (int)(((century) % 4) + (((century-16) / 4) * 3));
         if ((decade == 0) && ((century % 4) > 0))
           ep++;
         ep += (int)((century-18) / 3);
         if ((((century-18) % 3) > 0) || (decade > 0))
           ep++;
         if (ep > 29)
           ep %= 30;
         if (ep < 0)
           ep += 30;
         return((unsigned short)ep);
        }
   }


 static unsigned short GregorianEP(const unsigned int year)

/*
*****i* Date/GregorianEP ****************************************************
*
*   NAME
*	GregorianEP -- Returns the 'Epakte' (V33)
*
*   SYNOPSIS
*	ep = GregorianEP(year);
*
*	unsigned short GregorianEP(const unsigned int year);
*
*   FUNCTION
*	Returns the 'Epakte' of a year.
*
*   INPUTS
*	year     - For this year the 'Epakte' is calculatet.
*
*   RESULT
*	ep - The 'Epakte' for the specified year.
*
*   EXAMPLE
*	...
*	ep = GregorianEP(1994);
*	...
*
*   NOTES
*	Use this only for 1582 to 4100!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	GEP(),GregorianGZ()
*
*****************************************************************************
*
*
*/

 {unsigned short ep;

  if (year >= 1582)
   {
    ep = (unsigned short)(((GregorianGZ(year)-1)*11 + GEP(year)) % 30);
    if (ep == 0)
      ep = 30;
    return(ep);
   }
  else
    return(31);
 }


 static unsigned short GregorianJHStartSB(const unsigned short century)

/*
*****i* Date/GregorianJHStartSB *********************************************
*
*   NAME
*	GregorianJHStartSB -- Returns the 'Sonntagsbuchstabe' (V33)
*
*   SYNOPSIS
*	csb = GregorianJHStartSB(century);
*
*	unsigned short GregorianJHStartSB(const unsigned short century);
*
*   FUNCTION
*	Returns start 'SB' for a century.
*
*   INPUTS
*	century - For this century the start 'SB' is calculatet.
*
*   RESULT
*	csb - The start 'SB' for the specified century.
*
*   EXAMPLE
*	...
*	csb = GregorianJHStartSB(19);
*	...
*
*   NOTES
*	Use this only for 15 to 31!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	GregorianJHStartSB()
*
*****************************************************************************
*
*
*/

 {unsigned short sb;

  if (century == 15)
    return(4);
  else
   {
    sb = GregorianJHStartSB(century-1);
    if ((century % 4) > 0)
      sb++;
    sb %= 7;
    if (sb == 0)
      sb = 7;
    return(sb);
   }
 }


 static unsigned short GregorianJHSB(const unsigned int year)

/*
*****i* Date/GregorianSB ****************************************************
*
*   NAME
*	GregorianJHSB -- Returns the 'Sonntagsbuchstabe' (V33)
*
*   SYNOPSIS
*	sb = GregorianJHSB(year);
*
*	unsigned short GregorianJHSB(const unsigned int year);
*
*   FUNCTION
*	Returns the start 'SB' for a century year.
*
*   INPUTS
*	year - For this century year the start 'SB' is calculatet.
*
*   RESULT
*	sb - The start 'SB' for the specified year.
*
*   EXAMPLE
*	...
*	sb = GregorianJHSB(1994);
*	...
*
*   NOTES
*	Use this only for 1583 to 3199!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	GregorianLeapYear(),GregorianJHStartSB()
*
*****************************************************************************
*
*
*/

 {
  if (((year % 100) == 0) && (!GregorianLeapYear((int)year)))
    return((unsigned short)(((year / 100) % 4) *2 +1));
  else
    return(GregorianJHStartSB(year / 100));
 }


 static unsigned short GregorianSB(const unsigned int year)

/*
*****i* Date/GregorianSB ****************************************************
*
*   NAME
*	GregorianSB -- Returns the 'Sonntagsbuchstabe' (V33)
*
*   SYNOPSIS
*	sb = GregorianSB(year);
*
*	unsigned short GregorianSB(const unsigned int year);
*
*   FUNCTION
*	Returns the 'SB' for a year.
*
*   INPUTS
*	year - For this year the 'SB' is calculatet.
*
*   RESULT
*	sb - The 'SB' for the specified year.
*	    This means the day the first Sunday lies on :)
*
*   EXAMPLE
*	...
*	sb = GregorianSB(1994);
*	...
*
*   NOTES
*	Use this only for 1583 to 3199!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	GregorianLeapYear(),GregorianSZ(),GregorianJHStartSB()
*
*****************************************************************************
*
*
*/

 {unsigned short sz,csb,i;

  if (((year % 100) == 0) && (!GregorianLeapYear((int)year)))
    return((unsigned short)(((year / 100) % 4) *2 +1));
  else
   {
    sz = GregorianSZ(year);
    csb = GregorianJHStartSB(year / 100);
    if (sz == 28)
      return(csb);
    else
     {
      for (i=27;i>=sz;i--)
       {
        csb++;
        if (csb == 8)
          csb = 1;
        if (((i-1) % 4) == 0)
         {
          csb++;
          if (csb == 8)
            csb =1;
         }
       }
      return(csb);
     }
   }
 }


 static unsigned short MoonMonthAge(const unsigned short month, unsigned short ep, const int year)

/*
*****i* Date/MoonMonthAge ***************************************************
*
*   NAME
*	MoonMonthAge -- Calculates the age of the moon on month start (V33)
*
*   SYNOPSIS
*	ep = MoonMonthAge(month,ep);
*
*	unsigned short MoonMonthAge(unsigned short month, unsigned short ep,
*	    const int year);
*
*   FUNCTION
*	Returns the age of the moon on the start of a month.
*
*   INPUTS
*	month - Month for which the moonage is needed.
*	ep    - 'Epakte' of the newyears-day.
*
*   RESULT
*	ep - The moonage on the 1. of the specified month.
*
*   EXAMPLE
*	...
*	ep = MoonMonthAge(2,17); / * 17 is for 1994 * /
*	...
*
*   NOTES
*	This is only a experimental version!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	MoonMonthAge(),GregorianMonthDays()
*
*****************************************************************************
*
*
*/

   {if (month == 1)
      return(ep);
    else
      if (month % 2 == 0)
        ep = (unsigned short)((MoonMonthAge(month-1,ep,year) + GregorianMonthDays(month-1,year)) % 29);
      else
        ep = (unsigned short)((MoonMonthAge(month-1,ep,year) + GregorianMonthDays(month-1,year)) % 30);
      return(ep);
   }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   unsigned short __saveds __asm GregorianMoonAge(register __d0 const unsigned short day, register __d1 const unsigned short month, register __d2 const int year)
#else
   unsigned short GregorianMoonAge(const unsigned short day, const unsigned short month, const int year)
#endif

/*
******* Date/GregorianMoonAge ***********************************************
*
*   NAME
*	GregorianMoonAge -- Returns the age of the moon (V33)
*
*   SYNOPSIS
*	ep = GregorianMoonAge(day,month,year);
*	d0		       d0   d1   d2
*
*	unsigned short GregorianMoonAge(const unsigned short day,
*	    const unsigned short month, const int year);
*
*   FUNCTION
*	Returns the age of the moon on a specified date.
*
*   INPUTS
*	day   - For this day the age is calculated.
*	month - For this month the age is calculated.
*	year  - For this year the age is calculated.
*
*   RESULT
*	ep - The age of the moon on the specified date.
*
*   EXAMPLE
*	...
*	ep = GregorianMoonAge(18,9,1994);
*	...
*
*   NOTES
*	Use this only for 1582 to 4100!
*	This is only a experimental version!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*	MoonMonthAge(),GregorianEP()
*
*****************************************************************************
*
*
*/

 {unsigned short ep;

  ep = GregorianEP(year);
  ep = MoonMonthAge(month,ep,year);
  ep += day -1;
  if (month > 1)
    if (month % 2 == 0)
      {ep %= 30;
       if (ep == 0)
         ep = 30;
      }
    else
      {ep %= 29;
       if (ep == 0)
         ep = 29;
      }
  else
    if (ep > 29)
      ep %= 29;
  return(ep);
 }

/*
 void GregorianEasterOld(const int year, unsigned short *dday, unsigned short *dmonth)

/ *
*****i* Date/GregorianEasterOld ********************************************
*
*   NAME
*	GregorianEasterOld -- Returns the date of eastern in a year (V33)
*
*   SYNOPSIS
*	GregorianEasterOld(year,dday,dmonth);
*
*	void GregorianEasterOld(const int year, unsigned short *dday,
*	    unsigned short *dmonth);
*
*   FUNCTION
*	Returns the date of eastern for a specified year.
*
*   INPUTS
*	year  - eastern is calculated for this year
*
*   RESULT
*	dday   - day of easter-Sunday
*	dmonth - month of easter-Sunday
*
*   EXAMPLE
*	...
*	GregorianEasterOld(1994,&dday,&dmonth);
*	...
*
*   NOTES
*	Use this only for 1582 to 4100!
*	This is only a experimental version!
*
*   BUGS
*	In some years eastern lies one week behind!
*
*   SEE ALSO
*	GregorianMoonAge(),GregorianEP(),GregorianDaysAfterWeekday()
*
*****************************************************************************
*
*
* /

 {unsigned short ep;

  *dday = 21;
  *dmonth = 3;
  ep = GregorianMoonAge(21,3,year);
  if (ep < 14)
    *dday += (14-ep);
  else
    *dday += (29-ep) + 13;
  if ((*dday) > 31)
    {*dday -= 31;
     (*dmonth)++;
    }
  *dday += GregorianDaysAfterWeekday(*dday,*dmonth,year,Sunday);
  if ((*dday) > 31)
    {*dday -= 31;
     (*dmonth)++;
    }
 }
*/


#ifdef __MakeLib
   void __saveds __asm GregorianEaster(register __d0 const int year, register __a0 unsigned short *dday, register __a1 unsigned short *dmonth)
#else
   void GregorianEaster(const int year, unsigned short *dday, unsigned short *dmonth)
#endif

/*
******* Date/GregorianEaster ************************************************
*
*   NAME
*	GregorianEaster -- Returns the date of eastern in a year (V33)
*
*   SYNOPSIS
*	GregorianEaster(year,dday,dmonth);
*			 d0   a0    a1
*
*	void GregorianEaster(const int year, unsigned short *dday,
*	    unsigned short *dmonth);
*
*   FUNCTION
*	Returns the date of eastern for a specified year.
*
*   INPUTS
*	year  - eastern is calculated for this year
*
*   RESULT
*	dday   - day of easter-Sunday
*	dmonth - month of easter-Sunday
*
*   EXAMPLE
*	...
*	GregorianEaster(1994,&dday,&dmonth);
*	...
*
*   NOTES
*	Use this only for 1900 to 2099!
*	Tested for 1977-1994! But this formula is from Gauﬂ - so it must be
*	correct :) but extended by me (hope this will be a good thing too!)
*
*   BUGS
*	None.
*
*   SEE ALSO
*	GEP(),GregorianJHSB()
*
*****************************************************************************
*
*
*/

 {unsigned short a,d,e,f;
  short M,N;

  M = (short)((30 - GEP(year)) - 7);
  if (M < 0)
    M += 30;
  N = (short)(GregorianJHSB(year)-2);
  if (N<1)
    N += 7;
  a = (unsigned short)(year % 19);
  d = (unsigned short)((19*(unsigned int)a+M) % 30);
  e = (unsigned short)((2*(unsigned int)(year % 4)+4*(unsigned int)(year % 7)+6*(unsigned int)(d)+N) % 7);
  f = (unsigned short)(d+e);

  if (f < 10)
   {/* m‰rz */
    *dmonth = 3;
    *dday = (unsigned short)(22+f);
   }
  else
   {/* april */
    *dmonth = 4;
    *dday = (unsigned short)(f-9);
    if (*dday==26)
      *dday = 19;
    else
      if ((*dday==25) && (d==28) && (a>10))
        *dday = 18;
   }
 }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   short __saveds __asm TimeZoneFactor(register __d0 const short degree)
#else
   short TimeZoneFactor(const short degree)
#endif

/*
******* Date/TimeZoneFactor *************************************************
*
*   NAME
*	TimeZoneFactor -- Returns the value you have to add to GMT time (V33)
*
*   SYNOPSIS
*	addhours = TimeZoneFactor(degrees);
*	   d0			    d0
*
*	short TimeZoneFactor(const short degree);
*
*   FUNCTION
*	This gives you the hours you have to add to GMT time,
*	specified on the fact, that a timezone is 15 degrees
*	and that GMT is centered on 0 degrees!
*
*   INPUTS
*	degrees - Position of timezone you live in
*	(from -180 east to +180 west)
*
*   RESULT
*	addhours - Time to add to GMT time to get your locale zone time
*	    (-12 to +12)
*
*   EXAMPLE
*	...
*	addhours = TimeZoneFactor(-8);
*	...
*
*   NOTES
*	none
*
*   BUGS
*	No errorcheck, if you put in valid degrees (-180 to +180)
*	Only full degrees are supportet, keep sure that you
*	round in the right way for 0.x degree places
*	I am not sure about the correct +/- behaviour!!!
*
*   SEE ALSO
*
*
*****************************************************************************
*
*
*/

 {if (degree >= 0)
    return((short)(degree / 15.0 + 0.5));
  else
    return((short)(degree / 15.0 - 0.5));
 }


#ifdef __MakeLib
   long __saveds __asm LMT(register __d0 const unsigned long secs, register __d1 const float meridiandegree, register __d2 const float posdegree)
#else
   long LMT(const unsigned long secs, const float meridiandegree, const float posdegree)
#endif

/*
******* Date/LMT ************************************************************
*
*   NAME
*	LMT -- Calculates your local time in your timezone (V33)
*
*   SYNOPSIS
*	secs = LMT(secs,meridian,pos);
*	 d0	    d0     d1    d2
*
*	unsigned long LMT(const unsigned long secs,
*	    const float meridiandegree, const float posdegree);
*
*   FUNCTION
*	Calculates your Local Mean Time of your place!
*
*   INPUTS
*	secs     - Seconds of the running day (hours*3600+min*60+sec)
*	meridian - Degrees of your timezone-meridian
*	pos      - Degrees of your place
*
*   RESULT
*	secs - Local seconds of the running day
*
*   EXAMPLE
*	...
*	secs = LMT(76080,-15.0,-8.923055556);
*	...
*
*   NOTES
*	none
*
*   BUGS
*	No errorcheck, if you put in valid degrees (-180 to +180)
*
*   SEE ALSO
*
*
*****************************************************************************
*
*
*/

 {return((long)secs + (long)((meridiandegree / 15.0 - posdegree / 15.0)*3600.0));
 }


#ifdef __MakeLib
   unsigned long __saveds __asm TimeToSec(register __d0 const unsigned short hour, register __d1 const unsigned short min, register __d2 const unsigned short sec)
#else
   unsigned long TimeToSec(const unsigned short hour, const unsigned short min, const unsigned short sec)
#endif

/*
******* Date/TimeToSec ******************************************************
*
*   NAME
*	TimeToSec -- Returns the time in seconds (V33)
*
*   SYNOPSIS
*	secs = TimeToSec(hour,min,sec);
*	 d0		  d0   d1  d2
*
*	unsigned long TimeToSec(const unsigned short hour,
*	    const unsigned short min, const unsigned short sec);
*
*   FUNCTION
*	Gives you back the time in seconds
*
*   INPUTS
*	hour - hours you want (0-23)
*	min  - minutes you want (0-59)
*	sec  - seconds you want (0-59)
*
*   RESULT
*	secs - Time in seconds
*
*   EXAMPLE
*	...
*	secs = TimeToSec(21,15,00);
*	...
*
*   NOTES
*	Don't forget to convert AM/PM time to 24h time!
*
*   BUGS
*	No errorcheck, if you use a valid time
*
*   SEE ALSO
*	SecToTime()
*
*****************************************************************************
*
*
*/

 {return((unsigned long)hour*3600+(unsigned long)min*60+sec);
 }


#ifdef __MakeLib
   void __saveds __asm SecToTime(register __d0 unsigned long secs, register __a0 unsigned short *hour, register __a1 unsigned short *min, register __a2 unsigned short *sec)
#else
   void SecToTime(unsigned long secs, unsigned short *hour, unsigned short *min, unsigned short *sec)
#endif

/*
******* Date/SecToTime ******************************************************
*
*   NAME
*	SecToTime -- Returns the time from seconds (V33)
*
*   SYNOPSIS
*	SecToTime(secs,hour,min,sec);
*		   d0   a0   a1  a2
*
*	SecToTime(unsigned long secs, unsigned short *hour,
*	    unsigned short *min, unsigned short *sec);
*
*   FUNCTION
*	Gives you back the time from the specified seconds
*
*   INPUTS
*	secs - Time in seconds
*
*   RESULT
*	hour - hours (0-23)
*	min  - minutes (0-59)
*	sec  - seconds (0-59)
*
*   EXAMPLE
*	...
*	SecToTime(76860,&hour,&min,&sec);
*	...
*
*   NOTES
*	Don't forget to convert 24h time to AM/PM time if needed!
*
*   BUGS
*	No errorcheck, if you use a valid time
*
*   SEE ALSO
*	TimeToSec()
*
*****************************************************************************
*
*
*/

 {*hour = (unsigned short)(secs / 3600);
  secs -= (unsigned long)(*hour) * 3600;
  *min = (unsigned short)(secs / 60);
  *sec = (unsigned short)(secs - (unsigned long)(*min) * 60);
 }

 /* ----------------------------------------------------------------------- */

#ifdef __MakeLib
   unsigned short __saveds __asm JulianWeek(register __d0 const unsigned short day, register __d1 const unsigned short month, register __d2 const int year)
#else
   unsigned short JulianWeek(const unsigned short day, const unsigned short month, const int year)
#endif

/*
******* Date/JulianWeek *****************************************************
*
*   NAME
*	JulianWeek -- Gets the weeknumber of a specified date. (V33)
*
*   SYNOPSIS
*	weeknr = JulianWeek(day,month,year);
*	  d0		    d0    d1   d2
*
*	unsigned short JulianWeek(const unsigned short day,
*	    const unsigned short month, const int year);
*
*   FUNCTION
*	JulianWeek gets the weeknumber for a specified date.
*
*   INPUTS
*	day   - day of the date
*	month - month of the date
*	year  - year of the date
*
*   RESULT
*	week - This is the number of the week the specified date lies in.
*	    If the first day in a new year is a Friday, Saturday or
*	    Sunday, this would be the last week of the last year!
*	    If the 29.12. is a Monday, the 30.12. is a Monday or a Tuesday,
*	    the 31.12. is a Monday, Tuesday or a Wednesday this is the
*	    first week of the next year!
*
*   EXAMPLE
*	...
*	weeknr = JulianWeek(4,10,1582);
*	...
*
*   NOTES
*	It is is better only to use this function for years from 0 to 1582!
*
*   BUGS
*	For years < 0 errors could occur.
*
*   SEE ALSO
*	GregorianWeek(),HeisWeek(),JulianWeekday(),JulianDayDiff()
*
*****************************************************************************
*
*
*/

 {long days;
  Weekdays firstweekday;

   firstweekday = JulianWeekday(1,1,year);
   days = (JulianDayDiff(1,1,year,day,month,year) + (long)firstweekday -1) / 7;
   if (firstweekday > Thursday)
     {if (days == 0)
        days = JulianWeek(31,12,year-1);
      return((unsigned short)days);
     }
   else
     {if (!JulianDaySmaller(day,month,year,29,12,year))
        {firstweekday = JulianWeekday(day,12,year);
         switch (day)
           {case 31 : if (firstweekday == Wednesday)
                        days = 0;
            case 30 : if (firstweekday == Tuesday)
                        days = 0;
            case 29 : if (firstweekday == Monday)
                        days = 0;
                      break;
            default : ;
           }
        }
      return((unsigned short)(days +1));
     }
 }


#ifdef __MakeLib
   unsigned short __saveds __asm GregorianWeek(register __d0 const unsigned short day, register __d1 const unsigned short month, register __d2 const int year)
#else
   unsigned short GregorianWeek(const unsigned short day, const unsigned short month, const int year)
#endif

/*
******* Date/GregorianWeek **************************************************
*
*   NAME
*	GregorianWeek -- Gets the weeknumber of a specified date. (V33)
*
*   SYNOPSIS
*	weeknr = GregorianWeek(day,month,year);
*	  d0			d0   d1   d2
*
*	unsigned short GregorianWeek(const unsigned short day,
*	    const unsigned short month, const int year);
*
*   FUNCTION
*	GregorianWeek gets the weeknumber for a specified date.
*
*   INPUTS
*	day   - day of the date
*	month - month of the date
*	year  - year of the date
*
*   RESULT
*	week - This is the number of the week the specified date lies in.
*	    If the first day in a new year is a Friday, Saturday or
*	    Sunday, this would be the last week of the last year!
*	    If the 29.12. is a Monday, the 30.12. is a Monday or a Tuesday,
*	    the 31.12. is a Monday, Tuesday or a Wednesday this is the
*	    first week of the next year!
*
*   EXAMPLE
*	...
*	weeknr = GregorianWeek(4,10,1582);
*	...
*
*   NOTES
*	It is better only to use this function for years from 0 to 3000!
*
*   BUGS
*	For years < 0 errors could occur.
*
*   SEE ALSO
*	JulianWeek(),HeisWeek(),GregorianWeekday(),GregorianDayDiff()
*
*****************************************************************************
*
*
*/

 {long days;
  Weekdays firstweekday;

   firstweekday = GregorianWeekday(1,1,year);
   days = (GregorianDayDiff(1,1,year,day,month,year) + (long)firstweekday -1) / 7;
   if (firstweekday > Thursday)
     {if (days == 0)
        days = GregorianWeek(31,12,year-1);
      return((unsigned short)days);
     }
   else
     {if (!GregorianDaySmaller(day,month,year,29,12,year))
        {firstweekday = GregorianWeekday(day,12,year);
         switch (day)
           {case 31 : if (firstweekday == Wednesday)
                        days = 0;
            case 30 : if (firstweekday == Tuesday)
                        days = 0;
            case 29 : if (firstweekday == Monday)
                        days = 0;
                      break;
            default : ;
           }
        }
      return((unsigned short)(days +1));
     }
 }


#ifdef __MakeLib
   unsigned short __saveds __asm HeisWeek(register __d0 const unsigned short day, register __d1 const unsigned short month, register __d2 const int year)
#else
   unsigned short HeisWeek(const unsigned short day, const unsigned short month, const int year)
#endif

/*
******* Date/HeisWeek *******************************************************
*
*   NAME
*	HeisWeek -- Gets the weeknumber of a specified date. (V33)
*
*   SYNOPSIS
*	weeknr = HeisWeek(day,month,year);
*	  d0		   d0  d1    d2
*
*	unsigned short HeisWeek(const unsigned short day,
*	    const unsigned short month, const int year);
*
*   FUNCTION
*	HeisWeek gets the weeknumber for a specified date.
*
*   INPUTS
*	day   - day of the date
*	month - month of the date
*	year  - year of the date
*
*   RESULT
*	week - This is the number of the week the specified date lies in.
*	    If the first day in a new year is a Friday, Saturday or
*	    Sunday, this would be the last week of the last year!
*	    If the 29.12. is a Monday, the 30.12. is a Monday or a Tuesday,
*	    the 31.12. is a Monday, Tuesday or a Wednesday this is the
*	    first week of the next year!
*
*   EXAMPLE
*	...
*	weeknr = HeisWeek(4,10,1582);
*	...
*
*   NOTES
*	It is better only to use this function for years from 0 to 8000!
*
*   BUGS
*	For years < 0 errors could occur.
*
*   SEE ALSO
*	JulianWeek(),GregorianWeek(),HeisWeekday(),HeisDayDiff()
*
*****************************************************************************
*
*
*/

 {long days;
  Weekdays firstweekday;

   firstweekday = HeisWeekday(1,1,year);
   days = (HeisDayDiff(1,1,year,day,month,year) + (long)firstweekday -1) / 7;
   if (firstweekday > Thursday)
     {if (days == 0)
        days = HeisWeek(31,12,year-1);
      return((unsigned short)days);
     }
   else
     {if (!HeisDaySmaller(day,month,year,29,12,year))
        {firstweekday = HeisWeekday(day,12,year);
         switch (day)
           {case 31 : if (firstweekday == Wednesday)
                        days = 0;
            case 30 : if (firstweekday == Tuesday)
                        days = 0;
            case 29 : if (firstweekday == Monday)
                        days = 0;
                      break;
            default : ;
           }
        }
      return((unsigned short)(days +1));
     }
 }

 /* ----------------------------------------------------------------------- */

#ifdef __SASC_650
#ifdef __MakeLib
     int __saveds __UserLibInit(struct Library *libbase)
#else
     void _STI_600__DateInit(void)
#endif
#else
   void _DateInit(void)
#endif

/*
******* Date/_DateInit ******************************************************
*
*   NAME
*	_DateInit -- Procedure to initialize this module! (V33)
*
*   SYNOPSIS
*	_DateInit();
*
*	void _DateInit(void);
*
*   FUNCTION
*	Initialize this module, like the modulebody in Modula-II or Oberon-2
*
*   INPUTS
*	None.
*
*   RESULT
*	None.
*
*   EXAMPLE
*	...
*	_DateInit();
*	...
*
*   NOTES
*	This function is only needed/available if you do not compile this
*	with a SAS C Compiler (using Autoinitialization!)
*	If you are not using SASC - don't forget to init this module with
*	this function - or you will get into trouble!!!
*
*   BUGS
*	unknown.
*
*   SEE ALSO
*
*
*****************************************************************************
*
*
*/

 {/* Gregorian reform in Rom */
  BeforeGregorianDay = 4;
  BeforeGregorianMonth = 10;
  BeforeGregorianYear = 1582;
  AfterGregorianDay = 15;
  AfterGregorianMonth = 10;
  AfterGregorianYear = 1582;
  StartHeisDay = 1;
  StartHeisMonth = 1;
  StartHeisYear = 3200;
  /* Dates of Gregorian reform in
     Deutschland, Niederlande, Schweiz, D‰nemark:
       18.02.1700-01.03.1700
     Groﬂbritannien
       02.09.1752-14.09.1752
     Schweden
       17.02.1753-01.03.1753
     Ruﬂland
       ? (oktober Revolution)
     Griechenland
       ??.??.1923-??.??.1923 */
  /* Bremen/Arbergen = 8∞ 55' 23" East, 53∞ 4' 8" North */
#ifdef __MakeLib
    return(0);
#endif
 }


#ifdef __MakeLib
   void __saveds __UserLibCleanup(struct Library *libbase)
   {}
#endif
