typedef short bool;
#define true    1
#define false   0
typedef enum{dayerr,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday} Weekdays;

/* Module initialization */
#ifndef __SASC_650
  void _DateInit(void);
#undef __MakeLib
#endif

#ifndef __MakeLib
  bool JulianLeapYear(const int year);
  bool GregorianLeapYear(const int year);
  bool HeisLeapYear(const int year);
  unsigned short JulianMonthDays(const unsigned short month, const int year);
  unsigned short GregorianMonthDays(const unsigned short month, const int year);
  unsigned short HeisMonthDays(const unsigned short month, const int year);
  unsigned int JulianYearDays(const int year);
  unsigned int GregorianYearDays(const int year);
  unsigned int HeisYearDays(const int year);
  bool JulianDaySmaller(const unsigned short day1, const unsigned short month1, const int year1, const unsigned short day2, const unsigned short month2, const int year2);
  bool GregorianDaySmaller(const unsigned short day1, const unsigned short month1, const int year1, const unsigned short day2, const unsigned short month2, const int year2);
  bool HeisDaySmaller(const unsigned short day1, const unsigned short month1, const int year1, const unsigned short day2, const unsigned short month2, const int year2);
  bool JulianDayGreater(const unsigned short day1, const unsigned short month1, const int year1, const unsigned short day2, const unsigned short month2, const int year2);
  bool GregorianDayGreater(const unsigned short day1, const unsigned short month1, const int year1, const unsigned short day2, const unsigned short month2, const int year2);
  bool HeisDayGreater(const unsigned short day1, const unsigned short month1, const int year1, const unsigned short day2, const unsigned short month2, const int year2);
  long JulianDayDiff(const unsigned short day1, unsigned short month1, int year1, const unsigned short day2, unsigned short month2, int year2);
  long GregorianDayDiff(const unsigned short day1, unsigned short month1, int year1, const unsigned short day2, unsigned short month2, int year2);
  long HeisDayDiff(const unsigned short day1, unsigned short month1, int year1, const unsigned short day2, unsigned short month2, int year2);
  Weekdays JulianWeekday(const unsigned short day, unsigned short month, int year);
  Weekdays GregorianWeekday(const unsigned short day, unsigned short month, int year);
  Weekdays HeisWeekday(const unsigned short day, unsigned short month, int year);
  unsigned short JulianDaysBeforeWeekday(const unsigned short day, const unsigned short month, const int year, const Weekdays weekday);
  unsigned short GregorianDaysBeforeWeekday(const unsigned short day, const unsigned short month, const int year, const Weekdays weekday);
  unsigned short HeisDaysBeforeWeekday(const unsigned short day, const unsigned short month, const int year, const Weekdays weekday);
  unsigned short JulianDaysAfterWeekday(const unsigned short day, const unsigned short month, const int year, const Weekdays weekday);
  unsigned short GregorianDaysAfterWeekday(const unsigned short day, const unsigned short month, const int year, const Weekdays weekday);
  unsigned short HeisDaysAfterWeekday(const unsigned short day, const unsigned short month, const int year, const Weekdays weekday);
  void JulianDiffDate(const unsigned short day, const unsigned short month, const int year, int days, unsigned short *dday, unsigned short *dmonth, int *dyear);
  void GregorianDiffDate(const unsigned short day, const unsigned short month, const int year, int days, unsigned short *dday, unsigned short *dmonth, int *dyear);
  void HeisDiffDate(const unsigned short day, const unsigned short month, const int year, int days, unsigned short *dday, unsigned short *dmonth, int *dyear);
  unsigned int JYearToScaliger(const int year);
  unsigned int GYearToScaliger(const int year);
  unsigned int HYearToScaliger(const int year);
  int ScaligerYearToJ(const unsigned int syear);
  int ScaligerYearToG(const unsigned int syear);
  int ScaligerYearToH(const unsigned int syear);
  unsigned long JSYearToJD(const unsigned int syear);
  unsigned long GSYearToJD(const unsigned int syear);
  unsigned long HSYearToJD(const unsigned int syear);
  unsigned long JDtoMJD(const unsigned long jd);
  unsigned long MJDtoJD(const unsigned long mjd);
  unsigned long JulianToJD(const unsigned short day, const unsigned short month, const int year);
  unsigned long GregorianToJD(const unsigned short day, const unsigned short month, const int year);
  unsigned long HeisToJD(const unsigned short day, const unsigned short month, const int year);
  float TimeToJD(const unsigned short hour, const unsigned short min, const unsigned short sec);
  void JDToTime(float jd, unsigned short *rhour, unsigned short *rmin, unsigned short *rsec);
  unsigned short GregorianMoonAge(const unsigned short day, const unsigned short month, const int year);
  void GregorianEaster(const int year, unsigned short *dday, unsigned short *dmonth);
  short TimeZoneFactor(const short degree);
  long LMT(const unsigned long secs, const float meridiandegree, const float posdegree);
  unsigned long TimeToSec(const unsigned short hour, const unsigned short min, const unsigned short sec);
  void SecToTime(unsigned long secs, unsigned short *hour, unsigned short *min, unsigned short *sec);
  unsigned short JulianWeek(const unsigned short day, const unsigned short month, const int year);
  unsigned short GregorianWeek(const unsigned short day, const unsigned short month, const int year);
  unsigned short HeisWeek(const unsigned short day, const unsigned short month, const int year);
#else
#endif

