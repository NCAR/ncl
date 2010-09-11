/*
 *	$Id: rwchinfl.c,v 1.10 2008-07-27 12:23:45 haley Exp $
 */
/************************************************************************
*                                                                       *
*			     Copyright (C)  2000	                *
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/


#include <ncarg/c.h>
#include <stdlib.h>
#ifdef	FreeBSD
#include <sys/filio.h>
#else
#include <sys/file.h>
#endif	/* FreeBSD */
#include <fcntl.h>
#include <unistd.h>

#if defined (cray)
#include <fcntl.h>
#include <sys/types.h>
#include <fortran.h>
#endif


void NGCALLF(ngcpid,NGCPID)(ipid)
/*
 * This routine gets the current process id.
 */
  int *ipid;
{
  *ipid = getpid();
}


void NGCALLF(ngclfi,NGCLFI)(fdes)
/*
 * This routine closes an open file; the argument "fdes" is the file
 * descriptor which was returned by ngofro, ngofrw, or ngofwo.
 */
  int *fdes;
{
  (void)close((int)*fdes);
}


void NGCALLF(ngexit,NGEXIT)(nerr)
  int *nerr;
{
  exit(*nerr);
}


void NGCALLF(ngofro,NGOFRO)(flnm,fdes,stat)
/*
 * This routine opens an existing file for reading only.  The argument
 * "flnm" is input; it contains a file name, in the form of a FORTRAN
 * CHARACTER*1 string terminated by a null.  The argument "fdes"
 * receives the file descriptor for the file.  The argument "stat"
 * receives a zero if the operation was successful, non-zero otherwise.
 */
#if defined (cray)
  _fcd flnm;
#else
  char flnm[];
#endif
  int *fdes,*stat;
{
  int st,flgs;
  *stat = 0;
/*
 * Attempt to open the file.
 */
  flgs = O_RDONLY;
#if defined (cray)
  st = open(_fcdtocp(flnm),flgs,0);
#else
  st = open(flnm,flgs,0);
#endif
/*
 * If "st" is -1, set error flag; otherwise, return file descriptor.
 */
  if (st == -1)
    *stat = 1;
  else
    *fdes = st;
}


void NGCALLF(ngofrw,NGOFRW)(flnm,fdes,stat)
/*
 * This routine opens a file for reading and writing both.  The argument
 * "flnm" is input; it contains a file name, in the form of a FORTRAN
 * CHARACTER*1 string terminated by a null.  The argument "fdes"
 * receives the file descriptor for the file.  The argument "stat"
 * receives a zero if the operation was successful, non-zero otherwise.
 */
#if defined (cray)
  _fcd flnm;
#else
  char flnm[];
#endif
  int *fdes,*stat;
{
  int st,flgs;
  *stat = 0;
/*
 * Attempt to open the file.
 */
  flgs = O_RDWR | O_CREAT;
#if defined (cray)
  st = open(_fcdtocp(flnm),flgs,0666);
#else
  st = open(flnm,flgs,0666);
#endif
/*
 * If "st" is -1, set error flag; otherwise, return file descriptor.
 */
  if (st == -1)
    *stat = 1;
  else
    *fdes = st;
}


void NGCALLF(ngofwo,NGOFWO)(flnm,fdes,stat)
/*
 * This routine opens a file for writing only.  The argument "flnm" is
 * input; it contains a file name, in the form of a FORTRAN CHARACTER*1
 * string terminated by a null.  The argument "fdes" receives the file
 * descriptor for the file.  The argument "stat" receives a zero if the
 * operation was successful, non-zero otherwise.
 */
#if defined (cray)
  _fcd flnm;
#else
  char flnm[];
#endif
  int *fdes,*stat;
{
  int st,flgs;
  *stat = 0;
/*
 * Attempt to open the file.
 */
  flgs = O_WRONLY | O_CREAT;
#if defined (cray)
  st = open(_fcdtocp(flnm),flgs,0666);
#else
  st = open(flnm,flgs,0666);
#endif
/*
 * If "st" is -1, set error flag; otherwise, return file descriptor.
 */
  if (st == -1)
    *stat = 1;
  else
    *fdes = st;
}


void NGCALLF(ngrdch,NGRDCH)(fdes,buff,lbuf,stat)
/*
 * This routine reads characters (bytes) from a file to an internal
 * buffer.  The argument "fdes" is the file descriptor returned by the
 * routine that opened the file.  The argument "buff" is a FORTRAN
 * CHARACTER*1 array.  The argument "lbuf" says how many bytes are
 * to be transferred.  The argument "stat" is returned with a positive
 * value indicating how many bytes were actually transferred, a zero
 * value if an end-of-file was encountered, or a negative value if an
 * error occurred.
 */
#if defined (cray)
  _fcd buff;
#else
  char buff[];
#endif
  int *fdes,*lbuf,*stat;
{
  int ret;
#if defined (cray)
  ret = read((int)*fdes,_fcdtocp(buff),(int)*lbuf);
#else
  ret = read((int)*fdes,buff,(int)*lbuf);
#endif
  *stat = ret;
}


void NGCALLF(ngrdfl,NGRDFL)(fdes,buff,lbuf,stat)
/*
 * This routine reads floats (reals) from a file to an internal buffer.
 * The argument "fdes" is the file descriptor returned by the routine
 * that opened the file.  The argument "buff" is a FORTRAN REAL array.
 * The argument "lbuf" says how many reals are to be transferred.  The
 * argument "stat" is returned with a positive value indicating how
 * many reals were actually transferred, a zero value if an end-of-file
 * was encountered, or a negative value if an error occurred.
 */
  float buff[];
  int *fdes,*lbuf,*stat;
{
  int ret;
  ret = read((int)*fdes,(char*)buff,(int)(*lbuf*sizeof(float)));
  if (ret%sizeof(float)!=0) ret=-1;
  else if (ret>0) ret=ret/sizeof(float);
  *stat = ret;
}


void NGCALLF(ngrdin,NGRDIN)(fdes,buff,lbuf,stat)
/*
 * This routine reads integers from a file to an internal buffer.  The
 * argument "fdes" is the file descriptor returned by the routine that
 * opened the file.  The argument "buff" is a FORTRAN INTEGER array.
 * The argument "lbuf" says how many integers are to be transferred.
 * The argument "stat" is returned with a positive value indicating
 * how many integers were actually transferred, a zero value if an
 * end-of-file was encountered, or a negative value if an error
 * occurred.
 */
  int buff[];
  int *fdes,*lbuf,*stat;
{
  int ret;
  ret = read((int)*fdes,(char*)buff,(int)(*lbuf*sizeof(int)));
  if (ret%sizeof(int)!=0) ret=-1;
  else if (ret>0) ret=ret/sizeof(int);
  *stat = ret;
}


void NGCALLF(ngrmfi,NGRMFI)(flnm)
/*
 * This routine removes an existing file.  The argument "flnm" is input;
 * it contains a file name, in the form of a FORTRAN CHARACTER*1 string
 * terminated by a null.
 */
#if defined (cray)
  _fcd flnm;
#else
  char flnm[];
#endif
{
#if defined (cray)
  (void)unlink(_fcdtocp(flnm));
#else
  (void)unlink(flnm);
#endif
}


void NGCALLF(ngwrch,NGWRCH)(fdes,buff,lbuf,stat)
/*
 * This routine writes characters (bytes) to a file from an internal
 * buffer.  The argument "fdes" is the file descriptor returned by the
 * routine that opened the file.  The argument "buff" is a FORTRAN
 * CHARACTER*1 array.  The argument "lbuf" says how many bytes are to
 * be transferred.  The argument "stat" is returned with a positive
 * value indicating how many bytes were actually transferred or a value
 * less than or equal to zero if an error occurred.
 */
#if defined (cray)
  _fcd buff;
#else
  char buff[];
#endif
  int *fdes,*lbuf,*stat;
{
  int ret;
#if defined (cray)
  ret = write((int)*fdes,_fcdtocp(buff),(int)*lbuf);
#else
  ret = write((int)*fdes,buff,(int)*lbuf);
#endif
  *stat = ret;
}


void NGCALLF(ngwrfl,NGWRFL)(fdes,buff,lbuf,stat)
/*
 * This routine writes reals (floats) to a file from an internal
 * buffer.  The argument "fdes" is the file descriptor returned by the
 * routine that opened the file.  The argument "buff" is a FORTRAN
 * REAL array.  The argument "lbuf" says how many reals are to be
 * transferred.  The argument "stat" is returned with a positive value
 * indicating how many reals were actually transferred or a value less
 * than or equal to zero if an error occurred.
 */
  float buff[];
  int *fdes,*lbuf,*stat;
{
  int ret;
  ret = write((int)*fdes,(char*)buff,(int)(*lbuf*sizeof(float)));
  if (ret%sizeof(float)!=0) ret=-1;
  else if (ret>0) ret=ret/sizeof(float);
  *stat = ret;
}


void NGCALLF(ngwrin,NGWRIN)(fdes,buff,lbuf,stat)
/*
 * This routine writes integers to a file from an internal buffer.  The
 * argument "fdes" is the file descriptor returned by the routine that
 * opened the file.  The argument "buff" is a FORTRAN INTEGER array.
 * The argument "lbuf" says how many integers are to be transferred.
 * The argument "stat" is returned with a positive value indicating
 * how many integers were actually transferred or a value less than or
 * equal to zero if an error occurred.
 */
  int buff[];
  int *fdes,*lbuf,*stat;
{
  int ret;
  ret = write((int)*fdes,(char*)buff,(int)(*lbuf*sizeof(int)));
  if (ret%sizeof(int)!=0) ret=-1;
  else if (ret>0) ret=ret/sizeof(int);
  *stat = ret;
}


void NGCALLF(ngseek,NGSEEK)(fdes,offs,orig,stat)
/*
 * This routine repositions the read/write position of an open file.
 * The argument "fdes" is the file descriptor returned by the routine
 * that opened the file.  The argument "offs" is the desired position,
 * given as an offset from the origin specified by the argument "orig",
 * which is a "0" to specify the beginning of the file, a 1 to specify
 * the current position, or a 2 to specify the end of the file.
 */
  int *fdes,*offs,*orig,*stat;
{
  long ret;
  ret = lseek((int)*fdes,(long)*offs,(int)*orig);
  *stat = (int) ret;
}
