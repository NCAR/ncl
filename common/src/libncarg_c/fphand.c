/*
 *	$Id: fphand.c,v 1.1 1994-08-11 16:29:50 haley Exp $
 */
#include <stdio.h>
#include <floatingpoint.h>
#include <ncarg/c.h>

NGCALLF(fphand,FPHAND)()
{
	ieee_handler("set", "common", SIGFPE_ABORT);
}
