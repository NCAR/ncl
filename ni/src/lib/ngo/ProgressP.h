/*
(c) Copyright 1994, 1995, 1996 Microline Software, Inc.  ALL RIGHTS RESERVED
  
THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY BE COPIED AND USED 
ONLY IN ACCORDANCE WITH THE TERMS OF THAT LICENSE AND WITH THE INCLUSION
OF THE ABOVE COPYRIGHT NOTICE.  THIS SOFTWARE AND DOCUMENTATION, AND ITS 
COPYRIGHTS ARE OWNED BY MICROLINE SOFTWARE AND ARE PROTECTED BY UNITED
STATES COPYRIGHT LAWS AND INTERNATIONAL TREATY PROVISIONS.
 
THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE
AND SHOULD NOT BE CONSTRUED AS A COMMITMENT BY MICROLINE SOFTWARE.

THIS SOFTWARE AND REFERENCE MATERIALS ARE PROVIDED "AS IS" WITHOUT
WARRANTY AS TO THEIR PERFORMANCE, MERCHANTABILITY, FITNESS FOR ANY 
PARTICULAR PURPOSE, OR AGAINST INFRINGEMENT.  MICROLINE SOFTWARE
ASSUMES NO RESPONSIBILITY FOR THE USE OR INABILITY TO USE THIS 
SOFTWARE.

MICROLINE SOFTWARE SHALL NOT BE LIABLE FOR INDIRECT, SPECIAL OR
CONSEQUENTIAL DAMAGES RESULTING FROM THE USE OF THIS PRODUCT. SOME 
STATES DO NOT ALLOW THE EXCLUSION OR LIMITATION OF INCIDENTAL OR
CONSEQUENTIAL DAMAGES, SO THE ABOVE LIMITATIONS MIGHT NOT APPLY TO
YOU.

MICROLINE SOFTWARE SHALL HAVE NO LIABILITY OR RESPONSIBILITY FOR SOFTWARE
ALTERED, MODIFIED, OR CONVERTED BY YOU OR A THIRD PARTY, DAMAGES
RESULTING FROM ACCIDENT, ABUSE OR MISAPPLICATION, OR FOR PROBLEMS DUE
TO THE MALFUNCTION OF YOUR EQUIPMENT OR SOFTWARE NOT SUPPLIED BY
MICROLINE SOFTWARE.
  
U.S. GOVERNMENT RESTRICTED RIGHTS
This Software and documentation are provided with RESTRICTED RIGHTS.
Use, duplication or disclosure by the Government is subject to
restrictions as set forth in subparagraph (c)(1) of the Rights in
Technical Data and Computer Software Clause at DFARS 252.227-7013 or
subparagraphs (c)(1)(ii) and (2) of Commercial Computer Software -
Restricted Rights at 48 CFR 52.227-19, as applicable, supplier is
Microline Software, 41 Sutter St Suite 1374, San Francisco, CA 94104.
*/

#ifndef XmLProgressPH
#define XmLProgressPH

#include <Xm/XmP.h>
#include <X11/StringDefs.h>
#ifdef MOTIF11
#else
#include <Xm/PrimitiveP.h>
#include <Xm/DrawP.h>
#endif

#include <sys/types.h>
#include "Progress.h"

typedef struct _XmLProgressPart
	{
	int completeValue, value;
	Boolean showTime;
	Boolean showPercentage;
	XmFontList fontList;
	GC gc;
	time_t startTime;
	XFontStruct *fallbackFont;
	unsigned char meterStyle;
	int numBoxes;
	} XmLProgressPart;

typedef struct _XmLProgressRec
	{
	CorePart core;
	XmPrimitivePart primitive;
	XmLProgressPart progress;
	} XmLProgressRec;

typedef struct _XmLProgressClassPart
	{
	int null;
	} XmLProgressClassPart;

typedef struct _XmLProgressClassRec
	{
	CoreClassPart core_class;
	XmPrimitiveClassPart primitive_class;
	XmLProgressClassPart progress_class;
	} XmLProgressClassRec;

extern XmLProgressClassRec xmlProgressClassRec;

#endif
