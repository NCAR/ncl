/*
 *      $Id: FolderP.h,v 1.2 1997-10-03 20:07:35 dbrown Exp $
 */
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

#ifndef XmLFolderPH
#define XmLFolderPH

#include <Xm/XmP.h>

#ifdef MOTIF11
#else
#include <Xm/ManagerP.h>
#endif

#include "Folder.h"

typedef struct _XmLFolderPart
	{
	int debugLevel;
	Boolean serverDrawsArcsLarge;
	unsigned char cornerStyle, tabPlacement, resizePolicy;
	Boolean allowRotate, autoSelect;
	GC gc;
	Pixel inactiveBg, inactiveFg, blankBg;
	Pixmap blankPix;
	WidgetList tabs;
	int tabCount, tabAllocCount;
	Dimension marginWidth, marginHeight, spacing;
	Dimension cornerDimension, highlightThickness;
	Dimension pixmapMargin;
	Dimension tabHeight, tabWidth, tabBarHeight;
	int tabsPerRow, activeRow;
	XtTranslations primTrans;
	Widget focusW, activeW;
	int activeTab;
	char allowLayout;
	XtCallbackList activateCallback;
	XmFontList fontList;
	} XmLFolderPart;

typedef struct _XmLFolderRec
	{
	CorePart core;
	CompositePart composite;
	ConstraintPart constraint;
	XmManagerPart manager;
	XmLFolderPart folder;
	} XmLFolderRec;

typedef struct _XmLFolderClassPart
	{
	int unused;
	} XmLFolderClassPart;

typedef struct _XmLFolderClassRec
	{
	CoreClassPart core_class;
	CompositeClassPart composite_class;
	ConstraintClassPart constraint_class;
	XmManagerClassPart manager_class;
	XmLFolderClassPart folder_class;
	} XmLFolderClassRec;

extern XmLFolderClassRec xmlFolderClassRec;

typedef struct _XmLFolderConstraintPart
	{
	Position x, y;
	Dimension width, height;
	Dimension maxPixWidth, maxPixHeight;
	Dimension pixWidth, pixHeight;
	Dimension inactPixWidth, inactPixHeight;
	int row;
	Boolean firstInRow;
	Boolean freePix;
	Pixmap pix, inactPix;
	char *managedName;
	Widget managedW;
	} XmLFolderConstraintPart;

typedef struct _XmLFolderConstraintRec
	{
	XmManagerConstraintPart manager;
	XmLFolderConstraintPart folder;
	} XmLFolderConstraintRec, *XmLFolderConstraintPtr;

#endif 
