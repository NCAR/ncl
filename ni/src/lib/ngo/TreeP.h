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

#ifndef XmLTreePH
#define XmLTreePH

#include <Xm/XmP.h>
#ifndef MOTIF11
#include <Xm/ManagerP.h>
#include <Xm/DrawP.h>
#endif

#include "Tree.h"
#include "GridP.h"

/* row value mask for get/set values */
#define RVML XmLGridRowValueMaskLen
#define XmLTreeRowLevel      (1L << (RVML))
#define XmLTreeRowExpands    (1L << (RVML + 1))
#define XmLTreeRowIsExpanded (1L << (RVML + 2))
 
typedef struct _XmLTreeRowPart
	{
	Boolean expands;
	int level;
	Boolean hasChildren, hasSiblings, isExpanded;
	Dimension stringWidth;
	Boolean stringWidthValid;
	} XmLTreeRowPart;

struct _XmLTreeRowRec
	{
	XmLGridRowPart grid;
	XmLTreeRowPart tree;
	};

typedef struct _XmLTreePart
	{
	/* resources */
	Dimension levelSpacing;
	Pixel lineColor, pmColor;
	XtCallbackList collapseCallback, expandCallback;

	/* private data */
	char *linesData;
	int linesSize, linesMaxLevel;
	int recalcTreeWidth;

	char defaultPixmapsCreated;
	Pixel pixColors[4];
	Pixmap filePixmask, folderPixmask, folderOpenPixmask;
	Pixmap filePixmap, folderPixmap, folderOpenPixmap;

	/* row resources */
	int rowLevel;
	Boolean rowExpands, rowIsExpanded;
	} XmLTreePart;

typedef struct _XmLTreeRec
	{
	CorePart core;
	CompositePart composite;
	ConstraintPart constraint;
	XmManagerPart manager;
	XmLGridPart grid;
	XmLTreePart tree;
	} XmLTreeRec;

typedef struct _XmLTreeClassPart
	{
	int unused;
	} XmLTreeClassPart;

typedef struct _XmLTreeClassRec
	{
	CoreClassPart core_class;
	CompositeClassPart composite_class;
	ConstraintClassPart constraint_class;
	XmManagerClassPart manager_class;
	XmLGridClassPart grid_class;
	XmLTreeClassPart tree_class;
	} XmLTreeClassRec;

extern XmLTreeClassRec xmlTreeClassRec;

typedef struct _XmLTreeConstraintPart
	{
	int unused;
	} XmLTreeConstraintPart;

typedef struct _XmLTreeConstraintRec
	{
	XmManagerConstraintPart manager;
	XmLGridConstraintPart grid;
	XmLTreeConstraintPart tree;
	} XmLTreeConstraintRec, *XmLTreeConstraintPtr;

#endif 
