/*
 *      $Id: name.c,v 1.1 1993-07-19 22:28:14 clyne Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jul 19 13:26:55 MDT 1993
 *
 *	Description:	Map CGM element identifier indeces into 
 *			the element names
 */

#include <stdio.h>
#include "cgm_tools.h"

static	char *elementClasses[] = {
	"Delimiter",
	"Metafile Descriptor",
	"Picture Descriptor",
	"Control",
	"Graphical Primitive",
	"Attribute",
	"Escape"
};

static char *delimiterElements[] = {
	"No-Op",
	"Begin Metafile",
	"End Metafile",
	"Begin Picture",
	"Begin Picture Body",
	"End Picture",
};

static	char *descriptorElements[] = {
	"Illegal Element",
	"Metafile Version",
	"Metafile Description",
	"VDC Type",
	"Integer Precision",
	"Real Precision",
	"Index Precision",
	"Colour Precision",
	"Colour Index Precision",
	"Maximum Colour",
	"Colour Value Extent",
	"Metafile Element List",
	"Metafile Defaults Replacement",
	"Font List",
	"Character Set List",
	"Character Coding Announcer",
	"Name Precision",
	"Maximum Vdc Extent"
};
	

static	char	*picDescElements[] = {
	"Illegal Element",
	"Scale Mode",
	"Colour Select Mode",
	"Line Width Specification Mode",
	"Marker Size Specification Mode",
	"Edge Width Specification Mode",
	"VDC Extent",
	"Background Colour",
};


static	char	*controlElements[] = {
	"Illegal Element",
	"VDC Integer Precision",
	"VDC Real Precision",
	"Auxillary Colour",
	"Transparency",
	"Clip Rectangle",
	"Clip Indicator"
};

static	char	*primitiveElements[] = {
	"Illegal Element",
	"Polyline",
	"Disjoint Polyline",
	"Polymarker",
	"Text",
	"Restricted Text",
	"Append Text",
	"Polygon",
	"Polygon Set",
	"Cell Array",
	"Generalized Drawing Primitive",
	"Rectangle",
	"Circle",
	"Cirular Arc 3 Point",
	"Cirular Arc 3 Point Close",
	"Cirular Arc Centre",
	"Cirular Arc Centre Close",
	"Elipse",
};

static	char	*attributeElements[] = {
	"Illegal Element",
	"Line Bundle Index",
	"Line Type",
	"Line Width",
	"Line Colour",
	"Marker Bundle Index",
	"Marker Type",
	"Marker Width",
	"Marker Colour",
	"Text Bundle Index",
	"Text Font Index",
	"Text Precision",
	"Character Expansion Factor",
	"Character Spacing",
	"Text Colour",
	"Character Height",
	"Character Orientation",
	"Text Path",
	"Text Alignment",
	"Character Set Index",
	"Alternate Character Set Index",
	"Fill Bundle Index",
	"Interior Style",
	"Fill Colour",
	"Hatch Index",
	"Pattern Index",
	"Edge Bundle Index",
	"Edge Type",
	"Edge Width",
	"Edge Colour",
	"Edge Visibility",
	"Fill Reference Point",
	"Pattern Table",
	"Pattern Size",
	"Color Table",
	"Aspect Source Flag"
};
	
static	char	*escapeElements[] = {
	"Illegal Element",
	"escape"
};

static	char	*externalElements[] = {
	"Illegal Element",
	"Message",
	"Application"
};

#define	CLASS_SIZE (sizeof elementClasses / sizeof (char *))

#define	DEL_ELEMENT_SIZE (sizeof (delimiterElements) / sizeof (char *))
#define	DES_ELEMENT_SIZE (sizeof (descriptorElements) / sizeof (char *))
#define	PIC_DESC_SIZE (sizeof (picDescElements) / sizeof (char *))
#define	CON_ELEMENT_SIZE (sizeof (controlElements) / sizeof (char *))
#define	GRP_ELEMENT_SIZE (sizeof (primitiveElements) / sizeof (char *))
#define	ATT_ELEMENT_SIZE (sizeof (attributeElements) / sizeof (char *))
#define	ESC_ELEMENT_SIZE (sizeof (escapeElements) / sizeof (char *))


/*
 * Function:	CGM_ClassLookup()
 *
 * Description:	Map a CGM element class into the textual name for that class
 *
 * In Args:	
 *	class	: the class index
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	NULL if $class is invalid, else a pointer to the text
 *		string naming CGM class $class is returned
 * Side Effect:	
 */
const	char	*CGM_ClassLookup(class)
	unsigned int	class;
{
	if (class >= CLASS_SIZE) return(NULL);

	return(elementClasses[class]);
}

/*
 * Function:	CGM_ElementLookup()
 *
 * Description:	Map a CGM element into the textual name for that element
 *
 * In Args:	
 * 	class	: the class index
 * 	id	: the element id index
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	NULL if $class or $id is invalid, else a pointer to the text
 *		string naming CGM element  ($class,$id) is returned
 * Side Effect:	
 */
const	char	*CGM_ElementLookup(class, id)
	unsigned int	class;
	unsigned int	id;
{
	char	*s;

	if (class >= CLASS_SIZE) return(NULL);

	switch (class) {
	case	(int) DEL_ELEMENT:
		if (id >= DEL_ELEMENT_SIZE) s = NULL;
		else s = delimiterElements[id];

		break;

        case	(int) DES_ELEMENT:
		if (id >= DES_ELEMENT_SIZE) s = NULL;
		else s = descriptorElements[id];

		break;

        case	(int) PIC_DEC_ELEMENT:
		if (id >= PIC_DESC_SIZE) s = NULL;
		else s = picDescElements[id];

		break;

        case	(int) CON_ELEMENT:
		if (id >= CON_ELEMENT_SIZE) s = NULL;
		else s = controlElements[id];

		break;

        case	(int) GRP_ELEMENT:
		if (id >= GRP_ELEMENT_SIZE) s = NULL;
		else s = primitiveElements[id];

		break;

        case	(int) ATT_ELEMENT:
		if (id >= ATT_ELEMENT_SIZE) s = NULL;
		else s = attributeElements[id];

		break;

        case	(int) ESC_ELEMENT:
		if (id >= ESC_ELEMENT_SIZE) s = NULL;
		else s = delimiterElements[id];

		break;

	default:
		s = NULL;
	};

	return(s);
}
