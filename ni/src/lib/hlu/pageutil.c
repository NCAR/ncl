/*
 * $Id: pageutil.c,v 1.2 2010-03-29 16:30:03 brownrig Exp $
 *
 * Encode details about standard paper/page sizes.
 *
 */
/*************************************************************************
 *                                                                       *
 *                Copyright (C)  2010                                    *
 *        University Corporation for Atmospheric Research                *
 *                All Rights Reserved                                    *
 *                                                                       *
 ************************************************************************/

#include <strings.h>
#include <ncarg/hlu/hlu.h>
#include "pageutil.h"

const int DEFAULT_MARGIN = 36;    /* "0.5 */

/* encodes definitions of standard page/paper sizes */
typedef struct {
    char*    name;
    short    widthPts,  heightPts;
    float    widthIn,   heightIn;
    short    widthMM,   heightMM;
} NhlPageDefinition;


/* Paper definitions came from http://www.dpandi.com/paper/index.html */
static NhlPageDefinition _pageDefs[] = {
        /* ANSI/U.S. paper sizes */
        /* name    ptsX  ptsY   inX    inY   mmX   mmY   */
        {"A",       612,  792,  8.5,  11.0,  216,  279},
        {"letter",  612,  792,  8.5,  11.0,  216,  279},
        {"legal",   612, 1008,  8.5,  14.0,  216,  356},
        {"B",       792, 1224, 11.0,  17.0,  279,  432},
        {"ledger",  792, 1224, 11.0,  17.0,  279,  432},
        {"superA3", 936, 1368, 13.0,  19.0,  330,  483},
        {"superB",  936, 1368, 13.0,  19.0,  330,  483},
        {"C",      1224, 1584, 17.0,  22.0,  432,  559},
        {"D",      1584, 2520, 22.0,  34.0,  559,  864},
        {"E",      2448, 3168, 34.0,  44.0,  864, 1118},
        /* ISO/Metric paper sizes */
        {"A5",      418,  598,  5.8,   8.3,  148,  210},
        {"A4",      598,  842,  8.3,  11.7,  210,  297},
        {"A3",      842, 1188, 11.7,  16.5,  297,  420},
        {"A3+",     936, 1368, 13.0,  19.0,  329,  483},
        {"A2",     1188, 1685, 16.5,  23.4,  420,  594},
        {"A1",     1685, 2383, 23.4,  33.1,  594,  841},
        {"A0",     2383, 3370, 33.1,  46.8,  841, 1189},
        /* end-of-list marker */
        {NULL,        0,    0,   0.,    0.,    0,    0}
};

extern const NhlPageDefinition* nhlGetPageDefinition(const char* pageName);

/*
 * nhlGetPaperSize()
 *
 * Encodes logic regarding paper/page sizing, based upon resource specifications, that is common
 * to the postscript, PDF, and cairo document-based workstations.
 *
 * The rules are:
 *
 * - User can specify page size by using either wkPaperSize resource, providing a standard paper-size name,
 *   or by using the combination wkPaperWidthF/wkPaperHeightF (both *must* be used) to specify paper size
 *   in inches.
 * - If both methods are specified, the wkPaperSize prevails.
 * - If neither method is specified (signified as NULL or strlen < 0 for wkPaperSize, and
 *   wkPaperWidthF/wkPaperHeightF both less than 0.), then the default paper size is "letter".
 * - Suitable margins (0.5") are computed based upon the page size.
 *
 * Inputs:
 *   wkPaperSize value
 *   wkPaperSize resource name
 *
 *   wkPaperWidthF value
 *   wkPaperWidthF resource name
 *
 *   wkPaperHeightF value
 *   wkPaperHeightF resource name
 *
 * Outputs:
 *   computed paper size, in points
 *   possibly recomputed paper size, in inches
 *   page margins
 *
 */
const int nhlGetPaperSize(NhlPageInfo* pageInfo)
{
    const char* func = "nhlGetPaperSize";
    int ret = 0;

    if (pageInfo->paperSize != NULL && strlen(pageInfo->paperSize) > 0 && 
            strcasecmp(pageInfo->paperSize, PAGEUTIL_DEFAULT_PAPERSIZE)) 
    {
        /* page specified by a standard paper size */
        const NhlPageDefinition *pageDef = nhlGetPageDefinition(pageInfo->paperSize);
        if (pageDef == NULL) {
            NhlPError(NhlWARNING,NhlEUNKNOWN,
                    "%s: Unknown paper-size (%s), defaulting to %s", func,
                    pageInfo->paperSize, PAGEUTIL_DEFAULT_PAPERSIZE);
            ret = NhlWARNING;
            pageDef = nhlGetPageDefinition(PAGEUTIL_DEFAULT_PAPERSIZE);
        }

        if (pageInfo->paperWidthIn > 0. && pageInfo->paperHeightIn > 0.) {
            /* issue feedback to user that we're taking paperSize over pageWidth/pageHeight */
            NhlPError(NhlWARNING,NhlEUNKNOWN,
                    "%s: Both page %s/%s and %s resources given; using %s", func,
                    pageInfo->paperWidthResName, pageInfo->paperHeightResName,
                    pageInfo->paperSizeResName, pageInfo->paperSizeResName);
            ret = NhlWARNING;
        }

        pageInfo->pageWidthPts = pageDef->widthPts;
        pageInfo->pageHeightPts = pageDef->heightPts;
        /* recalculate page size in inches */
        pageInfo->paperWidthIn = pageInfo->pageWidthPts / 72.;
        pageInfo->paperHeightIn = pageInfo->pageHeightPts / 72.;
    }

    else if (pageInfo->paperWidthIn > 0. && pageInfo->paperHeightIn > 0.) {
        /* page specified by pageWidth/pageHeight */
        pageInfo->pageWidthPts = pageInfo->paperWidthIn * 72;
        pageInfo->pageHeightPts = pageInfo->paperHeightIn * 72;
    }

    else {
        /* either default, or improperly specified by pageWidth/pageHeight resources */
        if (pageInfo->paperWidthIn > 0. || pageInfo->paperHeightIn > 0.) {
            /* issue feedback to user that we're expecting to see both pageWidth/pageHeight */
            NhlPError(NhlWARNING, NhlEUNKNOWN,
                    "%s: Must specify both %s and %s together; using default page size (%s)",
                    func, pageInfo->paperWidthResName, pageInfo->paperHeightResName, PAGEUTIL_DEFAULT_PAPERSIZE);
            ret = NhlWARNING;
        }

        const NhlPageDefinition* pageDef = nhlGetPageDefinition(PAGEUTIL_DEFAULT_PAPERSIZE);
        pageInfo->pageWidthPts = pageDef->widthPts;
        pageInfo->pageHeightPts = pageDef->heightPts;
        /* recalculate page size in inches */
        pageInfo->paperWidthIn = pageInfo->pageWidthPts / 72.;
        pageInfo->paperHeightIn = pageInfo->pageHeightPts / 72.;
    }

    /* compute page margins; attempt to apply the default margin to smallest dimension, then compute a margin 
     * for the longer dimension that yields an NDC square.
     */
    if (pageInfo->pageWidthPts < pageInfo->pageHeightPts) {
        int ndcDimensions = pageInfo->pageWidthPts - (2 * DEFAULT_MARGIN);
        if (ndcDimensions <= 0) ndcDimensions = pageInfo->pageWidthPts;
        int actualMargin = (pageInfo->pageWidthPts - ndcDimensions) / 2;
        pageInfo->leftMargin = actualMargin;
        pageInfo->rightMargin = pageInfo->leftMargin + ndcDimensions;
        actualMargin = (pageInfo->pageHeightPts - ndcDimensions) / 2;
        pageInfo->bottomMargin = actualMargin;
        pageInfo->topMargin = pageInfo->bottomMargin + ndcDimensions;
    }
    else {
        int ndcDimensions = pageInfo->pageHeightPts - (2 * DEFAULT_MARGIN);
        if (ndcDimensions <= 0) ndcDimensions = pageInfo->pageHeightPts;
        int actualMargin = (pageInfo->pageHeightPts - ndcDimensions) / 2;
        pageInfo->bottomMargin = actualMargin;
        pageInfo->topMargin = pageInfo->bottomMargin + ndcDimensions;        
        actualMargin = (pageInfo->pageWidthPts - ndcDimensions) / 2;
        pageInfo->leftMargin = actualMargin;
        pageInfo->rightMargin = pageInfo->leftMargin + ndcDimensions;
    }

    return ret;
}

/*
 * NhlGetPageDefinition()
 *
 * Returns the page-size definition for the given page name.
 * Returns NULL if page name not found.
 *
 */
const NhlPageDefinition* nhlGetPageDefinition(const char* pageName)
{
    NhlPageDefinition *pageDef = NULL;
    int i = 0;
    while (pageName != NULL && _pageDefs[i].name != NULL) {
        if (!strcasecmp(pageName, _pageDefs[i].name)) {
            pageDef = (_pageDefs+i);
            break;
        }
        ++i;
    }

    return pageDef;
}
