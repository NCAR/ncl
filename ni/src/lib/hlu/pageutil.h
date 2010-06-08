/*
 * $Id: pageutil.h,v 1.2 2010-03-29 16:30:03 brownrig Exp $
 *
 * pageutil.h
 *
 */

#ifndef PAGEUTIL_H_
#define PAGEUTIL_H_


#define PAGEUTIL_DEFAULT_PAPERSIZE "letter"

/* a packet to request and receive page sizing information */
typedef struct {
    const char*  paperSize;          /* standard names:  "A2", "legal", etc. */
    const char*  paperSizeResName;   /* name of resource that identifies paper-size, for error reporting */
    float        paperWidthIn;       /* inches */
    const char*  paperWidthResName;  /* name of resource that identifies paper-width, for error reporting */
    float        paperHeightIn;      /* inches  */
    const char*  paperHeightResName; /* name of resource that identifies paper-height, for error reporting */
    int          pageWidthPts;       /* returned paper size in points */
    int          pageHeightPts;      /*              "                */
    int          leftMargin;         /* computed paper margin, pts */
    int          rightMargin;
    int          topMargin;
    int          bottomMargin;
} NhlPageInfo;

extern const int nhlGetPaperSize(NhlPageInfo* pageInfo);

#endif /* PAGEUTIL_H_ */
