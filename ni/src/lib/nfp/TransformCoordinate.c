#include <stdio.h>
#include <proj_api.h>
#include "TransformCoordinate.h"

int TransformCoordinate(char * SrcProjStr, char * DstProjStr,
        double * x, double * y, double * z,
        unsigned int nPoint) {
    projPJ SrcProj, DstProj;
    int Err, i;

    /* Constructing the projections */
    if (!(SrcProj = pj_init_plus(SrcProjStr))) {
        printf("FATAL ERROR: Can not make a projection out of <%s>\n", SrcProjStr);
        return (1);
    }
    if (!(DstProj = pj_init_plus(DstProjStr))) {
        printf("FATAL ERROR: Can not make a projection out of <%s>\n", DstProjStr);
        return (2);
    }

    /* Converting to radian if needed */
    if (pj_is_latlong(SrcProj)) {
        for (i = 0; i < nPoint; i++) {
            x[i] *= DEG_TO_RAD;
            y[i] *= DEG_TO_RAD;
        }
    }

    /* Transforming the coordinates */
    if ((Err = pj_transform(SrcProj, DstProj, nPoint, 1, x, y, z)) != 0) {
        printf("FATAL ERROR: %s\n", pj_strerrno(Err));
        return (3);
    }

    /* converting to degree if needed */
    if (pj_is_latlong(DstProj)) {
        for (i = 0; i < nPoint; i++) {
            x[i] *= RAD_TO_DEG;
            y[i] *= RAD_TO_DEG;
        }
    }

    /* freeing the projection */
    pj_free(DstProj);
    pj_free(SrcProj);
    return (0);
}

