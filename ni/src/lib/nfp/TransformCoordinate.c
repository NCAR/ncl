#include <stdio.h>
#include <proj.h>
#include "TransformCoordinate.h"

int TransformCoordinate(char * SrcProjStr, char * DstProjStr,
                        double * x, double * y, double * z,
                        unsigned int nPoint) {
  PJ_CONTEXT *CTX;
  PJ *P;
  size_t stride = sizeof(double);
  int Err, i;

  CTX = proj_context_create();
  P = proj_create_crs_to_crs(CTX, SrcProjStr, DstProjStr, NULL);

  /* Constructing the projections */
  if (P==0) {
    printf("FATAL ERROR: Can not make a transform out of <%s> and <%s>\n",
           SrcProjStr, DstProjStr);
    return (1);
  }

  /* Transforming the coordinates */
  Err = proj_trans_generic(P, PJ_FWD,
                           x, stride, nPoint,
                           y, stride, nPoint,
                           z, stride, nPoint,
                           0, 0, 0);
  if (Err != 0) {
    printf("FATAL ERROR: Could convert only %i out of %u points\n",
           Err, nPoint);
    return (3);
  }

  /* freeing the projection */
  proj_destroy(P);
  proj_context_destroy(CTX);
  return (0);
}
