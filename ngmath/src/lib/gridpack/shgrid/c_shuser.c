/*
 * $Id: c_shuser.c,v 1.8 2008-07-27 03:10:13 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/ngmath.h>

float *c_shgrid(int n, float x[], float y[], float z[], float f[],
                int nxo, int nyo, int nzo, float xo[], float yo[], float zo[],
                int *ier)
{

  float *uo, *rwk, *zor;
  int   *iwk, i, j, k;

/*
 *  Allocate space for returned output array.
 */
  uo   = (float *) calloc(nxo*nyo*nzo, sizeof(float));
  if (uo == NULL) {
    printf("Unable to allocate space for output in c_shgrid\n");
    *ier = 300;
    return((float *)NULL);
  }

/*
 *  Allocate workspace arrays.
 */
  rwk = (float *) calloc(11*n+6, sizeof(float));
  if (rwk == NULL) {
    printf("Unable to allocate float work space in c_shgrid\n");
    *ier = 301;
    return((float *)NULL);
  }
  iwk = (int *) calloc(2*n, sizeof(int));
  if (iwk == NULL) {
    printf("Unable to allocate int work space in c_shgrid\n");
    *ier = 302;
    return((float *)NULL);
  }

  NGCALLF(shgrid,SHGRID)(&n, x, y, z, f, &nxo, &nyo, &nzo, xo, yo, zo,
                         uo, iwk, rwk, ier);
  free(rwk);
  free(iwk);

/*
 *  Rearrange the uo array to be row dominant.
 */
  zor = (float *) calloc(nxo*nyo*nzo, sizeof(float));
  if (zor == NULL) {
    printf("Unable to allocate temp space in c_shgrid\n");
    *ier = 303;
    return((float *)NULL);
  }
 
  for (i = 0; i < nxo; i++) {
    for (j = 0; j < nyo; j++) {
      for (k = 0; k < nzo; k++) {
        zor[i*nzo*nyo + j*nzo + k] = uo[k*nxo*nyo + j*nxo + i];
      }
    }
  }
 
  free(uo);
 
  return(zor);

}

int c_shgetnp(float px, float py, float pz, int n, 
              float x[], float y[], float z[], int iflag, int *ier)
{
  static float *rwk;
  static int   *iwk;
  int    np;
 
/*
 *  Allocate workspace arrays.
 */
  if (iflag == 0) {
    rwk = (float *) calloc(11*n+6, sizeof(float));
    if (rwk == NULL) {
      printf("Unable to allocate float work space in c_shgetnp\n");
      *ier = 304;
      return((int) NULL);
    }
    iwk = (int *) calloc(2*n, sizeof(int));
    if (iwk == NULL) {
      printf("Unable to allocate int work space in c_shgetnp\n");
      *ier = 305;
      return((int) NULL);
    }
  }
 
  NGCALLF(shgetnp,SHGETNP)(&px, &py, &pz, &n, x, y, z, &iflag, iwk, rwk, 
          &np, ier);

/*
 *  Adjust the Fortran index for C and return.
 */
  return(np-1);
}

void c_shseti(char *pnam, int ival)
{
    NGstring pnam2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if ( !pnam ) {
        fprintf( stderr, "c_shseti:  illegal parameter name (NULL)\n");
        return;
    }
    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(shseti,SHSETI)(pnam,&ival,len);
}

int c_shgeti(char *pnam)
{
    NGstring pnam2;
    int len,ivp;
/*
 * Make sure parameter name is not NULL
 */
    if ( !pnam ) {
        fprintf( stderr, "c_shgeti:  illegal parameter name (NULL)\n");
        return(-1);
    }
    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(shgeti,SHGETI)(pnam2,&ivp,len);
    return(ivp);
}
