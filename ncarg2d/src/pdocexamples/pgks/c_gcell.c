/*
 *  Illustrate the use of gcell_array.  The GKS standard leaves
 *  the ordering of the color index array ambiguous.  This example
 *  illustrates what would be the most natural ordering of that
 *  array as it is implemented in NCAR GKS.
 */

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <stdlib.h>

#define IWTYPE 1
#define WKID   1

main() {

/*
 *  colia will be used to store the color index array.
 */
  int *colia;

/*
 *  Declare relevant variables.
 */
  Gpoint corner1,corner2;
  Grect rect1;
  Gint_size cell_size;
  Gpat_rep cell_colors;
  Gcolr_rep rgb;

/*
 *  Open GKS, open and activate a workstation.
 */
  gopen_gks ("stdout",0);
  gopen_ws (WKID, NULL, IWTYPE);
  gactivate_ws (WKID);

/*
 *  Specify the corner points.
 */
  corner1.x = 0.0;
  corner1.y = 1.0;
  corner2.x = 1.0;
  corner2.y = 0.0;
  rect1.p = corner1;
  rect1.q = corner2;

/*
 *  The cell array will have two divisions in the horizontal 
 *  and three in the vertical.
 */
  cell_size.size_x = 2;
  cell_size.size_y = 3;
  cell_colors.dims = cell_size;

/*
 *  Define a color table.
 */
  rgb.rgb.red = 0.0; rgb.rgb.green = 0.0; rgb.rgb.blue = 0.0;
  gset_colr_rep(WKID, 0, &rgb );
  rgb.rgb.red = 0.0; rgb.rgb.green = 0.0; rgb.rgb.blue = 0.0;
  gset_colr_rep(WKID, 1, &rgb );
  rgb.rgb.red = 1.0; rgb.rgb.green = 0.0; rgb.rgb.blue = 0.0;
  gset_colr_rep(WKID, 2, &rgb );
  rgb.rgb.red = 0.0; rgb.rgb.green = 1.0; rgb.rgb.blue = 0.0;
  gset_colr_rep(WKID, 3, &rgb );
  rgb.rgb.red = 0.0; rgb.rgb.green = 0.0; rgb.rgb.blue = 1.0;
  gset_colr_rep(WKID, 4, &rgb );
  rgb.rgb.red = 0.0; rgb.rgb.green = 1.0; rgb.rgb.blue = 1.0;
  gset_colr_rep(WKID, 5, &rgb );
  rgb.rgb.red = 1.0; rgb.rgb.green = 0.0; rgb.rgb.blue = 1.0;
  gset_colr_rep(WKID, 6, &rgb );
  rgb.rgb.red = 1.0; rgb.rgb.green = 1.0; rgb.rgb.blue = 0.0;
  gset_colr_rep(WKID, 7, &rgb );

/*
 *  Specify the color index array.
 */
  colia = (int*)malloc(6*sizeof(int));

  colia[0] = 2;   /* Red     */
  colia[1] = 3;   /* Green   */
  colia[2] = 4;   /* Blue    */
  colia[3] = 5;   /* Cyan    */
  colia[4] = 6;   /* Magenta */
  colia[5] = 7;   /* Yellow  */
  cell_colors.colr_array = colia;

/*
 *  Draw the cell array.
 */
  gcell_array(&rect1,&cell_colors);
  c_frame();

/*
 *  Close things down.
 */
  gdeactivate_ws (WKID);
  gclose_ws (WKID);
  gclose_gks();
}
