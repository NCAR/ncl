/*
**  program c_clbfil.c
** 
**  Demonstrate the c_lblbar utility.  This file is
**  a direct translation of clbfil.f.
*/

#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

/*
**  Define error file, Fortran unit number, and workstation type,
**  and workstation ID.
*/
#define IERRF  "stdout"
#define LUNIT  "gmeta"
#define IWTYPE SED_WSTYPE
#define IWKID  1
#define ISZDM  0

/*
** Title text.
*/
char* LAB1[4]={"Four","Different","Fill","Styles"};

int main()
{
	int ifill1[4];		/* fill type */

				/* define fill style */
	extern int lbfill_(int*,float*,float*,int*,int*);

	ifill1[0] = 1;
	ifill1[1] = 2;
	ifill1[2] = 3;
	ifill1[3] = 4;
/*
** Open GKS
*/
        gopen_gks(IERRF, ISZDM);
        gopen_ws(IWKID, LUNIT, IWTYPE);
        gactivate_ws(IWKID);
/*
** Draw a horizontal bar
*/
	c_lblbar(0,.05,.95,.05,.95,4,1.,.3,ifill1,2,LAB1,4,1);
/*
** Deactivate and close workstatione, close GKS.
*/
        gdeactivate_ws(IWKID);
        gclose_ws(IWKID);
        gclose_gks();
	return (0);
}


int lbfill_(int* iftp, float* xra, float* yra, int* nra, int* indx)
/*
** Fill an area according to a user defined pattern.
**
** iftp 	- user call input variable
** xra, yra	- X and Y coordinate defining rectangular box
** nra		- number of points in xra, yra
** indx		- pattern selector
*/
{
	int i,j;	/* counters */
/*
** Declare required dimensioned arrays.
*/
      	float dst[500];
	int ind[500];
      	int id1[8][8];
	int id2[8][8];
	int id3[8][8];
/*
** Define three different dot patterns.
*/
	id1[0][0]=1; id1[0][1]=1; id1[0][2]=0; id1[0][3]=0; id1[0][4]=0; 
        id1[0][5]=0; id1[0][6]=1; id1[0][7]=1; id1[1][0]=1; id1[1][1]=1;
        id1[1][2]=0; id1[1][3]=1; id1[1][4]=1; id1[1][5]=0; id1[1][6]=1; 
        id1[1][7]=1; id1[2][0]=0; id1[2][1]=0; id1[2][2]=0; id1[2][3]=1; 
        id1[2][4]=1; id1[2][5]=0; id1[2][6]=0; id1[2][7]=0; id1[3][0]=0; 
        id1[3][1]=1; id1[3][2]=1; id1[3][3]=1; id1[3][4]=1; id1[3][5]=1; 
        id1[3][6]=1; id1[3][7]=0; id1[4][0]=0; id1[4][1]=1; id1[4][2]=1; 
        id1[4][3]=1; id1[4][4]=1; id1[4][5]=1; id1[4][6]=1; id1[4][7]=0; 
        id1[5][0]=0; id1[5][1]=0; id1[5][2]=0; id1[5][3]=1; id1[5][4]=1; 
        id1[5][5]=0; id1[5][6]=0; id1[5][7]=0; id1[6][0]=1; id1[6][1]=1; 
        id1[6][2]=0; id1[6][3]=1; id1[6][4]=1; id1[6][5]=0; id1[6][6]=1; 
        id1[6][7]=1; id1[7][0]=1; id1[7][1]=1; id1[7][2]=0; id1[7][3]=0; 
        id1[7][4]=0; id1[7][5]=0; id1[7][6]=1; id1[7][7]=1;
           
	id2[0][0]=0; id2[0][1]=0; id2[0][2]=0; id2[0][3]=0; id2[0][4]=0; 
        id2[0][5]=0; id2[0][6]=0; id2[0][7]=0; id2[1][0]=0; id2[1][1]=1; 
        id2[1][2]=1; id2[1][3]=1; id2[1][4]=1; id2[1][5]=1; id2[1][6]=1; 
        id2[1][7]=0; id2[2][0]=0; id2[2][1]=1; id2[2][2]=1; id2[2][3]=1; 
        id2[2][4]=1; id2[2][5]=1; id2[2][6]=1; id2[2][7]=0; id2[3][0]=0; 
        id2[3][1]=1; id2[3][2]=1; id2[3][3]=0; id2[3][4]=0; id2[3][5]=1; 
        id2[3][6]=1; id2[3][7]=0; id2[4][0]=0; id2[4][1]=1; id2[4][2]=1; 
        id2[4][3]=0; id2[4][4]=0; id2[4][5]=1; id2[4][6]=1; id2[4][7]=0;
	id2[5][0]=0; id2[5][1]=1; id2[5][2]=1; id2[5][3]=1; id2[5][4]=1; 
        id2[5][5]=1; id2[5][6]=1; id2[5][7]=0; id2[6][0]=0; id2[6][1]=1; 
        id2[6][2]=1; id2[6][3]=1; id2[6][4]=1; id2[6][5]=1; id2[6][6]=1; 
        id2[6][7]=0; id2[7][0]=0; id2[7][1]=0; id2[7][2]=0; id2[7][3]=0; 
        id2[7][4]=0; id2[7][5]=0; id2[7][6]=0; id2[7][7]=0;

	id3[0][0]=0; id3[0][1]=0; id3[0][2]=0; id3[0][3]=0; id3[0][4]=0; 
        id3[0][5]=0; id3[0][6]=0; id3[0][7]=0; id3[1][0]=0; id3[1][1]=1; 
        id3[1][2]=1; id3[1][3]=0; id3[1][4]=0; id3[1][5]=1; id3[1][6]=1; 
        id3[1][7]=1; id3[2][0]=0; id3[2][1]=1; id3[2][2]=1; id3[2][3]=0; 
        id3[2][4]=0; id3[2][5]=1; id3[2][6]=1; id3[2][7]=0; id3[3][0]=0; 
        id3[3][1]=1; id3[3][2]=1; id3[3][3]=0; id3[3][4]=1; id3[3][5]=1; 
        id3[3][6]=0; id3[3][7]=0; id3[4][0]=0; id3[4][1]=1; id3[4][2]=1; 
        id3[4][3]=1; id3[4][4]=1; id3[4][5]=0; id3[4][6]=0; id3[4][7]=0;
	id3[5][0]=0; id3[5][1]=1; id3[5][2]=1; id3[5][3]=0; id3[5][4]=1; 
        id3[5][5]=1; id3[5][6]=0; id3[5][7]=0; id3[6][0]=0; id3[6][1]=1; 
        id3[6][2]=1; id3[6][3]=0; id3[6][4]=0; id3[6][5]=1; id3[6][6]=1; 
        id3[6][7]=0; id3[7][0]=0; id3[7][1]=1; id3[7][2]=1; id3[7][3]=0; 
        id3[7][4]=0; id3[7][5]=1; id3[7][6]=1; id3[7][7]=1;

/*
** Double the size of the GKS dot.
*/
      	gset_marker_size (2.0);
/*
** Fill the first box with a combination of lines and dots.
*/
	if (*indx == 1)
	{
	  c_sfsetr ("SP - SPACING OF FILL LINES",.012);
          c_sfseti ("DO - DOT-FILL FLAG",0);
          c_sfwrld (xra,yra,*nra,dst,500,ind,500);
          c_sfsetr ("SP - SPACING OF FILL LINES",.006);
          c_sfseti ("DO - DOT-FILL FLAG",1);
          c_sfnorm (xra,yra,*nra,dst,500,ind,500);
	}
/*
** Fill the second box with a specified dot pattern.
*/
	if (*indx == 2)
	{
	  c_sfsetr ("SP - SPACING OF FILL LINES",.004);
          c_sfsetp (id1);
          c_sfwrld (xra,yra,*nra,dst,500,ind,500);
	}
/*
** Fill the third box with a different dot pattern, tilted at an
** angle.
*/
	if (*indx == 3)
	{
	  c_sfseti ("AN - ANGLE OF FILL LINES",45);
          c_sfsetp (id2);
          c_sfwrld (xra,yra,*nra,dst,500,ind,500);
	}
/*
** Fill the last box with K"s, both large and small.
*/
	if (*indx == 4)
	{
	  gset_char_ht (.008);
          c_sfsetr ("SP - SPACING OF FILL LINES",.012);
          c_sfsetc ("CH - CHARACTER SPECIFIER","K");
          c_sfsetp (id3);
          c_sfwrld (xra,yra,*nra,dst,500,ind,500);
	}
/*
** Done.
*/
	return 1;
}
