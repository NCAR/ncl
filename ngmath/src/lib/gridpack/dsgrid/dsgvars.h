/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/*
 *  Define all global variables (they all begin with the sentinel
 *  characters "ds_").
 */
double    ds_missing_value = -99999., ds_exponent = 3., ds_scale = 1.,
          ds_max_dist, ds_epsilon_test;
int       ds_error_status = 0, ds_set_maxpts = 0, ds_maxpoints, 
          ds_first_call = 1, ds_shadowing = 0, ds_set_max_dist = 0;

#ifdef __linux__
FILE      *ds_filee = _IO_stderr;
#else
FILE      *ds_filee = stderr;
#endif
char      ds_error_file[256] = {"stderr"}, ds_emsg[256];

int       *ds_permutation_vector;

DSpointd3 *ds_input_points;
double    *ds_output, *ds_distances, *ds_weights;

DSpoints3 *ds_input_points_s;
float     *ds_output_s, *ds_distances_s, *ds_weights_s;
