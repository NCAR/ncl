/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

#include <stdio.h>

extern  double    ds_missing_value, ds_exponent, ds_scale, ds_max_dist,
                  ds_epsilon_test;

extern  int       ds_error_status, ds_set_maxpts, ds_maxpoints,
                  ds_first_call, ds_shadowing, ds_set_max_dist;

extern  FILE      *ds_filee;
extern  char      ds_error_file[], ds_emsg[];

extern  int       *ds_permutation_vector;

extern  DSpointd3 *ds_input_points;
extern  double    *ds_output, *ds_distances, *ds_weights;

extern  DSpoints3 *ds_input_points_s;
extern  float     *ds_output_s, *ds_distances_s, *ds_weights_s;
