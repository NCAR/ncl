/*
 *      $Id: ncl2v5dW.c,v 1.2 2007-08-23 19:38:10 grubin Exp $
 */

/************************************************************************
*                                                                       *
*                Copyright (C)  2003                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/

/*
 *  File:           ncl2v5dW.c
 *
 *  Author:         Rick Grubin (adapted from Jeff Boote's original code)
 *                  National Center for Atmospheric Research
 *                  POB 3000, Boulder, Colorado
 *
 *  Date:           $Date: 2007-08-23 19:38:10 $
 *
 *  Description:    Wrappers for ncl to output v5d files.
 *                  NOT complete - only the functions I had to have.
 */

# include   <stdio.h>
# include   <ncarg/hlu/hlu.h>
# include   <ncarg/hlu/NresDB.h>
# include   <ncarg/ncl/defs.h>
# include   <ncarg/ncl/NclDataDefs.h>
# include   <ncarg/ncl/NclBuiltIns.h>
# include   <ncarg/ncl/NclBuiltInSupport.h>

# include   <vis5d+/binio.h>
# include   <vis5d+/v5d.h>

# include   "wrapper.h"

static NhlBoolean   V5DOPEN = False;
static int  V5DNumTimes,
            V5DNumVars,
            V5DNumRows,
            V5DNumCols;
static int  *V5DNumLevels = NULL;

/*
 *  v5d_create_W
 *
 *  Create a Vis5d+ file.
 */

NhlErrorTypes v5d_create_W(void)
{
    NrmQuark    *outfileQ;
    char        *outfile;
    int *NumTimes,
        *NumVars,
        *Nr,
        *Nc,
        *Nl;
    ng_size_t dsizes[NCL_MAX_DIMENSIONS];
    int i;
    char    *tstr;
    NrmQuark    *VarNameQ;
    const char  VarName[MAXVARS][10];
    int *TimeStamp,
        *DateStamp,
        *CompressMode;
    int *Projection;
    float   *ProjArgs;
    int *Vertical;
    float   *VertArgs;
    NhlBoolean  arg_good;

    if (V5DOPEN) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
            "v5d_create: Can only have one v5d file open at a time!");

        return NhlFATAL;
    }

    /* filename */
    outfileQ = (NrmQuark *) NclGetArgValue(0, 14, NULL, NULL, NULL, NULL, NULL, 0);
    outfile = NrmQuarkToString(*outfileQ);

    /* NumTimes */
    NumTimes = (int *) NclGetArgValue(1, 14, NULL, NULL, NULL, NULL, NULL, 0);
    if (*NumTimes > MAXTIMES) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
            "v5d_create: NumTimes: %d > MAXTIMES: %d", *NumTimes, MAXTIMES);

        return NhlFATAL;
    }

    /* NumVars */
    NumVars = (int *) NclGetArgValue(2, 14, NULL, NULL, NULL, NULL, NULL, 0);
    if (*NumVars > MAXVARS) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
            "v5d_create: NumVars: %d > MAXVARS: %d", *NumVars, MAXVARS);

        return NhlFATAL;
    }

    /* Nr */
    Nr = (int *) NclGetArgValue(3, 14, NULL, NULL, NULL, NULL, NULL, 0);

    /* Nc */
    Nc = (int *) NclGetArgValue(4, 14, NULL, NULL, NULL, NULL, NULL, 0);

    /* Nl */
    Nl = (int *) NclGetArgValue(5, 14, NULL, dsizes, NULL, NULL, NULL, 0);
    if (dsizes[0] != *NumVars) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "v5d_create: Nl must be size[NumVars]");

        return NhlFATAL;
    }

    /* VarName */
    VarNameQ = (NrmQuark *) NclGetArgValue(6, 14, NULL, dsizes, NULL, NULL, NULL, 0);
    if (dsizes[0] != *NumVars){
        NhlPError(NhlFATAL, NhlEUNKNOWN,
        "v5d_create: VarNames must be size[NumVars]");
        return NhlFATAL;
    }

    for (i = 0; i < *NumVars; i++){
        tstr = NrmQuarkToString(VarNameQ[i]);
        (void) strncpy((char *) &VarName[i][0], tstr, 10);
    }

    /* TimeStamp */
    TimeStamp = (int *) NclGetArgValue(7, 14, NULL, dsizes, NULL, NULL, NULL, 0);
    if (dsizes[0] != *NumTimes){
        NhlPError(NhlFATAL, NhlEUNKNOWN,
        "v5d_create: TimeStamp must be size[NumTimes]");
        return NhlFATAL;
    }

    /* DateStamp */
    DateStamp = (int *) NclGetArgValue(8, 14, NULL, dsizes, NULL, NULL, NULL, 0);
    if (dsizes[0] != *NumTimes){
        NhlPError(NhlFATAL, NhlEUNKNOWN,
        "v5d_create: DateStamp must be size[NumTimes]");
        return NhlFATAL;
    }

    /* CompressMode */
    CompressMode = (int *) NclGetArgValue(9, 14, NULL, NULL, NULL, NULL, NULL, 0);
    switch (*CompressMode) {
        case 1:
            /* FALLTHROUGH */
        case 2:
            /* FALLTHROUGH */
        case 4:
            /* FALLTHROUGH */
            break;

        default:
            NhlPError(NhlFATAL,NhlEUNKNOWN,
            "v5d_create: CompressMode must be 1, 2, or 4 (bytes per grid point");
        return NhlFATAL;
    }

    /* Projection */
    Projection = (int *) NclGetArgValue(10, 14, NULL, NULL, NULL, NULL, NULL, 0);
    if ((*Projection < 0) || (*Projection > 4)){
        NhlPError(NhlFATAL, NhlEUNKNOWN,
            "v5d_create: Projection must be 0-4");
        return NhlFATAL;
    }
    /* ProjArgs */
    ProjArgs = (float *) NclGetArgValue(11, 14, NULL, dsizes, NULL, NULL, NULL, 0);
    arg_good = True;
    switch (*Projection) {
        case 0:
            /* FALLTHROUGH */
        case 1:
            if (dsizes[0] != 4)
                arg_good = False;
            break;

        case 2:
            if (dsizes[0] != 6)
                arg_good = False;
            break;

        case 3:
            if (dsizes[0] != 5)
                arg_good = False;
            break;

        case 4:
            if (dsizes[0] != 7)
                arg_good = False;
            break;
    }

    if (!arg_good){
        NhlPError(NhlFATAL, NhlEUNKNOWN,
        "v5d_create: size of ProjArgs[%d] does not match Projection", dsizes[0]);
        return NhlFATAL;
    }

    /* Vertical */
    Vertical = (int *) NclGetArgValue(12, 14, NULL, NULL, NULL, NULL, NULL, 0);

    /* VertArgs */
    VertArgs = (float *) NclGetArgValue(13, 14, NULL, dsizes,NULL, NULL, NULL, 0);
    arg_good = True;
    switch (*Vertical) {
        case 0:
            /* FALLTHROUGH */
        case 1:
            if (dsizes[0] != 2) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                    "v5d_create: size of VertArgs does not match Vertical");
                return NhlFATAL;
            }
            break;

        case 2:
            /* FALLTHROUGH */
        case 3:
            if (dsizes[0] > MAXLEVELS) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "v5d_create: VertArgs must be less then size[MAXLEVELS:%d]",
                    MAXLEVELS);
                return NhlFATAL;
            }
            break;

        default:
            NhlPError(NhlFATAL, NhlEUNKNOWN,
            "v5d_create: Vertical must be 0-3");
            return NhlFATAL;
    }

    if (!v5dCreate(outfile, *NumTimes, *NumVars, *Nr, *Nc, Nl, VarName,
            TimeStamp, DateStamp, *CompressMode, *Projection, ProjArgs,
            *Vertical, VertArgs)) {
        return NhlFATAL;
    }
    else {
        V5DOPEN = True;
        V5DNumTimes = *NumTimes;
        V5DNumVars = *NumVars;
        V5DNumRows = *Nr;
        V5DNumCols = *Nc;
        V5DNumLevels = NhlMalloc(sizeof(int) * *NumVars);
        memcpy(V5DNumLevels, Nl, sizeof(int) * *NumVars);
        return NhlNOERROR;
    }
}

/*
 *  Write to a Vis5d+ file.
 */
NhlErrorTypes v5d_write_W(void)
{
    ng_size_t dsizes [NCL_MAX_DIMENSIONS];
    int *it,
        *iv;
    float   *data;

    if (!V5DOPEN) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
            "v5d_write: v5d_create() must be called before write");
        return NhlFATAL;
    }

    /* Time Step */
    it = (int *) NclGetArgValue(0, 3, NULL, NULL, NULL, NULL, NULL, 0);
    if (*it <= 0) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "v5d_write - time step mismatch");
        return NhlFATAL;
    }

    /* Var Num */
    iv = (int *) NclGetArgValue(1, 3, NULL, NULL, NULL, NULL, NULL, 0);
    if (*iv <= 0) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "v5d_write - var index mismatch");
        return NhlFATAL;
    }

    /* Var Data */
    data = (float *) NclGetArgValue(2, 3, NULL, dsizes, NULL, NULL, NULL, 0);

    if ((V5DNumLevels != NULL) &&
        (dsizes[0] * dsizes[1] * dsizes[2]) != (V5DNumRows * V5DNumCols * V5DNumLevels[0])) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
                "v5d_write - data size mismatch");
        return NhlFATAL;
    }

    if (!v5dWrite(*it, *iv, data)) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,"v5d_write: Write Error");
        return NhlFATAL;
    }

    return NhlNOERROR;
}

/*
 *  Write a single variable to a Vis5d+ file.
 */
NhlErrorTypes v5d_write_var_W(void)
{
    ng_size_t dsizes [NCL_MAX_DIMENSIONS];
    float   *data;
    int i;

    if (!V5DOPEN) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "v5d_write_var: v5d_create() must be called before write");
        return NhlFATAL;
    }
    if (V5DNumVars != 1) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
            "v5d_write_var: v5d_create() was called with %d vars", V5DNumVars);
        return NhlFATAL;
    }

    /* Var Data */
    data = (float *) NclGetArgValue(0, 1, NULL, dsizes, NULL, NULL, NULL, 0);

    if ((dsizes[0] * dsizes[1] * dsizes[2] * dsizes[3]) !=
            (V5DNumTimes * V5DNumRows * V5DNumCols * V5DNumLevels[0])){
        NhlPError(NhlFATAL, NhlEUNKNOWN,
                "v5d_write - data size mismatch");
        return NhlFATAL;
    }

    for (i = 0; i < V5DNumTimes; i++){
        float   *dataptr;

        dataptr = &data[i * V5DNumLevels[0] * V5DNumCols * V5DNumRows];
        if (!v5dWrite(i + 1, 1, dataptr)){
            NhlPError(NhlFATAL, NhlEUNKNOWN,
                        "v5d_write: Write Error");
            return NhlFATAL;
        }
    }

    return NhlNOERROR;
}

/*
 *  Close a Vis5d+ file.
 */
NhlErrorTypes v5d_close_W(void)
{
    if (!V5DOPEN){
        NhlPError(NhlFATAL, NhlEUNKNOWN,
        "v5d_close: v5d_create() and v5d_write() must be called before write");
        return NhlFATAL;
    }

    v5dClose();
    if (V5DNumLevels)
        NhlFree(V5DNumLevels);
    V5DNumLevels = NULL;
    V5DOPEN = False;

    return NhlNOERROR;
}

/*
 *  Get a Vis5d+ missing value.
 */
NhlErrorTypes v5d_missing_W(void)
{
    float   missing = MISSING;
    ng_size_t dimsizes = 1;

    return NclReturnValue(&missing, 1, &dimsizes, NULL, NCL_float, True);
}
