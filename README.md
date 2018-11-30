# NCAR Command Language

<img src="http://www.ncl.ucar.edu/Images/NCLLogoWithoutText.jpg" width="150" align=right title="NCL Logo">

This is the source code for the NCAR Command Language (NCL).

NCL is a scripting language for the analysis and visualization of climate and weather data.

* Supports NetCDF, GRIB, HDF, HDF-EOS, and shapefile data formats
* Has hundreds of built-in computational routines
* Produces high-quality graphics

NCL is developed by the [Computational and Information Systems Lab](https://www2.cisl.ucar.edu) at the [National Center for Atmospheric Research](https://ncar.ucar.edu), and is funded by the [National Science Foundation](https://www.nsf.gov).

# Installation

The current version of NCL is [6.5.0](http://www.ncl.ucar.edu/current_release.shtml), which can be installed via [conda](http://www.ncl.ucar.edu/Download/conda.shtml).

```
conda create -n ncl_stable -c conda-forge ncl
source activate ncl_stable
```

# Documentation and support

Visit the [NCL website](http://www.ncl.ucar.edu) for documentation, examples, support, and installation.

* [NCL User Guide](http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/)
* [Extensive example suite](http://www.ncl.ucar.edu/Applications/)
* [Email list support](http://www.ncl.ucar.edu/Support/email_lists.shtml)
* [Detailed download and installation instructions](http://www.ncl.ucar.edu/Download/)

# NCL source code tree

The top level NCL source code tree contains the following directories and files:

| Directory          | Purpose |
| :--------------     | :------- |
| ```common/```      | Low-level library and fonts required by NCAR Graphics and NCL |
| ```config/```      | Configuration files for installation |
| ```external/```    | External libraries required by NCL  |
| ```install/```     | Auxiliary files for installation  |
| ```ncarg2d/```     | NCAR Graphics and GKS libraries and examples |
| ```ncarview/```    | NCGM-based applications and raster utilities |
| ```ngmath/```      | Interpolation libraries for 1D, 2D, and 3D data |
| ```ni/```          | NCL interpreter, HLU library, examples, color tables, GRIB2 code tables |
| **Files**          | **Purpose** |
| ```Configure```    | Configuration script for installation |
| ```Copyright```    | Detailed copyright notice  |
| ```NcargVersion``` | NCAR Graphics version file  |
| ```NclVersion```   | NCL version file   |
| ```version```      | Common version file  |
| ```yMakefile```    | Top level makefile |


# Bug reports and feature requests

Use the GitHub [issue tracker](https://github.com/NCAR/ncl/issues) to submit a bug or request.

# Citing NCL

Cite NCL using the following text:

> The NCAR Command Language (Version 6.5.0) [Software]. (2018). Boulder, Colorado: UCAR/NCAR/CISL/TDD. http://dx.doi.org/10.5065/D6WD3XH5

Update the NCL version and year as appropriate.
