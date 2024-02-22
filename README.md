# NCAR Command Language

> [!WARNING]
> **NCL was placed in maintenance mode as of 2019.** You can find more details in the [most recent announcement here](https://geocat.ucar.edu/blog/2020/11/11/November-2020-update).


<img src="http://www.ncl.ucar.edu/Images/NCLLogoWithoutText.jpg" width="150" align=right title="NCL Logo">

This is the source code for the NCAR Command Language (NCL).

NCL is a scripting language for the analysis and visualization of climate and weather data.

* Supports NetCDF, GRIB, HDF, HDF-EOS, and shapefile data formats
* Has hundreds of built-in computational routines
* Produces high-quality graphics

NCL is developed by the [Computational and Information Systems Lab](https://www2.cisl.ucar.edu) at the [National Center for Atmospheric Research](https://ncar.ucar.edu) (NCAR).

NCAR is sponsored by the [National Science Foundation](https://www.nsf.gov). Any opinions, findings and conclusions or recommendations expressed in this material do not necessarily reflect the views of the National Science Foundation.

# Important announcement on the future of NCL

NCAR has made the decision to adopt Python as the scripting language platform of choice for future development of analysis and visualization tools. Please read this [open letter to NCL users](https://www.ncl.ucar.edu/open_letter_to_ncl_users.shtml) to understand what kind of impact this will have on the future of NCL.

# Installation

The current version of NCL is [6.6.2](http://www.ncl.ucar.edu/current_release.shtml), which can be installed via [conda](http://www.ncl.ucar.edu/Download/conda.shtml).

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

| Directory                | Purpose |
| :--------------          | :------- |
| ```common/```            | Low-level library and fonts required by NCAR Graphics and NCL |
| ```config/```            | Configuration files for installation |
| ```external/```          | External libraries required by NCL |
| ```install/```           | Auxiliary files for installation |
| ```ncarg2d/```           | NCAR Graphics and GKS libraries and examples |
| ```ncarview/```          | NCGM-based applications and raster utilities |
| ```ngmath/```            | Interpolation libraries for 1D, 2D, and 3D data |
| ```ni/```                | NCL interpreter, HLU library, examples, color tables, GRIB2 code tables |
| **Files**                | **Purpose** |
| ```CODE_OF_CONDUCT.md``` | Code of Conduct for NCL user community |
| ```CONTRIBUTING.md```    | How to contribute to the NCL Project |
| ```Configure```          | Configuration script for installation |
| ```LICENSE```            | Apache 2.0 License |
| ```README.md```          | Information about NCL |
| ```version```            | version file |
| ```yMakefile```          | Top level makefile |

# Bug reports and feature requests

Use the GitHub [issue tracker](https://github.com/NCAR/ncl/issues) to submit a bug or request a feature.

# Citing NCL

Cite NCL using the following text:

<> The NCAR Command Language (Version 6.6.2) [Software]. (2019). Boulder, Colorado: UCAR/NCAR/CISL/TDD. http://dx.doi.org/10.5065/D6WD3XH5

Update the NCL version and year as appropriate.
