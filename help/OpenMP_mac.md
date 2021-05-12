## Installing OpenMP on Mac OSX

In some cases, you may get an error that does not allow installation of goldfish from source on Mac OSX versions, including under R 4.0.0. The error may relate to compiling the parts of goldfish that are written in C++, or whether OpenMP (for parallelisation) can be found. To install OpenMP, follow these steps:

1. Install [homebrew](https://brew.sh/) via the terminal. Homebrew allows you to easily install software.
2. Install the latest version of `gcc` via the terminal command `brew install gcc`. 
3. Check what version of `gcc` you have with: `brew list --versions gcc`
4. Go to `~/.R` and open the file `Makevars`. Add the following code to that file:

```
CC=/usr/local/bin/gcc-10
CXX=/usr/local/bin/g++-10
CXX1X=/usr/local/bin/g++-10
CXX11=/usr/local/bin/g++-10
SHLIB_CXXLD=/usr/local/bin/g++-10
FC=/usr/local/bin/gfortran-10
F77=/usr/local/bin/gfortran-10
MAKE=make -j8

SHLIB_OPENMP_CFLAGS=-fopenmp
SHLIB_OPENMP_CXXFLAGS=-fopenmp
SHLIB_OPENMP_FCFLAGS=-fopenmp
SHLIB_OPENMP_FFLAGS=-fopenmp
```

If you do not have version 10 of `gcc`, replace all the 10s in the code above with your version number. 

5. Go to `/Library/Frameworks/R.framework/Resources/etc/` and open the file `Makeconf`. Here you need to 
add the library path to `SHLIB_LIBADD`: 

```
SHLIB_LIBADD = -L/usr/local/lib
```

6. Installing goldfish from source should work now. Otherwise get in contact with the developer team.
