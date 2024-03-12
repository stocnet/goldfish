## -*- mode: autoconf; autoconf-indentation: 4; -*-
##
## Copyright Dirk Eddelbuettel for RcppArmadillo (GPL-2)

VERSION=$(grep -i ^version DESCRIPTION | awk '{print $2}')
## require at least autoconf 2.69
AC_PREREQ([2.69])

## Process this file with autoconf to produce a configure script.
AC_INIT([Goldfish], @VERSION@)

## Set R_HOME, respecting an environment variable if one is set
: ${R_HOME=$(R RHOME)}
if test -z "${R_HOME}"; then
    AC_MSG_ERROR([Could not determine R_HOME.])
fi

## Use R to set CXX and CXXFLAGS
CXX=$(${R_HOME}/bin/R CMD config CXX)
CXXFLAGS=$("${R_HOME}/bin/R" CMD config CXXFLAGS)

## We are using C++
AC_LANG(C++)
AC_REQUIRE_CPP

# check openMP
openmp_flag=""
openmp_cflag=""

AC_MSG_CHECKING([for macOS])
RSysinfoName=$("${R_HOME}/bin/Rscript" --vanilla -e 'cat(Sys.info()[["sysname"]])')
if test x"${RSysinfoName}" == x"Darwin"; then
   AC_MSG_RESULT([found])
   AC_MSG_WARN([OpenMP unavailable and turned off.])
   openmp_flag="-DARMA_DONT_USE_OPENMP"
else
   AC_MSG_RESULT([not found as on ${RSysinfoName}])
   AC_MSG_CHECKING([for OpenMP])
   allldflags=$(${R_HOME}/bin/R CMD config --ldflags)
   hasOpenMP=$(echo ${allldflags} | grep -- -fopenmp)
   if test x"${hasOpenMP}" == x""; then
      AC_MSG_RESULT([missing])
      openmp_flag="-DARMA_DONT_USE_OPENMP"
   else
      AC_MSG_RESULT([found])
      openmp_flag='$(SHLIB_OPENMP_CXXFLAGS)'
      openmp_cflag='$(SHLIB_OPENMP_CFLAGS)'
   fi
fi

AC_SUBST([OPENMP_CFLAG], ["${openmp_cflag}"])
AC_SUBST([OPENMP_FLAG], ["${openmp_flag}"])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

echo "
 --------------------------------------------------
  Configuration for goldfish version ${VERSION}
  ================
  
    openMP: ${openmp_flag} 
    cxxflags: ${CXXFLAGS}
    libs:     ${PKG_LIBS}

  --------------------------------------------------
"