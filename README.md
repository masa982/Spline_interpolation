# Spline_interpolation
Spline interpolation using Fortran

spline interpolation is a form of interpolation 
where the interpolant is a special type of piecewise polynomial called a spline.

reference URL : https://en.wikipedia.org/wiki/Spline_interpolation

inverse matrix is calculated using "row reduction" also as known "Gaussian elimination".

reference URL : https://en.wikipedia.org/wiki/Gaussian_elimination 
  
# Requirement 
 
 Nothing
 
# Installation
 
 Nothing
 
# Usage
 
```bash
git clone https://github.com/masa982/Spline_interpolation.git
gfortran -c sweep_sub.f90
gofortran sweep_sub.o test2.f90
./a.out
```
or

```bash
git clone https://github.com/masa982/Spline_interpolation.git
make
```

 
# Note
 
if you want to interpolate your data file, Please change data.dat
 
# Author
 
masa982

valley-well
