# Demo for comiplation using cmake (fortran 2008)

Code examples are taken from the book (with slight modifications)
- Rouson, D., Xia, J., and Xiaofeng, X.. 2014. Scientific Software 
Design: The Object-Oriented Way. </br> 2nd ed. Cambridge. Cambridge University Press

For more details see the official Fortran [webpage](https://fortran-lang.org/learn/)

## Direct compilation uisng command line
make a new dir
```shell
mkdir build
```

Compile the inner modules (dependencies in order) first
```shell
gfortran -std=f2008 -c kind_parameters.f08 -o build/kind_parameters.o -Jbuild/
gfortran -std=f2008 -c conduction_module.f08 -o build/conduction_module.o -Jbuild/
gfortran -std=f2008 -c problem_class.f08 -o build/problem_class.o -Jbuild/
gfortran -std=f2008 -c differentiator_class.f08 -o build/differentiator_class.o -Jbuild/
gfortran -std=f2008 -c conductor_classs.f08 -o build/conductor_classs.o -Jbuild/
```

followed by the final executable
```shell
gfortran -std=f2008 fin_test.f08 -o fin_test.o build/conduction_module.o build/kind_parameters.o build/problem_class.o build/differentiator_class.o build/conductor_classs.o -Jbuild/
```

option `Jbuild/` puts the intermediate build files in the directory `build`

or in one line using
```
gfortran -std=f2008 src/kind_parameters.f08 src/conduction_module.f08 src/problem_class.f08 src/differentiator_class.f08 src/conductor_classs.f08 fin_test.f08 -o fin_test.o -Jbuild/
```

## Build using CMAKE
Set the CMakeLists.txt file in root dir
```shell
cd build
cmake ..
make 
make clean # to clean builds
```

## Run test
```shell
cd ..
./fin_test.o
```

## Clean all build files
```shell
./AllcleanMake
```