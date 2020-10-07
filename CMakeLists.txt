cmake_minimum_required(VERSION 3.5)

# name of project, could be any name
project(oneDimHeatTransfer)

# set .f08 to be treated as Fortran file in cmake
set_source_files_properties(fin_test.f08 PROPERTIES LANGUAGE Fortran)

enable_language(Fortran)

# only gfortran compiler
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(dialect "-ffree-form -std=f2008 -fimplicit-none")
endif()

# get all files for sources
file(GLOB SOURCES "src/*.f08")

# set all .f08 file in src dir to be treated as Fortran files
set_source_files_properties(${SOURCES} PROPERTIES LANGUAGE Fortran)

# put all dependencies in 'dependencyModules'
add_library(dependencyModules SHARED ${SOURCES})

# put the executable in the main (root) dir 
set(EXECUTABLE_OUTPUT_PATH ../)

# executable to run - output name matches target name
add_executable(fin_test fin_test.f08)

# set custom exeecutable name and extension for target `fin_test`
set_target_properties(fin_test PROPERTIES OUTPUT_NAME "fin_test" SUFFIX ".o")

# Link dependencies with the final executable (cmake figures order automatically)
# This should be after executable is set
# Make sure to link targets with this command. It can also link libraries and
# even flags, so linking a target that does not exist will not give a configure-time error.
target_link_libraries(fin_test PRIVATE dependencyModules)