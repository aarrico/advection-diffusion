# advection-diffusion
Fortran program from grad school to solve the advection-diffusion equation

Input is contained in the file input.dat
Output is contained in the file output.dat

Compile with gfortran adv_diff.f90 mesh.f90 init.f90 
           output.f90 flux.90 pde.f90 compare.f90 trapz.f90 -o ad

Plot in gnuplot with:
pl for [K=0:2] 'output.dat' i K u 1:2 w l title columnheader(4)
