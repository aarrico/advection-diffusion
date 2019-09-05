program adv_diff

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Created by: Alexander Arrico
! Created on: 03.24.14
! Updated on: 04.01.14
!
! This is the main routine of a code to calculate 1-D
! advection-diffusion using finite volume discretization.
!
! Input is contained in the file input.dat
! Output is contained in the file output.dat
!
! Compile with gfortran adv_diff.f90 mesh.f90 init.f90 
!           output.f90 flux.90 pde.f90 compare.f90 trapz.f90 -o ad
!
! Plot in gnuplot with; 
! pl for [K=0:2] 'output.dat' i K u 1:2 w l title columnheader(4)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     implicit none

!Variable declarations
!From input file
     integer :: MM                 
     double precision :: a,b,dtout,tmax,factor,D,v

!Needed variables for execution
     integer :: stat,M,nsteps,Nmax,j
     double precision :: dt,dx,time,tout,dtexpl,error
     double precision, dimension(:), allocatable :: x,U,F

!Read data from input file
     open(unit=2,file='input.dat',iostat=stat)

     if (stat .ne. 0) then
          print*, "Error opening input.dat, stat=",stat
          stop
     else
          read(2,*)               !Read two comment lines 
          read(2,*)
          read(2,*) MM, tmax, dtout, factor, a, b, D, v
     endif
     close(2)

!Open file for output
     open(unit=3,file='output.dat',iostat=stat)
     if (stat .ne. 0) then
          print*, "Error opening output.dat, stat=",stat
          stop
     endif

!Find number of nodes and allocate arrays
     M  = (b-a)*MM
     dx = 1.0/MM
     allocate(x(0:M+1),U(0:M+1),F(1:M+1), stat=stat)
     if (stat .ne. 0) then 
          print*,'Cannot allocate memory for arrays! stat=',stat
          stop
     endif

!Set up mesh
     call mesh(a,b,M,dx,x)

!Set timestep
     dtexpl = 1/(v/dx + 2*D/(dx*dx))         !Max timestep for stability
     dt = factor*dtexpl
     Nmax = int(tmax/dt)+1

!Initialize
     call init(M,x,U)
     nsteps = 0
     time = 0.0
     tout = max(dtout,dt)
     call output(M,nsteps,dx,time,error,x,U)

!Begin timestepping
    do nsteps = 1,Nmax
         time = time + dt

         call flux(M,v,D,dx,U,F)
         call pde(M,dx,dt,time,x,F,U)
         !call compare(M,D,time,error,x,U)

         if (time .ge. tout) then
              call output(M,nsteps,dx,time,error,x,U)
              tout = tout + dtout
         endif
         if (time .ge. tmax) then
              print*,'Done at time=',time,' nsteps=',nsteps
              print*,'   max error=',error
              stop
         endif
    enddo
    print*,'Out of timesteps!! Set Nmax larger.'

end program adv_diff
