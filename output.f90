subroutine output(M,nsteps,dx,time,error,x,U)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Created by: Alexander Arrico
! Created on: 03.23.14
! Updated on: 04.03.14
!
! Prints profile of U
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     implicit none
!Variables
     integer, intent(in) :: M,nsteps
     double precision, intent(in) :: dx,time
     double precision, dimension(0:M+1), intent(in) :: x,U
     double precision, intent(out) :: error
     integer :: j
     character(len=50) :: fmt
     double precision,external :: TrapzRule
 
      error = TrapzRule(M,dx,U)

      fmt = "(A,f9.4,A,I6)"
      write(3,fmt) "Profile at time: ",time,"  nsteps=",nsteps
      fmt = "(A,e15.6)"
      write(3,fmt) "# Error up to this time: ", error
      fmt = "( f12.4, g16.8, g16.8 )"
      DO j = 0, M+1
           write(3,fmt) x(j), U(j)
      ENDDO
      write(3,"(/)")        !! blank line for gnuplot to break plot

end subroutine output
