subroutine compare(M,D,time,error,x,U,uExact)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Created by: Alexander Arrico
! Created on: 03.24.14
! Updated on: 03.24.14
!
! Compares numerical solution vs analytic.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

!Variables
     integer, intent(in) :: M
     double precision, intent(in) :: D,time
     double precision, intent(out) :: error
     double precision, dimension(0:M+1), intent(in) :: x,U
     double precision, dimension(0:M+1), intent(out) :: uExact
     integer :: j
     double precision :: tmperr,arg

!Reset error for this time iteration
     error = 0.0

!Calculate the exact u and error with numerical solution
     do j=0,M+1
          arg = (0.5*x(j))/sqrt(D*time)
          uExact(j) = derfc(arg)
          tmperr = abs(U(j) - uExact(j))
          error = max(tmperr,error)
     enddo

end subroutine compare
