subroutine init(M,x,U)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Created by: Alexander Arrico
! Created on: 03.24.14
! Updated on: 03.24.14
!
! Initialize  U for the advection-diffusion
! problem.
! Inital profile is a square bump
! U(x) = 5.0 for 1 <= x <= 2 and 0 otherwise  
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     implicit none

!Variables
     integer, intent(in) :: M
     double precision, dimension(0:M+1), intent(in) :: x
     double precision, dimension(0:M+1), intent(out) :: U
     integer :: j

     do j=0,M+1
          if(x(j) .ge. 1.0 .and. x(j) .le. 2.0) then
               U(j) = 5.0
          else
               U(j) = 0.0
          endif
     enddo

end subroutine init
