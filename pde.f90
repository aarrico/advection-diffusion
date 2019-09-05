subroutine pde(M,dx,dt,time,x,F,U)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Created by: Alexander Arrico
! Created on: 03.24.14
! Updated on: 04.03.14
!
! Solves the PDE.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

!Variables
     integer, intent(in) :: M
     double precision, intent(in) :: dx,dt,time
     double precision, dimension(0:M+1), intent(in) :: x
     double precision, dimension(1:M+1), intent(in) :: F
     double precision, dimension(0:M+1), intent(out) :: U
     integer :: j

     U(0) = 0.0
     do j=1,M
          U(j) = U(j) + dt/dx*(F(j)-F(j+1))
     enddo
     U(M+1) = 0.0

end subroutine pde
