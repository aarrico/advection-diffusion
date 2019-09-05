subroutine flux(M,v,D,dx,U,F)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Created by: Alexander Arrico
! Created on: 03.23.14
! Updated on: 04.02.14
!
! Calculates flux using backwards difference.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

!Variables
     integer, intent(in) :: M
     double precision, intent(in) :: v,D,dx
     double precision, dimension(0:M+1), intent(in) :: U
     double precision, dimension(1:M+1), intent(out) :: F
     integer :: j

!Calculate internal fluxes
     do j=1,M+1
          F(j) = v*U(j-1) - D/dx*(U(j)-U(j-1))
     enddo

end subroutine flux
