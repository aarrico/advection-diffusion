subroutine mesh(a,b,M,dx,x)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Created by: Alexander Arrico
! Created on: 03.22.14
! Updated on: 03.24.14
!
! Set up space-time mesh for finite volume 
! discretization.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     implicit none

!Variables
     integer, intent(in) :: M
     double precision, intent(in) :: a,b,dx
     double precision, dimension(0:M+1), intent(out) :: x
     integer :: j
     double precision, dimension(M+1) :: faces

!Set points in array
     faces(1) = 0.0
     do j=2,M+1
         faces(j) = faces(j-1)+dx   
     enddo
     
     x(0) = a
     do j=1,M
        x(j) = 0.5*(faces(j+1) + faces(j))
     enddo
     x(M+1) = b

end subroutine mesh
