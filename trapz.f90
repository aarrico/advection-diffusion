     double precision function TrapzRule(M,dx,U)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Created by: Alexander Arrico
! Created on: 04.03.14
! Updated on: 04.03.14
!
! Function to calculate the area under the curve of U(x)
! Based on code provided by Dr. Alexiades for Lab 8 in
! MATH 475 at University of Tennessee.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          integer, intent(in) :: M
          double precision, intent(in) :: dx
          double precision, dimension(0:M+1), intent(in) :: U
          integer :: j
          double precision :: total
     
          total = (U(0) + 3*(U(1)+U(M)) + U(M+1))/4
          do j=2,M-1
               total = total + U(j)
          enddo
          TrapzRule = dx*total
 
     end function TrapzRule
