program hilbert
   implicit none
! TODO: Write function interface
!      for the calculation of the determinant of 3x3 matrix
  interface
     function getdet(minor) result(determinant)
       real (kind=4) :: determinant
       real (kind=4), intent(in) :: minor(3,3)
     end function getdet
  end interface

! TODO: Complete variable declerations
   real (kind=4) :: hmat(4,4), minor(3,3)
   real (kind=8) :: dethmat
   integer (kind=4) :: i, j, k, l, m, i_minor, j_minor 

! initialise Hilbert matrix and print
   do i = 1, 4
      do j = 1, 4
         hmat(i,j) = 1.0/(i + j - 1)
      end do
      write(*,*) hmat(i,:)
   end do

! Find determinant of 4x4 Hilbert matrix using Cramer rule
   dethmat = 0.0
   do i = 1, 4
      if (mod(i, 2) /= 0) then
         m = 1
      else
         m = -1
      end if
      
      do j = 2, 4
         l = 1
         do k = 1, 4
            if (k /= i) then
               minor(j-1,l) = hmat(j, k)
               l = l + 1
            end if
         end do
      end do
      write(*,*) m, hmat(1,i), " * ", getdet(minor)
      dethmat = dethmat + m * hmat(1,i) * getdet(minor)
   end do

! TODO: Print calculated determinant
    write(*,*) "Determinant of the Hilbert matrix is ", dethmat
    stop
end program hilbert


! TODO: Complete function definition to get determinant
real (kind=4) function getdet(minor)
   real (kind=4), intent(in) :: minor(3,3)
   getdet = minor(1,1) * (minor(2,2) * minor(3,3) - minor(3,2) * minor(2,3)) &
   	  - minor(1,2) * (minor(2,1) * minor(3,3) - minor(3,1) * minor(2,3)) &
   	  + minor(1,3) * (minor(2,1) * minor(3,2) - minor(3,1) * minor(2,2))
end function getdet


