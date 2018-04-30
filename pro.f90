module subrutyny
contains
	subroutine fill_table_1 (table, size)
	implicit none
	integer (kind=4) :: size
	real (kind=4) :: table(size)

	call random_number(table)

	!write (*,*) table

	end subroutine


	subroutine fill_table_2 (table, size1, size2)
	implicit none
	integer (kind=4) :: size1, size2
	real (kind=4) :: table(size1, size2)

	call random_number(table)

	!write (*,*) table

	end subroutine
end module subrutyny



program main

use subrutyny

implicit none

integer (kind=4) :: i, j, iSize
real (kind=4) :: c
real (kind=4), allocatable, dimension(:,:) :: a
real (kind=4), allocatable, dimension(:) :: x

iSize = 10


! ALOKOWANIE TABLIC

allocate (a(iSize,iSize))
allocate (x(iSize))

! WYPEŁNIANIE TABLIC

call fill_table_1 (x, iSize)
call fill_table_2 (a, iSize, iSize)

! ALGORYTM OBLICZENIOWY

do i=1,iSize
	do j=1,iSize
		if (i .ne. j) then
			c = ( a(i,j) / a(i,i) )
				a(:,j) = a(:,j) - c*a(:,i)
				x(j) = x(j) - c*x(i)
				x(i) = x(i) / a(i,i)
				a(:,i) = a(:,i)/a(i,i)
		end if
	end do
end do

!write (*,*) size(a(:,:))
!write (*,*) size(x)

write (*,*) a
write (*,*) x

end program