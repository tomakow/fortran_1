module subrutyny
contains

	! SUBRUTYNY WYPEŁNIANIA TABEL - wypełniają tabele losowymi liczbami z funkcji random_number

	subroutine fill_table_1 (table, size)
	implicit none
	integer (kind=4), intent (in) :: size
	real (kind=4), intent (out) :: table(size)

	call random_number(table)

	!write (*,*) table

	end subroutine


	subroutine fill_table_2 (table, size1, size2)
	implicit none
	integer (kind=4), intent (in) :: size1, size2
	real (kind=4), intent (out) :: table(size1, size2)

	call random_number(table)

	!write (*,*) table

	end subroutine

	! SUBRUTYNA ALGORYTMU - przyjmuje dwie table i stosuje algorytm Gauss'a

	subroutine calculation_algorithm  (table_a, table_x, size_a1, size_a2, size_x1)
	implicit none
	integer (kind=4), intent (in) :: size_a1, size_a2, size_x1
	integer (kind=4) :: i, j
	real (kind=4), intent (out) :: table_a(size_a1, size_a2), table_x(size_x1)
	real (kind=4) :: c

	do i=1,size_a1
		do j=1,size_a2
			if (i .ne. j) then
				c = ( table_a(i,j) / table_a(i,i) )
				table_a(:,j) = table_a(:,j) - c*table_a(:,i)
				table_x(j) = table_x(j) - c*table_x(i)
				table_x(i) = table_x(i) / table_a(i,i)
				table_a(:,i) = table_a(:,i)/table_a(i,i)
			end if
		end do
	end do

	end subroutine

end module subrutyny



program main

! DOŁĄCZANE MODUŁY

use subrutyny

implicit none

! DEFINICJA ZMIENNYCH

integer (kind=4) :: iSize
real (kind=4), allocatable, dimension(:,:) :: a
real (kind=4), allocatable, dimension(:) :: x

iSize = 3

! ALOKOWANIE TABLIC

allocate (a(iSize,iSize))
allocate (x(iSize))

! WYPEŁNIANIE TABLIC

call fill_table_1 (x, iSize)
call fill_table_2 (a, iSize, iSize)

! ALGORYTM OBLICZENIOWY

call calculation_algorithm (a, x, iSize, iSize, iSize)

!write (*,*) a
!write (*,*) x

end program