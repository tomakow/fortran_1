#define n 10

program main

implicit none

integer (kind=4) :: i, j, c
real (kind=8) :: a(n,n), x(n)

do i=1,n
	do j=1,n
		if (i .ne. j) then
			c = ( a(i,j) / a(i,i) )
				a(:,j) = a(:,j) - c*a(:,i)
				x(j) = x(j) - c*x(i)
				x(i) = x(i) / a(i,i)
				a(:,i) = a(:,i)/a(i,i)
		end if
	end do
end do

print *, c

end program