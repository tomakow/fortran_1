program main

implicit none

integer (kind=4) :: i, j, c, test
real (kind=8), allocatable, dimension(:,:) :: a
real (kind=8), allocatable, dimension(:) :: x

test = 10

allocate (a(test,test))
allocate (x(test))


do i=1,test
	do j=1,test
		if (i .ne. j) then
			c = ( a(i,j) / a(i,i) )
				a(:,j) = a(:,j) - c*a(:,i)
				x(j) = x(j) - c*x(i)
				x(i) = x(i) / a(i,i)
				a(:,i) = a(:,i)/a(i,i)
		end if
	end do
end do

write (*,*) size(a(:,:))
write (*,*) size(x)

print *, c

end program