program linear_regression
    implicit none
! TODO: Complete variable declarations
    integer (kind=4) :: n, i
    real (kind=4) :: x(100), y(100), alpha, beta

! TODO: Enter the Number of data points    
    n = 50

! We check that n is less than the maximum array size.
    if(n>100) then
        write(6,*) "Number of data points must be less than 100"
        stop
    end if

! TODO: Enter (xi, yi) values i=1, 2, ..., n
    do i = 1, n
    	x(i) = i
    	y(i) = i * 0.658 + 2.367
   end do

! TODO: Calculate alpha and beta formulas
!      Use ** operator for the calculation of power of 2 in the formula.
   alpha = (n * sum(x(1:n) * y(1:n)) - sum(x(1:n)) * sum(y(1:n)))/(n * sum(x(1:n) ** 2) - sum(x(1:n)) ** 2)
   
!      Check and exit the code if the denominator for a is equal to zero.
   beta = (sum(y(1:n)) - alpha * sum(x(1:n)))/n

! TODO:  Print the equation of the line 
   write(6,"(a, F5.2, 1x, a, F5.2)") "Regression formula: y =", alpha, "* x +", beta
   
end program linear_regression

