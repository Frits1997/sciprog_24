program Trapezodial
    implicit none
    real (kind=4) :: a = 0.0
    real (kind=4) :: b=3.1415927/3
    real (kind=4) :: sum_val=0.0
    real (kind=4) :: equidistian_val=0.0
    
    integer (kind=4) :: i
    do i = 1, 12, 1
       equidistian_val = 2 * tan((b/12)*i)
       sum_val = sum_val + equidistian_val
    end do
    
    sum_val = sum_val * (b-a)/(2*i)
    
    write(6,*) sum_val
    write(6,*) log(2.0)
    
end program Trapezodial
