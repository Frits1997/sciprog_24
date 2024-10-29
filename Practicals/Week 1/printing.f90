program printing
   implicit none
   integer (kind=4) :: accountnum=1
   real (kind=4) :: subtotal=1234.56, total=7890.12

   write(6,'(a,i2,a,f8.2,a,f8.2)') 'Account:',accountnum,' Subtotal:', &
&           subtotal,' Total:',total

   stop
end program printing



