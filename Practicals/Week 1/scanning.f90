
program scanning
   integer (kind=4) :: num1, num2, error
   logical (kind=1) :: test

   error = 1
   write(6,*) 'Please input two positive integers'
   do while (error .ne. 0)
     read(5,*,iostat=error) num1,num2
     if (error .ne. 0) then     
       write(6,*) 'Please try again'
     endif
   end do

   if (num1.le.0 .or. num2.le.0) then
     write(6,*) 'These numbers are not positive!'
     
   else

     write(6,'(a,i0,a,i0)') ' You entered ',num1,' and ',num2
     if (mod(num1,2) .eq. 0) then
        write(6,'(a)',advance='NO') ' num1 is even and'
     else
        write(6,'(a)',advance='NO') ' num1 is odd and'
     endif

     if (mod(num2,2) .eq. 0) then
        write(6,'(a)',advance='NO') ' num2 is even and'
     else
        write(6,'(a)',advance='NO') ' num2 is odd and'
     endif
     write(6,*)
   endif

end program scanning



