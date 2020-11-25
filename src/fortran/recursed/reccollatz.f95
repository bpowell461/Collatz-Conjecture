program collatz
implicit none

interface

        function recursiveCollatz(collatzNum)
                integer*16 :: recursiveCollatz
                integer*16 :: collatzNum
                
        end function recursiveCollatz
                
end interface



        integer*16 :: num, j, maxNum, minNum, counter, collatzNum, minLength, tempIndex, tempNum, newIndex
        integer*16, dimension(10) :: lengthArray
        integer*16, dimension(10) :: magnitudeArray
        integer :: i, k, isSame, steps
        num = 5000000000
       


       do i=1,10
        lengthArray(i) = 0
        magnitudeArray(i) = 0
       end do
 
       do while (num .ne. 0)
        
        collatzNum = num
        steps = recursiveCollatz(collatzNum)
        isSame = 0
        minLength = lengthArray(1)
        minNum = 1
        newIndex = 1
        

        do i=1, 10
                if( minLength > lengthArray(i) ) then
                        minLength = lengthArray(i)
                        minNum  = i
                end if

                if ( lengthArray(i) == steps) then
                        isSame = 1
                        newIndex = i               
                end if         
        end do
        
        j = minNum

        if( steps > lengthArray(j) .and. isSame == 0) then
                magnitudeArray(j) = num
                lengthArray(j) = steps
        end if

        if( num  < lengthArray(newIndex) .and. isSame == 1) then
                magnitudeArray(newIndex) = num
        end if

        num = num - 1
        end do


        do i=1,10
        
                do k= 1, 10-i
                
                        if(lengthArray(k) < lengthArray (k+1)) then 

                                tempIndex = lengthArray(k)
                                lengthArray(k) = lengthArray(k+1)
                                lengthArray(k+1)= tempIndex
                                

                                tempNum = magnitudeArray(k)
                                magnitudeArray(k) = magnitudeArray(k+1)
                                magnitudeArray(k+1) = tempNum
                        end if
                end do
        end do
        print *, "Sequence Length Array (Magnitude -> Steps)"
         
        do i=10,1,-1
                print *,  magnitudeArray(i), lengthArray(i)
        end do

        do i=1,10

                do k= 1, 10-i

                        if(magnitudeArray(k) < magnitudeArray (k+1)) then

                                tempIndex = lengthArray(k)
                                lengthArray(k) = lengthArray(k+1)
                                lengthArray(k+1)= tempIndex


                                tempNum = magnitudeArray(k)
                                magnitudeArray(k) = magnitudeArray(k+1)
                                magnitudeArray(k+1) = tempNum
                        end if
                end do
        end do


        print *, "Magnitude Array (Magnitude -> Steps)"

        do i=10,1, -1
                print *,  magnitudeArray(i), lengthArray(i)
        end do

end program collatz

recursive integer(kind=16) function recursiveCollatz(collatzNum) result(recsteps)
implicit none
            integer*16 :: collatzNum
            !integer :: recsteps
            if(collatzNum == 1) then
                  recsteps = 0
                  return
            else if((mod(collatzNum,2)) == 1) then
                 recsteps = recursiveCollatz((collatzNum * 3) + 1) + 1
            else
                  recsteps = recursiveCollatz(collatzNum / 2) + 1
            end if
end function recursiveCollatz
