program collatz

        integer*16 :: num, j, maxNum, minNum, counter, collatzNum, minLength, tempIndex, tempNum
        integer*16, dimension(10) :: lengthArray
        integer*16, dimension(10) :: magnitudeArray

        num = 5000000000
       


       do i=1,10
        lengthArray(i) = 0
        magnitudeArray(i) = 0
       end do
 
       do while (num .ne. 0)
        
        collatzNum = num
        steps = 0
        isSame = 0
        minLength = lengthArray(1)
        minNum = 1
        newIndex = 1
        do while (collatzNum .ne. 1)

                if((mod(collatzNum,2)) == 1) then
                        collatzNum = (collatzNum * 3) + 1
                        
                else
                        collatzNum = collatzNum / 2
                end if

                steps = steps + 1
        end do

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

                do k= 1, 10-i !Rosetta Code Implementation of Bubble Sort

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
