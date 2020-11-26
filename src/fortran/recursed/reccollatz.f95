program collatz
implicit none

interface

        function recursiveCollatz(collatzNum)
                integer*8 :: recursiveCollatz
                integer*8 :: collatzNum
                
        end function recursiveCollatz

        subroutine bubbleSort(keyArray, otherArray)
                integer*8, dimension(10), intent(inout) :: keyArray
                integer*8, dimension(10), intent(inout) :: otherArray

                integer*8 :: temp
                INTEGER :: i, j
                LOGICAL :: swapped

        end subroutine bubbleSort
                
end interface



        integer*8 :: num, j, maxNum, minNum, counter, collatzNum, minLength, tempIndex, tempNum, newIndex
        integer*8, dimension(10) :: stepArray
        integer*8, dimension(10) :: magnitudeArray
        integer :: i, k, isSame, steps
        num = 5000000000
       


       do i=1,10
        stepArray(i) = 0
        magnitudeArray(i) = 0
       end do
 
       do while (num .ne. 0)
        
        collatzNum = num
        steps = recursiveCollatz(collatzNum)
        isSame = 0
        minLength = stepArray(1)
        minNum = 1
        newIndex = 1
        

        do i=1, 10
                if( minLength > stepArray(i) ) then
                        minLength = stepArray(i)
                        minNum  = i
                end if

                if ( stepArray(i) == steps) then
                        isSame = 1
                        newIndex = i               
                end if         
        end do
        
        j = minNum

        if( steps > stepArray(j) .and. isSame == 0) then
                magnitudeArray(j) = num
                stepArray(j) = steps
        end if

        if( num  < stepArray(newIndex) .and. isSame == 1) then
                magnitudeArray(newIndex) = num
        end if

        num = num - 1
        end do


        call bubbleSort(stepArray, magnitudeArray)
        print *, "Sequence Length Array (Magnitude -> Steps)"
         
        do i=10,1,-1
                print *,  magnitudeArray(i), stepArray(i)
        end do

        call bubbleSort(magnitudeArray, stepArray)
        print *, "Magnitude Array (Magnitude -> Steps)"

        do i=10,1, -1
                print *,  magnitudeArray(i), stepArray(i)
        end do

end program collatz

recursive integer(kind=8) function recursiveCollatz(collatzNum) result(recsteps)
implicit none
            integer*8 :: collatzNum
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


subroutine bubbleSort(keyArray, otherArray)
implicit none
        integer*8, dimension(10), intent(inout) :: keyArray
        integer*8, dimension(10), intent(inout) :: otherArray

        integer*8 :: temp
        INTEGER :: i, j
        LOGICAL :: swapped
        

 
         DO j = 10-1, 1, -1
                swapped = .FALSE.
                DO i = 1, j
                        IF (keyArray(i) < keyArray(i+1)) THEN
                                temp = keyArray(i)
                                keyArray(i) = keyArray(i+1)
                                keyArray(i+1) = temp

                                temp = otherArray(i)
                                otherArray(i) = otherArray(i+1)
                                otherArray(i+1) = temp
                                swapped = .TRUE.
                        END IF
                END DO
        IF (.NOT. swapped) EXIT
        END DO
end subroutine bubbleSort
