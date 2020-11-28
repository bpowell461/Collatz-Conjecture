program collatz
implicit none

interface

        subroutine bubbleSort(keyArray, otherArray)
                integer*8, dimension(10), intent(inout) :: keyArray 
                integer*8, dimension(10), intent(inout) :: otherArray

                integer*8 :: temp
                INTEGER :: i, j
                LOGICAL :: swapped

        end subroutine bubbleSort
        
        function loopCollatz(collatzNum) result(out)
                integer*8 :: collatzNum
                integer :: out
                
        end function loopCollatz
end interface



        integer*8 :: num, j, maxNum, minNum, counter, collatzNum, minLength, tempIndex, tempNum, newIndex
        integer*8, dimension(10) :: stepArray
        integer*8, dimension(10) :: magnitudeArray
        integer :: i, k, isSame, steps
        num = 5000000000
       


       do i=1,10
        stepArray(i) = 0      !initializing arrays to hold 0 for comparisons
        magnitudeArray(i) = 0
       end do
 
       do while (num .ne. 0)
        
        collatzNum = num
        steps = loopCollatz(collatzNum) !num and amount of steps in sequence
        isSame = 0                      !duplicate checker
        minLength = stepArray(1)        !smallest steps in seq.
        minNum = 1                      !smallest number in seq.
        newIndex = 1                    !used for replacing minimum steps and num

        do i=1, 10
                if( minLength > stepArray(i) ) then         !Is the current num steps greater than the steps in the seq.?
                        minNum  = i
                        minLength = stepArray(i)
                end if

                if ( stepArray(i) == steps) then        !Do the numbers have the same amount of steps? Take the smaller number. 
                        isSame = 1
                        newIndex = i               
                end if         
        end do
        
        j = minNum                                      !Index to replace with the smaller number
        
        if(isSame == 0) then                            !Is it not the same?
            if( steps > stepArray(j)) then
                magnitudeArray(j) = num                 !Place in array accordingly
                stepArray(j) = steps
            end if
        else 
            if( num  < stepArray(newIndex)) then
                magnitudeArray(newIndex) = num          !If it is the same then place the newest number, which will be smaller
            end if
        end if
        num = num - 1
        end do


        call bubbleSort(stepArray, magnitudeArray)      !Sort array and print
        print *, "Sequence Length Array (Magnitude -> Steps)"
         
        do i=10,1,-1
                print *,  magnitudeArray(i), stepArray(i)
        end do

        call bubbleSort(magnitudeArray, stepArray)      !Sort array and print
        print *, "Magnitude Array (Magnitude -> Steps)"

        do i=10,1, -1
                print *,  magnitudeArray(i), stepArray(i)
        end do

end program collatz


subroutine bubbleSort(keyArray, otherArray)         !Rosetta Code implementation of Bubble Sort
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

function loopCollatz(collatzNum) result(out)
                integer*8 :: collatzNum
                integer :: out
                 out = 0
                do while (collatzNum .ne. 1)            !Generating Collatz Seq. for a specific number

                if((mod(collatzNum,2)) == 1) then
                        collatzNum = (collatzNum * 3) + 1
                        
                else
                        collatzNum = collatzNum / 2
                end if

                out = out + 1
        end do
end function loopCollatz
