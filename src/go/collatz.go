package main

import (
"fmt"
)
var (
	IsSame bool
	num int64 = 5000000000
	steps int64
	collatzNum int64
	newIndex int
	j int
	i int
	stepArray[10]int64
	magnitudeArray[10]int64
	minNum int
	minSteps int64
)
func bubbleSort(input []int64, other []int64) {
    n := 10
    swapped := true
    for swapped {
        swapped = false
        for i := 1; i < n; i++ {
            if input[i-1] > input[i] {
                input[i], input[i-1] = input[i-1], input[i]
		other[i], other[i-1] = other[i-1], other[i]
                swapped = true
            }
        }
    }
}
func loopCollatz(collatzNum int64) int64{
	steps = 0
	for (collatzNum != 1){
			if(collatzNum%2 == 1){
				collatzNum = (collatzNum*3)+1

			} else {

				collatzNum = (collatzNum/2)
			}
		
			steps = steps + 1
		}

	return steps
}

func main(){

	/*stepArray = [10]int64{0,0,0,0,0,0,0,0,0,0}*/
	/*magnitudeArray = [10]int64{0,0,0,0,0,0,0,0,0,0}*/

	
	for (num != 0){
		collatzNum=num
		minNum = 0
		IsSame = false
		minSteps = stepArray[0]
		steps = loopCollatz(collatzNum)
		newIndex = 0

		for i = 0; i < 10; i++{
			if(minSteps > stepArray[i]){
				minSteps = stepArray[i]
				minNum = i
			}
			if(stepArray[i] == steps){
				IsSame = true
				newIndex = i
			}

		}

		j = minNum
		/*fmt.Println(IsSame)*/
		if(!IsSame){
		if(steps > stepArray[j]){
			magnitudeArray[j] = num
			stepArray[j] = steps
		}
		}else{
			if(num < stepArray[newIndex]){
				magnitudeArray[newIndex] = num
			}
		}
		num = num -1
	}

	bubbleSort(stepArray[:], magnitudeArray[:])	
	fmt.Println("Sequence Length Array (Magnitude -> Steps)")
	for i = 0; i < 10; i++{
		fmt.Println(magnitudeArray[i], " ", stepArray[i])
	}
	bubbleSort(magnitudeArray[:], stepArray[:])
        fmt.Println("Magnitude Array (Magnitude -> Steps)")
        for i = 0; i < 10; i++{
                fmt.Println(magnitudeArray[i], " ", stepArray[i])
        }

}
