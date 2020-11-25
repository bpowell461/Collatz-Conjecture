function bubblesort(keyArray, otherArray) #rosetta code implementation of bubble sort
    for _ in 2:length(keyArray), j in 1:length(keyArray)-1
        if keyArray[j] > keyArray[j+1]
            keyArray[j], keyArray[j+1] = keyArray[j+1], keyArray[j]
            otherArray[j], otherArray[j+1] = otherArray[j+1], otherArray[j]
        end
    end
end

global num = 5000000000

stepArray = [0,0,0,0,0,0,0,0,0,0]
magnitudeArray = [0,0,0,0,0,0,0,0,0,0]


while (num != 0)

	collatzNum = num
	isSame = 0
	minNum = 1
	minSteps = stepArray[1]

	steps = 0

	newIndex = 1

	while(collatzNum != 1)
		if(collatzNum % 2 == 1)

			 collatzNum = (collatzNum*3)+1
		else
			collatzNum = collatzNum/2
		end
		steps=steps+1
	end
	for i = 1:10
		if(minSteps > stepArray[i])

			minSteps = stepArray[i]
			minNum = i
		end
		if(stepArray[i] == steps)

			 isSame = 1
			 newIndex = i
		end
	end

	j = minNum
	if(steps > stepArray[j] && isSame == 0)
		magnitudeArray[j] = num
		stepArray[j] = steps
	end
	if(num < stepArray[newIndex] && isSame == 1)
		magnitudeArray[newIndex] = num
	end
	
	global num = num - 1

end


bubblesort(stepArray, magnitudeArray)
println("Sequence Length Array (Magnitude -> Steps)")
for i=1:10
	println(magnitudeArray[i], " ", stepArray[i])
end
bubblesort(magnitudeArray, stepArray)
println("Magnitude Array (Magnitude -> Steps)")
for i=1:10
        println(magnitudeArray[i], " ", stepArray[i])
end

exit(0)

