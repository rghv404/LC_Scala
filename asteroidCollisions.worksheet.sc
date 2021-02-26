object Solution {
    def asteroidCollision(asteroids: Array[Int]): Array[Int] = {
        val stack = scala.collection.mutable.Stack.empty[Int]
        stack.push(asteroids(0))
        for (i <- 1 until asteroids.length){
            // enter the main logic once we see two elements going into right and left respectively
            if (!stack.isEmpty && stack.head > 0 && asteroids(i) < 0){
                // continue popping elements till we have top of stack strictly less than curr absolute
                while (!stack.isEmpty && stack.head > 0 && stack.head < asteroids(i).abs)
                    stack.pop
                // pop again if same elements, this if allows us to only push the -ve value
                // to the stack inside if else otherwise we fail at situations like [8,-8]
                if (!stack.isEmpty && stack.head == asteroids(i).abs)
                    stack.pop
                // else if not equal i.e not removing both elems then push the elem 
                // if at beginning of stack OR the head elem is also negative
                else if (stack.isEmpty || stack.head < 0) stack.push(asteroids(i))
            }
            else stack.push(asteroids(i))
        }
        stack.toArray.reverse
    }
}

val test = Array(1,-2,3,4)
val test2 = Array(-2,-4,-5,-6)
val test3 = Array(-2,-4,-5,-6,1,2,3,4)
val test4 = Array(3,4,-7,4,6,8,-10)
val test5 = Array(19,-8, 11, 3, 4, -7, 4, 6, 8, -10)
Solution.asteroidCollision(test)
Solution.asteroidCollision(test2)
Solution.asteroidCollision(test3)
Solution.asteroidCollision(test4)
Solution.asteroidCollision(test5)