def prefixSum(lst: List[Int], target: Int): Int = {
    // var count = 0
    // var currSum= 0
    val map = scala.collection.mutable.HashMap.empty[Int, Int]
    val (count, _, _) = lst.foldLeft((0, 0 , map))((ans, num) => {
        val (currCount, currSum, currMap) = ans
        val newSum = currSum + num
        currMap(newSum) = 1
        if (currSum == target)
            ((currCount + 1 + currMap.getOrElse(newSum-target, 0), newSum, currMap))
        else
            ((currCount + currMap.getOrElse(newSum-target, 0), newSum, currMap))
    })
    count
}

val lst = List(3,4,1,6,-3)
prefixSum(lst, 7)