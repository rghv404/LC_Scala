def longestSubStringEqualCharNums(arr: String): String = {
    // the idea is to signify char with +1 and num with -1
    // keep summing while iterating the string and store new sums to a map
    // and when same sun is seen again signifying that  
    val sumMap = scala.collection.mutable.HashMap.empty[Int, Int]
    sumMap(0) = -1
    var currSum = 0
    val (startInd, endInd) = arr.indices.foldLeft((0, 0)){ (tup, ind) => {
        if (arr(ind) >= 'a' && arr(ind) <= 'z') currSum = currSum + 1
        else currSum = currSum - 1
        if (!sumMap.contains(currSum)) {
            sumMap(currSum) = ind
            // println(tup)
            tup
        }
        else if (tup._2 - tup._1 < ind - sumMap(currSum)){
            // println(tup, (sumMap(currSum), ind))
            (sumMap(currSum), ind)
        }
        else tup
    }}
    println(startInd, endInd)
    arr.substring(startInd + 1, endInd + 1)
}

val test = "abcfgt564nhj67898"
val another = "abcde123lmnop123345"//"abcde1234"

val ans = longestSubStringEqualCharNums(another)