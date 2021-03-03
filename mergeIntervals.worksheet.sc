import scala.collection.mutable.ListBuffer
// the idea is to take a string input of kind
// "1:2,    4:5, 3:4" with extra space in b/w 
// and then merge the intervals such that we get 
// "1:5" for above case

// another example "1:4,        7:10,  8:9"
// then we get "1:4, 7:10"
def mergeIntervals(rangeString: String): String = {
    // split the string on comma, then trim each element
    // then split on ":" and trim again and store each as a long tuple
    val cleanRangeList = rangeString.split(",").map(_.trim()).foldLeft(Seq.empty[(Long, Long)])((currAns, x) => {
        val aSplit = x.split(":")
        currAns ++ Seq((aSplit(0).trim.toLong, aSplit(1).trim.toLong))
    })

    val sortedList = cleanRangeList.sortWith(_._1 < _._1)
    // now that we have the soted list of longs we iterate on it and create the string
    var lastRangeEnd: Long = 0
    val res = ListBuffer.empty[String]
    for (i <- 0 until sortedList.length){
        val currTup = sortedList(i)
        println(currTup)
        // if lastRangeEnd is 0 i.e first value or current range start is greater than lastRangeEnd then we have 
        // a dijointed range and we can add it to the finalString ans
        if (lastRangeEnd == 0 || currTup._1 - 1 > lastRangeEnd) {
            res += currTup._1.toString + ":" + currTup._2.toString
            lastRangeEnd = currTup._2
        }
        else {
            val newEndRange = currTup._2 max lastRangeEnd
            println(newEndRange)
            res(res.length - 1) = res.last.split(":")(0) + ":" + newEndRange
            lastRangeEnd = newEndRange
        }
    }
    println(res)
    res.mkString(",")
}
mergeIntervals("1:4,        8:9,  7:10")
mergeIntervals("1:4,        8:22,  7:10")
mergeIntervals("1:4")
mergeIntervals("1:2,3: 12, 14:15,7:10, 11:12")
mergeIntervals("4:7,8:9,2:5,10:12,45:89,12:17,17:42")
mergeIntervals("34:36,37:45,42:48")