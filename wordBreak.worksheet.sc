// brute force check of all possible sub strings
def wordBreak(s: String, wordDict: List[String]): Boolean = {
    val wordSet = wordDict.toSet
    def check(start: Int, curRes: Boolean): Boolean = {
        if (start >= s.length) true
        val flag = (start until s.length).foldLeft(curRes)((res, ind) => {
            if (wordSet.contains(s.substring(start, ind+1)) && check(ind+1, true)){
                return true
            }else false
        })
        flag
    }

    // def matchWord(subString: String): Boolean = {
    //     if (wordSet.contains(subString)) true
    // }
    check(0, true)
}


val testStr = "timeistime"
val wordDict = List("time", "is")
wordBreak(testStr, wordDict)

val wordDict2 = List("time", "i")
wordBreak(testStr, wordDict2)