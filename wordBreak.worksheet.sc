import scala.collection.mutable.ArrayBuffer
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

def wordBreakDP(s: String, wordDict: List[String]): Boolean = {
    val dp = (0 until s.length()+1).map(_ => false).toBuffer
    dp(0) = true
    val wordSet = wordDict.toSet
    def loop(j:Int, end: Int): Int = {
        if (j < end && dp(j) && wordSet.contains(s.substring(j, end))){
                dp(end) = true
                end
        }
        else if (j < end) loop(j + 1, end)
        else return -1
    }

    for(i <- 1 until s.length() + 1)
        loop(0, i)
    println(dp)
    dp(s.length())
}

val testStr = "timeistime"
val wordDict = List("time", "is")
wordBreak(testStr, wordDict)

val wordDict2 = List("time", "i")
wordBreak(testStr, wordDict2)

val testStr2 = "abcd"

wordBreakDP(testStr2, wordDict)