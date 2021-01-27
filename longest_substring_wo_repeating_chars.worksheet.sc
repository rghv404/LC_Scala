object Solution {
    def lengthOfLongestSubstring(s: String): Int = {
        var indexMap = scala.collection.mutable.HashMap.empty[Char, Int]
        var start = 0
        var maxLen = 0
        s.indices.foreach{ i =>
            if (indexMap.contains(s(i)) && indexMap(s(i)) >= start){
                maxLen = maxLen max (i - start)
                start = indexMap(s(i)) + 1
            }
            indexMap(s(i)) = i
        }
        maxLen max (s.size - start)
    }
}

val ans = Solution.lengthOfLongestSubstring("helkemitokejmjqwmymnz")
print(ans)