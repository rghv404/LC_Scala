import scala.collection.mutable._
object Solution {
    def partitionLabels(S: String): List[Int] = {
        //create a map of with val as key and last ind as character
        val lastIndMap = S.zipWithIndex.foldLeft(HashMap.empty[Char, Int])((currMap, indValTup) => {
            val (value, ind) = indValTup
            currMap.update(value, ind)
            currMap
        })
        // now that we have index of each last val whenever we find the value to be 
        // equal to that index while iterating we can partiton the string there
        val (_, _, res) = S.indices.foldLeft((0, 0, List.empty[Int]))((currAns, ind) => {
            val (anchor, end, retVal) = currAns
            if (lastIndMap(S(ind)) > end) (anchor, lastIndMap(S(ind)), retVal)
            else if (ind == end) (ind + 1, ind + 1, retVal :+ (end - anchor + 1))
            else currAns
        })
        res
    }
}

Solution.partitionLabels("ababcbacadefegdehijhklij")
Solution.partitionLabels("abbacacd")