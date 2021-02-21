import java.util.Random
import scala.collection.mutable.ArrayBuffer

object Solution {
    def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {
        // create counter bucket of nums
        val countBucket = nums.indices.foldLeft(scala.collection.mutable.HashMap.empty[Int, Int])((mp, ind) => {
            if (mp.contains(nums(ind)))
                mp(nums(ind)) = mp(nums(ind)) + 1
            else
                mp(nums(ind)) = 1
            mp
        })
        println(countBucket)
        val arrayList = Array.ofDim[Int](nums.length + 1, 0)
        
        for (i <- countBucket.keySet){
            // if (countBucket.contains(nums(i)) && !countBucket(nums(i)).contains(nums()))
            arrayList(countBucket(i)) = arrayList(countBucket(i)) ++ Array(i)
        }
        arrayList.flatten.reverse.take(k)
        // Array.empty[Int]
    }
}

object SolutionUsingQuickSelect{
    def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {
        //create a map of count 
        val countBucket = nums.indices.foldLeft(scala.collection.mutable.HashMap.empty[Int, Int])((map, ind) => {
            if (map.contains(nums(ind))){
                map(nums(ind)) = map(nums(ind)) + 1
            }
            else map(nums(ind)) = 1
            map
        })
        println(countBucket)
        val listToSelect = countBucket.values.toList
        println(listToSelect)
        val pvt = quickSelect(listToSelect, listToSelect.length - k)
        println(pvt)
        val neededMap = countBucket.filter(tup => tup._2 >= pvt)
        println(neededMap)
        neededMap.keys.toArray.takeRight(k)
    }

    def isSorted[T](arr: List[T])(implicit ord: Ordering[T]): Boolean = arr match {
        case Nil => true
        case x :: Nil => true
        case x :: xs => ord.lteq(x, xs.head) && isSorted(xs)
    }

    def quickSelect(nums: List[Int], k: Int): Int = {
        // if the input array is sorted then no point partitioning furher
        // and go into a potential infinite loop even with random pivot
        // logical to pick the kth element from sorted array
        if (isSorted(nums)) return nums(k)
        // else start the partition logic
        val pvt = (new scala.util.Random).nextInt(nums.length)
        val (lower, higher) = nums.partition( _ < nums(pvt))
        println(pvt, lower, higher)
        if (lower.length > k) quickSelect(lower, k)
        else if (lower.length < k) quickSelect(higher, k - lower.length)
        else nums(pvt)
    }
}

def checkQS(nums: List[Int], k: Int): List[Int] = {
    val pvt = SolutionUsingQuickSelect.quickSelect(nums, k)
    println(pvt)
    nums.filter(_ <= pvt).take(k)
}

Solution.topKFrequent(Array(3,0,1,0), 2)
SolutionUsingQuickSelect.topKFrequent(Array(3,4,0,1,0,2,0), 3)
SolutionUsingQuickSelect.topKFrequent(Array(3,0,1,0), 2)