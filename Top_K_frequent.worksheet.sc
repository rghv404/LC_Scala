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

Solution.topKFrequent(Array(3,0,1,0), 2)