object Solution {
    def twoSum(nums: Array[Int], target: Int): Array[Int] = {
        // create map of (index -> value)
        val numsMap = nums.indices.foldLeft(Map.empty[Int, Int])((mp, ind) => {
            mp ++ Map(nums(ind) -> ind)
        })
        // iterate through nums and check if it has 
        val result: Set[Int] = nums.indices.foldLeft(Set.empty[Int])((res, ind) => {
            val neededNum: Int = target - nums(ind)
            if (numsMap.contains(neededNum) && numsMap(neededNum) != ind) 
                res ++ List(ind, numsMap(neededNum))
            else res
        })
        result.toArray
    }
}

val ans = Solution.twoSum(Array(3,4,2), 6)
