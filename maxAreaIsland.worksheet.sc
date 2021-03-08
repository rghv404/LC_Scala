object Solution {
    def maxAreaOfIsland(grid: Array[Array[Int]]): Int = {
        var area = 0
        var maxArea = 0
        def dfs(i: Int, j: Int): Unit = {
            if (i < 0 || i >= grid.length || j < 0 || j >= grid(0).length || grid(i)(j) == 0) return
            area += 1
            grid(i)(j) = 0
            dfs(i-1, j) // up
            dfs(i+1, j) // down
            dfs(i, j-1) // left
            dfs(i, j+1) // right
        }
        for {
            i <- 0 until grid.length
            j <- 0 until grid(0).length
        } yield {
            area = 0
            if (grid(i)(j) == 1){
                dfs(i, j)
                maxArea = maxArea max area
            }
        }
        maxArea
    }

    def maxAreaOfIsland_2(grid: Array[Array[Int]]):Int = {
        val seen = Array.ofDim[Boolean](grid.length, grid(0).length)
        var maxArea = 0
        def dfs(i: Int,j: Int): Int = {
            if (i < 0 || i >= grid.length 
                || j < 0 || j >= grid(0).length || seen(i)(j) == true 
                || grid(i)(j) == 0) return 0
            seen(i)(j) = true
            1 + dfs(i+1, j) + dfs(i-1, j) + dfs(i, j+1) + dfs(i, j-1)
        }
        for {
            i <- 0 until grid.length
            j <- 0 until grid(0).length
        } yield (maxArea = maxArea max dfs(i, j))
        maxArea
    }
}

val island = Array(Array(1,1,0,0,0), Array(1,1,0,0,0), Array(0,0,0,1,1), Array(0,0,0,1,1))
val island2 = Array(Array(0,0,0,0,0,0,0,0))
val island3 = Array(Array(0,0,1,0,0,0,0,1,0,0,0,0,0),
 Array(0,0,0,0,0,0,0,1,1,1,0,0,0),
 Array(0,1,1,0,1,0,0,0,0,0,0,0,0),
 Array(0,1,0,0,1,1,0,0,1,0,1,0,0),
 Array(0,1,0,0,1,1,0,0,1,1,1,0,0),
 Array(0,0,0,0,0,0,0,0,0,0,1,0,0),
 Array(0,0,0,0,0,0,0,1,1,1,0,0,0),
 Array(0,0,0,0,0,0,0,1,1,0,0,0,0))
 val island4 = Array(Array(1), Array(1), Array(1), Array(1))
Solution.maxAreaOfIsland(island)
Solution.maxAreaOfIsland(island2)
Solution.maxAreaOfIsland(island3)
Solution.maxAreaOfIsland_2(island4)

// a lsightly better solution w/o incrementing a mutable var area and w/o mutating the input grid

val lst = Vector("hello", "heman")
lst.filter(s => s.startsWith("hel"))