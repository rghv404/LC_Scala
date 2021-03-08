import scala.collection.mutable._
object Solution {
    
    final case class Island(x:Int, y:Int)
    def numDistinctIslands(grid: Array[Array[Int]]): Int = {
        // idea is to store translated (from 0,0) cooridiantes in hashset
        // such that it holds only one copy of each repeated island
        // we are able to do this because translated coords will be the same for 
        // same island
        val seen = Array.ofDim[Boolean](grid.length, grid(0).length)
        val islandSet = scala.collection.mutable.Set.empty[ArrayBuffer[Island]]
        var currIsland = ArrayBuffer.empty[Island]
        def dfs(i:Int, j:Int, origRow: Int, origCol:Int): Unit = {
            if (i < 0 || i>=grid.length 
                || j < 0 || j >= grid(0).length 
                || seen(i)(j) == true || grid(i)(j) == 0) return
            seen(i)(j) = true
            currIsland.append(Island(i - origRow, j - origCol))
            dfs(i-1, j, origRow, origCol)
            dfs(i+1, j, origRow, origCol)
            dfs(i, j-1, origRow, origCol)
            dfs(i, j+1, origRow, origCol)
        }

        for {
            i <- 0 until grid.length
            j <- 0 until grid(0).length
        } yield{
            // println(i, j, seen(i)(j))
            if (seen(i)(j) != true){
                currIsland = ArrayBuffer.empty[Island]
                dfs(i, j, i, j)
                if (!currIsland.isEmpty) islandSet.add(currIsland)
            }
        }
        islandSet.size
    }
}

val test = Array(Array(1,1,0,0,0), Array(1,1,0,0,0), Array(0,0,0,1,1), Array(0,0,0,1,1))
val test2 = Array(Array(0,0,1,1), Array(1,1,0,0))
val test3 = Array(Array(1,1), Array(0,1))
Solution.numDistinctIslands(test)
Solution.numDistinctIslands(test2)
Solution.numDistinctIslands(test3)