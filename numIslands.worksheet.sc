import scala.collection.mutable.ArrayBuffer
def numIslands(mutableGrid: Array[Array[Char]]): Int = {
    // val mutableGrid = grid.flatMap(x => List(x.toBuffer))
    
    def bfs(i:Int, j:Int): Unit = {
            if (i < 0 || i >= mutableGrid.length || j < 0 || j >= mutableGrid(0).length || mutableGrid(i)(j) == '0')
                return
            mutableGrid(i)(j) = '0'
            bfs(i, j+1) // right
            bfs(i+1, j) // down
            bfs(i-1, j) // up
            bfs(i, j-1) // left
        }
    var count = 0
    for {
        i <- 0 until mutableGrid.length
        j <- 0 until mutableGrid(0).length
    } yield (
        if (mutableGrid(i)(j) == '1'){
            count += 1
            bfs(i, j)
        }
    )
    count
}

val grid = Array(Array('1','0','0','0'), Array('0','1','0','1'), Array('0','1','0','1'))
println(grid)
numIslands(grid)
