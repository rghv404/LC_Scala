case class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
}

def pathSum_Three(node: TreeNode, target: Int): Int = {
    val map = scala.collection.mutable.HashMap.empty[Int, Int]
    var count = 0
    def preOrder(node: TreeNode, currSum: Int): Unit = {
        if (node == null) return 
        val newSum = currSum + node.value
        if (newSum == target) count += 1
        
        count += map.getOrElse((newSum-target), 0)
        map.update(newSum, map.getOrElse(newSum, 0) + 1)
        
        // preOrder to left and right
        preOrder(node.left, newSum)
        preOrder(node.right, newSum)

        // once the subtree is traversed remove the curr node's contrib from map
        // to prevent parallel computaion problems
        map(newSum) = map(newSum) - 1
    }
    preOrder(node, 0)
    count
}

val testTree = TreeNode(5, 
                TreeNode(7,TreeNode(-2, null, TreeNode(2)), TreeNode(-3)),
                     TreeNode(1, null, TreeNode(1, TreeNode(2), TreeNode(-4, TreeNode(11), null))))
pathSum_Three(testTree, 7)