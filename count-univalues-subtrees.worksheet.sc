sealed trait Tree[+A]
object Leaf extends Tree[Nothing]
case class BinTree[A](left: Tree[A], value: A, right: Tree[A]) extends Tree[A]


// def countUniValues[A](root: Tree[A]): Int = {
//     var count = 0
//     def dfs[A](node: Tree[A], parentVal: A): Boolean = node match {
//         case Leaf => true
//         case BinTree(left, value, right) => case (left, right) => {
//             case (Leaf, Leaf) => 
//         }
//     }
//     // node match {
//     //     case Leaf => 0
//     //     case BinTree(left, value, right) => (left, right) match {
//     //         case (Leaf, Leaf) => 1
//     //         case (Leaf, x: BinTree[A]) => if (x.value == value) 1 + dfs(left) + dfs(right) else dfs(left) + dfs(right)
//     //         case (x: BinTree[A], Leaf) => if (x.value == value) 1 + dfs(left) + dfs(right) else dfs(left) + dfs(right)
//     //         case (x: BinTree[A], y: BinTree[A]) => if (x.value == value && y.value == value) 1 + dfs(left) + dfs(right) else dfs(left) + dfs(right)
//     //     }
//     // }
//     dfs(root)
// }
val testTree = BinTree(Leaf, 4, BinTree(BinTree(Leaf,2,Leaf), 2, BinTree(Leaf,2,Leaf)))
val testAnotherTree = BinTree(BinTree(Leaf, 4, BinTree(BinTree(Leaf, 4, Leaf), 4, BinTree(Leaf, 4, Leaf))), 2, Leaf)
// countUniValues(testTree)
// countUniValues(testAnotherTree)

// testing more traditional tree
 class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
}

def countUniValues_2(root: TreeNode): Int = {
    var count = 0
    def dfs(node: TreeNode, parent: Int): Boolean = {
        if (node == null) return true
        val left = dfs(node.left, node.value)
        val right = dfs(node.right, node.value)
        // leaf node
        if (left && right) count += 1
        // return left and right from subtrees and whether curr node value equals
        // parent node value
        (left && right && node.value == parent)
    }
    dfs(root, Int.MinValue)
    count
}
val javaTree = new TreeNode(4, new TreeNode(2, new TreeNode(2),null), null)
countUniValues_2(javaTree)