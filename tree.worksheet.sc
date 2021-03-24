sealed trait Tree[A]{
    def inOrder[B <: A]: Seq[B] = this match{
        case Leaf(value: B) => Seq(value)
        case BinTree(left: Tree[B], value: B, right: Tree[B]) => left.inOrder ++ Seq(value) ++ right.inOrder
    }

    def preOrder[B <: A]: Seq[B] = this match{
        case Leaf(value: B) => Seq(value)
        case BinTree(left: Tree[B], value: B, right: Tree[B]) => Seq(value) ++ left.preOrder ++ right.preOrder
    }

    def map[B](fn: A => B): Tree[B] = this match {
        case Leaf(value) => Leaf(fn(value))
        case BinTree(left, value, right) => BinTree(left.map(fn), fn(value), right.map(fn))
    }

    def maxHeight: Int = this match {
        case Leaf(value) => 1
        case BinTree(left, value, right) => 1 + (left.maxHeight max right.maxHeight)
    } 
}
case class BinTree[A](left: Tree[A], value: A, right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

val testTree =  BinTree(Leaf(2), 5, 
                BinTree(BinTree(Leaf(10), 15, Leaf(17)), 18, Leaf(20)))

testTree.inOrder
testTree.preOrder
testTree.maxHeight
//is valid BST -- using typical inOrder and then check the list of having increasing order
def isValidBST(root: Tree[Int]): Boolean = {
    val res: Seq[Int] = root.inOrder
    def isSorted[A](lst: Seq[A])(implicit ord: Ordering[A]): Boolean = lst match {
        case Nil => true
        case x :: Nil => true 
        case x :: xs => ord.lteq(x, xs.head) && isSorted(xs)
    }
    isSorted(res)
}
isValidBST(testTree)

val invalidBST = BinTree(Leaf(2), 5, 
                BinTree(BinTree(Leaf(3), 15, Leaf(17)), 18, Leaf(20)))
isValidBST(invalidBST)
