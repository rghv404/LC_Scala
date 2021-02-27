import scala.collection.mutable
import scala.collection.mutable.ListBuffer
object Solution {
    def maxProfit(inventory: Array[Int], orders: Int): Int = {
        // createa a aray of tuple with counter
        val tup = inventory.foldLeft(mutable.Map.empty[Int, Int])((mp, item) => {
            if (mp.contains(item))
                mp(item) += 1
            else
                mp(item) = 1  
            mp
        })
        val countList = (tup ++ mutable.Map(0 -> 0)).toSeq.sortWith(_._1 > _._1).toList

        //once we have the arry of counts we iterate
        def loop(orders: Int, ans: Long, ind: Int, currWidth: Int): Long = orders match{
            case 0 => ans
            case ord:Int => 
                // add the currentWidht to the count of current biggest elements
                val width = currWidth + countList(ind)._2
                // number of items to sell becomes min of orders remaining and diff b/w two biggest numbers
                val sell = orders min width * (countList(ind)._1 - countList(ind + 1)._1)
                // figure out if sell is sellable w/o going over orders if not use remainder value to find sum of ap from 
                // current biggest to whatever quotient is
                val (whole, rem) = ((sell / width), sell % width)
                println(whole, rem)
                val wholeAns: Long = width * (whole * (2 * countList(ind)._1 - (whole - 1)).toLong) / 2
                val remAns: Long = (rem * (countList(ind)._1 - whole).toLong)
                // loop again with increments and currAns
                println(wholeAns, remAns)
                ans + loop(orders - sell, wholeAns + remAns, ind + 1, width)
        }
        val longAns = loop(orders, 0, 0, 0) 
        println(longAns)
        (longAns % (1E9 + 7).toLong).toInt
    }
}

val test = Array(6,5,5,1)
val test2 = Array(2,4,6,8,10)
Solution.maxProfit(test, 12)
Solution.maxProfit(test2, 20)
Solution.maxProfit(Array(1000000000), 1000000000)

val test3 = Array(565259708,715164401,716563713,958255469,844600740,823949511,180479359,287829385,164248818,73361150,230686692,322986846,598720034,338241127,748922260,181241085,833659853,509571179,250093451,690995620,703292727,595636202)
Solution.maxProfit(test3, 650114768)
