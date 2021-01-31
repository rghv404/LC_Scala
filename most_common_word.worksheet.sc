object Solution {
    def mostCommonWord(paragraph: String, banned: Array[String]): String = {
        val cleanPar = paragraph
                            .replaceAll("""[\p{Punct}]""", " ")
                            .map(_.toLower)
        // once we have clean para we can count the words
        val bannedSet = banned.toSet
        val wordMap = scala.collection.mutable.HashMap.empty[String, Int]
        var maxCount = 0
        var wordAns = ""
        print(cleanPar)
        cleanPar.split(" ").toList.foreach{ word => 
            if (!bannedSet.contains(word)) {
                if (wordMap contains word) wordMap(word) += 1 else wordMap(word) = 1
                if (maxCount <= wordMap(word)){
                    wordAns = word
                    maxCount = wordMap(word)
                } 
            }   
        }
        wordAns   
    }
}


// not running for below case 
val str = "a, a, a, a, b,b,b,c, c"
val banned = Array("a")
Solution.mostCommonWord(str, banned)


// test flatten using map
val testList = List(List(3), List(4), List(5), List())
val testList2 = Array(2,2,3,4,5,6)
testList2.flatMap(item => List(Nil))

// create count bucket
testList2.indices.foldLeft(scala.collection.mutable.HashMap.empty[Int, Int])((mp, ind) => {
    if (mp.contains(testList2(ind)))
        mp(testList2(ind)) = mp(testList2(ind)) + 1
    else
        mp(testList2(ind)) = 1
    mp
})