package rahul.bayes

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions
import scala.collection.JavaConversions._

import java.util.StringTokenizer

class NaiveBayesClassifier {
  val catWordCnt = new HashMap[(String, String), Int]
  val catCnt = new HashMap[String, Int]

  def train(category: String, text: String) {

    val strTokens = new StringTokenizer(text).filter(x => (NaiveBayesClassifier.stopWords.contains(x)))
    strTokens map {
      curToken =>
        catWordCnt += ((category, curToken.asInstanceOf[String]) ->
          (catWordCnt.getOrElse((category, curToken.asInstanceOf[String]), 0) + 1))
    }
    catCnt += (category -> (catCnt.getOrElse(category, 0) + 1))
  }

  def classify(text: String): String = {
    val clasTextTokenizer = new StringTokenizer(text).filter(x => (NaiveBayesClassifier.stopWords.contains(x)))
    val totalCatCnt = catCnt.foldLeft(0)(_ + _._2).toDouble
    val avgProb: Double = 1 / catCnt.size
    val w = 1
    val probByCatMap = catCnt map {
      catCntSet =>
        val cat = catCntSet._1
        val catCount = catCntSet._2
        //1. Get  P(Y)
        val catProb = catCount / totalCatCnt
        //2. get P(X(i)|Y(i))/P(X(i))
        val pXigivenYiList = clasTextTokenizer map {
          t =>
            val token = t.asInstanceOf[String]
            // Calculate the token coming in that category.
            val wProb: Double = catWordCnt.getOrElse((cat, token), 0) / catCount
            // Calculate the total num of words in the all classifiers
            val total = catWordCnt.filter(_._1._2 == token).map(_._2).sum
            (avgProb * w + total * wProb) / (w + total)
        }
        // Pi[P(X(i)|Y(i))/P(X(i))]
        val wholeTextProb = pXigivenYiList.foldLeft(1.0)(_ * _)
        // Calculate P(Y|X) using Bayes Rule
        val catTextProb = catProb * wholeTextProb
        (cat, catTextProb)
    }
    probByCatMap.maxBy(_._2)._1
  }
}

object NaiveBayesClassifier {
  val stopWords = Array[String]("a", "about", "above", "across", "after", "afterwards",
    "again", "against", "all", "almost", "alone", "along",
    "already", "also", "although", "always", "am", "among",
    "amongst", "amoungst", "amount", "an", "and", "another",
    "any", "anyhow", "anyone", "anything", "anyway", "anywhere",
    "are", "around", "as", "at", "back", "be",
    "became", "because", "become", "becomes", "becoming", "been",
    "before", "beforehand", "behind", "being", "below", "beside",
    "besides", "between", "beyond", "bill", "both", "bottom",
    "but", "by", "call", "can", "cannot", "cant", "dont",
    "co", "computer", "con", "could", "couldnt", "cry",
    "de", "describe", "detail", "do", "done", "down",
    "due", "during", "each", "eg", "eight", "either",
    "eleven", "else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every",
    "everyone", "everything", "everywhere", "except", "few", "fifteen",
    "fify", "fill", "find", "fire", "first", "five",
    "for", "former", "formerly", "forty", "found", "four",
    "from", "front", "full", "further", "get", "give",
    "go", "had", "has", "hasnt", "have", "he",
    "hence", "her", "here", "hereafter", "hereby", "herein",
    "hereupon", "hers", "herself", "him", "himself", "his",
    "how", "however", "hundred", "i", "ie", "if",
    "in", "inc", "indeed", "interest", "into", "is",
    "it", "its", "itself", "keep", "last", "latter",
    "latterly", "least", "less", "ltd", "made", "many",
    "may", "me", "meanwhile", "might", "mill", "mine",
    "more", "moreover", "most", "mostly", "move", "much",
    "must", "my", "myself", "name", "namely", "neither",
    "never", "nevertheless", "next", "nine", "no", "nobody",
    "none", "noone", "nor", "not", "nothing", "now",
    "nowhere", "of", "off", "often", "on", "once",
    "one", "only", "onto", "or", "other", "others",
    "otherwise", "our", "ours", "ourselves", "out", "over",
    "own", "part", "per", "perhaps", "please", "put",
    "rather", "re", "same", "see", "seem", "seemed",
    "seeming", "seems", "serious", "several", "she", "should",
    "show", "side", "since", "sincere", "six", "sixty",
    "so", "some", "somehow", "someone", "something", "sometime",
    "sometimes", "somewhere", "still", "such", "system", "take",
    "ten", "than", "that", "the", "their", "them",
    "themselves", "then", "thence", "there", "thereafter", "thereby",
    "therefore", "therein", "thereupon", "these", "they", "thick",
    "thin", "third", "this", "those", "though", "three",
    "through", "throughout", "thru", "thus", "to", "together",
    "too", "top", "toward", "towards", "twelve", "twenty",
    "two", "un", "under", "until", "up", "upon",
    "us", "very", "via", "was", "we", "well",
    "were", "what", "whatever", "when", "whence", "whenever",
    "where", "whereafter", "whereas", "whereby", "wherein", "whereupon",
    "wherever", "whether", "which", "while", "whither", "who",
    "whoever", "whole", "whom", "whose", "why", "will",
    "with", "within", "without", "would", "yet", "you", "your", "yours",
    "yourself", "yourselves")

}