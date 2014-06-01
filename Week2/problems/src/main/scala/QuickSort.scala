import org.slf4j._
import scala.collection.immutable._

object QuickSort {

	private val logger:Logger = LoggerFactory.getLogger(QuickSort.getClass)

	def sort(input: Array[Int]): (Array[Int], Long) = {
		val result = qsort(input, 0, input.size - 1)
		logger.debug("[END] {} Comparisons = {}\n\n", pa(input), result._2)
		result
	}

	private def qsort(input: Array[Int], left: Int, right: Int): (Array[Int], Long) = {
		logger.debug("[QStart] input = {}, left = {}, right = {}, #comp = {}", pa(input), left.toString, 
			right.toString)
		var noOfComparisons:Long = 0
		if(left < right) {
			logger.trace("[Q] left < right = {} < {}", left, right)
			
			//first index as pivot
			//val pivotIndex = left;

			//last index as pivot
			//swap(input, left, right)
			//val pivotIndex = left

			//median as pivot
			val medianIndex = getMedian(input, left, right)
			swap(input, left, medianIndex._1)
			val pivotIndex = left

			noOfComparisons = noOfComparisons + (right - left)
			val pivotNewIndex:Int = partition(input, left, right, pivotIndex)
			logger.trace("[Q] Got new pivot index = {}, making recursive calls", pivotNewIndex)
			val res1 = qsort(input, left, pivotNewIndex - 2)
			val res2 = qsort(input, pivotNewIndex, right)
			noOfComparisons = noOfComparisons + res1._2
			noOfComparisons = noOfComparisons + res2._2
		} 
		logger.debug("[QEnd] input = {}, noOfComparisons = {}", pa(input), noOfComparisons)
		(input, noOfComparisons)
	}

	def getMedian(input: Array[Int], left: Int, right: Int) = {
		var median = ((right - left) / 2)
		if(median == 0) median = left
		else median = median + left
		logger.debug("\t[Med] input = {}, left = {}, right = {}, median = {}", pa(input), left.toString, right.toString, median.toString)
		val threeElems = List((left, input(left)), (median, input(median)), (right, input(right)))
		val sorted = threeElems.sortWith((A, B) => A._2.compareTo(B._2) < 0)
		logger.debug("\t[Med] sorted-elems = {}", sorted)
		sorted(1)
	}

	private def partition(input: Array[Int], left: Int, right: Int, pivotIndex: Int): Int = {
		logger.debug("\t\t[PStart] input = {}, left = {}, right = {}, pivotIndex = {}", pa(input), left.toString, right.toString, pivotIndex.toString)
		val pivot = input(pivotIndex)
		logger.debug("\t\t[P*] pivot = {}", pivot)
		var i = left + 1
		for(j <- left + 1 to right) {
			if(input(j) < pivot) {
				logger.trace("[P] {} < {} -> swap", input(j), pivot)
				swap(input, i, j)
				logger.trace("[P] {}", pa(input))
				i = i + 1
			}
		}
		swap(input, left, i - 1)
		logger.debug("\t\t[PEnd->] i = {}, input = {}", i, pa(input))
		i
	}

	private def swap(array: Array[Int], first: Int, second: Int) {
		var temp = -1
		temp = array(second)
		array(second) = array(first)
		array(first) = temp
	}

	private def pa(array: Array[Int]): String = "[" +(array mkString ",") +"]"

}