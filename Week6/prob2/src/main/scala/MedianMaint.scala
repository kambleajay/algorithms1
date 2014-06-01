import scala.collection.mutable
import org.slf4j._

object MedianMaint {

	private val logger = LoggerFactory.getLogger(MedianMaint.getClass)
	
	def sum(arr: Array[Int]): Int = {
		logger.debug("[Sum] input is {}", arr)

		val low = mutable.PriorityQueue(arr(0) min arr(1))
		val high = mutable.PriorityQueue(arr(0) max arr(1))(MinOrdering)
		val sums = mutable.ListBuffer(arr(0), low.head)

		logger.debug("[Sum] before loop, low = {}, high = {}, sums = {}", low, high, sums)

		for(i <- 2 until arr.length) {
			val current = arr(i)
			logger.debug("\t[Sum] current elem = {}", current)
			placeCurrent(current, low, high)
			logger.debug("\t[Sum] after placing, low = {}", low)
			logger.debug("\t[Sum] after placing, high = {}", high)
			balance(low, high)
			logger.debug("\t[Sum] after balance, low = {}", low)
			logger.debug("\t[Sum] after balance, high = {}", high)
			sums += median(low, high)
			logger.debug("\t[Sum] sums = {}", sums)
		}

		(sums foldLeft 0)(_ + _) % 10000

	}

	private def placeCurrent(current: Int, low: mutable.PriorityQueue[Int], high: mutable.PriorityQueue[Int]) {
		if(current < high.head) {
			low += current
		} else {
			high += current
		}
	}

	private def balance(low: mutable.PriorityQueue[Int], high: mutable.PriorityQueue[Int]) {
		if(high.size - low.size == 2) {
			low += high.dequeue
		} else if(high.size - low.size == -2) {
			high += low.dequeue
		}
	}

	private def median(low: mutable.PriorityQueue[Int], high: mutable.PriorityQueue[Int]): Int = {
		if(high.size > low.size) {
			high.head
		} else {
			low.head
		}
	}

}

object MinOrdering extends Ordering[Int] {
	def compare(x: Int, y: Int) = y compare x
}