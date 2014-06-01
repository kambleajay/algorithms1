import scala.collection.mutable
import org.slf4j._

object TwoSumFinder {

	val logger = LoggerFactory.getLogger(TwoSumFinder.getClass)
	
	def count(arr: Array[Int], interval: Interval): Set[Solution] = {
		//create map from array
		val m = arr.map(x => (x,x)).toMap
		val solutions = mutable.Set.empty[Solution]
		//loop over interval
		for(t <- interval.lower to interval.upper) {
		//	let t = next target
		//		pass t, array and map to countSum
			solutions ++= countSum(t, arr, m) 
		}

		logger.debug("Solutions = {}", solutions mkString "\n")

		Set.empty[Solution] ++ solutions
	}

	def countSum(t: Int, arr: Array[Int], m: Map[Int, Int]): mutable.Set[Solution] = {
		var results = mutable.Set.empty[Solution]
		for(elem <- arr) {
			val sumPart = t - elem
			if(sumPart != elem && m.contains(sumPart)) {
				results += new Solution(t, elem, sumPart)
			}
		}
		results
	}

}