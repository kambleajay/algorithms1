import org.slf4j._
import scala.util.Random
import scala.collection.mutable.ListBuffer

object MincutCounter {

	private val logger = LoggerFactory.getLogger(MincutCounter.getClass)
	
	def count(graph: Graph1): Int = {
		val noOfRuns = scala.math.pow(graph.nodes.size, 2)
		val minCuts = ListBuffer.empty[Int]
		for(i <- 0 until 500) {
			logger.trace("RUN NO {}", i) 
			val minCut = rcount(copy(graph), new Random(new Random().nextInt(7777777)))
			minCuts += minCut
		}
		logger.info("Mincuts are {} and smallest is {}", minCuts, minCuts min)
		minCuts min
	}

	private def copy(grh: Graph1) = {
		val res = new Graph1
		res.nodes = grh.nodes.clone
		res.edges = grh.edges.clone
		res
	}

	private def rcount(graph: Graph1, gen: Random): Int = {
		logger.trace("[MC] graph to contract {}", graph)
		//if there are more than 2 nodes
		//select an edge (u, v) uniformly at random
		//merge or contract u and v into a single vertex
		//remove self loops
		if(graph.nodes.size > 2) {
			val edgeIndex = gen.nextInt(graph.edges.size)
			logger.trace("[MC] random index is {}", edgeIndex)
			graph.contract(edgeIndex)
			rcount(graph, gen)
		} else {
			graph.edges.size
		}
	}

}