import scala.collection.mutable
import org.slf4j._

object DijkstraShortestPathsFinder {

	private val logger = LoggerFactory.getLogger(DijkstraShortestPathsFinder.getClass)
	
	def find(g: Graph, s: Int) = {
		logger.debug("-------start find-------")
		logger.debug("input graph = {}, start node = {}", g, s)
		val spaths = mutable.Map.empty[Int, Int]

		spaths(s) = 0

		while(spaths.size < g.size) {
			logger.debug("\tshortest paths so far = {}", spaths)
			val spCandidates = mutable.ListBuffer.empty[DijkstraScoreCard]
			for(aNode <- spaths.keySet) {
				for(edge <- frontierEdges(g, Set.empty ++ spaths.keySet, aNode)) {
					val edgeScore = spaths(aNode) + edge.weight
					spCandidates += DijkstraScoreCard(edge.toNode, edgeScore)
				}
			}
			logger.debug("\tshortest path candidates = {}", spCandidates)
			if(!spCandidates.isEmpty) {
				val minScoreEdge = (spCandidates foldLeft spCandidates(0))((x, y) => if(x.greedyScore < y.greedyScore) x else y)
				spaths(minScoreEdge.toNode) = minScoreEdge.greedyScore
			} else {
				val unreachableNodes = g.allNodes diff (spaths.keySet.map(Node(_)))
				for(unreachableNode <- unreachableNodes) {
					spaths(unreachableNode.id) = Int.MaxValue
				}
			}
		}

		val result = (Map.empty ++ spaths)
		logger.debug("result = {}", result)
		logger.debug("-------end find-------")
		(for { aNode <- result.keySet.toList.sorted } yield result(aNode)) toList
	}

	private def frontierEdges(g: Graph, explored: Set[Int], fromNode: Int) = {
		//frontier edge - tail in explored part, head in not explored part
		val fromNodeEdges = g.edgesOf(Node(fromNode))
		val resultFrontEdges = mutable.Set.empty[Edge]

		for(anEdge <- fromNodeEdges) {
			if(!explored.contains(anEdge.toNode)) {
				resultFrontEdges += anEdge
			}
		}

		Set.empty ++ resultFrontEdges
	}

}

case class DijkstraScoreCard(val toNode: Int, val greedyScore: Int) {
	override def toString = toNode +" -> " +greedyScore
}