import org.slf4j._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set

class Graph1 {

	private val logger = LoggerFactory.getLogger(this.getClass)

	type Edge = (String, String)

	var nodes = Set.empty[String]
	var edges = ListBuffer.empty[Edge]

	def addEdge(edge: Edge) {
		logger.trace("Adding edge {}", edge)
		nodes += edge._1
		nodes += edge._2
		if(!edges.contains((edge._2, edge._1))) {
			edges += edge
		}
	}

	def contract(edgeIndex: Int) {
		val edgeToContract = edges(edgeIndex)
		logger.trace("Contracting edge {}", edgeToContract)

		nodes -= edgeToContract._1
		nodes -= edgeToContract._2

		val newNodeName = edgeToContract._1 + edgeToContract._2

		nodes += newNodeName

		val edgesToRm = ListBuffer.empty[Edge]

		for(i <- 0 until edges.size) {
			val anEdge = edges(i)
			anEdge match {
				case `edgeToContract` => edgesToRm += anEdge
				case (`edgeToContract`._2, `edgeToContract`._1) => edgesToRm += anEdge
				case (`edgeToContract`._1, y) => edges(i) = (newNodeName, y)
				case (x, `edgeToContract`._2) => edges(i) = (x, newNodeName)
				case (`edgeToContract`._2, y) => edges(i) = (newNodeName, y)
				case (x, `edgeToContract`._1) => edges(i) = (x, newNodeName)
				case _ =>
			}
		}

		edges --= edgesToRm
	}

	override def toString = "\n\tNodes> " +nodes.toString +"\n\tEdges> " +edges.toString
	
}