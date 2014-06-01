import scala.io.Source
import scala.collection.mutable
import org.slf4j._

class Graph(inputFileName: String) {

	private val logger = LoggerFactory.getLogger(this.getClass)

	private val nodes: Map[Node, Set[Edge]] = build(inputFileName)

	private def build(fileName: String) = {
		logger.debug("-------start parsing {}-------", fileName)
		val resultNodes = mutable.Map.empty[Node, Set[Edge]]
		val lines = Source.fromURL(getClass.getResource(fileName)).getLines
		for(line <- lines) {
			val parts = line.split("\\s")
			logger.debug("parts {}", parts mkString "|")
			//node
			val node = parts(0) toInt
			//edges
			val edges = mutable.ListBuffer.empty[Edge]
			for(i <- 1 until parts.length) {
				if(!parts(i).trim.isEmpty) {
					val edgeParts = parts(i) split ","
					logger.debug("\tEdge -> {}", edgeParts)
					edges += Edge(edgeParts(0) toInt, edgeParts(1) toInt)
				}
			}
			resultNodes(Node(node)) = edges.toSet
			logger.debug("Nodes -> {}", resultNodes)
		}
		logger.debug("-------end parsing {}-------", fileName)
		Map.empty ++ resultNodes
	}

	def allNodes = nodes keySet

	def edgesOf(id: Node) = nodes(id)

	def size = nodes.size

	override def toString = nodes toString
}

case class Node(val id: Int)
case class Edge(val toNode: Int, val weight: Int)

