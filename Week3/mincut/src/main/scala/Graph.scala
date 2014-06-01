import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import org.slf4j._

class Graph {

	private val logger = LoggerFactory.getLogger(this.getClass)

	val nodes = Map.empty[Node, ListBuffer[Node]]
	val edges = ListBuffer.empty[Edge]

	def addEdge(edge: Edge) {
		if(!edges.contains(Edge(edge.nodes._2, edge.nodes._1))) {
			edges += edge
		}
		addOrUpdateNodes(edge)
		logger.trace("<G> added edge: {}", edges)
	}

	private def addOrUpdateNodes(edge:Edge) {
		def linkNodes(n: Node, link: Node) {
			if(nodes.contains(n)) {
				nodes(n) = (nodes(n) += link)
			} else {
				nodes(n) = ListBuffer(link)
			}
		}

		val node1 = edge.nodes._1
		val node2 = edge.nodes._2
		logger.trace("[G] building graph {}", nodes)
		linkNodes(node1, node2)
		logger.trace("[G] building graph {}", nodes)
		//linkNodes(node2, node1)
		//logger.debug("[G] building graph {}", nodes)
	}

	def contract(edgeIndex: Int) {
		val edgeToContract = edges(edgeIndex)

		logger.debug("[G] Contracting ... {}", edgeToContract)

		val nodeToRm1 = edgeToContract.nodes._1
		val nodeToRm2 = edgeToContract.nodes._2

		nodes -= nodeToRm1
		nodes -= nodeToRm2

		//create new node 12
		val newNode = Node(nodeToRm1.name + nodeToRm2.name)
		val newLinkNodes = ListBuffer.empty[Node]

		logger.trace("[Cont] Edges size {}", edges.size)

		logger.debug("\t\t[A] Edges {}", edges)

		//update all references
		for(edge <- edges) {

			logger.debug("\t[Cont] edge {}", edge)

			edge.nodes match {

				case (`nodeToRm1`, `nodeToRm2`) => 
					logger.debug("\t\tFirst case {}", edge.nodes)
					edges -= edge

				case (`nodeToRm1`, second) if second != `nodeToRm2` => 
					logger.debug("\t\tSecond case {}", edge.nodes)
					val newEdge = Edge(newNode, second)
					edges.update(edges.indexOf(edge), newEdge)
					newLinkNodes += second
					nodes(second).map((x) => if(x == `nodeToRm1`) newNode else x)

				case (first, `nodeToRm2`) if first != `nodeToRm1` =>
					logger.debug("\t\tThird case {}", edge.nodes)
					val newEdge = Edge(first, newNode)
					edges.update(edges.indexOf(edge), newEdge)
					newLinkNodes += first
					nodes(first).map((y) => if(y == `nodeToRm2`) newNode else y)

				case(`nodeToRm2`, second) if second != `nodeToRm1` =>
					logger.debug("\t\tFourth case {}", edge.nodes)

				case(first, `nodeToRm1`) if first != `nodeToRm2` =>
					logger.debug("\t\tFifth case {}", edge.nodes)

				case _ =>
			}

			logger.debug("\t\tEffect on edges {}", edges.toString)
			logger.debug("\t\tEffect on nodes {}", nodes.toString)
		}

		nodes(newNode) = newLinkNodes

		//delete all edges with one end in node1 and other end in node2
		//delete nodes 1 and 2

	}

	override def toString = "\n\tNodes> " +nodes.toString +"\n\tEdges> " +edges.toString
}

case class Node(val name: String) {
	override def toString = name
}

case class Edge(val nodes: (Node, Node)) {
	override def toString = nodes._1 +"." +nodes._2
}