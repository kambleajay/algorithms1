import java.io.File
import org.slf4j._

object GraphBuilder {

	private val logger = LoggerFactory.getLogger(GraphBuilder.getClass)

	def build(fileLines: List[String]) = {
		val grh = new Graph1
		for(aLine <- fileLines) {
			logger.trace("Line {}", aLine)
			val nodes = aLine split "\\s+"
			logger.trace("<GB> Nodes {}", nodes.size)
			for(aNode <- nodes.tail) {
				grh.addEdge((nodes.head, aNode))
			}
		}
		grh
	}

}