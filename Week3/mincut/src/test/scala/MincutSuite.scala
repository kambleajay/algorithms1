import org.scalatest._
import scala.io.Source

class MincutSuite extends FlatSpec {

	trait TestData {
		val graph1 = GraphBuilder.build(Source.fromURL(getClass.getResource("smallInput1.txt")).getLines.toList)
		val graph2 = GraphBuilder.build(Source.fromURL(getClass.getResource("smallInput2.txt")).getLines.toList)
		val bigGraph = GraphBuilder.build(Source.fromURL(getClass.getResource("kargerMinCut.txt")).getLines.toList)
	}
	
	"GraphBuilder" should "correctly build graph from input file" in {
		new TestData {
			assert(graph1.nodes.size === 4)
			assert(graph1.edges.size === 5)

			assert(graph2.nodes.size === 8)
			assert(graph2.edges.size === 14)

			assert(bigGraph.nodes.size === 200)
		}
	}

	"MincutCounter" should "count mincut" in {
		new TestData {
			assert(MincutCounter.count(graph1) >= 2)
			assert(MincutCounter.count(graph2) >= 2)
			assert(MincutCounter.count(bigGraph) > 1)
		}
	}
}