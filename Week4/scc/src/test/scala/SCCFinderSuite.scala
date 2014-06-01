import org.scalatest._
import scala.io.Source

class SCCFinderSuite extends FlatSpec {

	trait TestData {
		val graph1: Graph = GraphBuilder.build(getFileLines("smallInput1.txt"))
		val graph2: Graph = GraphBuilder.build(getFileLines("smallInput2.txt"))
		val graph3: Graph = GraphBuilder.build(getFileLines("test7.txt"))
		val graph4 = GraphBuilder.build(getFileLines("smallInput3.txt"))
		val graph5 = GraphBuilder.build(getFileLines("smallInput4.txt"))

		val graph1revFromFile = GraphBuilder.build(getFileLines("smallInput1Rev.txt"))
		val graph2revFromFile = GraphBuilder.build(getFileLines("smallInput2Rev.txt"))
		val graph3revFromFile = GraphBuilder.build(getFileLines("test7Rev.txt"))
		val graph4revFromFile = GraphBuilder.build(getFileLines("smallInput3Rev.txt"))

		val graph1rev = GraphBuilder.reverse(graph1)
		val graph2rev = GraphBuilder.reverse(graph2)
		val graph3rev = GraphBuilder.reverse(graph3)
		val graph4rev = GraphBuilder.reverse(graph4)
		val graph5rev = GraphBuilder.reverse(graph5)

		def getFileLines(fileName: String): List[String] = Source.fromURL(getClass.getResource(fileName)).getLines.toList
	}

	"GraphBuilder" should "correctly build graphs" in {
		new TestData {
			assert(graph1.nodes.size === 11)
			assert(graph2.nodes.size === 9)
			//assert(graph3.nodes.size === 50)
		}
	}

	"Graph" should "correctly compute reverse graph" in {
		new TestData {
			assert(graph1rev === graph1revFromFile)
			assert(graph2rev === graph2revFromFile)
			//assert(graph3rev === graph3revFromFile)
			assert(graph4rev === graph4revFromFile)
		}
	}

	ignore should "correctly compute finish times" in {
		new TestData {
			val graph2FT = SCCFinder.computeFinishTimes(graph2rev)
			assert(graph2FT === Map(7 -> 1, 8 -> 4, 9 -> 7, 6 -> 9, 1 -> 3, 5 -> 6, 4 -> 8, 3 -> 2, 2 -> 5))
		}
	}

	ignore should "correctly compute leaders" in {
		new TestData {
			val graph2FT = SCCFinder.computeFinishTimes(graph2rev)
			val graph2Leaders = SCCFinder.computeLeaders(graph2, graph2FT)
			assert(graph2Leaders === Map(9 -> Set(1,7,4), 6 -> Set(3,9,6), 4 -> Set(2,5,8)))
		}
	}

	"SCCFinder" should "correctly return big 5 SCCs" in {
		new TestData {
			/*val graph1SCCs = SCCFinder.bigFive(graph1)
			val graph2SCCs = SCCFinder.bigFive(graph2)
			val graph3SCCs = SCCFinder.bigFive(graph3)
			//val graph4SCCs = SCCFinder.bigFive(graph4)
			val graph5SCCs = SCCFinder.bigFive(graph5)
			
			assert(graph1SCCs === Seq(4,3,3,1,0))
			assert(graph2SCCs === Seq(3,3,3,0,0))
			assert(graph3SCCs === Seq(36,7,1,1,1))
			//assert(graph4SCCs === Seq(4, 3, 3, 0, 0))
			assert(graph5SCCs === Seq(8,5,2,1,0))*/

			val reallyBigGraph = GraphBuilder.build(getFileLines("SCC.txt"))
			val reallyBigGraphSCC = SCCFinder.bigFive(reallyBigGraph)
			println(reallyBigGraphSCC)
		}
	}

}