import org.scalatest._

class DijkstraTestSuite extends FlatSpec {

	trait TestData {
		val graph1 = new Graph("input1.txt")
		val graph2 = new Graph("input2.txt")
		val graph3 = new Graph("input3.txt")
	}

	"GraphBuilder" should "correctly build graph from input" in {
		new TestData {
			assert(graph1.size === 5)
			assert(graph1.allNodes.size === 5)
			assert(graph1.edgesOf(Node(1)).size === 3)
			assert(graph1.edgesOf(Node(5)).size === 0)

			assert(graph2.size === 6)

			assert(graph3.size === 10)
			assert(graph3.edgesOf(Node(3)).size === 4)
		}
	}

	"DijkstraShortestPathsFinder" should "correctly compute shortest paths from given node" in {
		new TestData {
			val spaths1 = DijkstraShortestPathsFinder.find(graph1, 1)
			assert(spaths1 === List(0, 10, 50, 30, 60))

			val spaths21 = DijkstraShortestPathsFinder.find(graph2, 1)
			val spaths22 = DijkstraShortestPathsFinder.find(graph2, 5)
			val spaths23 = DijkstraShortestPathsFinder.find(graph2, 6)
			assert(spaths21 === List(0, 45, 10, 25, 45, Int.MaxValue))
			assert(spaths22 === List(Int.MaxValue, 50, Int.MaxValue, 30, 0, Int.MaxValue))
			assert(spaths23 === List(Int.MaxValue, 23, Int.MaxValue, 3, 33, 0))

			val spaths3 = DijkstraShortestPathsFinder.find(graph3, 1)
			assert(spaths3 === List(0, 10, 6, 7, 5, 13, 9, 16, 20, 19))
		}
	}

	it should "correctly compute shortest paths for big graph" in {
		val bigGraph = new Graph("dijkstraData.txt")
		assert(bigGraph.size === 200)

		val spaths = DijkstraShortestPathsFinder.find(bigGraph, 1)
		//7,37,59,82,99,115,133,165,188,197
		val target = List(7,37,59,82,99,115,133,165,188,197)
		println(spaths(0))
		for(node <- target) {
			print(spaths(node - 1) +",")
		}
		println("")
	}
	
}