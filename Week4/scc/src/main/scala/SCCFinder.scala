import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import org.slf4j._
import scala.annotation.tailrec

object SCCFinder {

	private val logger = LoggerFactory.getLogger(SCCFinder.getClass)
	
	def bigFive(g: Graph):Seq[Int] = {
		logger.info("[B5] Processing graph \n{}", g)
		//1. compute grev
		val grev = GraphBuilder.reverse(g)
		logger.info("[B5] Reverse graph = {}", grev)
		//2. compute finish times
		val finishTimes = computeFinishTimes(grev)
		logger.info("[B5] Finish times \n{}", finishTimes.toSeq.sortBy(_._1).map(_._2))
		//3. compute leaders
		val leaders = computeLeaders(g, finishTimes)
		logger.info("[B5] Leaders \n{}", leaders)

		val result = leaders.values.toList.map(_.size).sortWith(_.compareTo(_) > 0)

		logger.info("[B5] result {}", result)

		if(result.size >= 5) {
			result take 5
		} else {
			result padTo (5, 0)
		}

	}

	def computeFinishTimes(g: Graph): mutable.Map[Int, Int] = {
		logger.debug("\n\n\n[CFT] got graph {}",  g)
		val finishTimes = mutable.Map.empty[Int, Int]
		val explored = mutable.Set.empty[Int]
		var t = 0

		def dfs(j: Int) {
			logger.debug("\t\t[CFT] dfs got {}", j)
			explored += j
			for(head <- g.nodes(j)) {
				if(!(explored contains head)) {
					dfs(head)
				}
			}
			t = t + 1
			finishTimes(t) = j
			logger.debug("\t\t[CFT] finish times {}", finishTimes)
		}

		/*@tailrec
		def dfs(nodes: mutable.Stack[Int], order: mutable.Stack[Int]) {
			logger.info("<DFS> got stack = {}", nodes)
			logger.info("\t<DFS> order = {}", order)
			logger.info("\t<DFS>Explored = {}", explored)
			
			if(nodes.isEmpty) {
				return
			}
			val current = nodes.pop
			explored += current
			order push current
			var deadEnd = true

			logger.info("\t\tProcessing -> {}", current)
			for(head <- g.nodes(current)) {
				if(!(explored contains head)) {
					nodes push head
					//explored += head
					deadEnd = false
				}
			}

			if(deadEnd) {
				var done = false
				while(!done && !order.isEmpty) {
					val checkNode = order.top
					logger.info("\t\tCheck for {} is {}", checkNode, g.nodes(checkNode).diff(explored))
					if(g.nodes(checkNode).diff(explored) isEmpty) {
						val finished = order.pop
						t = t + 1
						finishTimes(t) = finished
					} else {
						done = true
					}
				}
				logger.info("\t\t---DeadEnd ORDER = {}", order)
				logger.info("\t\tFN = {}", finishTimes)
			}

			logger.info("\t\t<DFS> Processed edges, stack = {}", nodes)
			dfs(nodes, order)
		}*/

		/*def dfs(j: Int) {
			logger.debug("\t\t[CFT] dfs got {}", j)
			explored += j
			if(g.nodes(j).diff(explored) isEmpty) {
				logger.debug("Found! {}", j)
			}
			for(head <- g.nodes(j)) {
				if(!(explored contains head)) {
					dfs(head)
				}
			}
			t = t + 1
			finishTimes(t) = j
			logger.debug("\t\t[CFT] finish times {}", finishTimes)
		}*/

		/*def dfs(j: Int) {
			logger.debug("\t[DFS] Starting with {}", j)
			val stk = mutable.Stack(j)
			var next = -1
			while(!stk.isEmpty) {
				logger.trace("\t\t[DFS] Last stack value {}", next)
				next = stk.pop
				logger.debug("\t\t[DFS] Popped from stack {}", next)
				if(!(explored contains next)) {
					explored += next
					for(head <- g.nodes(next)) {
						stk push head
						if(explored contains head) {
							t = t + 1
							finishTimes(t) = next
						}
						logger.debug("\t\t\t[DFS] Pushed {} Stack is {}", head, stk)
					}
					//t = t + 1
					//finishTimes(t) = next
					//logger.debug("\t\t[DFS] FinTim {}", finishTimes)
				}
			}
		}*/

		/*def dfs(j: Int) {
			logger.debug("\tStarting with {}", j)
			explored += j
			val stk = mutable.Stack(j)
			while(!stk.isEmpty) {
				val next = stk.top
				val initSize = stk.size
				logger.debug("\t\tGot {} from stack", next)
				for(head <- g.nodes(next)) {
					logger.debug("\t\t\tChecking {}, Explored {}", head, explored)
					if(!(explored contains head)) {
						explored += head
						stk push head
					}
				}
				if(initSize == stk.size) {
					//dead end- no new additions to stack
					while(!stk.isEmpty) {
						val node = stk.pop
						t = t + 1
						finishTimes(t) = node
					}
					logger.debug("\t\t\tFinish Times {}", finishTimes)
				}
				logger.debug("\t\t\tStack is {}", stk)
			}
		}*/

		val nodeItr = g.nodes.keySet.toList.sorted.reverse

		for(i <- nodeItr) {
			logger.debug("\t[CFT] dfs-loop {}", i)
			if(!(explored contains i)) {
				//dfs(mutable.Stack(i), mutable.Stack.empty[Int])
				dfs(i)
			}
		}

		finishTimes
	}

	def computeLeaders(g: Graph, ft: mutable.Map[Int, Int]): mutable.Map[Int, mutable.Set[Int]] = {
		logger.debug("\n\n\n[CL] Got graph {}", g)
		logger.debug("[CL] Got finish times {}", ft)
		var s:Int = -1
		val explored = mutable.Set.empty[Int]
		val leaders = mutable.Map.empty[Int, mutable.Set[Int]]

		def dfs(j: Int) {
			explored += j
			if(leaders contains s) {
				leaders(s) = (leaders(s) += j)
			} else {
				leaders(s) = mutable.Set(j)
			}
			logger.debug("\t\t[CL] Leaders {}", leaders)
			for(head <- g.nodes(j)) {
				if(!(explored contains head)) {
					dfs(head)
				}
			}
		}

		/*@tailrec
		def dfs(nodes: mutable.Stack[Int]) {
			logger.debug("<DFS> got stack = {}", nodes)
			if(nodes.isEmpty) {
				return
			}
			val current = nodes.pop
			explored += current
			if(leaders contains s) {
				leaders(s) = (leaders(s) += current)
			} else {
				leaders(s) = mutable.Set(current)
			}
			logger.debug("\t\t[CL] Leaders {}", leaders)
			for(head <- g.nodes(current)) {
				if(!(explored contains head)) {
					nodes push head
				}
			}
			dfs(nodes)
		}*/

		/*def dfs(j: Int) {
			explored += j
			val stk = mutable.Stack(j)
			while(!stk.isEmpty) {
				val next = stk.top
				val initSize = stk.size
				for(head <- g.nodes(next)) {
					logger.debug("\t\tlooking at {}", head)
					if(!(explored contains head)) {
						explored += head
						stk push head
					}
					logger.debug("\t\tStack = {}", stk)
				}
				if(initSize == stk.size) {
					//dead end- no new additions to stack
					while(!stk.isEmpty) {
						val node = stk.pop
						if(leaders contains s) {
							leaders(s) = (leaders(s) += node)
						} else {
							leaders(s) = mutable.Set(node)
						}
					}
					logger.debug("\t\t\tDeadEnd: leaders {}", leaders)
				}
			}
		}*/

		val nodeItr = ft.keySet.toList.sorted.reverse

		logger.debug("\t[CL] Iterate order {}", nodeItr)

		for(i <- nodeItr) {
			val ftNode= ft(i)
			logger.debug("\t[CL] processing node {}", ftNode)
			if(!(explored contains ftNode)) {
				s = i
				dfs(ftNode)
			}
		}

		leaders
	}

}