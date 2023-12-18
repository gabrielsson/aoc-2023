package dev.gabrielsson

import scala.annotation.tailrec
import scala.collection.mutable

object Graphs {
  object dfs {
    def apply[A](start: A)(neighborsFunction: A => Iterable[A]): Iterable[A] = {
      def _dfs(s: A, seen: Iterable[A]): Iterable[A] = {
        if (seen.iterator.contains(s)) seen
        else {
          val neighbors = neighborsFunction(s).filterNot(seen.iterator.contains)
          neighbors.foldLeft(Iterable(s) ++ seen)((b, a) => _dfs(a, b))
        }
      }

      _dfs(start, Nil)
    }
  }

  object dfsStack {
    def apply[A](start: A)(neighborsFunction: A => Iterable[A]): Iterable[A] = {
      val stack = mutable.Stack(start)
      val seen = mutable.Set[A]()
      while (stack.nonEmpty) {
        val node = stack.pop()
        if (seen.add(node)) {
          for (neighbor <- neighborsFunction(node)) {
            stack.push(neighbor)
          }
        }
      }
      seen
    }
  }


  object bfs {
    def apply[A](start: A)(neighborsFunction: A => Iterable[A]): Map[A, Int] = {
      @tailrec
      def _bfs(seen: Map[A, Int], unseen: Map[A, Int]): Map[A, Int] = {
        val neighbors = for {(node, cost) <- unseen; newNode <- neighborsFunction(node)} yield
          newNode -> (cost + 1)
        val seen2 = seen ++ unseen
        val unseen2 = neighbors.filterNot(n => seen.contains(n._1))
        if (unseen2.isEmpty) seen2 else _bfs(seen2, unseen2)
      }

      _bfs(Map.empty, Map(start -> 0))
    }
  }

  object bfsQueue {
    def apply[A](start: A)(neighborsFunction: A => Iterable[A]): Map[A, Int] = {
      val queue = mutable.Queue(start -> 0) // Queue to hold nodes with their level (distance from the start)
      val seen = mutable.Map[A, Int]() // Map to hold seen nodes with their shortest path distance

      while (queue.nonEmpty) {
        val (currentNode, currentLevel) = queue.dequeue()

        // If we haven't seen the node or found a shorter path, process it
        if (!seen.contains(currentNode)) {
          seen(currentNode) = currentLevel // Mark the node as seen with its level

          // Enqueue all unseen neighbors
          for (neighbor <- neighborsFunction(currentNode)) {
            if (!seen.contains(neighbor)) {
              queue.enqueue(neighbor -> (currentLevel + 1))
            }
          }
        }
      }

      seen.toMap // Convert to immutable map before returning
    }
  }

  object aStar {
    def apply[A](starts: Iterable[A])
                (goalFunction: A => Boolean)
                (costFunction: (A, A) => Int)
                (neighborsFunction: A => Iterable[A])
                (heuristicFunction: A => Int):  (Map[A, (Int, Iterable[A])], Option[(A, (Int, Iterable[A]))]) = {
      val seen: mutable.Map[A, (Int, Iterable[A])] = mutable.Map.empty
      val unseen: mutable.PriorityQueue[(Int, Int, A, Iterable[A])] =
        mutable.PriorityQueue.empty(Ordering.by(-_._1))
      starts.foreach(start => unseen.enqueue((heuristicFunction(start), 0, start, Seq(start))))
      while (unseen.nonEmpty) {
        val (_, dist, node, ps) = unseen.dequeue()
        if (!seen.contains(node)) {
          seen(node) = (dist, ps)
          if (goalFunction(node)) {
            return (seen.toMap, Some(node -> (dist, ps)))
          } else {
            def visit(n: A, d: Int) =
              if (!seen.contains(n)) {
                unseen.enqueue((dist + d + heuristicFunction(n), dist + d, n, ps ++ Seq(n)))
              }

            neighborsFunction(node).map(n => (n, costFunction(node, n))).foreach(n => visit(n._1, n._2))
          }
        }
      }
      (seen.toMap, None)
    }
  }

  object dijkstra {
    def apply[A](start: A, goal: A)(neighborFunction: A => Iterable[A])(costFunction: (A, A) => Int): (Map[A, Int], Option[(A, Int)]) = {
      val seen: mutable.Map[A, Int] = mutable.Map.empty
      val unseen: mutable.PriorityQueue[(Int, A)] = mutable.PriorityQueue.empty(Ordering.by(-_._1))
      unseen.enqueue((0, start))
      while (unseen.nonEmpty) {
        val (dist, node) = unseen.dequeue()
        if (!seen.contains(node))
          seen(node) = dist
        if (node == goal)
          return (seen.toMap, Some(node -> dist))
        else {
          def visit(n: A, d: Int) =
            if (!seen.contains(n)) unseen.enqueue((dist + d, n))

          neighborFunction(node).map(neighbor => (neighbor, costFunction(node, neighbor))).foreach(n => visit(n._1, n._2))
        }
      }
      (seen.toMap, None)
    }
  }
}