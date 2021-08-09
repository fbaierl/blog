import java.util.concurrent.ForkJoinTask
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

package object pathsum {

  type Path = List[Int]

  // https://docs.scala-lang.org/overviews/core/futures.html
  def parallel[A, B](taskA: => A, taskB: => B)
                    (implicit ec: ExecutionContext): (A, B) = {
    val ta = Future(taskA)
    val b = taskB
    val a = Await.result(ta, Duration.Inf)
    (a, b)
  }

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  implicit def treeExtension: TreeNode => TreeNodeExtension = TreeNodeExtension

  case class TreeNodeExtension(node: TreeNode) {
    def isLeaf: Boolean = node.left == null && node.right == null
    def leftOpt: Option[TreeNode] = Option(node.left)
    def rightOpt: Option[TreeNode] = Option(node.right)
    def prettyPrint: String = s"(" +
      s"${node.value}, " +
      s"${leftOpt.map(_.prettyPrint).getOrElse("null")}, " +
      s"${rightOpt.map(_.prettyPrint).getOrElse("null")})"
  }

}
