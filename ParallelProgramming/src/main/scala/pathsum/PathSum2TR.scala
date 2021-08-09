package pathsum

import scala.annotation.tailrec

object PathSum2TR {

  private type Sum = Int
  private type Step = (TreeNode, Sum, Path)

  @tailrec
  def pathSum(steps: List[Step] = Nil,
              acc: List[Path] = Nil): List[Path] = {

    /* calculates the next steps from step */
    def nextSteps(step: Step): List[Step] = {
      val (node, sum, path) = step
      val left = node.leftOpt.map(l => (l, sum - node.value, path :+ node.value))
      val right = node.rightOpt.map(r => (r, sum - node.value, path :+ node.value))
      List(left, right).flatten
    }

    /* checks if step leads to a solution and returns its path if it does */
    def solution(step: Step): Option[Path] = {
      step match {
        case (node, sum, path) if node.isLeaf &&
          sum - node.value == 0 =>
          Some(path :+ node.value)
        case _ => None
      }
    }

    val nextAcc = acc ++ steps.flatMap(solution)
    steps.flatMap(nextSteps) match {
      case empty if empty.isEmpty => nextAcc
      case next @ _ => pathSum(next, nextAcc)
    }
  }

  def pathSum(root: TreeNode, targetSum: Int): List[Path] = {
    if(root == null) Nil else pathSum((root, targetSum, Nil) :: Nil)
  }

}
