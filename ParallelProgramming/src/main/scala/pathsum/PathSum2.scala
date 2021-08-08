package pathsum

import scala.concurrent.ExecutionContext

object PathSum2 {

  def pathSumSeq(root: TreeNode,
                 targetSum: Int,
                 acc: List[Path] = Nil,
                 current: Path = Nil): List[Path] = {
    def nextPath: Path = current :+ root.value
    (root.leftOpt, root.rightOpt) match {
      case (Some(left), Some(right)) =>
        pathSumSeq(left, targetSum - root.value, acc, nextPath) ++
          pathSumSeq(right, targetSum - root.value, acc, nextPath)
      case (Some(left), _) => pathSumSeq(left, targetSum - root.value, acc, nextPath)
      case (_, Some(right)) => pathSumSeq(right, targetSum - root.value, acc, nextPath)
      case (_, _) if root.isLeaf && root.value == targetSum => acc :+ nextPath
      case _ => acc
    }
  }

  def pathSumPar(parallelism: Int)
                (root: TreeNode,
                 targetSum: Int,
                 acc: List[Path],
                 current: Path)
                (implicit ec: ExecutionContext): List[Path] = {
    if(parallelism <= 0) pathSumSeq(root, targetSum, acc, current)
    else {
      def nextPath: Path = current :+ root.value
      (root.leftOpt, root.rightOpt) match {
        case (Some(left), Some(right)) =>
          val parF = pathSumPar(parallelism - 2) _
          val (a, b) = parallel(
            parF(left, targetSum - root.value, acc, nextPath),
            parF(right, targetSum - root.value, acc, nextPath))
          a ++ b
        case (Some(left), _) =>
          val parF = pathSumPar(parallelism) _
          parF(left, targetSum - root.value, acc, nextPath)
        case (_, Some(right)) =>
          val parF = pathSumPar(parallelism) _
          parF(right, targetSum - root.value, acc, nextPath)
        case (_, _) if root.isLeaf && root.value == targetSum =>
          acc :+ (nextPath)
        case _ => acc
      }
    }
  }

  def pathSum(root: TreeNode, targetSum: Int): List[Path] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val cores = Runtime.getRuntime.availableProcessors()
    if(root == null) Nil else pathSumPar(cores)(root, targetSum, Nil, Nil)
  }

}
