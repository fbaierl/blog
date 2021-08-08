package pathsum

import org.scalameter.{Key, Warmer, config}
import pathsum.PathSum2.{pathSum, pathSumSeq}

import scala.util.Random

class PathSum2Suite extends munit.FunSuite {

  private def constructTestTree(depth: Int): TreeNode = {
    val max = 1000
    val min = -1000
    val seed = 123
    val rand = new scala.util.Random(seed)

    def construct(rand: Random, depth: Int): TreeNode = {
      val value = rand.nextInt(max - min) + min
      if(depth - 1 <= 0) new TreeNode(value)
      else new TreeNode(value, construct(rand, depth - 1), construct(rand, depth - 1))
    }

    construct(rand, depth)
  }

  test("should work with very small tree including null and negative numbers") {
    val input1 = new TreeNode(-2, null, new TreeNode(-3))
    assert(pathSum(input1, -5) == List(List(-2, -3)))
  }

  test("parallel and sequential version of path sum should return same result") {
    val randTree = constructTestTree(11) // 2^10 nodes (1024=
    assertEquals(pathSumSeq(randTree, 20), pathSum(randTree, 20))
  }

  test("the parallel algorithm should be faster than the sequential algorithm for large data input") {

    val standardConfig = config(
      Key.exec.minWarmupRuns := 10,
      Key.exec.maxWarmupRuns := 30,
      Key.exec.benchRuns := 15,
      Key.verbose := false
    ) withWarmer Warmer.Default()

    val randTree = constructTestTree(23) // 16777215 nodes

    val seqtime = standardConfig measure {
      pathSumSeq(randTree, 7)
    }

    val partime = standardConfig measure {
      pathSum(randTree, 7)
    }

    val speedup = seqtime.value / partime.value
    println(s"sequential time: $seqtime")
    println(s"parallel time: $partime")
    println(s"speedup: $speedup")

    assert(speedup > 1.5)
  }




}
