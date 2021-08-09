package pathsum

import pathsum.PathSum2TR.pathSum

class PathSum2TRSuite extends munit.FunSuite {

  test("should work with very small tree including null and negative numbers") {
    val input1 = new TreeNode(-2, null, new TreeNode(-3))
    assertEquals(pathSum(input1, -5), List(List(-2, -3)))
  }

  test("should return same path twice if it is there") {
    val input1 = new TreeNode(0, new TreeNode(1), new TreeNode(1))
    assertEquals(pathSum(input1, 1), List(List(0,1), List(0,1)))
  }


}
