package fpinscala.datastructures

import org.scalatest.{Matchers, WordSpec}

class TreeTest extends WordSpec with Matchers{

  "Tree.size" when {
    val input = Leaf("a")
    s""" size($input) """ should {
      val expected = 1

      s"""return $expected""" in {
        val actual = Tree.size(input)

        actual should be (expected)
      }
    }

    val input2 = Branch(Leaf("a"), Leaf("b"))
    s""" size($input2) """ should {
      val expected = 2

      s"""return $expected""" in {
        val actual = Tree.size(input2)

        actual should be (expected)
      }
    }

    val input3 = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    s""" size($input3) """ should {
      val expected = 4

      s"""return $expected""" in {
        val actual = Tree.size(input3)

        actual should be (expected)
      }
    }

    val input4 = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Branch(Leaf("d"), Leaf("e"))))
    s""" size($input4) """ should {
      val expected = 5

      s"""return $expected""" in {
        val actual = Tree.size(input4)

        actual should be (expected)
      }
    }
  }


  "Tree.maximum" when {
    val input = Leaf(1)
     s"""Tree.maximum($input)""" should {
       val expected = 1
       s"""return $expected""" in {
         val actual = Tree.maximum(input)

         actual should be (expected)
       }
     }

    val input2 = Branch(Leaf(1), Leaf(2))
    s"""Tree.maximum($input2)""" should {
      val expected = 2
      s"""return $expected""" in {
        val actual = Tree.maximum(input2)

        actual should be (expected)
      }
    }

    val input3 = Branch(Leaf(3), Leaf(2))
    s"""Tree.maximum($input3)""" should {
      val expected = 3
      s"""return $expected""" in {
        val actual = Tree.maximum(input3)

        actual should be (expected)
      }
    }

    val input4 = Branch(Branch(Leaf(10), Leaf(1)), Branch(Leaf(5), Leaf(7)))
    s"""Tree.maximum($input4)""" should {
      val expected = 10
      s"""return $expected""" in {
        val actual = Tree.maximum(input4)

        actual should be (expected)
      }
    }

    val input5 = Branch(Branch(Leaf(6), Leaf(7)), Branch(Leaf(3), Branch(Leaf(1), Leaf(2))))
    s"""Tree.maximum($input5)""" should {
      val expected = 7
      s"""return $expected""" in {
        val actual = Tree.maximum(input5)

        actual should be (expected)
      }
    }
  }


  "Tree.depth" when {
    val input = Branch(Leaf("a"), Leaf("b"))

    s"""Tree.depth($input)""" should {
      val expected = 1

      s"""return $expected""" in {
        val actual = Tree.depth(input)

        actual should be (expected)
      }
    }

    val input2 = Branch(Branch(Leaf("a"), Leaf("b")), Leaf("c"))

    s"""Tree.depth($input2)""" should {
      val expected = 2

      s"""return $expected""" in {
        val actual = Tree.depth(input2)

        actual should be (expected)
      }
    }

    val input3 = Branch(Branch(Branch(Leaf("a"), Leaf("b")), Leaf("c")), Leaf("d"))

    s"""Tree.depth($input3)""" should {
      val expected = 3

      s"""return $expected""" in {
        val actual = Tree.depth(input3)

        actual should be (expected)
      }
    }

    val input4 = Branch(Branch(Branch(Leaf("a"), Leaf("b")), Leaf("c")), Branch(Leaf("d"), Leaf("e")))

    s"""Tree.depth($input4)""" should {
      val expected = 3

      s"""return $expected""" in {
        val actual = Tree.depth(input4)

        actual should be (expected)
      }
    }
  }
}
