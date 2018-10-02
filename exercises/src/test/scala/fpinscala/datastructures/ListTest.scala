package fpinscala.datastructures

import fpinscala.datastructures.List.{drop, init, setHead, tail}
import fpinscala.datastructures.ListMain.dropWhileCurry
import org.scalatest.{Matchers, WordSpec}

class ListTest extends WordSpec with Matchers{
  "List.tail" when {
    val input = List()
    s""" tail($input) """ should {
      val expected = List()

      s"""return $expected""" in {
        val actual = tail(input)

        actual should be (expected)
      }
    }

    val input2 = List(1)
    s""" tail($input2) """ should {
      val expected = List()

      s"""return $expected""" in {
        val actual = tail(input2)

        actual should be (expected)
      }
    }

    val input3 = List(1, 2)
    s""" tail($input3) """ should {
      val expected = List(2)

      s"""return $expected""" in {
        val actual = tail(input3)

        actual should be (expected)
      }
    }

    val input4 = List(1, 2, 3)
    s""" tail($input4) """ should {
      val expected = List(2, 3)

      s"""return $expected""" in {
        val actual = tail(input4)
        
        actual should be (expected)
      }
    }
  }

  "List.setHead" when {
    val input = List(1, 2)
    val value = 3

    s"""setHead($input, $value)""" should {
      val expected = List(3, 2)

      s"""return $expected""" in {
        val actual = setHead(input, value)

        actual should be (expected)
      }
    }
  }

  "List.drop" when {
    val input = List(1, 2, 3)
    val value = 2

    s"""drop($input, $value)""" should {
      val expected = List(3)

      s"""return $expected""" in {
        val actual = drop(input, value)

        actual should be (expected)
      }
    }
  }

  "List.dropWhileCurry" when {
    val input = List(1, 2, 3)


    s"""dropWhileCurry($input)(i => i < 2)""" should {
      val expected = List(2, 3)

      s"""return $expected""" in {
        val actual = dropWhileCurry(input)(i => i < 2)

        actual should be (expected)
      }
    }
  }

  "List.init" when {
    val input = List(1, 2, 3, 4)


    s"""init($input)""" should {
      val expected = List(1, 2, 3)

      s"""return $expected""" in {
        val actual = init(input)

        actual should be (expected)
      }
    }
  }
}
