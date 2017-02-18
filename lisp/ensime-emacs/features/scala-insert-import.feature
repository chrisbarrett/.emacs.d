Feature: Insert Scala Import
  As a user
  I want to import a class into the buffer for a scala file

  Scenario: Insert Import With No Package Or Import Statement
    When I open temp scala file "test"
    And I insert:
    """
    class C {
      def f = 1
    }
    """

    And I go to line "2"
    And I go to end of line
    And I insert import "org.example"

    Then I should see:
    """
    import org.example

    class C {
      def f = 1
    }
    """

  Scenario: Insert Import With Package Statement
    When I open temp scala file "test"
    And I insert:
    """
    package com.example
    class C {
      def f = 1
    }
    """

    And I go to line "3"
    And I go to end of line
    And I insert import "org.example"

    Then I should see:
    """
    package com.example

    import org.example

    class C {
      def f = 1
    }
    """

  Scenario: Insert Import after existing multiline import
    When I open temp scala file "test"
    And I insert:
    """
    package com.example

    import com.example.{ Test1, Test2,
      Test3,
      Test4
    }

    class C {
      def f = 1
    }
    """

    And I go to line "9"
    And I go to end of line
    And I insert import "scala.collection.immutable.Seq"

    Then I should see:
    """
    package com.example

    import com.example.{ Test1, Test2,
      Test3,
      Test4
    }
    import scala.collection.immutable.Seq

    class C {
      def f = 1
    }
    """

  Scenario: Insert Import With Import Statement
    When I open temp scala file "test"
    And I insert:
    """
    import m

    import n

    import p
    class C {
      def f = 1
    }
    """

    And I go to line "7"
    And I go to end of line
    And I insert import "org.example"

    Then I should see:
    """
    import m

    import n
    import org.example

    import p
    class C {
      def f = 1
    }
    """

  Scenario: Insert Import Stays Above Point
    When I open temp scala file "test"
    And I turn on scala-mode
    And I insert:
    """
    class C {
      import example._
      def f = 1
    }
    """

    And I go to line "2"
    And I go to the end of the line
    And I insert import "org.example"

    Then I should see:
    """
    class C {
      import org.example
      import example._
      def f = 1
    }
    """

  Scenario: Insert Import Groups Classes Under The Same Package
    When I open temp scala file "test"
    And I insert:
    """
    package com.example

    import org.example.Example2

    class C {
      def f = 1
    }
    """

    And I go to line "6"
    And I go to end of line
    And I insert import "org.example.Example1"

    Then I should see:
    """
    package com.example

    import org.example.{ Example1, Example2 }

    class C {
      def f = 1
    }
    """

  Scenario: Insert in File without package
    When I open temp scala file "test"
    And I insert:
    """
    import scala.concurrent.duration._
    class A(value:String){
      def hello(){
        new ListBuffer()
      }
    }
    """

    And I go to line "4"
    And I go to end of line
    And I insert import "org.scala.collections.mutable.ListBuffer"

    Then I should see:
    """
    import scala.concurrent.duration._
    import org.scala.collections.mutable.ListBuffer
    class A(value:String){
      def hello(){
        new ListBuffer()
      }
    }
    """
  Scenario: Insert Import Groups Import Under The Same Package Even When Already Grouped
    When I open temp scala file "test"
    And I insert:
    """
    package com.example

    import org.example.{ ClassA => AsB, Example1, Example3 }

    class C {
      def f = 1
    }
    """

    And I go to line "6"
    And I go to end of line
    And I insert import "org.example.Example2"

    Then I should see:
    """
    package com.example

    import org.example.{ ClassA => AsB, Example1, Example2, Example3 }

    class C {
      def f = 1
    }
    """

  Scenario: Add Class to first import block in file
    When I open temp scala file "test"
    And I insert:
    """
    import java.util.GregorianCalendar

    class C {
      def f = 1
    }
    """
    And I go to line "4"
    And I insert import "java.util.Calendar"

    And I go to line "1"
    Then the line should match "import java.util.{ Calendar, GregorianCalendar }"
