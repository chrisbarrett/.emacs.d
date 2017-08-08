Feature: Hierarchy buffer
  As a user
  I want to see the type hierarchy

  Scenario: Format hierarchy types
    When I open temp scala file "test"

    Given I got a type hierarchy type for "Future" in file "concurrent/Future.scala" in line "22"
    When I format the hierarchy type in the current buffer

    Then I should see:
    """
    concurrent/Future.scala:22: Future
    """

  Scenario: Format hierarchy types with line 0 should write line 1
    When I open temp scala file "test"

    Given I got a type hierarchy type for "Future" in file "concurrent/Future.scala" in line "0"
    When I format the hierarchy type in the current buffer

    Then I should see:
    """
    concurrent/Future.scala:1: Future
    """
