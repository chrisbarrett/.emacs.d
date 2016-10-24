Feature: Insert Java Import
  As a user
  I want to import a class into the buffer for a java file

  Scenario: Insert Import With No Package Or Import Statement
    When I open temp java file "test"
    And I insert:
    """
    class C {
      int f = 1;
    }
    """

    And I go to line "2"
    And I go to end of line
    And I insert import "org.example"

    Then I should see:
    """
    import org.example;

    class C {
      int f = 1;
    }
    """

  Scenario: Insert Import With Package Statement
    When I open temp java file "test"
    And I insert:
    """
    package com.example;
    class C {
      int f = 1;
    }
    """

    And I go to line "3"
    And I go to end of line
    And I insert import "org.example"

    Then I should see:
    """
    package com.example;

    import org.example;

    class C {
      int f = 1;
    }
    """

  Scenario: Insert Import With Import Statement
    When I open temp java file "test"
    And I insert:
    """
    import m;

    import n;

    import p;
    class C {
      int f = 1;
    }
    """

    And I go to line "7"
    And I go to end of line
    And I insert import "org.example"

    Then I should see:
    """
    import m;

    import n;
    import org.example;

    import p;
    class C {
      int f = 1;
    }
    """
