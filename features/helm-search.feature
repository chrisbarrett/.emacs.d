Feature: Helm Search

  Scenario: Format helm search result for Future
    Given I got a search result for "Future" in file "concurrent/Future.scala" in line "22"
    Then the formatted result should match
      """
      Future
      concurrent/Future.scala:22
      """

  Scenario: Format helm search result for HashMap
    Given I got a search result for "HashMap" in file "util/HashMap.scala" in line "30"
    Then the formatted result should match
      """
      HashMap
      util/HashMap.scala:30
      """
