Feature: Completion Prefix Lexing

  Background:
    Given I am in buffer "*ensime-completion-lexing*"
    And I turn on scala-mode
    And I insert:
    """
    package com.example
    class c {
      def main {
        val rat = dog
        rat ++=
        rat !
        rat CaptainCrun
        rat.++=
        rat.toSt
        x
        /*heello*/dog
        while (moose)prin
        case _ =>r
        dfkjsdfj
        dfkjsdfj132
        dfkjsdfj132:+
        dfkjsdfj132_:+
        _MAX_LEN_
        `yield`
        αρετη
        _y
        dot_product_*
        __system
        empty_?
        .
        .kjsdf
          kjsdf
          \"abc $hell\"
          abc_*=efg
      }
    }
    """

  Scenario: Empty Prefix Lexing
    Given I go to line "25"
    And I go to the end of the line
    Then I should have an empty completion prefix
    And I clear the buffer

  Scenario: Complete Unique Prefixes
    Then I should see unique prefixes:
      | prefix         |
      | !              |
      | toSt           |
      | CaptainCrun    |
      | moose          |
      | dfkjsdfj       |
      | dfkjsdfj132    |
      | dfkjsdfj132_:+ |
      | package        |
      | _MAX_LEN_      |
      | `yield`        |
      | αρετη          |
      | _y             |
      | dot_product_*  |
      | __system       |
      | empty_?        |
      | prin           |
      | hell           |
      | efg            |

    And I clear the buffer

  Scenario: Complete Prefixes At EOL
    Then I should see prefixes at the end of lines:
      | prefix | line |
      | dog    | 4    |
      | ++=    | 5    |
      | ++=    | 8    |
      | x      | 10   |
      | dog    | 11   |
      | r      | 13   |
      | :+     | 16   |
      | kjsdf  | 26   |
      | kjsdf  | 27   |

    And I clear the buffer
