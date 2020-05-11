from collections import defaultdict
import re
from typing import List, DefaultDict, Tuple


# Build regex cases iteratively to cover all the questions
splits = [r"\([A-D]\)",    # (A) python (B) haskell (C) javascript (D) ruby
          r"\s[A-D]\.\s",  #  A. python  B. haskell  C. javascript  D. ruby
          r"\s[1-4]\.\s",  #  1. python  2. haskell  3. javascript  4. ruby
          r"\s[A-D]\s",    #  A  python  B  haskell  C  javascript  D  ruby
          r"\s[FGHJ]\s",   #  F  python  G  haskell  H  javascript  J  ruby
          r"\n [A-D]\s"]   #   A python
                           #   B haskell
                           #   C javascript
                           #   D ruby


# Transitions regex pattern
"""
Matches
    - a period
    - a comma
    - a question mark
    - a word containing none of the above or spaces
"""
regex = r"[^ ?\.,]+|\?|\.|\,"


def collect(questions: List[str], patterns: List[str] = splits) -> Tuple[List[str], List[str]]:
    """
    Takes a set of questions and answers and separates the two in two lists
    according to passed list of regex patterns (splits).
    """
    q_list = []
    a_list = []
    for q in questions:
        for pattern in patterns:
            pieces = [x.strip() for x in re.split(pattern, q)]
            # If we collected 4 to 5 pieces
            if len(pieces) in [4, 5]:
                # Add the first to the questions and the rest to the answers
                q_list.append(pieces[0])
                a_list.extend(pieces[1:])
                # Move to the next questions
                break
        else:
            # Executed only when the loop is not terminated by break
            # This finds elements that the if does not catch
            # print(q + '\n')

    return q_list, a_list


def make_transitions(sentences: List[str], regex: str = regex) -> DefaultDict[str, str]:
    """
    Takes a list of sentences and a regex pattern and:
        - Splits the words in each sentence
        - Enters each word as a dictionary key
        - Appends subsequent words to a list for each key

    Each word in a list appears with higher / lower frequency
    according to how often is found in the sentences.

    This allows picking words to chain by markov_chain
    according to higher probability.
    """
    START = "__START__"
    STOP = "__STOP__"
    transitions = defaultdict(list)

    for sentence in sentences:
        words = [START] + re.findall(regex, sentence) + [STOP]
        for prev_word, next_word in zip(words, words[1:]):
            transitions[prev_word].append(next_word)

    return transitions
