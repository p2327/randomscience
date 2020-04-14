import random
from typing import DefaultDict, List


# Initialise sentinel tokens
START = "__START__"
STOP = "__STOP__"


# Sentence generators
def smart_join(words: List[str]) -> str:
    """
    Joins a sequence of words into a sentence:
        - with no space if word is either '.', ',', '!' or '?'
        - with a space for any other word
    """
    symbols = ['.', ',', '?', '!']
    separators = ["" if w in symbols else " " for w in words]
    return ''.join(token
                   for word, separator in zip(words, separators)
                   for token in [separator, word]).strip()


assert smart_join(['Purescript', 'is', 'great', '!', '!']) == 'Purescript is great!!'


def markov_gen(transitions: DefaultDict[str, str]) -> str:
    """
    Generates a sequence of words; starting at the START sentinel, 
    repeatedly chooses random subsequence words from transitions, 
    until the STOP sentinel is encountered.
    """
    word = START
    words = []

    while True:
        word = random.choice(transitions.get(word, [STOP]))
        if word == STOP:
            return smart_join(words)
        else:
            words.append(word)


