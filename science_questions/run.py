import csv
import json
import logging
from generate_data_json import collect, make_transitions
from pathlib import Path


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    logger = logging.getLogger(__name__)

    filepath = Path.cwd().parent / 'data' / 'questions.csv'

    # Read the data
    with open(filepath, encoding="utf8") as f:
        reader = csv.DictReader(f)
        raw_questions = [row['question'] for row in reader]

    # Generate questions, answers and transitions
    questions, answers = collect(raw_questions)

    q_transitions = make_transitions(questions)
    a_transitions = make_transitions(answers)

    # Save the transitions as json
    p = Path.cwd().parent
    q_path = p / 'data' / 'questions.json'
    a_path = p / 'data' / 'answers.json'

    if not q_path.exists():
        with open(q_path, 'w') as f:
            f.write(json.dumps(q_transitions))

    if not a_path.exists():
        with open(a_path, 'w') as f:
            f.write(json.dumps(a_transitions))

    logger.info('JSONs generated succesfully')
