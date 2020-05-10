from flask import Flask
from flask_cors import CORS, cross_origin
import json
from pathlib import Path
import os
import random
from utilities import START, STOP, markov_gen


app = Flask(__name__)
cors = CORS(app)    # CORS is a mechanism that defines a procedure in which
                    # the browser and the web server interact to determine whether
                    # to allow a web page to access a resource from different origin.
                    # flask-cors allows to call this API cross-origin


app.config['CORS_HEADERS'] = 'Content-Type'

print(Path.cwd().parent)
# p = Path.cwd().parent
p = Path.cwd()
a_path = p / 'data' / 'answers.json'
q_path = p / 'data' / 'questions.json'


with open(a_path) as f:
    a_transitions = json.loads(f.read())


with open(q_path) as f:
    q_transitions = json.loads(f.read())


# Generate JSON response with question, answer and correct answer
@app.route('/')
@cross_origin()
def get_question():
    return json.dumps({
        "questionText": markov_gen(q_transitions),
        "answers": [markov_gen(a_transitions) for _ in range(4)],
        "correctAnswer": random.randint(0, 3)
    })


'''
if __name__ == '__main__':
    PORT = int(os.environ.get('PORT', 8080))
    app.run(port=PORT)
'''

if __name__ == "__main__":
    app.run(host='127.0.0.1', port=8080, debug=True)
