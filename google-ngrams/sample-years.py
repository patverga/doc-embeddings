__author__ = 'pv'

from gensim.models.word2vec import LineSentence
from collections import defaultdict
from numpy import cumsum,  sum, searchsorted
from numpy.random import rand
import sys

if len(sys.argv) <= 1:
    print('must supply year. (example : "1850")')
    sys.exit(-1)
else:
    print('Sampling data from ' + sys.argv[1])
    year = int(sys.argv[1])
   
ngrams = defaultdict(int)
input = './years/'
output = './years-sampled/'
#output = './years-sampled-unweighted/'
file = input + str(year) + '.gz'
lines = LineSentence(file)
n_samples = 10000000  # 10 million

try:
    for i, line in enumerate(lines):
        if '_' not in ''.join(line):
            if i % 100000 == 0:
                print(line)
            parts = line[-1].rsplit(",", 1)
            ngram = line[:-1]
            ngram.append(parts[0])
            try:                
                count = 1
                ngrams[' '.join(ngram).lower()] += count
            except:
                print("could not parse " + str(line))
except:
    print("file broke")

print ("sampling...")
words = ngrams.keys()
weights = ngrams.values()
t = cumsum(weights)
s = sum(weights)

with open(output + str(year), 'w') as f:
    for index in searchsorted(t, rand(n_samples)*s):
        f.write((words[index] + "\n").encode('utf-8'))