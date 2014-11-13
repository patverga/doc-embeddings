__author__ = 'pv'

from gensim.models.word2vec import LineSentence
from collections import defaultdict
#from Walker import WalkerRandomSampling
import random
import sys

if len(sys.argv) <= 1:
    print('must supply year. (example : "1850")')
    sys.exit(-1)
else:
    print('Sampling data from ' + sys.argv[1])
    year = int(sys.argv[1])
   
ngrams = defaultdict(int)
input = '/home/pat/work3/doc-embeddings/google-ngrams/years/'
output = '/home/pat/work3/doc-embeddings/google-ngrams/years-sampled/'
output = '/home/pat/work3/doc-embeddings/google-ngrams/years-sampled-unweighted/'
file = input + str(year) + '.gz'
lines = LineSentence(file)

try:
    for i, line in enumerate(lines):
        if '_' not in ''.join(line):
            if i % 100000 == 0:
                print(line)
            parts = line[-1].rsplit(",", 1)
            ngram = line[:-1]
            ngram.append(parts[0])
            try:
                #count = int(parts[1])
                count = 1
                ngrams[' '.join(ngram).lower()] += count
            except:
                print("could not parse " + str(line))
except:
    print("file broke")

print ("sampling...")
#walker = WalkerRandomSampling(weights=ngrams.values(), keys=ngrams.keys())
ngram_values = ngrams.keys()
random.shuffle(ngram_values)

print("writing samples to file\n")
with open(output + str(year), 'w') as f:
    for sample in ngram_values[:10000000]:
        #for sample in walker.random(1000):
        f.write((sample + "\n").encode('utf-8'))
