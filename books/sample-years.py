__author__ = 'pv'

from gensim.models.word2vec import LineSentence
from collections import defaultdict
from Walker import WalkerRandomSampling

ngrams = defaultdict(int)
decade = 1850

files = []
for year in range(decade, decade + 10):
    files.append("data/years/" + str(year) + ".gz")
lines = LineSentence(files)

try:
    for i, line in enumerate(lines):
        if i % 100000 == 0:
            print(line)
        parts = line[-1].rsplit(",", 1)
        ngram = line[:-1]
        ngram.append(parts[0])
        try:
            count = int(parts[1])
            ngrams[' '.join(ngram).lower()] += count
        except:
            print("could not parse " + str(line))
except:
    print("file broke")

print ("sampling...")
walker = WalkerRandomSampling(weights=ngrams.values(), keys=ngrams.keys())

print("writing samples to file\n")
with open('data/decades-sampled/' + str(decade), 'w') as f:
    for i in range(0, 1000):
        for sample in walker.random(1000):
            f.write(sample + "\n")