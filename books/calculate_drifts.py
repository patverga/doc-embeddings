__author__ = 'pv'

import logging
from scipy.spatial.distance import cosine
from gensim.models.word2vec import LineSentence
from gensim.models import Word2Vec
from collections import Counter
import io

logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)

data_dir = '../google-ngrams/decades/'
vector_dir = '../google-ngrams/vectors/'


def drifts(words, model_t1, model_t2):
    print ("calculating drifts.")
    # list of word shifts between time steps
    word_shift = [(1.0 - cosine(model_t1.__getitem__(word.rstrip()), model_t2.__getitem__(word.rstrip())), word)
                  for word in words]
    sorted_shifts = sorted(word_shift, reverse=True)

    drift_file = open(vector_dir + 'drifts', 'w')
    for sim, word in sorted_shifts:
        drift_file.write((word + '\t' + str(sim) + '\n').encode('utf-8'))
    drift_file.close()


def word_counts():
    total_freq = Counter()
    for decade in range(1850, 2010, 10):
        print ('Parsing ' + str(decade) + "...")
        decade_freq = Counter()
        in_files = [(data_dir + str(decade) + '.gz')]
        for i, line in enumerate(LineSentence(in_files)):
            if i % 100000 == 0:
                print(i)
            decade_freq.update(line)

        # write this decade counts to file
        with io.open(data_dir + '/counts/' + str(decade), 'w', encoding='utf8') as f:
            for key, count in decade_freq.iteritems():
                f.write(key + "\t" + str(count)+"\n")
        # add the decade counts to total counts
        total_freq.update(decade_freq)

    # write the total counts to file
    with io.open(data_dir + '/counts/total', 'w', encoding='utf8') as f:
        for key, count in total_freq.iteritems():
            f.write(key + "\t" + str(count)+"\n")


# word_counts()

with io.open(data_dir+'counts/words3', 'r', encoding='utf8') as f:
    words = f.readlines()
drifts(words, Word2Vec.load(vector_dir + "1900.vectors"),
              Word2Vec.load(vector_dir + "2000.vectors"))