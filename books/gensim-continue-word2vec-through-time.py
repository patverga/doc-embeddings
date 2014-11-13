from copy import deepcopy
import logging
import getopt
from scipy.spatial.distance import cosine
import numpy as np
import sys
import gensim
from gensim.models.word2vec import LineSentence
from collections import defaultdict


logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)

input_dir = '../google-ngrams/decades-unweighted/'
output_dir = '../google-ngrams/vectors-unweighted/'
decades = range(1850, 2010, 10)
size = 200
window = 4
negative_sample = 10
down_sample = 1e-5
min_occurrence_train = 10
min_occurrence_drift = 500
alpha_initial = .01
threshold = .016
max_epochs = 10


# get unique word counts to prune infrequent words
def unique_words(words_file):
    word_counts = defaultdict(int)
    for line in words_file:
        for word in line:
            word_counts[word] += 1
    return word_counts


def word_vector_change(word, model0, model1):
    v1 = model0.__getitem__(word)
    v2 = model1.__getitem__(word)
    num = np.dot(v1, v2)
    den = (np.linalg.norm(v1) * np.linalg.norm(v2))
    rsult = np.arccos(min((num / den), 1.0))
    # print rsult
    return rsult


def train_until_converge(word2vec_model, sentences):
    last_epoch = deepcopy(word2vec_model)
    change = 1
    epoch = 0
    while change > threshold and epoch < max_epochs:
        word2vec_model.train(sentences, total_words=100000000)
        changes = [word_vector_change(w, last_epoch, word2vec_model) for w in word2vec_model.vocab]
        change = sum(changes) / len(word2vec_model.vocab)
        last_epoch = deepcopy(word2vec_model)
        epoch += 1
        print ('epoch : ' + str(epoch), 'change : ' + str(change))


def calculate_drifts(word_counts, model_t1, model_t2):
    print ("calculating drifts.")
    # list of word shifts between time steps
    word_shift = [(cosine(model_t1.__getitem__(word), model_t2.__getitem__(word)), word)
                  for word in word_counts.iterkeys()]
    sorted_shifts = sorted(word_shift, reverse=True)

    drift_file = open(output_dir + 'drifts', 'w')
    for sim, word in sorted_shifts:
        drift_file.write((word + '   ' + str(sim) + '\n').encode('utf-8'))
    drift_file.close()


def main(argv):
    vocab_file = ''
    if len(argv) > 0:
        opts, args = getopt.getopt(argv, "v:", ["vocab="])
        for opt, arg in opts:
            if opt in ("-v", "--vocab"):
                vocab_file = arg

    in_files = [(input_dir + str(decade) + '.gz') for decade in decades]
    out_files = [(output_dir + str(decade) + '.vectors') for decade in decades]

    if vocab_file is not '':
        print ('Loading vocab from : ' + vocab_file)
        model = gensim.models.Word2Vec.load(vocab_file)
    else:
        print ('Creating vocab.')
        model = gensim.models.Word2Vec(workers=8, size=size, window=window, alpha=alpha_initial,
                                       negative=negative_sample, sample=down_sample, min_count=min_occurrence_train)
        model.build_vocab(LineSentence(in_files))

    # sequentially train the model to convergence on each time slice
    model.save(output_dir + 'vocab')
    for i, in_file in enumerate(in_files):
        print ('training on : ' + in_file)
        train_until_converge(model, LineSentence(in_file))
        model.save(out_files[i])

    # get the words occuring in corpus at least min_occurrence_drift times
    word_counts = {w: c for (w, c) in unique_words(LineSentence(in_files)).iteritems() if c > min_occurrence_drift}

    model_1900 = gensim.models.Word2Vec.load(output_dir + "1900.gz.vectors")
    calculate_drifts(word_counts, model, model_1900)


if __name__ == "__main__":
    main(sys.argv[1:])
