from copy import deepcopy
import logging
import getopt
from scipy.spatial.distance import cosine
import sys
import gensim
from gensim.models.word2vec import LineSentence
from collections import defaultdict


logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)

# get unique word counts to prune infrequent words
def unique_words(words_file):
    # return [word for line in open(words_file, 'r') for word in line.split()]
    words = defaultdict(int)
    for line in words_file:
        for word in line:
            words[word] += 1
    return words


def main(argv):
    input_file_t1 = ''
    input_file_t2 = ''
    output_file_t1 = ''
    output_file_t2 = ''
    size = 100
    window = 10
    negative_sample = 10
    down_sample = 1e-5
    min_occurance = 20
    try:
        opts, args = getopt.getopt(argv, "hi1:i2:o1:02:s:", ["ifile1=", "ifile2=", "ofile1=", "ofile2=", "size="])
    except getopt.GetoptError:
        print 'test.py -i <input_file> -o <output_file>'
        sys.exit(2)
    for opt, arg in opts:
        if opt == '-h':
            print 'test.py -i <input_file> -o <output_file>'
            sys.exit()
        elif opt in ("-i1", "--ifile1"):
            input_file_t1 = arg
        elif opt in ("-i2", "--ifile2"):
            input_file_t2 = arg
        elif opt in ("-o1", "--ofile1"):
            output_file_t1 = arg
        elif opt in ("-o2", "--ofile2"):
            output_file_t2 = arg
        elif opt in ("-s", "--size"):
            size = int(arg)
    print 'T1 Input file is ', input_file_t1
    print 'T2 Input file is ', input_file_t2
    print 'T1 Output file is ', output_file_t1
    print 'T2 Output file is ', output_file_t2
    print 'Size is ', size

    model = gensim.models.Word2Vec(workers=8, size=size, window=window,
                                   negative=negative_sample, sample=down_sample, min_count=1)

    t1_sentences = LineSentence(input_file_t1)
    t2_sentences = LineSentence(input_file_t2)
    # combined_sentences = LineSentence([input_file_t1, input_file_t2])

    # we want a vocabulary of only words that occur in both files
    t_words = [unique_words(t1_sentences), unique_words(t2_sentences)]
    # prune infrequent words
    for t_word_dict in t_words:
        for w, c in t_word_dict.items():
            if c < min_occurance:
                del t_word_dict[w]

    # get the words occuring enough time in both time slices
    intersect_words = list(set(t_words[0].keys()) & set(t_words[1].keys()))
    # print "t"
    # t1_intersect_words = [x for x in t1_words if x in intersect_words]


    # model.build_vocab(t2_sentences)
    # print(len(model.vocab))
    model.build_vocab([intersect_words])
    # print(len(model.vocab))
    # model.build_vocab(combined_sentences)
    # print(len(model.vocab))

    # train the model at time T1
    model.train(t1_sentences)
    model.save(output_file_t1)

    # continue the same model with dat from T2
    model.train(t2_sentences)
    model.save(output_file_t2)

    model_t1 = gensim.models.Word2Vec.load(output_file_t1)
    # list of word shifts between time steps
    word_shift = []
    for word in model.vocab:
        t1_vector = model_t1.__getitem__(word)
        t2_vector = model.__getitem__(word)
        word_shift.append((1 - cosine(t2_vector, t1_vector), word))
    sorted_shifts = sorted(word_shift, reverse=True)

    drift_file = open('drifts', 'w')
    for sim, word in sorted_shifts:
        drift_file.write(word + '   ' + str(sim) + "\n")
    drift_file.close()


if __name__ == "__main__":
    main(sys.argv[1:])
