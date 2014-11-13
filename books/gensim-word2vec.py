import logging
import getopt
import sys
import gensim
from gensim.models.word2vec import LineSentence


logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)


def main(argv):
    input_file = ''
    output_file = ''
    size = 200
    window = 4
    negative_sample = 10
    down_sample = 1e-5
    min_count = 1
    try:
        opts, args = getopt.getopt(argv, "hi:o:s:", ["ifile=", "ofile="])
    except getopt.GetoptError:
        print 'test.py -i <input_file> -o <output_file>'
        sys.exit(2)
    for opt, arg in opts:
        if opt == '-h':
            print 'test.py -i <input_file> -o <output_file>'
            sys.exit()
        elif opt in ("-i", "--ifile"):
            input_file = arg
        elif opt in ("-o", "--ofile"):
            output_file = arg
        elif opt in ("-s", "--size"):
            size = int(arg)
    print 'Input file is ', input_file
    print 'Output file is ', output_file
    print 'Size is ', size

    sentences = LineSentence(input_file)
    model = gensim.models.Word2Vec(sentences, workers=8, size=size, window=window,
                                   negative=negative_sample, sample=down_sample, min_count=min_count)
    model.save(output_file)



if __name__ == "__main__":
    main(sys.argv[1:])