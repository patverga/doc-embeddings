from sklearn import linear_model

__author__ = 'pv'

from gensim.models import Word2Vec as Word2Vec
from gensim import matutils
import numpy as np


k_anchor_words = 100
t1_file = 'vectors/1850-300-fast'
t2_file = 'vectors/1910-300-fast'
# anchor_file = "/home/pv/doc-embeddings/src/main/resources/function_words"
anchor_file = 'anchors'

# read in the drift file to choose anchor words
with open(anchor_file) as drift_file:
    anchor_words = [next(drift_file) for x in xrange(k_anchor_words)]

print "loading models... "
model_t1 = Word2Vec.load(t1_file)
model_t2 = Word2Vec.load(t2_file)
print "done."

X = np.zeros((k_anchor_words, model_t1.layer1_size))
Z = np.zeros((k_anchor_words, model_t2.layer1_size))

for i, word in enumerate(anchor_words):
    clean_Word = word.strip()
    if model_t1.__contains__(clean_Word) and model_t2.__contains__(clean_Word):
        X[i] = model_t1.__getitem__(clean_Word)
        Z[i] = model_t2.__getitem__(clean_Word)


# train linear regression model
clf = linear_model.Ridge()
clf.fit(X, Z)

oil_t1 = model_t1.__getitem__('oil')
oil_t2 = model_t2.__getitem__('oil')

print(model_t2.most_similar_vector(oil_t2))
print(model_t2.most_similar_vector(oil_t1))
print(model_t2.most_similar_vector(clf.predict(oil_t1)))
