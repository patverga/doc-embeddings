from sklearn import linear_model

__author__ = 'pv'

from gensim.models import Word2Vec as Word2Vec
from gensim import matutils
import numpy as np


k_anchor_words = 50
t1_file = 'vectors/1850-300-fast'
t2_file = 'vectors/1910-300-fast'

# read in the drift file to choose anchor words
with open("/home/pv/doc-embeddings/src/main/resources/function_words") as drift_file:
    anchor_words = [next(drift_file) for x in xrange(k_anchor_words)]

print "loading models... "
model_t1 = Word2Vec.load(t1_file)
model_t2 = Word2Vec.load(t2_file)
print "done."

X = np.zeros((k_anchor_words, model_t1.layer1_size))
Z = np.zeros((k_anchor_words, model_t2.layer1_size))

for i, word in enumerate(anchor_words):
    clean_Word = word.strip()
    if model_t1.__contains__(clean_Word):
        X[i] = model_t1.__getitem__(clean_Word)
        Z[i] = model_t2.__getitem__(clean_Word)

# W = (XX^T)^T XZ^T
# a = np.transpose(np.transpose(X).dot(X))
# b = np.transpose(Z).dot(X)
# W = matutils.unitvec(a.dot(b))

clf = linear_model.LinearRegression()
clf.fit(X, Z)

oil_t1 = model_t1.__getitem__('orange')
oil_t2 = model_t2.__getitem__('orange')

print(model_t2.most_similar_to_vector(oil_t1))
print(model_t2.most_similar_to_vector(oil_t2))
print(model_t2.most_similar_to_vector(clf.predict(oil_t1)))
