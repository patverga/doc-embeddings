__author__ = 'pv'

from google_ngram_downloader import readline_google_store
from itertools import product
from string import ascii_lowercase, digits
import codecs


letter_indices = ((''.join(i) for i in product(ascii_lowercase, ascii_lowercase + '_')))
letter_indices = (l for l in letter_indices if l != 'qk')

fs = []

try:
    for year in range(1850, 2010):
        fs.append(codecs.open('google-ngrams/' + str(year), 'w', "utf-8"))
except:
    print("couldnt open files.", year)

i = 0
for fname, url, records in readline_google_store(ngram_len=5, lang='eng-fiction', indices=letter_indices):
    print (fname)
    for record in records:
        if 1850 <= record.year <= 2010:
            out = fs[record.year - 1850]
            out_str = record.ngram + "\t" + str(record.match_count) + "\n"
            out.write(out_str)

for f in fs:
    f.close()