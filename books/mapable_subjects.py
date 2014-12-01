__author__ = 'pv'

import gzip
import sys
from collections import defaultdict

reload(sys)
sys.setdefaultencoding("utf-8")

def map_subjects():
    book_file = gzip.open('meta-data2.gz', 'r')

    # get the subjects we can map
    with open("classifications/all-subjects") as f:
        subjects = {line.decode('utf-8').replace(u'\u2011', "-").strip() for line in f.readlines()}

    # see how many books are in the list
    books_mapped = 0
    subjects_mapped = set()
    for book in book_file:
        meta_data = book.split("\t")
        for book_subject in meta_data[1].split("|"):
            if book_subject in subjects:
                books_mapped += 1
                subjects_mapped.add(book_subject)
                break

    print books_mapped, len(subjects_mapped)
    book_file.close()

    return subjects_mapped


def map_subjects_IDS():
    subject_id_map = defaultdict()
    # get the subjects we can map
    with open("classifications/all-unfiltered") as f:
        for line in f.readlines():
            parts = line.decode('utf-8').replace(u'\u2011', "-").split("\t", 1)
            id = parts[0].strip()
            if len(parts) > 1 and len(id) > 0:
                subject_id_map[parts[1].strip()] = id

    return subject_id_map


def export(dictionary):
    with open("/home/pv/doc-embeddings/src/main/resources/subject-id-map", "w") as w:
        for key, val in dictionary.iteritems():
            if key:
                w.write(key+'|'+val+"\n")


export(map_subjects_IDS())