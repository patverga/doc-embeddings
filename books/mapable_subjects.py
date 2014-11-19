__author__ = 'pv'

import gzip


def map_subjects():
    book_file = gzip.open('meta-data2.gz', 'r')

    # get the subjects we can map
    with open("classifications/all-subjects") as f:
        subjects = {line.strip() for line in f.readlines()}

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