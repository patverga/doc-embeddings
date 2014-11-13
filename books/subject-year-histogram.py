import gzip
from collections import defaultdict

min_year = 165
max_year = 201
subject_years_counts = defaultdict(lambda: [0] * (max_year-min_year))

f = gzip.open('meta-data2.gz')
for line in f:
    # 'book_id, subjects, date, lang, title, author'
    meta_data = line.split('\t')
    date = int(meta_data[2])/10
    if min_year <= date <= max_year:
        for subject in meta_data[1].split('|'):
            index = (date-min_year)
            subject_years_counts[subject][index] += 1

for subject, years in subject_years_counts.iteritems():
    non_zeros = [year for year in years if year != 0]
    if len(non_zeros) > 25:
        print (subject, years)






f.close()
