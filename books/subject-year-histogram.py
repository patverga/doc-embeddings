import gzip
from collections import defaultdict
import matplotlib.pyplot as plt

min_year = 180
max_year = 201
min_time_slices = 17
plot_max = 10000

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

fig = plt.figure()
ax = plt.subplot(111)
for subject, years in subject_years_counts.iteritems():
    non_zeros = [year for year in years if year != 0]
    if len(non_zeros) > min_time_slices:
        print (subject, years)
        ax.plot(range(min_year*10, max_year*10, 10),
                [min(plot_max, year) for year in years], marker='x', label=subject)

box = ax.get_position()
ax.set_position([box.x0, box.y0, box.width * 0.7, box.height])
ax.legend(loc='center left', bbox_to_anchor=(1, 0.5),prop={'size':10})
plt.show()






f.close()
