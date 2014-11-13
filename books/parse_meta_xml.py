from BeautifulSoup import BeautifulSoup
import gzip
import bz2
import os
import re

base_dir = '/mnt/nfs/collections/million'
output_file = '/home/pat/work3/books/meta-data2.gz'
out = gzip.open(output_file, 'w')

lang_regex = re.compile('en[g|glish]?', re.IGNORECASE)
date_regex = re.compile('^\d{4}$', re.IGNORECASE)

# iterating over all the books
for d in os.listdir(base_dir):
   if (len(d) == 2):
      sub_dir = base_dir +"/"+d
      for sd in os.listdir(sub_dir):
         sub_sub_dir = sub_dir +"/"+sd
         for ssd in os.listdir(sub_sub_dir):
            fname = sub_sub_dir +"/"+ssd+"/" + ssd+'_meta.xml.bz2'
            print(fname)

           #fname = '/mnt/nfs/collections/million/00/00/labormarketinforstillwater2000rich/labormarketinforstillwater2000rich_meta.xml.bz2'
            
            try:
               bz_file = bz2.BZ2File(fname)
               line_list = bz_file.readlines()
               xml = BeautifulSoup(str(line_list))

               subjects = [s.string for s in xml.findAll('subject')]
               date = xml.findAll('date')[0].string
               lang = xml.findAll('language')[0].string
               title = xml.findAll('title')[0].string
               author = [a.string for a in xml.findAll('author')]
               
               # only want books with atleast 1 subject, english, and a valid date
               if (len(subjects) > 0 and lang_regex.match(lang) != None and date_regex.match(date) != None):
                  #print('subjects, date, lang, title, author')
                  out.write(ssd +'\t'+ "|".join(subjects) +'\t'+ date +'\t'+ lang +'\t'+ title +'\t'+ "|".join(author) + "\n")

            except:
               print('could not parse book : ', fname)               
out.close()
