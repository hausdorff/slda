from __future__ import with_statement

"""
Processes the words taken from the MSQL stop words list found at:
http://www.ranks.nl/resources/stopwords.html

Assumes the words are put in a text file called `mysql_stop_words_raw.txt`, and that
you want the results dropped in a file called `TNG_STOP_WORDS_MSQL`.
"""


def proc_mysql_stop_words (rname, wname):
    with open(rname) as f:
        raw = f.read().strip().split()
    with open(wname, 'wb') as w:
        for word in raw:
            w.write("%s\n" % word)


if __name__ == '__main__':
    proc_mysql_stop_words('mysql_stop_words_raw.txt', 'TNG_STOP_WORDS_MSQL')
