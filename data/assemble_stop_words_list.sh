# Takes raw lists of stop words, produces a list of unique stop words printed in
# alphabetical order.

cat TNG_STOP_WORDS_DEFAULT TNG_STOP_WORDS_LONG TNG_STOP_WORDS_MSQL | sort | uniq > TNG_STOP_WORDS 
