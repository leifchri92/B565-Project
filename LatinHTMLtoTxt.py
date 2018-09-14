# Extracts just the ASCI-II text from an html file downloaded from the Latin library
import re
import os

path = './Works-HTML/'
files = os.listdir(path)
for file in files:
	if (file.startswith('.')):
		continue

	print(file)

	f = open(path + file, 'r')
	txt = f.read()
	f.close()

	# get rid of whitespace
	txt = txt.replace('&nbsp;','')
	# get rid of links
	txt = re.sub('<a.*?/a>', ' ', txt)
	# get rid of HTML
	txt = re.sub('<.*?>', ' ', txt)
	txt = txt.replace('  ', ' ')
	txt = txt.replace('\t', '')
	# get rid of some font numbers left over
	txt = re.sub(r'0?1?2?3?4?5?6?7?8?9?','',txt)
	# remove empty lines
	txt = re.split('\s*\n', txt)

	filename = txt[1]
	# print(filename)
	filename = './Works-txt/' + filename.replace(' ','_')
	# print(filename)
	txt = '\n'.join(txt[2:])

	f = open(filename + '.txt', 'w')
	f.write(txt)
	f.close()