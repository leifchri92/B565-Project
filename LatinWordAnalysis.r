#########################################################
# Code written by leifchri
# 
# 
# 
# 
#########################################################

gen_viz = FALSE; # use for saving plots, plots are high rez so set to FALSE to save time
fix_dist = FALSE; # Livy and Cicero are very overrepresented in data set, 
				  # set to TRUE to throw out some of the Livy and Cicero observations and lessen the skew of the a priori distribution

Sys.setlocale('LC_ALL','C'); # to supress errors from text files generated in different countries/locales

# Latin function words
func_words = c('a','ab','ac','ad',
			'an','at','atque','aut',
			'autem','cum','cur','de',
			'donec','dum','enim','et',
			'etiam','iam','in','inter',
			'mox','nam','ne','nec',
			'neque','neu','nisi','non',
			'nunc','per','post',
			'quia','quidem','quodsi',
			'quoque','saepe','sed',
			'seu','si','sic','sine',
			'sive','sub','tam','tamen',
			'tandem','ubi','unde','ut',
			'vel','velut');
# regular expressions for our function words to ignore capitilazation
func_words_reg = c('[aA]','[aA]b','[aA]c','[aA]d',
			'[aA]n','[aA]t','[aA]tque','[aA]ut',
			'[aA]utem','[cC]um','[cC]ur','[dD]e',
			'[dD]onec','[dD]um','[eE]nim','[eE]t',
			'[eE]tiam','[iI]am','[iI]n','[iI]nter',
			'[mM]ox','[nN]am','[nN]e','[nN]ec',
			'[nN]eque','[nN]eu','[nN]isi','[nN]on',
			'[nN]unc','[pP]er','[pP]ost',
			'[qQ]uia','[qQ]uidem','[qQ]uodsi',
			'[qQ]uoque','[sS]aepe','[sS]ed',
			'[sS]eu','[sS]i','[sS]ic','[sS]ine',
			'[sS]ive','[sS]ub','[tT]am','[tT]amen',
			'[tT]andem','[uU]bi','[uU]nde','[uU]t',
			'[vV]el','[vV]elut');

######################################################
# 
# Generate data
# 
######################################################

# collect all files
# these files were generated with LatinToTxt.py
if (fix_dist) {
	path = './Works-data-fix_dist'
} else {
	path = './Works-data';	
}

files = list.files(path);
x = matrix(ncol=length(func_words)); #matrix(rep(0,length(files)*length(func_words)), nrow=length(files), ncol=length(func_words));
colnames(x) = func_words;
y = c(); #rep('',length(files));
works = c();

# count occurences of function words
q = 0 # counter for number of observations
for (i in 1:length(files)) {

	file = files[i];

	# extract the author from the file name
	author = '';
	for (c in strsplit(file,'')[[1]]) {
		if ( (c=='_' | c==':') & nchar(author)>1) {
			break;
		}
		author = paste(author,c,sep='');
	}

	filename = paste(path,'/',file,sep='');
	txt = readChar(filename, file.info(filename)$size);
	spaces = gregexpr('[[:blank:]\n]', txt)[[1]]
	num_words = length(spaces);

	print(paste(file, 'words', num_words));

	# count number of function words in each 1000 word segment, only segments with a full 1000 words are counted
	# note that some words at the ends of text will not be counted	
	ni = floor(num_words/1000); # the number of 1000 word segments to be measured
	# add the correct number of rows onto x
	mat_tmp = matrix(0, nrow=ni, ncol=length(func_words));
	colnames(mat_tmp) = func_words;
	if (nrow(x) == 1) {
		x = mat_tmp;
	} else {
		x = rbind(x, mat_tmp);
	}

	for (p in 1:ni) {
		for (j in 1:length(func_words_reg)) {
			# print( paste('From:', (p-1)*1000+1, 'to', p*1000) ) # print for debugging
			str = substr(txt, spaces[(p-1)*1000+1], spaces[p*1000]);
			search = gregexpr( paste('[[:blank:]\n\t]',func_words_reg[j],'[[:blank:]\n\t]',sep='') , str )[[1]];
			if (search[1] != -1) {
				x[q,j] = length(search); # * (1000/num_words);
			}
		}
		y = c(y, author);
		works = c(works,file)
		q = q+1;
	}
}
y[y=="_Justin"] = "Justin"; # fix typo in one of the authors names


######################################################
# 
# Generate visualizations using data
# 
######################################################
if (gen_viz) {
	func_words_new = func_words;
	x_new = x[,func_words_new];
	x_new = scale(x_new, center=FALSE, scale=TRUE);
	norm = function(a){(a-min(a))/(max(a)-min(a))};
	x_new = apply(x_new,2,norm);
	x_new[is.nan(x_new)]=0;

	# plot starplots for each work, grouped by author
	for (author in unique(y)) {
		path = './figures/';
		title = paste("Function words per 1000 -",author);
		png(paste(path,title,'.png',sep=''), width = 6, height = 6, units = 'in', res = 600);
		stars(x_new[y==author,], main=title, scale=FALSE);
		dev.off();

		path = './figures/Author_Subsets/'
		title = paste("Function words per 1000 -",author,"Subset");
		png(paste(path,title,'.png',sep=''), width = 6, height = 6, units = 'in', res = 600);
		sub = sample(1:nrow(x_new[y==author,]), 9);
		stars(x_new[y==author,][sub,], main=title, scale=FALSE);
		dev.off();
	}

	path = './figures/'
	# plot one starplot per author, averaged over all works of author
	authors = unique(y);

	star_x = matrix(rep(0,length(authors)*length(func_words_new)), nrow=length(authors), ncol=length(func_words_new));
	colnames(star_x) = func_words_new;
	for (i in 1:length(authors)) {
		author = authors[i];
		star_x[i,] = colSums(x_new[y==author,]) / length(which(y==author));
	}
	title = 'Function words per 1000 - Averaged by Author';
	png(paste(path,title,'.png',sep=''), width = 6, height = 6, units = 'in', res = 600);
	stars(star_x, labels=authors, main=title, scale=FALSE);
	dev.off();
}

stopp
######################################################
# 
# Reduce features using PCA
# 
######################################################
x = scale(x);
S = t(x) %*% x / nrow(x);
svd = svd(S);
U = svd$u;
x = x %*% U[,1:5];

######################################################
# 
# Naive Bayes classification
# 
######################################################

# seperate data into train and test sets 
######################################################
# use half data set for training and half for testing
set.seed(12); # set seed for reproducability

train = matrix();
c_train = c(); # classes for training set
test = matrix();
c_test = c(); # classes for testing set
# loop through data and choose half of each author for train and test
authors = unique(y);
for (author in authors) {
	n = nrow(x[y==author,]);
	sample = sample(1:n, n/2);
	sample_inv = (1:n)[!(1:n %in% sample)];
	if (nrow(train)==1) {
		train = x[y==author,][sample,];
		test = x[y==author,][sample_inv,];
	} else {
		train = rbind(train, x[y==author,][sample,]);
		test = rbind(test, x[y==author,][sample_inv,]);
	}
	c_train = c(c_train, rep(author, n/2));
	c_test = c(c_test, rep(author, ceiling(n/2))) # ceiling accounts for sample taking the floor of n/2
}
n_train = nrow(train);
n_test = nrow(test);

# train the classifier
######################################################
pC = rep(0,length(authors)); # a priori probablity of class
means = list();
covMs = list(); # covariance matrices
covMsinv = list(); # inverse covariance matrices
for (i in 1:length(pC)) {
	temp = train[c_train==authors[i],];
	n = nrow(temp);
	pC[i] = nrow(temp)/nrow(train);
	means[[i]] = colSums(temp)/n;

	X = scale(temp, center=TRUE, scale=FALSE);
	X = as.matrix(X);
	S = t(X) %*% X / n;

	covMs[[i]] = S;
	covMsinv[[i]] = solve(S);
}

# test on test data
######################################################
y_hat_test = rep(-1,nrow(test));
for (i in 1:nrow(test)) {
	obs = test[i,];
	max = -1000000;
	c = '';
	for (p in 1:length(authors)) {
		mahal = t(as.matrix(obs-means[[p]])) %*% covMsinv[[p]] %*% as.matrix(obs-means[[p]]);
		prob = log(pC[p]) - 0.5*log(det(covMs[[p]])) - 0.5*mahal;
		if (prob > max) {
			max = prob;
			c = authors[p];
		}
	}
	y_hat_test[i] = c;
}
err_test = length( which(y_hat_test != c_test) ) / length(c_test);
print(paste("Test error:",err_test));

for (author in authors) {

}

# test on train data
######################################################
y_hat_train = rep(-1,nrow(train));
for (i in 1:nrow(train)) {
	obs = train[i,];
	max = -1000000;
	c = '';
	for (p in 1:length(authors)) {
		mahal = t(as.matrix(obs-means[[p]])) %*% covMsinv[[p]] %*% as.matrix(obs-means[[p]]);
		prob = log(pC[p]) - 0.5*log(det(covMs[[p]])) - 0.5*mahal;
		if (prob > max) {
			max = prob;
			c = authors[p];
		}
	}
	y_hat_train[i] = c;
}
err_train = length( which(y_hat_train != c_train) ) / length(c_train);
print(paste("Train error:",err_train));

# extra useful code for studying error rates
# code for calculating error rate per class
a = c_test[y_hat_test != c_test];
b = y_hat_test[y_hat_test != c_test];
for (author in authors) {
	err = length(which(c_test[y_hat_test != c_test] == author)) / length(which(c_test == author));
	print(paste(author,err));
	c = rep(0, length(authors));
	for (i in 1:length(authors)) {
		c[i] = length(which(b[a==author]==authors[i]))
	}
	miss_auth = authors[which(c==max(c))]
	print(paste('Most often misclassified as', miss_auth, length(which(b[a==author]==miss_auth))/length(b[a==author]) ));
	print(paste('Total Errors', length(b[a==author]), 'Total Obs', length(which(c_test==author))));
}

