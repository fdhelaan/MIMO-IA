import gc
from math import factorial
from multiprocessing import cpu_count, Process, Queue
import itertools
from enum import Enum
from sage.all import *
#from pympler import asizeof
#from guppy import hpy
#h = hpy()	#Guppy

#from pympler.tracker import SummaryTracker
#tracker = SummaryTracker()

class Term_list(object):		#List of terms in a combination of columns
	def __init__(self, comb, tlist):
		self.tlist = tlist
		self.comb = comb

class Term(object):
	def __init__(self, product, pos, neg):
		self.product = product
		self.pos = pos			#Number of positive instances
		self.neg = neg			#Number of negative instances
		self.perms = []

	def __eq__(self, other):
		return self.product == other.product

class Term_message(object):
	def __init__(self, product, parity, perm):
#		self.comb = comb
		self.product = product
		self.parity = parity
		self.perm = perm

class Comb_prods(object):				#A Combination and an iterator for its permutations
	def __init__(self, comb, prods):
		self.comb = comb
		self.prods = prods

#Parity of a permutation
#    Given a permutation of the digits 0..N in order as a list,
#     returns its parity (or sign): +1 for even parity; -1 for odd
def perm_parity(l):
	parity = 1
	lst = list(l)
	ordered = list(l)
	ordered.sort()
	for i in range(0,len(lst)-1):
		if lst[i] != ordered[i]:
			parity *= -1
			mn = min(range(i,len(lst)), key=lst.__getitem__)
			lst[i],lst[mn] = lst[mn],lst[i]
	return parity

def perm_parity2(l):
	lst = list(l)
	parity = 1
	for i in range(len(lst)-1):
		if lst[i] != i:
			parity *= -1
			try:
				j = lst.index(i)
				lst[i], lst[j] = lst[j], lst[i]
			except:
				print "WHAAAT", lst, i
	return parity

def zero_diagonal(LIAM, perm):
	for i in range(LIAM.nrows()):
		if LIAM[i, perm[i]] == 0:
			return True
	return False


def dump_garbage():

	# force collection
	print "\nGARBAGE:"
	gc.collect()

	print "\nGARBAGE OBJECTS:"
	for x in gc.garbage:
		s = str(x)
		if len(s) > 80:
			s = s[:80]
		print type(x),"\n  ", s

def build_adj(m):

	adj_list = []		#List of adjacency lists for each column.
	for c in range(m.ncols()):
		adj = []
		for r in range(m.nrows()):
			if not (m[r,c] == 0):
				adj.append(r)

		if not adj == []:
			adj_list.append(adj)

	return adj_list

#Count products in an adjacency list
def prod_count(adj_list):
	if not adj_list:
		return 0
	product = 1
	print "adj_list received:", adj_list
	for x in adj_list:
		if x:
			product = product * len(x)
	return product

def has_repeat(t):
	seen = set()
	for i in t:
		if i in seen:
			return True
		seen.add(i)
	return False

def get_perm(comb, prod):
	perm = [None]*len(comb)
	for i in range(len(comb)):
		perm[ prod[i] ] = comb[i]
	return perm


#Check if each process in status_list is done
#True : all processes are done
def finished(status_list):
	nonzero= False
	for (p_id, a, b,c) in status_list:
		if b != 0:
			nonzero=True
		if(a != b):
			return False
	return nonzero	#Return True if one process is not init and finished.

#Print progress
def print_progress(status_list, combs_total, q, pq, sq, terms):
	combs_sum = 0

	for (p_id, a, b,combs_done) in status_list:
		if(b != 0):
			percent = (1.0 * a) / b
		else :
			percent = 0

		bar = ('=' * int( percent * 20 )).ljust(20)
		percent = int( percent * 100 )
		sys.stdout.write("%s [%s] %s%%\t(%s/%s)\t%s\n" % (p_id, bar, percent, a, b, combs_done) )
		combs_sum += combs_done

	percent = (1.0 * combs_sum) / combs_total
	bar = ('=' * int( percent * 20 )).ljust(20)
	percent = int( percent * 100)
	sys.stdout.write("%s [%s] %s%%\t(%s/%s) \n" % ("t", bar, percent, combs_sum, combs_total) )
	sys.stdout.flush()



terms = None
proc_count = cpu_count() - 1
#proc_count = 1
#Return a list of permutations as objects of class Term
def prods_parallel(LIAM):
	global terms
	q = Queue()			#Child -> Parent
	permq = Queue()			#Parent -> Child
	sq = Queue()			#Child -> Parent (status updates)
	jobs = []
	terms = []
	prods_list = []
	total_prods = 0	#Total number of products
	comb_prods_list = []
	status_list = []

	for c in range(proc_count):
		p = Process(group=None, target=get_terms, args=(LIAM,c,q,permq,sq))
		jobs.append(p)
		p.start()
		print "Spawning process", c
		status_list.append( (c,0,0,0) )

	adj_list = build_adj(LIAM)

	cols = range(0, LIAM.ncols() )
	combs = itertools.combinations( range(LIAM.ncols()) , LIAM.nrows())	#Combinations of columns equal in cardinality to the number of rows.
	comb_count = 0
	for comb in combs:
		prods = itertools.product(*[adj_list[x] for x in comb] )	#Generator for matchings.
		comb_prods_list.append( Comb_prods(comb, prods) )		#Append combination and it's matching generator.
		permq.put(comb)
		comb_count += 1

	print "combinations : ", comb_count
	combs = itertools.combinations( range(LIAM.ncols()), LIAM.nrows() )	#Combinations of columns equal in cardinality to the number of rows.


	#Wait for processes to finish round and gather data.
	nones = 0
	active_processes = min(comb_count, proc_count)				#If combs < processes.

	while not finished(status_list) or not permq.empty():
		if finished(status_list):
			print "finished status list"
		if permq.empty():
			print "permq empty"
		s_update = sq.get()						#Status queue (child -> Parent )
		p_id, prods_done, total_prods = s_update
		p_combs_done = (status_list[p_id]) [3]

		if prods_done == total_prods:		#This process just finished
			p_combs_done += 1
			t2 = q.get()
			if t2 != None:
				#print "Found message! ", len(t2.tlist), " terms received for combination:", t2.comb,  "\t qsize():", q.qsize()
				terms.append(t2)

		#Update progress bars here
		status_list[p_id] = (p_id, prods_done, total_prods,  p_combs_done)
		#Display progress
		#if p_id == 0:		# debugging only (monitoring process 0's memory)
		print_progress(status_list, comb_count,q,permq,sq,terms)



	print "Round done! Clean house!"

	#end while
	for p in jobs:
		p.terminate()
	file = open("results", 'w+')
	print_terms(file, terms)
	file.close()
	return terms

#Input list of terms
def term_in_tlist(terms, t2):
	found = False
	for t in terms:
		if t.product == t2.product:
			if t2.parity > 0:
				t.pos += 1
			else:
				t.neg += 1
			t.perms.append(t2.perm)
			found = True
			break

	if found == False:
		if t2.parity >0:
			t = Term(t2.product,1,0)
		else:
			t = Term(t2.product, 0, 1)
		t.perms.append(t2.perm)
		terms.append(t)
	return terms



################################Child process#####################################
#Wait for combinations. Generate permutations from each and find the terms therein.
#Input:
#Liam matrix : symbolic matrix
#Process number : int
#q : Queue to return (combination, term_list) in.
#permq: Queue to fetch next combination from.
#Status q
def get_terms(LIAM, process_number,q, permq, sq):

	while True:
		tlist = []
		comb = permq.get()
		M = LIAM.matrix_from_columns(comb)
		adj = build_adj(M)
		prods = itertools.product(*adj)
		nprods = 0
		i = 1

		for prod in prods:
			nprods = nprods +1
		prods = itertools.product(*adj)


		for prod in prods:
			if not has_repeat(prod):
				perm = get_perm(comb, prod)

				zeros = is_zeros(LIAM, perm)
				if zeros == True:
					continue


				product = 1
				for j in range( len(perm) ):
					product *= LIAM[j,perm[j]]
				parity = perm_parity(perm)

				t = Term_message(product, parity, perm)

				tlist = term_in_tlist(tlist,t)

			if (i % 1000000 == 1) or i == nprods:			#Status update
				sq.put( (process_number, i, nprods) )
			i = i + 1

		q.put(Term_list(comb, tlist) )

		if(permq.qsize() == 0):
			q.put(None )

def is_zeros(LIAM, perm):
	for j in range( LIAM.nrows() ):
		if(perm[j] == None):
			print "perm:", perm
			print "comb:", comb
			print "prod:", prod
		if LIAM[ j,perm[j] ] == 0:
			return True
	return False

def is_Txstrong(LIAM,perm):
	return False

def is_Rxstrong(LIAM,perm):
	return False

def is_strong(LIAM, perm):
	return is_Txstrong(LIAM,perm) or is_Rxstrong(LIAM,perm)

def print_terms(f,terms):
	for tlist in terms:
		for t in tlist.tlist:
			f.write( str(tlist.comb) +  " : (" + str(t.pos) + "," + str(t.neg) + "): " + str(t.product ) + "\n" )

#BUG: Tests if streamlined (m[i][i] only appears in previous row or previous columns )  w.r.t. the current ordering of rows.
def is_streamlined(LIAM, verbose=False):
	if LIAM.dimensions()[0] != LIAM.dimensions()[1]:
		if verbose:
			print "Not a square matrix"
		return False
	n = LIAM.dimensions()[0]
	for i in range(0, n) :
		x = LIAM[i][i]
		for r in range(i+1,n):
			for c in range(i+1,n):
				if LIAM[r][c] == x and r != c:
					if verbose:
						print "Not streamlined: LIAM[" + str(i) + "][" + str(i) +"] = " + str(x) + "\t also at ["+ str(r) + "][" + str(c) + "]"
					return False
	return True

def find_streamlined(terms, LIAM):
	streamlined = []
	for combination in terms:
		for t in combination.tlist:
			for p in t.perms:
				m = LIAM.matrix_from_columns(p)
				if is_streamlined(m) :
					streamlined.append(m)
				else:
					print str(p) + " not streamlined"
	return streamlined
