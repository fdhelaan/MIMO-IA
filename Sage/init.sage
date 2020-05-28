import itertools
from enum import Enum
#from sage import all

class Ordering(Enum):
	jkpq = 1
	kjqp = 2
	jpkq = 3
	kqjp = 4

################################################################################
#A communication link.
class User(object):
	def __init__(self,ind=0,M=0,N=0,d=0):
		self.M = M
		self.N = N
		self.d = d
		self.index = ind
		self.outNeighbors = []
		self.inNeighbors = []
		self.BFvars = []

		self.ZFvars = []
		for i in range(0, d ):
			for j in range(0, M-d):
				v = ZFBFvar(self, True, var("v_"+str(self.index)+"_"+str(i)+str(j) ) )
				self.ZFvars.append( v )

			for j in range(0, N-d):
				v = ZFBFvar(self, False, var("u_"+str(self.index)+"_"+str(i)+str(j)) )
				self.BFvars.append( v )

		print "Added user:" +str(self.index)

	def __str__(self):
		return "{0}:({1} * {2}, {3})".format(self.index, self.M, self.N, self.d)

	def __repr__(self):
		return str(self)

	def __eq__(self, other):
		if isinstance(other, User):
			return self.index == other.index
		else:
			return False
	def __neq__(self, other):
		return not self.__eq__(other)

	def affect(self,e):
		for i in self.outNeighbors:												#Duplicate interference not allowed.
			if i.Rx == e.Rx:
				print "Error: Receiver " + str(e.Rx) + " already in outgoing of " + str(self)
				return
		self.outNeighbors.append(e)
		return

	def get_affect(self, e):
		for i in self.inNeighbors:												#Duplicate interference arc not allowed.
			if i.Tx == e.Tx:
				print "Error: Receiver " + str(e.Tx) + " already in outgoing of " + str(self)
				return
		self.inNeighbors.append(e)

		return

	def is_outNeighbor(self, Rx):
		for i in self.outNeighbors:
			if i.Rx == Rx:
				return True
		return False

	def is_inNeighbor(self, Tx):
		for i in self.inNeighbors:
			if i.Tx == Tx:
				return True

		return False
	#Returns true if instance is reducible at Tx:
	def is_Tx_reducible(self):
		if not self.outNeighbors:
			if self.ZFvars:														#revise?
				return True
			return False

		sum = self.d
		for e in self.outNeighbors:
			sum += e.Rx.d
		return self.M >= sum

	#Returns true if instance is reducible at Rx:
	def is_Rx_reducible(self):
		if not self.inNeighbors:
			if self.BFvars:														#revise?
				return True
			return False

		sum = self.d
		for e in self.inNeighbors:
			sum += e.Tx.d
		return self.N >= sum

################################################################################
#An interference edge.
class Interference(object):
	def __init__(self, Tx, Rx):
		self.Tx = Tx
		self.Rx = Rx

		Tx.affect(self)
		Rx.get_affect(self)
		self.alist = []
		self.blist = []
		self.equations = []

		self.avars, self.bvars, self.alist, self.blist = self.generate_vars()
		self.A = []
		self.B = []
		self.AB = (self.A, self.B)

	def __str__(self):
		return "( {0} -> {1} )".format(str(self.Tx) , str(self.Rx))

	def __repr__(self):
		return str(self)

	def __eq__(self, other):
		if isinstance(self, Interference):
			return self.Tx == other.Tx and self.Rx == other.Rx
		else:
			return False
	def __neq__(self, other):
		return not self.__eq__(other)

	def generate_vars(self):
		a = []																	#a vars
		b = []																	#b vars

		for c in range(0, self.Rx.d ):
			for r in range(0, self.Tx.M - self.Tx.d ):
				a.append( var("a_" + str(self.Tx.index)+ str(self.Rx.index) + "_" + str(c) + str(r) ) )

		for c in range(0, self.Tx.d ):
			for r in range(0, self.Rx.N - self.Rx.d ):
				b.append( var("b_" + str(self.Tx.index)+ str(self.Rx.index) + "_" + str(c) + str(r) ) )

		aspace = MatrixSpace(SR, self.Rx.d, self.Tx.M - self.Tx.d, a)
		bspace = MatrixSpace(SR, self.Tx.d, self.Rx.N - self.Rx.d, b)

		avars = aspace(a)
		bvars = bspace(b)


		print "A var matrix:"
		print str(avars)
		bvars = bspace(b)
		print "B var matrix:"
		print str(bvars)
		return avars, bvars, a,b

	def generate_AB(self,variables):
		spaceA  = MatrixSpace(SR, self.Tx.d * self.Rx.d , self.Tx.d * (self.Tx.M - self.Tx.d) , variables)
		spaceB  = MatrixSpace(SR, self.Tx.d * self.Rx.d , self.Rx.d * (self.Rx.N - self.Rx.d) , variables)
		a = []
		b = []
		Arows = []
		Brows = []

		for r in range(0, self.Tx.d * self.Rx.d):
			Arow = []
			rblock = r / self.Tx.d
			for c in range(0, self.Tx.d * (self.Tx.M - self.Tx.d) ):
				if  ( c / (self.Tx.M - self.Tx.d) ) ==  (r / self.Rx.d  ) : 	#Non-zero Ablock
					Arow.append( self.avars[r % (self.Rx.d), c % (self.Tx.M - self.Tx.d )] )
				else:
					Arow.append(0)
			Arows.append(Arow)

		#print "A was:" + str(self.A)
		#print "Arows:" + str(Arows)
		self.A = spaceA(Arows)
		print "A becomes:\n" + str(self.A)
		for r in range(0, self.Tx.d * self.Rx.d):
			Brow = []
			for c in range(0, self.Rx.d * (self.Rx.N - self.Rx.d) ):
				if (r % self.Rx.d == c % self.Rx.d) :
					Brow.append(self.bvars[r / self.Rx.d, c / self.Rx.d])
				else:
					Brow.append(0)

			#offset = (offset + 1) % self.Rx.d
			Brows.append(Brow)




		self.B = spaceB(Brows)
		print "B becomes:\n" + str(self.B)

		return self.A,self.B

	def generate_Eq(self, p,q, row_index):
		eq = Eq(self, p,q, row_index)
		self.equations.append(eq)
		return eq

	#Generate LIAM row for (j,k,p,q)
	def generate_row(self, ZFBFvars, p, q):
		row = []
		i1 = i2 = 0

		for x in ZFBFvars:
			if x.user == self.Tx and x.ZF:
				row.append(self.A[q + (p * self.Rx.d), i1])
				i1 = i1 + 1
			elif x.user == self.Rx and not x.ZF:
				row.append(self.B[q + (p * self.Rx.d), i2])
				i2 = i2 + 1
			else:
				row.append(0)
		return row


	#Deprecated:
	#Create rows for LIAM and link them with EQ objects in ordering (p,q)
	#ZFBFvars : ZF/BF variables
	#Base: row index to start creating from.
	#pq : Ordering pq. Default is True. False means qp.
	def generate_rows_pq(self,ZFBFvars, base, pq = True):
		edge_rows = []
		index = base

		for p in range(0, self.Tx.d):
			for q in range(0, self.Rx.d):
				row = self.generate_row(ZFBFvar, p,q)
				self.generate_Eq(p,q,index)
				index = index + 1
				edge_rows.append(row)

		return edge_rows

################################################################################
#A ZF or BF variable
class ZFBFvar(object):															#BF/ZF variable and LIAM column
	def __init__(self, user, ZF, v):
		self.user = user
		self.ZF = ZF
		self.v = var(v)
		self.col = -1
	def __str__(self):
		if(self.ZF):
			return str(self.col) +":ZF:" + str(self.v)
		else:
			return str(self.col) +":BF:" + str(self.v)
	def __repr__(self):
		return self.__str__()

	def __eq__(self,other):
		if isinstance(other, ZFBFvar):
			return self.user == other.user and self.ZF == other.ZF and self.v == other.v
		else:
			return False

	def __neq__(self,other):
		return not self.__eq__(other)
################################################################################
#A LIA equation. (f^{j,k}_{rc} )
class Eq(object):
	def __init__(self, edge, p, q, row):
		self.edge = edge
		self.p = p
		self.q = q
		self.row = row															#Row index in LIAM
	def __str__(self):
		return ( "f^(" + str(self.edge.Tx.index) + ", " +
		str(self.edge.Rx.index) + ")_" + str(self.q) + "," +
		str(self.p) + "LIAM row : " + str(self.row) )
	def __repr__(self):
		return str(self)
	def __eq__(self, other):
		if isinstance(other, Eq):
			return self.p == other.p and self.q == other.q and self.edge == other.edge
		else:
			return False

	def __neq__(self, other):
		return not self.__eq__(other)

################################################################################

#An instance.
class LIA(object):
	def __init__(self,users=[],edges=[]):
		self.m = None
		self.users = users
		self.edges = edges
		self.variables = []							#Random Variables (a-vars and b-vars)
		self.ZFBFvars = []							#ZF and BF vars (v^j_{rp} and u^k_{cq})
		self.ZFvars = []
		self.BFvars = []

		if not self.users:
			self.users = self.read_users()
		for u in self.users:
			self.BFvars = self.BFvars + u.BFvars
			self.ZFvars = self.ZFvars + u.ZFvars

		self.ZFBFvars = self.ZFvars + self.BFvars
		if not edges:
			self.edges = self.read_edges()
		for e in self.edges:
			self.variables + e.alist + e.blist
		for e in self.edges:						#Need ALL variables prepared by previous loop.
			e.generate_AB(self.variables)

	#Get users from keyboard
	def read_users(self):
		users = []
		K = input("K:")
		for i in range(0,K):
			print "For user number "+str(i)
			M = input("M:")
			N = input("N:")
			d = input("d:")
			x = User(i,M,N,d)
			users.append(x)
		return users

	#Get users from keyboard
	def read_edges(self):
		e = (1,1)
		edges = []
		while e != (0,0):
			e = input("edge:")
			Txfound = False
			Rxfound = False

			if e[0] != e[1]: #Not 1-loop
				for tempTx in self.users:			#Find Tx
					if tempTx.index == e[0]:	#Found.
						Txfound = True
						break
				for tempRx in self.users:			#Find Rx
					if tempRx.index == e[1]:	#Found
						Rxfound = True
						break
				if not Txfound:
					print "Tx not found"
					continue
				if not Rxfound:
					print "Rx not found"
					continue

				edge = Interference(tempTx,tempRx)
				edges.append(edge)
		return edges

	def is_local_Txstrong(self,perm, e, base):									# Assumed perm is a proper matching!
		print "Examining", self.m.matrix_from_rows_and_columns(perm,perm)
		for c in range(e.Tx.d):
			tx = False
			rx = False
			print "Column:", c
			for r in range(e.Rx.d):
				x = base + c * e.Tx.d + r
				print "c:", c, "\t c * e.Tx.d :", c* e.Tx.d, "\t r:", r
				print "Checking", self.m[x,x]
				if str( self.m[x,x] ) [0] == 'a':
					tx = True
				if str( self.m[x,x]) [0] == 'b':
					rx = True
			if tx and rx:		# This column is not uniformally matched
				print "Not local tx-strong"
				return False
		return True

	def is_local_Rxstrong(self, perm, e, base):
		for c in range(e.Rx.d):
			tx = False
			rx = False
			for r in range(e.Tx.d):
				x = base+c+r * e.Rx.d
				print "c:", c, "\t r:",r,"\t r * e.Rx.d:", r * e.Rx.d
				print "Checking", self.m[x, x]

				if str ( self.m[x, x])[0] == 'a':
					tx = True
				if str ( self.m[x, x])[0] == 'b':
					rx = True
			if tx and rx:
				return False
		return True

	def is_local_strong(self, perm):
		window_base = 0
		for e in self.edges:
			print "Checking edge", e
			dim = e.Tx.d * e.Rx.d
			if self.is_local_Txstrong(perm[window_base:dim],e, window_base) == False and self.is_local_Rxstrong(perm[window_base:dim],e, window_base) == False :
				return False

			window_base += dim
		return True

	def is_global_strong(self, perm):
		tx = True
		rx = True
		for e in self.edges:
			if self.is_local_Txstrong(perm, e) == False:
				tx = False
			if self.is_local_Rxstrong(perm, e) == False:
				rx = False
		return tx or rx

	#Returns a user if the instance is reducible, None otherwise
	def reducible(self):
		for u in self.users:
			if u.is_Tx_reducible() or u.is_Rx_reducible():
				return u
		return None

	#Generate LIAM and link ZFBFvars and Eqs to columns and rows.
	def generate_LIAM(self,ordering = Ordering.jkpq):
		rowc = 0
		colc = 0


		for i in self.edges:
			rowc = rowc + i.Tx.d * i.Rx.d
		for i in self.users:
			colc = colc + len(i.ZFvars)+  len(i.BFvars)

		print "Creating LIAM of dimensions: " + str(rowc) + " x " + str(colc)

		cols = []
		rows = []
		i=0
		for c in self.ZFBFvars:
			c.col = i
			i = i + 1

		#Setup rows:
		index = 0
		if ordering == Ordering.jkpq :
			for u in self.users:												#j
				for e in u.outNeighbors:										#k
					for p in range(0,e.Tx.d):									#p
						for q in range(0,e.Rx.d):								#q
							row = e.generate_row(self.ZFBFvars,p,q)
							e.generate_Eq(p,q,index)
							index = index + 1
							rows = rows + row

		if ordering == Ordering.jpkq :
			for u in self.users:												#j
				for p in range(0, u.d):											#p
					for e in u.outNeighbors:									#k
						for q in range(0,e.Rx.d):								#q
							row = e.generate_row(self.ZFBFvars,p,q)
							e.generate_Eq(p,q,index)
							index = index + 1
							rows = rows + row

		#print "rows:" + str(rows)
		self.m = Matrix(rowc, colc, rows)
		#print "Bye"

		return self.m

	#rearrange columns of matrix and update their handles at ZFBFvars
	def match(self, col_order):
		cols = len(col_order)
		if cols != self.m.dimensions()[0]:
			print "Cannot match. Expected " + str(self.m.dimensions()[1]) + " items and got " + str(len(col_order))
			return

		temp_vars = self.ZFBFvars
		self.ZFBFvars = []

		for c in range(0, self.m.dimensions()[1]):		#Append unmatched columns at the end
			if c not in col_order:
				col_order.append(c)

		for r in col_order:
			self.ZFBFvars.append( temp_vars[ r ] )

		self.m = self.m.matrix_from_columns(col_order)

	#Reduce untill irreducible.
	def reduce(self):
		user = self.reducible()
		if user == None:
			return False

		cols = range(0,self.m.dimensions()[1])
		rows = range(0,self.m.dimensions()[0])		#LIAM rows. Remove ones to be reduced.

		while user != None:
				print "Reducing at " + str(user)
				if user.is_Tx_reducible():
					print "Tx reducible."

					for v in user.ZFvars[:]:			#Remove columns
						print "Removing var" + str(v)
						cols.remove(v.col)
						user.ZFvars.remove(v)

					print "Removing rows:"
					for e in user.outNeighbors[:]:		#Remove rows
						for f in e.equations[:]:
							print "removing row :" + str(f.row)
							rows.remove(f.row)
							print "removing equation :" + str(f)
							e.equations.remove(f)
						print "removing edge : " + str(e)
						e.Rx.inNeighbors.remove(e)
						user.outNeighbors.remove(e)
				else:								#Rx reducible.
					print "Rx reducible."
					for u in user.BFvars[:]:			#Remove columns
						print "Removing var" + str(u)
						cols.remove(u.col)
						user.BFvars.remove(u)

					for e in user.inNeighbors[:]:		#Remove rows
						for f in e.equations[:]:
							rows.remove(f.row)
							e.equations.remove(f)
						e.Tx.outNeighbors.remove(e)
						user.inNeighbors.remove(e)

				user = self.reducible()

		self.m = self.m.matrix_from_rows_and_columns(rows, cols)
		return True

	def zero_column(self, c):
		for r in range(0, self.m.dimensions()[0]):
			if self.m[r][c] != 0:
				return False
		return True

	#Reduce only zero columns.
	def reduce_zeroes(self):
		zcols = []

		for c in range(0, self.m.dimensions()[1]):								#Build list of zero columns.
			if self.zero_column(c):
				zcols.append(c)

		for v in self.ZFBFvars[:]:												#Remove their ZFBFvars
			if v.col in zcols:
				self.ZFBFvars.remove(v)

		for v in self.ZFBFvars:													#Shift succeding columns to the left.
			shift = 0
			for c in zcols:
				if v.col > c:
					shift = shift + 1
			v.col = v.col - shift

		cols = []
		for c in range(0, self.m.dimensions()[1]):								#Build list of remaining columns.
			if c not in zcols:
				cols.append(c)

		self.m = self.m.matrix_from_columns(cols)


	#Calculates upper bound for alpha.
	def calculate_alpha(self):
		nominator = 0
		denominator = 0
		for u in self.users:
			if u.outNeighbors:
				nominator += u.M * u.d
				denominator += u.d * u.d
			if u.inNeighbors:
				nominator += u.N * u.d
				denominator += u.d * u.d

		for e in self.edges:
			denominator += 2 * e.Tx.d * e.Rx.d

		return nominator/denominator

	#Divide demand of all users by alpha
	def divide_demand(self, alpha):
		users2 = []
		edges2 = []

		for u in self.users:
			u2 = User(u.index, u.M, u.N, int(u.d * alpha))						#casting to integer because sage might treat u.d * alpha as rational
			users2.append(u2)

		for e in self.edges:
			for u in users2:
				if e.Tx.index == u.index:
					Tx = u
				if e.Rx.index == u.index:
					Rx = u

			e2 = Interference(Tx,Rx)
			edges2.append(e2)

		li = LIA(users2, edges2)
		li.generate_LIAM()
		return li

	def half_demand(self, ceiling=False):
		users2 = []
		edges2 = []

		for u in self.users:
			if ceiling:
				u2 = User(u.index, u.M, u.N, int(ceil(u.d/2)) )					#casting to integer because sage treats u.d/2 as a rational number.
			else:
				u2 = User(u.index, u.M, u.N, int(u.d/2))						#casting to integer because sage treats u.d/2 as a rational number.
			users2.append(u2)

		for e in self.edges:
			for u in users2:
				if e.Tx.index == u.index:
					Tx = u
				if e.Rx.index == u.index:
					Rx = u

			e2 = Interference(Tx,Rx)
			edges2.append(e2)

		li = LIA(users2, edges2)
		li.generate_LIAM()
		return li
################################################################################
################################################################################
################################################################################

def get_matching(LIA):
	m = []
	n = -1
	while(n == -1):
		m = []
		i = 0
		while( i < LIA.m.nrows() ):
			n = input("Match row "+ str(i)+ ":")
			if n == -1:
				print "Starting over!"
				break
			if n in m:
				print "Already matched! Try again"
				continue
			if n >= LIA.m.ncols():
				print "Illegal column number!"
				continue
			if LIA.m[i, n] == 0:
				print "Illegal matching. Not adjacent!"
				continue
			i = i + 1
			print "Matched with " + str(LIA.ZFBFvars[n]) + " !"
			m.append(n)
	return m

def print_users(users):
	for i in users:
		print i
def print_edges(edges):
	for i in edges:
		print i
def print_vars(variables):
	for i in variables:
		print i

#Don't recall wtf this is:
def alg(LIAM, edges, users):
	base = 0
	for e in edges:
		rows = range(base, base + e.Tx.d * e.Rx.d)
		cols = range(base, base + e.Tx.d * e.Rx.d)

		target_cols = []

		for i in range(base + e.Tx.d * e.Rx.d, LIAM.ncols() ):
			for r in rows:
				if LIAM[r, i] != 0 and (i not in target_cols):
					target_cols.append(i)

		b = LIAM.matrix_from_rows_and_columns(rows, target_cols)
		print A
		print "To be zeroed:"
		print b

		base = base + e.Tx.d * e.Rx.d
		X = A.solve_right(b)
		print "Solution is the vector"
		view(X)

def match_rows(l):
		g = l.as_bipartite_graph()
		matching = g.matching(use_edge_labels=true)
		matching_of_rows = [None] * l.dimensions()[0]

		for e in matching:
			if(e[0] <= l.dimensions()[0]):											#e[0] is a row
				r = e[0] - 1
				c = e[1] - l.dimensions()[0] - 1
			else:
				r = e[1] - 1
				c = e[0] - l.dimensions()[0] - 1

			print "Matching row [" + str(r)+ "] to column [" + str(c)+"]"
			matching_of_rows[ r ] = c

		return matching_of_rows

def initLIAM() :
	instance = LIA()


	ordering = 'red'
	while not hasattr(Ordering, ordering):
		ordering = raw_input("Choose ordering (jkpq,jpkq):")

	instance.generate_LIAM(Ordering[ordering])
	print "LIAM:"
	print str(instance.m)

	choice = 0

	while choice != 1 and choice != 2 and choice != 3:
		print "1)automatic matching"
		print "2)manual matching"
		print "3)No matching"
		choice = input("Choice:")
	if choice == 3:
		return instance

	if choice == 2:
		M = get_matching(instance)
		print M
		instance.match(M)
		print instance.m

	if choice == 1:
		matching_of_rows = match_rows(instance.m)
		print "Rows matched. List is at matching_of_rows"

		instance.match(matching_of_rows)
		print instance.m

	return instance
