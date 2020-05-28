#ifndef DET_H
#define DET_H

#include <set>
#include "Variable.h"
#include "Matrix.h"
#include "Term.h"
#include <vector>
#include<iostream>
#include <curses.h>
#include <thread>

template <class T>
void print_vector(const std::vector<T> &comb, std::ostringstream &out = std::cout) {
	out << "{";
	for(int i=0;i < comb.size();i++)
		out << comb[i] << ",";
	out << "}" << std::endl;
}

int perm_sign(const std::vector<int> &initial_perm, const std::vector<int> &perm) {

	int n = perm.size();
	std::vector<int> equiv_perm(n);			//Equivalent permutation, notated as [0,...,n-1]
	for(int i = 0; i < n;i++)
		for(int j = 0; j < n;j++)
			if(initial_perm[j] == perm[i] )
				equiv_perm[i] = j;

	std::vector<bool> seen(n);
	int swaps = 0;
	for(int i = 0; i < n;i++ )
		if( seen[i] )
			continue;
		else {
			seen[i] = true;
			for(int j = equiv_perm[i]; !seen[j];j = equiv_perm[j]){
				seen[j] = true;
				swaps++;
			}
		}
	return swaps;
}

int vector_find(const std::vector<int> &v, int x) {
	for(int i =0; i < v.size();i++)
		if(v[i] == x)
			return i;
	return -1;
}
bool vector_duplicate(const std::vector<int> &v) {
	std::vector<int> p2 = v;
	std::sort(p2.begin(), p2.end());

	for(int i =0; i < p2.size()-1;i++)
		for(int j = i+1; j < p2.size();j++)
			if(p2[i] == p2[j]){
				return true;
			}
	return false;

}
/*This should never become useful! By permutation construction*/
bool zero_diag(std::vector<int> &perm, const std::vector<std::vector<int> > &adj_list) {
	for(int i = 0; i < perm.size();i++)
		if( std::find(adj_list[i].begin(), adj_list[i].end(), perm[i]) == adj_list[i].end() ) //m[i][i] is zero
			return true;
	return false;
}

/*	Reject:
	1) column matched to multiple rows
	2) zero in the diagonal - not interesting permutation.
*/
bool valid_perm(std::vector<int> &perm, const std::vector<std::vector<int> > &adj_list) {
	return ! (vector_duplicate(perm) || zero_diag(perm, adj_list) );
}

/*Increments the passed digit to the next one in row_adj*/
/*Return false if digit is reset*/
bool inc(int &digit,const std::vector<int>  &row_adj ) {
	int index = vector_find(row_adj,digit);
	if(index == row_adj.size() -1 ) {		//Last digit.
		digit = row_adj[0];			//Reset digit to smallest value.
		return false;
	}
	else {						//Not last. Just increment.
		digit = row_adj[index + 1];
		return true;
	}
}

void init_perm(std::vector<int> &perm, const std::vector<std::vector<int> > &adj) {
	perm.clear();
	for(int r = 0; r < adj.size();r++)
		if ( adj.at(r).size() != 0 )
			perm.push_back( adj.at(r).at(0) );	//minimal element
}

/* Returns true if not overflow*/
bool next_perm(std::vector<int> &perm, const std::vector<std::vector<int> > &adj ) {
	int r=adj.size()-1;				//Last row
	while(r >= 0 && inc(perm[r], adj.at(r)) == false){		//Incrememnt one row (False = digit reset, inc next digit)
		r--;
	}
	if(r == -1)			//Perm completely reset - overflow
		return false;
	else
		return true;
	for(r = adj.size()-1;r >= 0;r++)		//Check that all digits are not in init position
		if(perm.at(r) != adj.at(r).at(0) )
			return true;			//Not overflow
	return false;					//Overflow - init state
}

/***********************************************************************************************/

class TwoInts {
	public:
	TwoInts() { pos=neg=0; }
	TwoInts(int x, int y) { pos = x; neg = y; }
	int pos, neg;
};

class Det {							//One obj per combination
	public:
	Det(const Instance &, const std::vector<int> &, WINDOW *, std::mutex &);
	void insert(const Term &, const int);
	int insert_bulk();
	void total_calculate();
	void one_zero_test();

	friend std::ostream& operator<< (std::ostream &, const Det & );

	unsigned long long total;
	unsigned long long done;						//Number of potential perms processed.
	bool one_zero;					//Full-rank -> (1,0) or (0,1)
	bool full_rank;

	const Instance &L;
	std::vector<int> comb;
	std::vector<int> initial_perm;
	std::vector<int> current_perm;
	std::map<Term,TwoInts> terms;				//Term and (pos, neg) occurance.
	std::vector<std::vector<int> > adj_list;
};


Det::Det(const Instance &L2, const std::vector<int> &c, WINDOW *thread_w, std::mutex &pmutex)
	: L(L2) {
	done=0;
	total = 0;
	comb = c;
	adj_list= L.m.adjacency(comb);
	full_rank = false;

	total_calculate();
	init_perm(current_perm, adj_list);
}

void Det::total_calculate() {
	total =1;

	for(int i =0; i < adj_list.size();i++) {
		total *= adj_list[i].size();
	}
}

int Det::insert_bulk() {
	int sign;
	unsigned long long done_now = 0;

	do{
		done_now++;
		done += 1;
		if(valid_perm(current_perm, adj_list)) {
			if(initial_perm.empty())
				initial_perm = current_perm;
			Term t(L, current_perm);
			if(perm_sign(initial_perm, current_perm) % 2)
				sign = -1;
			else
				sign = 1;
			insert(t, sign);
		}
	}while(next_perm(current_perm, adj_list) &&  done_now < (total/100) );
	return done;
}


void Det::insert(const Term &t, const int sign) {
	std::map<Term, TwoInts>::iterator it = terms.find(t);
	if(it == terms.end() ) {
		if(sign == 1)
			terms[t] = TwoInts(1,0);
		else if (sign == -1)
			terms[t] = TwoInts(0,1);
	}  else {
		if(sign == 1)
			it->second.pos = it->second.pos + 1;
		else
			it->second.neg = it->second.neg + 1;
	}
}

void Det::one_zero_test() {
	std::map<Term,TwoInts>::iterator it = terms.begin();
	full_rank = false;
	one_zero = false;
	for(;it != terms.end(); it++) {
		if(it->second.pos != it->second.neg)
			full_rank = true;
		if( ( it->second.pos == 1 && it->second.neg == 0) || ( it->second.pos == 0 && it->second.neg == 1 ) )
			one_zero = true;
	}
}

std::ostream & operator<<(std::ostream &s, std::vector<int> comb) {
	s << "{";
	for(std::vector<int>::const_iterator it = comb.begin(); it != comb.end(); it ++ ) {
		if(it != comb.begin() )
			s << ",";
		s << *it;
	}
	s << "}";
	return s;
}
std::ostream & operator<<(std::ostream& s, const Det &det ) {
	s << "Comb:" << det.comb;
	s << ( det.full_rank? "F" : "S") << ":" << (det.one_zero? "O" : "N");
	if( det.full_rank && !det.one_zero )
		s << "\t***";
	s << std::endl;					//Failed hypothesis test. Mark!
	for( std::map<Term,TwoInts>::const_iterator it = det.terms.begin(); it != det.terms.end(); it++) {
		s << "(+" << it->second.pos << ",-"<< it->second.neg << ")";
		s << (it->second.neg > it->second.pos ? "-":"") << it->first << std::endl;
	}

	return s;
}
#endif
