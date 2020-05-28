#ifndef TERM_H
#define TERM_H

#include "Variable.h"
#include "Matrix.h"
#include <vector>
#include <map>

class Term {							//Product of factors. One obj. for each permutation
	public:
	Term(const Instance &, const std::vector<int> &);
	void insert(const Variable &);
	void remove(const Variable &);
	bool operator==(const Term &) const;
	bool operator!=(const Term &) const;
	bool operator<(const Term &) const;
	bool operator<=(const Term &) const;
	bool operator>(const Term &) const;
	bool operator>=(const Term &) const;

	friend std::ostream &operator<<(std::ostream &, const Term &);
	std::map<Variable,int> factors;				//Factor & power
};

Term::Term(const Instance &L, const std::vector<int> &perm ) {
	for(int i =0; i < L.m.rows;i++)
		insert(L.m[i][perm[i]]);
}

void Term::insert(const Variable &f) {
	std::map<Variable,int>::const_iterator it = factors.find(f);
	if(it == factors.end()){
//		std::cout << f << " Not found. inserting." << std::endl;
		factors[f] = 1;
	} else{
/*		std::cout << f << " found. increasing power." << std::endl;
		std::cout << f << " and " << it->first << " are identical" << std::endl;
		std::cout << f << "==" << it->first << ":" << ( f == (it->first)  )<< std::endl;
		std::cout << f << "!=" << it->first << ":" << ( f != (it->first) ) << std::endl;
		std::cout << f << "<" << it->first << ":" << (f < (it->first)) << std::endl;
		std::cout << it->first << "<" << f <<":" << (f < (it->first)) << std::endl;
		std::cout << f << "<=" << it->first << ":" << (f <= (it->first)) << std::endl;
		std::cout << f << ">" << it->first << ":" << (f > (it->first)) << std::endl;
		std::cout << f << ">=" << it->first << ":" << (f >= (it->first)) << std::endl;
*/		//it->second = it->second + 1;
		factors[f] = factors[f] + 1;
	}
}

void Term::remove(const Variable &f) {
	std::map<Variable,int>::iterator it = factors.find(f);
	if(it == factors.end()) {
		return ;
	} else
		(it->second)--;
	if(it->second == 0)
		factors.erase(it);
}

bool Term::operator==(const Term &guest) const {
	return factors == guest.factors;		//Don't compare signs or powers?
}

bool Term::operator!=(const Term &t) const {
	return !( *this == t );
}

bool Term::operator<(const Term &t) const {
	std::map<Variable,int>::const_iterator it = factors.begin();
	std::map<Variable,int>::const_iterator it2 = t.factors.begin();
	while(true) {
		if(it != factors.end() && it2 == t.factors.end() )
			return true;					//this < t
		else if(it == factors.end() )
			return false;					//len(this) < len(t) or equivalent and prev elements are equiv.
		else {							//it,it2 != end
			if(it->first < it2->first)
				return true;
			else if (it->first > it2->first)
				return false;
			else {
				++it;
				++it2;
			}
		}
	}
}

bool Term::operator<=(const Term &t) const {
	return ( *this < t || *this == t );
}
bool Term::operator>=(const Term &t) const {
	return ! ( *this < t );
}
bool Term::operator>(const Term &t) const {
	return ! ( *this <= t );
}

std::ostream &operator<<(std::ostream &s, const Term &t) {
	for(std::map<Variable,int>::const_iterator it = t.factors.begin(); it != t.factors.end();it++ ) {
		s << "(" << it->first << ")^" << it->second;
	}
	return s;
}
#endif
