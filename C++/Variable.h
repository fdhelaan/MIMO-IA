#ifndef VARIABLE_H
#define VARIABLE_H

#include <iostream>
#include <sstream>
#include <string>


//using namespace std;

class Variable {
	public:
	Variable(char,int,int,int,int);
	Variable();
	Variable(const Variable &);
	std::string to_string() const;
	bool operator==(const Variable&) const;
	bool operator!=(const Variable&) const;
	bool operator==(const int) const;
	bool operator!=(const int) const;
	bool operator<(const Variable&) const;
	bool operator<=(const Variable&) const;
	bool operator>(const Variable&) const;
	bool operator>=(const Variable&) const;

	friend std::ostream& operator<< (std::ostream&, const Variable  &);

	bool zero;
	char letter;
	int j,k,p,q;
};

Variable::Variable() {
	zero = true;
}

Variable::Variable(const Variable &v) {
	zero = v.zero;
	letter = v.letter;
	j = v.j;
	k = v.k;
	p = v.p;
	q = v.q;
}
Variable::Variable(char l1, int j1, int k1, int p1, int q1) {
	zero = false;
	letter = l1;
	j = j1;
	k = k1;
	p = p1;
	q = q1;
}

std::string Variable::to_string() const{
	if(zero) {
		return std::string("0");
	}
	else {
		std::ostringstream ss;
		ss << letter << "^" << j << k << "_" << p << q;
		return ss.str();
	}
}

bool Variable::operator==(const Variable &v2) const {
	if(zero && v2.zero)
		return true;
	if(zero != v2.zero )
		return false;
	if(letter == v2.letter && j == v2.j
	&& k == v2.k && p == v2.p && q == v2.q )
		return true;
	else
		return false;
}
bool Variable::operator!=(const Variable &v2) const {
	return !( (*this) == v2);
}
bool Variable::operator==(const int x) const {
	if(zero && x==0)		//0 == 0
		return true;
	return false;
}
bool Variable::operator!=(const int x) const {
	return !( (*this) == x);
}
bool Variable::operator<(const Variable&v2) const{
	if(zero && v2.zero) return false;
	if(zero) return true;
	if(v2.zero) return false;

/*	if( letter < v2.letter && j < v2.j && k < v2.k && p < v2.p && q < v2.q )
		return true;*/
	if( letter > v2.letter)
		return false;
	if (letter < v2.letter)
		return true;

	if( j > v2.j )
		return false;
	if (j < v2.j)
		return true;

	if (k > v2.k )
		return false;
	if (k < v2.k )
		return true;

	if(p > v2.p)
		return false;
	if(p < v2.p)
		return true;

	if(q > v2.q)
		return false;
	if(q < v2.q)
		return true;

	return false;	// == holds
}

bool Variable::operator<=(const Variable&v2) const{
	return (*this < v2) || (*this == v2);
}

bool Variable::operator>(const Variable&v2) const{
	return !((*this) <= v2 );
}
bool Variable::operator>=(const Variable&v2) const{
	return !((*this) < v2 );
}


std::ostream& operator<< (std::ostream& s, const Variable &v) {
	s << v.to_string();
	return s;
}

#endif
