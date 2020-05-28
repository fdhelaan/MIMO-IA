#ifndef ZFBFVAR_H
#define ZFBFVAR_H

#include <iostream>
#include <sstream>


class ZFBFVar {
	public:
	ZFBFVar(char, int,int,int);
	std::string to_string() const;

	friend std::ostream& operator<< ( std::ostream&, const ZFBFVar);
	char letter;
	int j, r, c;
};

ZFBFVar::ZFBFVar(char l, int j1, int r1, int c1) {
	letter = l;
	j = j1;
	r = r1;
	c = c1;
}


std::string ZFBFVar::to_string() const{
	std::ostringstream ss;
	ss << letter << "^" << j << "_" << r << c;
	return ss.str();
}

std::ostream& operator<< (std::ostream& os, const ZFBFVar v) {
	os << v.to_string();
	return os;
}

#endif
