#ifndef USER_H
#define USER_H

class User{
	public:
	int M, N, d;
};

class InterferenceLink {
	public:
	User &t;
	User &r;
};

#endif
