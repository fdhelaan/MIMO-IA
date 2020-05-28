#ifndef INTERFERENCE_H
#define INTERFERENCE_H

#include <iostream>
#include <vector>
#include "Matrix.h"
#include "Variable.h"
#include "ZFBFVar.h"

class InterferenceLink;
class User{
	public:
	int id, M, N, d;
	std::vector<InterferenceLink > out;
	std::vector<InterferenceLink > in;
};

class InterferenceLink {
	public:
	InterferenceLink(User &, User & );

	void generateVars();
	void generateAVars();
	void generateBVars();

	User &t;
	User &r;
	std::vector<Variable> AVars;
	std::vector<Variable> BVars;
	Matrix<Variable> AMatrix;
	Matrix<Variable> BMatrix;

};

class Instance {
	public:
	Instance() {
		K = 0;
	}
	Instance(int l, std::vector<User> user_list, std::vector<InterferenceLink> top): users(user_list), topology(top) {
		K = l;
	}

	void generateMatrix();
	void read_instance();
	void print_users();
	void print_topology();

	Matrix<Variable> m;
	int K;
	std::vector<User> users;
	std::vector<InterferenceLink> topology;
	std::vector<ZFBFVar> zfbf;
};

InterferenceLink::InterferenceLink(User &t1, User &r1) : t(t1), r(r1) {
	generateVars();
}
void InterferenceLink::generateVars() {
	generateAVars();
	generateBVars();
}

void InterferenceLink::generateAVars() {
	AMatrix.resize(r.d, (t.M - t.d) );
	for( int i =0; i < r.d ;i++ )
		for(int j =0; j < (t.M - t.d);j++){
			Variable v('a', t.id, r.id, i, j);
			AVars.push_back(v);
			AMatrix[i][j] = v;
		}
}
void InterferenceLink::generateBVars() {
	BMatrix.resize(t.d, (r.N - r.d) );
	for( int i =0; i < t.d ;i++ )
		for(int j =0; j < (r.N - r.d);j++){
			Variable v('b', t.id, r.id, i, j);
			BVars.push_back( v) ;
			BMatrix[i][j] = v;
		}
}

void Instance::generateMatrix() {
	int rows=0,cols=0, row_count;

	//Calculate # of rows
	for(int i =0; i < topology.size();i++) {
		rows += topology[i].t.d * topology[i].r.d;
	}
	//Calculate # of cols
	for(int i =0; i < users.size();i++) {
	//	if( !(users[i].out.empty()) )
			cols += (users[i].M - users[i].d) * users[i].d;
	//	if( !(users[i].in.empty()) )
			cols += (users[i].N - users[i].d) * users[i].d;
	}

	std::cout << "M dimensions : (" << rows << "x" << cols << ")" << std::endl;
	m.resize(rows, cols);
	print_topology();		//debug
	//Create ZFVars.
	for(int i = 0; i < users.size();i++)
		for(int q= 0; q < users[i].d;q++ )
			for(int n = 0; n < users[i].M - users[i].d;n++) {
				zfbf.push_back( ZFBFVar('v', users[i].id, n ,q) );

				//Populate elements of corresponding column. (enumerate over rows)
				row_count = 0;
				for(int ln = 0; ln < topology.size();ln++)
					for(int q1 = 0; q1 < topology[ln].t.d;q1++)
						for(int p1 = 0; p1 < topology[ln].r.d ;p1++) {
							if( i == topology[ln].t.id		//Same V matrix
								&& q1 == q )		//correct column in F^{jk}
								m[row_count][zfbf.size()-1] = topology[ln].AMatrix[p1][n];
							else
								m[row_count][zfbf.size()-1] = Variable();
							row_count++;
						}
			}
	//Create BFVars.
	for(int i = 0; i < users.size();i++)
		for(int y = 0; y < users[i].N - users[i].d;y++)
			for(int p= 0; p < users[i].d;p++){
				zfbf.push_back( ZFBFVar('u', users[i].id,y,p ) );

				//Populate elements of corresponding column. (enumerate over rows)
				row_count = 0;
				for(int ln = 0; ln < topology.size();ln++)
					for(int q1 = 0; q1 < topology[ln].t.d;q1++)
						for(int p1 = 0; p1 < topology[ln].r.d ;p1++) {
							if( i == topology[ln].r.id			//Same U matrix
								&& p1 == p )				//correct row in F^{jk}

								m[row_count][zfbf.size()-1] = topology[ln].BMatrix[q1][y];
							else
								m[row_count][zfbf.size()-1] = Variable();
							row_count++;
						}
			}
}

void Instance::read_instance() {
	std::cout << "K:";
	std::cin >> K;

	for(int i =0; i < K;i++) {
		User u;
		std::cout << "M:";
		std::cin >> u.M;
		std::cout << "N:";
		std::cin >> u.N;
		std::cout << "d:";
		std::cin >> u.d;
		u.id = i;
		users.push_back(u);
	}

	bool done = false;
	while(true) {
		int t, r;
		std::cout << "Interference from transmitter:";
		std::cin >> t;
		if(t == -1)
			break;
		std::cout << "To receiver:";
		std::cin >> r;
		if(r == -1)
			break;

		if(t == r){
			std::cout << "Can't inflict interference on self." << std::endl;
			continue;
		}
		InterferenceLink l(users[t], users[r]);;
		topology.push_back(l);

		InterferenceLink p =  topology.back();
		users[t].out.push_back(p );
		users[r].in.push_back(p );
		std::cout << "Adding: " << p.t.id << "-> " << p.r.id << std::endl;
	}
	generateMatrix();
}

void Instance::print_users() {
	std::cout << "Users: ";
	for(int i =0; i < users.size();i++){
		std::cout << "\t" << users[i].id << ":(" << users[i].M << "," << users[i].N << "," << users[i].d << ")";

		std::cout << std::endl << "Out neighbors:";
		for(int o =0 ; o < users[i].out.size();o++) {
			std::cout << users[i].out[o].r.id;
		}
		std::cout  << std::endl << "In neighbors:";
		for(int o =0 ; o < users[i].in.size();o++) {
			std::cout << users[i].in[o].t.id;
		}
		std::cout << std::endl;
	}
}
void Instance::print_topology() {
	std::cout << "Topology: ";
	for(int i=0; i < topology.size();i++){
		std::cout << "(" << topology[i].t.id << ", " << topology[i].r.id << ")" << std::endl;
		std::cout << "A:" << std::endl << topology[i].AMatrix << std::endl;
		std::cout << "B:" << std::endl << topology[i].BMatrix << std::endl;
	}
}

#endif
