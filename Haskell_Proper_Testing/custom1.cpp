#include <vector>
#include <set>
#include <iostream>
#include <cstdlib>
#include <iterator>
#include <algorithm>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fstream>
#include <string>
#include <sstream>

using std::ofstream;
using std::set;
using std::vector;
using std::cout;
using std::endl;
using std::ostream;
using std::pair;
using std::atoi;
using std::cin;

#define MAX 5
class Equation;

typedef pair<int,int> Pair;
typedef set<Equation> eqSet;
typedef set<eqSet> eqPowersetType;
typedef set<Pair> pairset;
typedef set< set<Pair> > pairpowerset;

int min(int x, int y) { return (x > y)? y: x;}

class Binary {
	public:
	Binary( int x) { n = x; for(int i =0; i < n;i++) bits.push_back(false);}
	void inc()
	{
		int i=n-1, j=n-1;
		if( last() ) return;
		for(;i >0 && bits[i];i--);
		bits[i] = true;
		for(;j > i;j--) bits[j] = false;
	}

	bool last()
	{
		for(int i = 0; i < n; i++)
			if(bits[i] == false)
				return false;
		return true;
	}
	bool operator[](int i){
		return bits[i];
	}

	int n;
	vector<bool> bits;
};

class Variable {
	public:
	Variable() {user = r = c = -1; sender = false;}
	Variable(int u, bool s=false, int i=-1, int j=-1) {user = u; sender = s;r = i; c=j;}
	bool operator==(Variable const &guest) const { return (user == guest.user && sender == guest.sender && r == guest.r && c == guest.c);}
	bool operator!=(Variable const &guest) const { return ! ((*this) == guest); }
	bool operator<(Variable const &guest) const 
		{

			if (sender == true && guest.sender == false)
				return false;
			else if(sender == false && guest.sender == true)
				return true;
			else{
				if( user < guest.user)
					return true;
				else if (user > guest.user)
					return false;
				else{
					if(r < guest.r)
						return true;
					else if (r > guest.r)
						return false;
					else{
						if ( c < guest.c)
							return true;
						else
							return false;
					}
				}
			}
		}
	bool operator>(Variable const &guest) const { return !((*this) < guest )   && !((*this) == guest) ; }
	bool operator<=(Variable const &guest) const { return (*this) < guest || (*this) == guest ;}
	bool operator>=(Variable const &guest) const { return (*this) > guest || (*this) == guest ;}

	int user;
	bool sender;
	int r;
	int c;
};

class User {
	public:
	User() { id = 0; n = 1; m =1; d= 0;n_max = MAX, m_max = MAX;}
	User(const User &u)
		{ id = u.id;n = u.n; m = u.m; d = u.d;n_max = u.n_max; m_max = u.m_max;}
	User(int i, int x, int y, int z, int a =MAX, int b = MAX)
		{ id = i; n = x; m = y;d=z; n_max = a; m_max = b;}
	bool last() { return ( n == n_max && m == m_max && d == ( min(n,m) -1) ); }
	User next();

	int id;
	int n;
	int m;
	int d;

	set<Variable> txVars;
	set<Variable> rcVars;
	int n_max;
	int m_max;
};

class Equation {
	public:
	Equation(int l=-1, int k=-1, int i=-1, int j=-1) {tx=l;rv=k;r=i;c=j;}
	bool operator==(Equation const &guest) const { return (tx == guest.tx && rv == guest.rv && r == guest.r && c == guest.c);}
	bool operator<(Equation const &guest) const { return (tx < guest.tx || rv < guest.rv || r < guest.r || c < guest.c);}
	bool operator>(Equation const &guest) const { return !((*this) < guest )   && !((*this) == guest) ; }
	bool operator<=(Equation const &guest) const { return (*this) < guest || (*this) == guest ;}
	bool operator>=(Equation guest) { return (*this) > guest || (*this) == guest ;}

	int tx;
	int rv;
	int r;
	int c;
	set<Variable> vars;
};


class Instance {
	public:
	Instance( int );
	Instance(const Instance &);
	Instance nextInstance();
	void setTopology(const set<Pair> &);
	bool last();

	vector<User> users;
	set<Pair> E;
	set<Variable> variables;
	set<Equation> equations;
	int K;
};

/* IO FUNCTIONS*/
ostream &operator<<(ostream &out, Binary b)
{
	out << b.n << "[";
	for(int i = 0; i < b.n;i++) out << (b[i]? "1":"0");
	out << "]";
	return out;
}

ostream &operator<<(ostream &out, Variable v)
{
	out << (v.sender?"V[": "U[") << v.user << "](" << v.r << ", " << v.c << ")";
	return out;
}

ostream &operator<<(ostream &out, Equation eq)
{
	out << "E[" << eq.tx << ", " << eq.rv << "](" << eq.r << ", " << eq.c << ")" << endl;
	for(set<Variable>::iterator iter = eq.vars.begin(); iter != eq.vars.end(); iter++)
		out << "\t" << *iter;
	return out;
}

ostream &operator<<(ostream &out, Pair p)
{
	out << "(" << p.first << ", " << p.second << ")";
	return out;
}

ostream &operator<<(ostream &out, User u)
{
	out << "(" << u.id << ", " << u.n << ", " << u.m << ", " << u.d << " )";
	return out;
}

ostream &operator<<(ostream &out, Instance inst)
{
	out << "Instance: {";
	for(int i =0; i < inst.K ;i++){
		out << inst.users[i];
	}

	out << "}\tE:"<<endl;

	int j=0;
	for(set<Pair>::iterator iter = inst.E.begin(); iter != inst.E.end();iter++)
		out << *iter;
	return out;
}

/* INSTANCE METHODS */
Instance::Instance(int x)
{
	User temp(0,2,2,1);
	K = x;
	E.insert(Pair(0,0));

	for(int i = 0; i < K;i++){
		temp.id = i;
		users.push_back( temp );
	}
}

Instance::Instance(const Instance &inst)
{
	K = inst.K;
	users = inst.users;
	setTopology(inst.E);
	equations = inst.equations;
}

/*Next Instance structure (cycling through topology at higher level function) */
Instance Instance::nextInstance(){
	Instance result(K);
	int j;
	result.users = users;
	result.setTopology(E);

	for(j=K-1; j>=0 && (users[j].last()) ; j-- );
	result.users[j] = users[j].next();

	if(j < K-1)
		for(int i=K-1;i>j;i--){
			result.users[i].n = result.users[i].m = 2;
			result.users[i].d = 1;
		}
//	cout << "NEXT INSTANCE: " << result << endl;
	return result;
}

bool Instance::last()
{
	int j;
	for(j=0; j < K && (users[j].last()) ; j++ );
//	cout << "LAST INSTANCE?:" << j << endl;
	return j == K;
}

void Instance::setTopology(const set<Pair> &top)
{
	E.clear();
	E = top;
	Variable temp;

	variables.clear();
	equations.clear();

	/*Update variable set*/
	for(int k =0; k < K;k++){
//		cout << "k=" << k << endl;
		for(int r=0;r < (users[k].n-users[k].d);r++)
			for(int c=0;c < users[k].d;c++){
				Variable temp(k, true, r,c);
				variables.insert(temp);
				users[k].txVars.insert(temp);
			}
		for(int r=0;r < (users[k].m-users[k].d);r++)
			for(int c=0;c < users[k].d;c++){
				Variable temp(k, false, r,c);
				variables.insert(temp );
				users[k].rcVars.insert(temp);
			}
	}


	/*Update Equations*/
	if(E.empty()) return;
	for(set<Pair>::iterator iter = E.begin(); iter != E.end(); iter++) {
//		cout << "iii" << endl;
		for(int c=0; c < users[ iter->first ].d;c++){
//			cout << "\tc" << c  << endl;
			for(int r=0; r < users[ iter->second ].d;r++){
//				cout << "\tr" << r << endl;
				Equation eq = Equation(iter->first, iter->second, r, c);

				for(int var_row = 0; var_row < users[iter->first].n - users[iter->first].d ;var_row++ ){
//					cout << "\t\tvar_row 1" << "\t" << var_row << endl;
					temp = Variable(iter->first, true, var_row, c);
					eq.vars.insert(temp);
				}

				for(int var_row = 0; var_row < users[iter->second].m - users[iter->second].d;var_row++){
					temp = Variable(iter->second, false, var_row, r);
					eq.vars.insert( temp );

				}
//				cout << "Inserted: " << eq << endl;
				equations.insert(eq);
//				cout <<" ------------------------------- " << equations.size() << endl;
			}
		}
	}
}

User User::next()
{
	if(d < (m -1 ) && d < (n -1 ) )
		return User(id,n,m,d+1);
	if(m < m_max)
		return User(id,n,m+1,1);
	if(n < n_max)
		return User(id,n+1,2,1);
//	throw exception("User::next");
}


pairpowerset powerset(pairset set)
{
  typedef pairset::const_iterator set_iter;
  typedef std::vector<set_iter> vec;
  typedef vec::iterator vec_iter;

  struct local
  {
    static Pair dereference(set_iter v) { return *v; }
  };

  pairpowerset result;

  vec elements;
  do
  {
    pairset tmp;
    std::transform(elements.begin(), elements.end(),
                   std::inserter(tmp, tmp.end()),
                   local::dereference);
    result.insert(tmp);
    if (!elements.empty() && ++elements.back() == set.end())
    {
      elements.pop_back();
    }
    else
    {
      set_iter iter;
      if (elements.empty())
      {
        iter = set.begin();
      }
      else
      {
        iter = elements.back();
        ++iter;
      }
      for (; iter != set.end(); ++iter)
      {
        elements.push_back(iter);
      }
    }
  } while (!elements.empty());

  return result;
}


eqPowersetType powerset(set<Equation> set)
{
  typedef eqSet::const_iterator set_iter;
  typedef std::vector<set_iter> vec;
  typedef vec::iterator vec_iter;

  struct local
  {
    static Equation dereference(set_iter v) { return *v; }
  };

  eqPowersetType result;

  vec elements;
  do
  {
    eqSet tmp;
    std::transform(elements.begin(), elements.end(),
                   std::inserter(tmp, tmp.end()),
                   local::dereference);
    result.insert(tmp);
    if (!elements.empty() && ++elements.back() == set.end())
    {
      elements.pop_back();
    }
    else
    {
      set_iter iter;
      if (elements.empty())
      {
        iter = set.begin();
      }
      else
      {
        iter = elements.back();
        ++iter;
      }
      for (; iter != set.end(); ++iter)
      {
        elements.push_back(iter);
      }
    }
  } while (!elements.empty());

  return result;
}


pairpowerset generate_configurations(int K)
{
	pairset baseSet;
	for(int k=0; k < K; k++)
		for(int l=0; l < K;l++)
			if(k != l){
				Pair temp(k,l);
				baseSet.insert( temp );
			}
	return powerset(baseSet);
}

void test_configs(pairpowerset configs)
{
	cout << "Yeeha~" << endl;
	for( pairpowerset::iterator iter = configs.begin();
		iter != configs.end();
		iter++)
	{
		cout << "{";
		char const* prefix = "";
		for( set<Pair>::iterator iter2 = iter->begin();
			iter2 != iter->end();
			++iter2)
		{
			cout << prefix << *iter2;
			prefix = ", ";
		}
		cout << " }" << endl;
	}
}

eqSet *getSubset(const set<Equation> &base, Binary b)
{
	set<Equation> *result = new set<Equation>();
	int i=0;
//	cout << "Getting subset of " << b << endl;
	for(set<Equation>::iterator iter = base.begin(); iter != base.end();iter++){
		if (b[i])
			result->insert(*iter);
		i++;
	}
	return result;
}

void print_eqs(Instance inst)
{
	for(set<Equation>::iterator iter = inst.equations.begin(); iter != inst.equations.end(); iter++) {
		cout << *iter << endl;
	}
}

bool hallTest(const set<Equation> *eqs)
{
	set<Variable> neighborhood;

	if(eqs == NULL) { return true; }
	if(eqs->empty())
		{ return true;}

	for(set<Equation>::iterator iter = eqs->begin(); iter != eqs->end();iter++) {
		for(set<Variable>::iterator it = iter->vars.begin(); it != iter->vars.end();it++)
			neighborhood.insert(*it);
	}

/*	cout << "Hall Test on: " << endl;
	for(eqSet::iterator iter2 = eqs->begin(); iter2 != eqs->end(); iter2++)
		cout << *iter2 << endl;

	cout << "Vars: " << endl;
	for(set<Variable>::iterator it = neighborhood.begin(); it != neighborhood.end();it++)
		cout << *it << ", ";
	cout << endl;

	cout << eqs->size() << "Vs" << neighborhood.size() << endl; */
	return eqs->size() <= neighborhood.size();
}


bool proper(const Instance &inst)
{
	Binary b( inst.equations.size() );
//	cout << "Subset of size: 2^" << b.n << endl;
	do{
		eqSet *eqset = getSubset(inst.equations, b);
//		cout << "0" << endl;
		if( eqset != NULL ) {
//			cout << "1" << endl;
			if( !hallTest( eqset) ) {
//				cout << "2" << endl;
				delete eqset;
				return false;
			}
			if( b.last() ) {
//				cout << "3" << endl;
				delete eqset;
				break;
			}
		}
//		else cout << "WHOAAA" << endl;
//		cout << "4" << endl;

		delete eqset;
		if(b.last()) break;
		b.inc();
//		cout << "5" << endl;
	}while(1);
	return true;
}

set<Equation> *weakSet(const Instance &inst, Binary b)
{
	set<Equation> *result = new set<Equation>;
	Equation temp;
//	cout << "SUBSET CODE:" << b << endl;
	for(set<Equation>::iterator eq_it = inst.equations.begin(); eq_it != inst.equations.end(); eq_it++){
		if( b[ eq_it->tx + b.n/2] && b[eq_it->rv] ) {
			temp = *eq_it;
			for(set<Variable>::iterator v_it = temp.vars.begin(); v_it != temp.vars.end(); v_it++) {
				if( v_it->sender && b[ v_it->user + b.n/2] == 0){
					temp.vars.erase(v_it);
					continue;
				}
				if( !(v_it->sender) && b[ v_it->user ] == 0) {
					temp.vars.erase(v_it);
					continue;
				}
			}
//			cout << "Inserting eq: " << temp << " into weak set" << endl;
			result->insert(temp);
		}
	}
	return result;
}
bool weaklyProper(const Instance &inst)
{
	Binary b(inst.K*2);

	do{
		set<Equation> *eqset = weakSet(inst, b);

/*		if(eqset->size() != 0)
			cout << "Weak Set:" << endl;
		else
			cout << "WTF EMPTY WEAK SET" << endl;
		for(set<Equation>::iterator it = eqset->begin(); it != eqset->end();it++)
			cout << (*it) << endl;
		cout << endl;
*/		if( !hallTest( eqset) ){
			delete eqset;
			return false;
		}
		if( b.last() ){
			delete eqset;
			break;
		}
		delete eqset;
		b.inc();
	}while(1);
	return true;
}


int main(int argc, char **argv)
{
	ofstream file;
	std::ostringstream ss;
	std::string name;

	const int cores = 4;
	int i=0, beginAt = 1, chunkSize = MAX/cores, pid;
	if(argc != 2) { cout << "Wrong number of arguments" << endl; return 0; }
	int K = atoi(argv[1]);
	Instance current(K);
	pairpowerset configurations = generate_configurations(K);

//	cout << "Generated Pairs" << K << " " << n_max << " " << m_max << endl;
	cout << configurations.size() << endl;

//	test_configs(configurations);
/*	beginAt = 1;
	chunkSize = MAX / cores;	//Rough

	if( fork() == 0 ){
		pid = 0;
		beginAt += 2*chunkSize;
	}else{
		pid = 2;
		beginAt += chunkSize;
	}
	if(  fork() == 0 )
		beginAt += 2*chunkSize;
	else
		pid++;
*/
	ss << pid;
	name = "out_4_5";
	file.open( name.c_str() );
	file << "OUTPUT OF PID :" << pid << endl;
	cout << pid << "\tBEGINNING AT\t" << beginAt << " Until " << beginAt+chunkSize << endl;

	current.users[0].n =4;
	current.users[0].m=5;
	current.users[0].d=2;

	current.users[1].n=4;
	current.users[1].m=5;
	current.users[1].d=3;

	current.users[2].n=5;
	current.users[2].m=5;
	current.users[2].d=2;
	i = 94313;
	bool p,wp;
	do{

		for(pairpowerset::iterator iter = configurations.begin();
			iter != configurations.end();
			iter++) {
			if(iter->size() == 0)
				continue;
			current.setTopology(*iter);

			wp = weaklyProper(current);
			if(wp == true ) {
				p = proper(current);
				if(wp != p){
					cout << pid << " got a HIT\t" << current << endl;
					cout << current << endl;
					cout << "***" << endl;
				}
			}
			i++;
			file << i << "\t"<< current << endl;
		}
		if( current.last() )
			break;
		current = current.nextInstance();
//		file << "Instance:" << current << endl;

	}while(1);
	file.close();
	return 0;
}
