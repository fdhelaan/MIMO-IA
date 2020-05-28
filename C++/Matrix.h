#ifndef MATRIX_H
#define MATRIX_H

#include <iostream>
#include <sstream>
#include <queue>
#include <string>
#include <vector>


template <class T>
class Matrix_row{
	public:
	std::vector<T> row;
	Matrix_row() {
		row.resize(0);
	}
	Matrix_row(int x) {
		row.resize(x);
	}
	Matrix_row(std::vector<T>& r) : row(r) {
	}
	T& operator[](unsigned int c){
		return row.at(c);
	}
	const T& operator[](unsigned int c) const {
		return row.at(c);
	}
	void resize(int x) { row.resize(x); }

	std::vector<int> adjacency( std::vector<int> comb) const{
		std::vector<int> adj;
		for(int i =0; i < row.size();i++)
			if( row[i] != 0
				&& std::find(comb.begin(), comb.end(), i) != comb.end() )
				adj.push_back(i);
		std::sort(adj.begin(), adj.end());
		return adj;
	}
};

template <class T>
class Matrix{
	public:
	Matrix() { rows = cols = 0;}
	Matrix(int r, int c) {
		resize(r, c);
	}
	Matrix_row<T>& operator[](unsigned int r) {
		return m.at(r);
	}
	const Matrix_row<T>& operator[](unsigned int r) const {
		return m.at(r);
	}
	void resize(int r, int c) {
		rows = r;
		cols = c;
		m.resize(r);
		for(int i = 0; i < r;i++)
			m[i].resize(c);
	}
	std::vector<int> rowAdjacency(const int, const std::vector<int>) const;
	std::vector<std::vector<int> > adjacency(const std::vector<int>) const;
	std::string to_string() const;

	template <class T1>
	friend std::ostream& operator<<(std::ostream& , const Matrix<T1> &);

	std::vector< Matrix_row<T> > m;
	int rows, cols;
};

template <class T>
std::string Matrix<T>::to_string() const{
	std::ostringstream ss;

	for(int r=0; r< rows;r++) {
		for(int c=0; c< cols;c++) {
			ss << (m.at(r))[c] << "\t";
		}
		ss << std::endl;
	}
	return ss.str();
}

template <class T>
std::vector<int> Matrix<T>::rowAdjacency(const int r, const std::vector<int> comb) const{
	return m.at(r).adjacency(comb);
}
template <class T>
std::vector<std::vector<int> > Matrix<T>::adjacency(const std::vector<int> comb) const{
	std::vector<std::vector<int> > adj_list;
	for(int r=0;r < rows;r++)
		adj_list.push_back( rowAdjacency(r, comb) );
	return adj_list;
}

template <class T>
std::ostream& operator<<(std::ostream& s, const Matrix<T> &mx){
	s << mx.to_string();
	return s;
}

#endif
