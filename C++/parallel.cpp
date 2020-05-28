#include <iostream>
#include <algorithm>
#include <thread>
#include <condition_variable>
#include <atomic>
#include <chrono>
#include <fstream>
#include <sstream>
#include <ostream>
#include "Instance.h"
#include "Det.h"
#include <curses.h>
#include <boost/math/special_functions/binomial.hpp>

	int max_threads = std::thread::hardware_concurrency() - 1;
	//Max # of child threads.
//	int max_threads = 1;							//Debug mode~

class Update {
	public:
	Update(int x, unsigned long long y, unsigned long long z) { 
		id = x; done = y; total = z; 
	}
	int id;
	unsigned long long done;
	unsigned long long total;
};

void to_file(std::ofstream &file, std::vector<Det> &det_list) {
	std::vector<Det>::const_iterator it = det_list.begin();
	for(; it != det_list.end(); it++) {
		file << *it << std::endl;
	}
}

void print_progress( const std::vector<Update> &progress, 
			const double &comb_count, const double &total_combs, 
			WINDOW *prog_w) {
	int barwidth = 200;
	clear();

	for(int i = 0; i < progress.size();i++) {
		if(progress[i].total) {
			std::ostringstream bar,out;
			float percent =  (float) progress[i].done 
					/ progress[i].total;
			int pos = percent * barwidth;
			for(int j = 0; j < barwidth;j++)
				if(j < pos)
					bar << "=";
				else if (j == pos)
					bar << ">";
				else
					bar << " ";
			out << "Thread #" << progress[i].id << "[" 
				<< bar.str() << "] " << (int) (100 * percent)
				<< "\tperc\t" << "(" << progress[i].done 
				<< "/" << progress[i].total << ")";

			mvwprintw(prog_w, i+1, 1, out.str().c_str());
		}
	}
	float percent = (float) comb_count / total_combs;
	int pos = percent * barwidth;
	std::ostringstream bar, out;
	for(int j = 0; j < barwidth;j++)
		if(j < pos)
			bar << "=";
		else if (j == pos)
			bar << ">";
		else	bar << " ";
	out << "Combs  #:[" << bar.str() << "]" << (int) (100 * percent) 
		<< "\tperc\t" << "("
		<< comb_count << "/" << total_combs << ")";
//	mvwprintw(prog_w, progress.size()+1, 1, "Combs: %f / %f", comb_count, 
//	total_combs);
	mvwprintw(prog_w, progress.size()+1, 1, out.str().c_str());
	wrefresh(prog_w);
}

/*Child process starts here*/
void get_terms(const Instance &L, std::queue<std::vector<int>> &comb_queue
		, std::vector<Update> &progress, int thread_id
		, WINDOW *thread_w
		, std::condition_variable &cv
		, std::condition_variable &qReadyCv
		, std::condition_variable &uReadyCv
		, std::mutex &qmutex,  std::mutex &pmutex
		, std::atomic<bool> &finished, std::atomic<bool> &update
		, std::atomic<int> &active_threads){

	std::vector<Det> det_list;
	int comb_count = 0;
	while(true){
		std::vector<int> comb;
		{
			/*Wait for qReadycv*/
			std::unique_lock<std::mutex> lk(qmutex);
			while(comb_queue.empty() && finished.load() == false) {	
				//Loop for spurious wake up.
				// {
				// 	std::unique_lock<std::mutex> ulock(pmutex);
				// 	mvwprintw(thread_w, 2, 1, "Zzzz");
				// 	wrefresh(thread_w);
				// }
				qReadyCv.wait(lk);
				// {
				// 	std::unique_lock<std::mutex> ulock(pmutex);
				// 	mvwprintw(thread_w, 2, 1, "Up! ");
				// 	wrefresh(thread_w);
				// }

			}
			if(finished.load() == true && comb_queue.empty() )	
				//Server has no more combinations. Check queue.
				break;
			//Else:Queue not empty.

			comb = comb_queue.front();
			comb_queue.pop();

			if(comb_queue.empty() )
				cv.notify_all();	//Notify parent: There is room in the queue

		}
		int done = 0;
		comb_count++;
		// {
		// 	std::unique_lock<std::mutex> ulock(pmutex);
		// 	mvwprintw(thread_w, 3, 1, "In ");
		// 	wrefresh(thread_w);
		// }
		Det d(L, comb, thread_w, std::ref(pmutex));
		// {
		// 	std::unique_lock<std::mutex> ulock(pmutex);
		// 	mvwprintw(thread_w, 3, 1, "out");
		// 	wrefresh(thread_w);
		// }
		do{
			// if (d.total == 0)
			// 	break;
			unsigned long long old_done;
			done = d.insert_bulk();

			{			/*Critical section to protect update_queue */
				std::unique_lock<std::mutex> ulock(pmutex);

				progress[thread_id].done = done;
				progress[thread_id].total = d.total;
				update = true;

				mvwprintw(thread_w, 1, 1, "%d / %d : %d", done, d.total
						, comb_count);
				wrefresh(thread_w);
				cv.notify_one();
			}
		}while(done < d.total);
		d.one_zero_test();
		det_list.push_back(d);

	}		//End While
	std::ostringstream ss;
	ss << thread_id << "_output";
	std::ofstream file;
	file.open(ss.str());
	to_file(file,det_list);
	file.close();
	--active_threads;
	cv.notify_one();
}

/*********************************************/

bool zero_row(int row,const Instance &L, const std::vector<int> &comb) {
	for(int i=0; i < comb.size();i++)
		if(L.m[row][ comb[i] ] != 0 )
			return false;
	return true;
}

bool zero_col(int col, const Instance &L) {
	for(int r =0; r < L.m.rows; r++){
		if(L.m[r][col] != 0 )
			return false;
	}
	return true;
}
/* Reject combinations with:
	1) All zero rows.
	2) All zero columns
*/
bool valid_comb(const Instance &L, std::vector<int> &comb) {
	std::vector<std::vector<int>> adj_list = L.m.adjacency(comb);
	//Check all zero rows
	for(int r=0;r < L.m.rows;r++)
		if(zero_row(r, L, comb) ){
			return false;
		}
	//Check all zero columns
	for(int c=0; c < comb.size();c++)
		if(zero_col(comb[c], L) ){
			return false;
	}
	return true;
}

void comb_list(Instance &L, std::vector<WINDOW *> thread_subw
		, WINDOW *log_w, WINDOW *prog_w) {
	std::vector<bool> b;
	bool more_combs=true;
	int i=0;
	/*threading vars*/
//	int active_threads=0;
	double total_combs=0,comb_count=0;

	std::vector<std::thread> threads;
	std::vector<Update> progress;
	std::vector<int> comb;
//	std::queue<Update> update_queue;

	std::atomic<bool> finished(false);				//No more combinations.
	std::atomic<bool> update_ready(false);
	std::atomic<int> active_threads(0);

	std::queue<std::vector<int>> comb_queue;
	std::mutex qmutex;
	std::condition_variable cv;						//Some child is done!
	std::condition_variable qReadyCv;
	std::condition_variable uReadyCv;				//User update is ready.
	std::mutex pmutex;								//Protects update_queue.

	for(i = 0; i < L.m.rows; i++)
		b.push_back(true);

	for(;i < L.m.cols;i++)
		b.push_back(false);

	sort( b.begin(), b.end() );
	reverse(b.begin(), b.end() );

	total_combs = boost::math::binomial_coefficient<double> 
		(L.m.cols, L.m.rows);

	while(active_threads < max_threads) {
		//In first iteration: spawn threads.
		threads.push_back( std::thread(&get_terms, L, std::ref(comb_queue)
						, std::ref(progress), active_threads.load()
						, thread_subw[active_threads], std::ref(cv)
						, std::ref(qReadyCv), std::ref(uReadyCv)
						, std::ref(qmutex), std::ref(pmutex)
						, std::ref(finished), std::ref(update_ready)
						, std::ref(active_threads) ) );
		progress.push_back( Update(active_threads, 0, 0 ) );
		active_threads++;
	}

	do{									//Event Loop
		{
			std::unique_lock<std::mutex> lk(qmutex);
			{
				std::unique_lock<std::mutex> ulk(pmutex);
				mvwprintw(log_w, 6, 1, "comb_queue size: %d\tcomb_count:%f"
						, comb_queue.size(), comb_count );
				wrefresh(log_w);
			}
			while(comb_queue.size() >= max_threads && !update_ready.load() ){
				//Loop for spurious wake.
				{
					std::unique_lock<std::mutex> ulk(pmutex);
					mvwprintw(log_w, 5, 1, "ZZz");
					wrefresh(log_w);
				}
				cv.wait(lk);
				{
					std::unique_lock<std::mutex> ulk(pmutex);
					mvwprintw(log_w, 5, 1, "Up!");
					wrefresh(log_w);
				}
			}
			if(update_ready.load()) {
				// Status update from a child
				std::unique_lock<std::mutex> ulk(pmutex);
				print_progress(progress, comb_count, total_combs, prog_w);
				update_ready = false;
			}
			if(comb_queue.size() >= max_threads)
				continue;

			/*Figure out combination from binary permutation*/

			do{
				comb.clear();
				for(i=0;i < b.size();i++)
					if(b[i] == true)
						comb.push_back(i);

				comb_count++;
				more_combs = prev_permutation(b.begin(), b.end());
				if (valid_comb(L, comb)){
					comb_queue.push(comb);
					qReadyCv.notify_one();
					//inform threads that a comb is ready
					break;
				}
			} while( more_combs && comb_queue.size() < max_threads);
		}	//Release qmutex.


	} while ( more_combs);
	/*No more combinations*/

	finished = true;
	{
		std::unique_lock<std::mutex> lk(qmutex);
		//Not necessary?
		while(!comb_queue.empty() ||  active_threads.load() > 0 ) {
			qReadyCv.notify_all();
			{
				std::unique_lock<std::mutex> ulk(pmutex);
				mvwprintw(log_w, 5, 1, "ZZz2");
				wrefresh(log_w);
			}
			cv.wait(lk);
			{
				std::unique_lock<std::mutex> ulk(pmutex);
				mvwprintw(log_w, 5, 1, "Up!2");
				wrefresh(log_w);
			}
			if(update_ready.load()) {
				// Status update from a child
				std::unique_lock<std::mutex> ulk(pmutex);
				print_progress(progress, comb_count, total_combs, prog_w);
				update_ready = false;
			}
		}
	}
	std::this_thread::sleep_for(std::chrono::seconds(1) );

	for(int i = 0; i < threads.size();i++)
		if(threads[i].joinable()) {
//			std::cout << "Thread #" << i << " is joinable." << std::endl;
			threads[i].join();
		} else qReadyCv.notify_all();
//			std::cout << "Thread #" << i << " is not joinable." << std::endl;
//	std::cout << "Good night everybody! " <<  comb_queue.size() << std::endl;
}

void init_ncurses() {
	initscr();
	cbreak();
	noecho();
	keypad(stdscr,TRUE);
}

int main () {
	std::vector<WINDOW *> thread_subw(max_threads); /*One window per thread*/
	WINDOW *log_w, *prog_w;
	int y0=0,x0=0;

	Instance *l = new Instance();
	l->read_instance();

	if(l->m.rows > l->m.cols) {
		std::cerr << "Rows > columns.Not proper." << std::endl;
		return 0;
	}
	init_ncurses();
	refresh();
	for(int i = 0; i < max_threads;i++ ) {
		thread_subw[i] = newwin( LINES/3, COLS/max_threads, 0, x0 );
		box(thread_subw[i], 0, 0);

		mvwprintw(thread_subw[i], 1, 1, "subwindow %d/%d=%d", COLS
				, max_threads, COLS/max_threads);

		x0 += COLS/max_threads + 1;
		wrefresh(thread_subw[i]);
	}
	y0 += LINES/3;

	log_w = newwin(LINES/3, COLS, y0, 0);
	mvwprintw(log_w, 1, 1, "Dimensions: (%d x %d)", l->m.rows, l->m.cols);
	box(log_w, 0,0);
	wrefresh(log_w);
	y0 += LINES/3;
	prog_w = newwin(LINES/3, COLS,y0,0);
	box(prog_w, 0, 0);
	mvwprintw(prog_w, 1, 1, "progress");
	wrefresh(prog_w);

	comb_list(*l, thread_subw, log_w, prog_w);

	mvwprintw(log_w, 2, 0, l->m.to_string().c_str());
	wprintw(log_w, "\nDONE" );
	wrefresh(log_w);

	wgetch(log_w);
	endwin();
	std::cout << l->m << std::endl;
	return 0;
}
