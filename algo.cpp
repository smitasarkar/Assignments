//Libraries Used
#include<iostream>
#include<fstream> // for file streaming
#include<vector>  // for associative arrays
#include<time.h>  // for timer
#include<map>     // to convert char to int to get mc fast
#include "algo.h"
using namespace std;

//State Algorithms
State::State()
{
	best_cost = -1;
}

State::State(std::vector<int> v)
{
	coordinate = v;
	best_cost = -1;
}

State::State(std::vector<int> v, int K)
{
	coordinate = v;
	best_cost = -1;
	solution.resize(K,"");
}

void State::Print_State()
{
	std::cout << "STATE BEGIN" << std::endl;
	std::cout << "Coordinate size : " << (coordinate.size()) << std::endl;
	for (size_t i = 0; i < coordinate.size(); i++)
	{
		std::cout << coordinate[i] << ' ';
	}
	std::cout << std::endl;
	std::cout << " best cost : " << best_cost << std::endl;
	std::cout << "Strings size " << solution.size() << std::endl;
	for (size_t i = 0; i < coordinate.size(); i++)
	{
		std::cout << solution[i] << std::endl;
	}
	std::cout << "STATE END" << std::endl;
}

State::State(std::vector<int> v, double best_cos, std::vector<std::string> soln)
{
	coordinate = v;
	best_cost = best_cos;
	solution = soln;
}

double State::Step_Cost(State z, std::vector<std::vector<double> > MC, double cc, std::map<char,int> mp, std::vector<std::string> X)
{
	std::vector<bool> v;
	int dash_counter = 0;
	for(size_t i=0; i<coordinate.size(); i++)
	{
		int q = z.coordinate[i] - this->coordinate[i];
		if(q > 2)
			return -1;
		if(q != 0)
			v.push_back(true);
		else
		{
			v.push_back(false);
			dash_counter++;
		}
	}
	std::vector<char> temp;
	double cost = dash_counter * cc;
	for (size_t i = 0; i < coordinate.size(); i++)
	{
		if(v[i])
			temp.push_back(X[i][coordinate[i]]);
		else
			temp.push_back('-');
	}
	for (size_t i = 0; i < temp.size(); i++)
	{
		for (size_t j = i+1; j < temp.size(); j++)
		{
			cost += MC[mp[temp[i]]][mp[temp[j]]];
		}
	}
	return cost;
}

bool State::is_Goal(int K, std::vector<std::string> X)
{
	for (size_t i = 0; i < (size_t)K; i++)
	{
	if((int)X[i].size() == coordinate[i])
		continue;
	return false;
	}
	return true;
}

bool State::is_Valid(int K, std::vector<std::string> X)
{
	for (size_t i = 0; i < (size_t)K; i++)
	{
		if((int)X[i].size() >= coordinate[i])
			continue;
		return false;
	}
return true;
}

//Algo function to check if format is correct
void algo_that_doesnt_work(std::vector<std::string> &X, std::vector<std::string> &S)
{
	int maxs = X[0].size();
	for (size_t i = 0; i < X.size(); i++)
	{
		maxs = std::max(maxs,(int)X[i].size());
	}
	for (size_t i = 0; i < X.size(); i++)
	{
		std::string s = X[i];
		while(maxs - s.size() > 0)
		{
			s.push_back('-');
		}
		S.push_back(s);
	}
}

//Dynamic Programming Solution
void dp(time_t& start, double& timer, int& v_size, std::vector<char>& v, int& K, std::vector<std::string>& X, std::vector<std::string>& S, double& cc, std::vector<std::vector<double> >& MC, std::map<char, int> mp)
{
	std::vector<int> vec(K,0);
	State starter(vec,K);
	starter.Print_State();
	dp_solution(starter, start, timer, v_size, v, K, X, S, cc, MC, mp);
	S = starter.solution;
}

void dp_solution(State z, time_t& start, double& timer, int& v_size, std::vector<char>& v, int& K, std::vector<std::string>& X, std::vector<std::string>& S, double& cc, std::vector<std::vector<double> >& MC, std::map<char, int> mp)
{
	if(z.is_Goal(K,X))
	{
		z.best_cost = 0;
		z.solution.resize(K,"");
		return;
	}
	if(z.best_cost != -1)
		return;
	int i = 0;
	int z0 = 1<<(K+1);
	double cost = -1;
	State ans;
	std::vector<char> vcc(K,'-');
	while(i < z0)
	{
		i++;
		std::vector<int> vaa = z.coordinate;
		int z1 = i;
		int counter = 0;
		std::vector<char> vc(K,'-');
		while(z1 > 0)
		{
			if(z1 & 1)
			{       
				vaa[counter]++;
				vc[counter] = X[counter][z.coordinate[i]];
			}
			counter++;
			z1 /= 2;
		}
		State next(vaa,K);
		if(next.is_Valid(K,X))
		{
			dp_solution(next,start,timer,v_size,v,K,X,S,cc,MC,mp);
			if(cost == -1)
				cost = z.Step_Cost(next, MC, cc, mp, X) + next.best_cost;
			else
			{
				double coster = z.Step_Cost(next, MC, cc, mp, X) + next.best_cost;
				if(coster < cost)
				{
					cost = coster;
					ans = next;
					vcc = vc;
				}
			}
		}
		else
			continue;
	}
	z.best_cost = cost;
	for (size_t i = 0; i < (size_t)K; i++)
	{
		z.solution[i] = vcc[i] + ans.solution[i];
	}
}

void tester1(std::vector<std::vector<double> > MC, double cc, std::map<char,int> mp, std::vector<std::string> X)
{
	std::vector<int> al(2), bl(2);
	al[0] = 0;
	al[1] = 1;
	State a({0,0},-1,X);
	State b({0,0},-1,X);
	std::cout << a.Step_Cost(b,MC,cc,mp,X) << std::endl;
}
