#include<bits/stdc++.h>
using namespace std;
vector<int> parent;
vector<int> height;
int maxHeight, branchingFactor;

class node
{
public:
	int index;
	int value=0;
	int alpha=0;
	int beta=0;
};

node root;
vector<node> *l;
int internalNodes, totalNodes, leafNodes;
int minimax(node &x);
int maxValue(node &x);
int minValue(node &x);
int checkTerminal(node &x);
int alphaBetaPrune(node &x, int &, int &);
int maxval(node &x, int &, int &);
int minval(node &x, int &, int &);
void optimalPlay(node &x, int depth);

void buildTree(int maxHeight, int branchingFactor)
{
	int k =1;
	int a = pow(branchingFactor,maxHeight)-1;
	int b =pow(branchingFactor,maxHeight+1)-1;
	internalNodes = a/(branchingFactor-1);
	totalNodes = b/(branchingFactor-1);
	leafNodes = totalNodes - internalNodes;
	height.resize(totalNodes);
	parent.resize(totalNodes);	
	root.index=0; parent[root.index]=-1; height[root.index]=0;
	l = (vector<node>*)malloc(internalNodes * sizeof(vector<node>));
		
	//for internal nodes
	for(int i=0;i<internalNodes;i++)
	{
		for(int j=0;j<branchingFactor;j++)
		{
			node n;
			n.index = k; k++;
			parent[n.index]=i;
			height[n.index]=height[parent[n.index]]+1;			
			l[i].push_back(n);
		}
	}
	
	//for external nodes
	int h = height[internalNodes-1];
	int i= internalNodes-1;
	srand((unsigned)time(NULL));
	while(height[i]==h)
	{
	    for(int j=0;j<l[i].size();j++)
		{
	       node n = l[i][j];
	       n.value = rand()%100;
	       l[i][j].value = n.value;
	    }    
	    i--;
	}	
}
void showTree()
{
    cout<<"root index= "<< root.index<<";	root value= "<<root.value<<endl;
	for(int i=0;i<internalNodes;i++)
	{
		for(int j=0;j<l[i].size();j++)
		{
			node n = l[i][j];
			cout<<"node index= "<< n.index<<";	node value= "<<n.value<<endl;
		}
	}
}


int minimax(node &x)
{
	int v = maxValue(x);
	return v;
}

int maxValue(node &x)
{
	if(checkTerminal(x))	    
	    return x.value;		
	int v = 0;
	for (int j =0;j<l[x.index].size();j++)
		v = max(v, minValue(l[x.index][j]));
	x.value = v;	
	return v;
}

int minValue(node &x)
{
	if(checkTerminal(x))
		return x.value;	
	int v = 0;
	for (int j =0;j<l[x.index].size();j++)
		v = min(v, maxValue(l[x.index][j]));
	x.value=v;	
	return v;
}

int checkTerminal(node &x)
{
	if(maxHeight == height[x.index])
		return 1;
	else
		return 0;
}


int alphaBetaPrune(node &x, int &alpha, int &beta)
{
	int v = maxval(x,alpha,beta);
	return v;
}

int minval(node &x, int &alpha, int &beta)
{
    x.alpha=alpha; x.beta = beta;
	if(checkTerminal(x))
		return x.value;
	int v=0;
	for(int j =0; j<l[x.index].size();j++)
	{
		int child = maxval(l[x.index][j],x.alpha,x.beta);
		x.beta = min(x.beta,child);
		v = min(v,child);
		if(x.alpha >= x.beta)
		{
		    return child;
		}
	}
  	x.value=v;
	return v;
}

int maxval(node &x, int &alpha, int &beta)
{
    x.alpha=alpha; x.beta = beta;
	if(checkTerminal(x))
		return x.value;
	int v=0;
	for(int j=0;j<l[x.index].size();j++)
	{
		int child = minval(l[x.index][j],x.alpha, x.beta);
		x.alpha = max(x.alpha,child);
		v = max(v, child);
		if(x.alpha >= x.beta)
		{
		    return child;
		}
	}
	x.value=v;
	return v;
}



void optimalPlay(node &x, int depth)
{
	if(depth == maxHeight)
		return;
	else
	{
		for(int j=0;j<l[x.index].size();j++)
		{
			if(l[x.index][j].value == root.value)
			{
				if(height[l[x.index][j].index]%2==0)
					cout<<"MAX-> "<< l[x.index][j].index<<endl;
				else
					cout<<"MIN-> "<<l[x.index][j].index<<endl;
				optimalPlay(l[x.index][j], depth+1);
				return;
			}
		}
	}
}

void pruned()
{
	int sum=0;
	for(int i=0;i<internalNodes;i++)
	{
		for(int j=0;j<l[i].size();j++)
		{
			if(l[i][j].value==0)
			{
				if(height[l[i][j].index]%2==0)
				{
					for(int k =0;k<l[l[i][j].index].size();k++)
					{
						if(l[i][j].alpha==l[l[i][j].index][k].value)
						{
						    sum += (branchingFactor-k-1);
						    break;
						}
					}
				}
				
				if(height[l[i][j].index]%2!=0)
				{
					for(int k =0;k<l[l[i][j].index].size();k++)
					{
						if(l[i][j].beta==l[l[i][j].index][k].value)
						{
						    sum +=(branchingFactor-k-1);
						    break;
						}														
					}
				}
			}
		}
	}
	cout<<"\nNo. of pruned nodes using alpha beta: "<<sum;
}

int main()
{
	cout<<"Enter depth: ";
	cin>>maxHeight;
	cout<<"Enter branching factor: ";
	cin>>branchingFactor;
	buildTree(maxHeight,branchingFactor);
	showTree();
    int x = minimax(root);
	cout<<"\nMinimax value at the root: "<<x<<endl;
	buildTree(maxHeight,branchingFactor);
	int y = alphaBetaPrune(root,root.alpha,root.beta);
	cout<<"\nMinimax value at the root using alpha beta: "<<y;
	cout<<"\nOptimal path is:\n";
	cout<<"MAX-> root\n"; optimalPlay(root,height[root.index]);
	pruned();
	return 0;	
}
