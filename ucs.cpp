#include <bits/stdc++.h>
using namespace std;
typedef vector<int> vec1;
typedef pair<int,int> p;
typedef vector<p> vec2;
#define INF 100000
vec2 *G; 
vec1 Dist;
void dijkstraAlgo(int source, int N) {
    priority_queue<p, vector<p>, greater<p> > Q;
    Dist.assign(N,INF);
    Dist[source] = 0;
    Q.push({0,source});
    while(!Q.empty()){
        int u = Q.top().second;
        Q.pop();
        for(auto &c : G[u]){
            int v = c.first;
            int w = c.second;
            if(Dist[v] > Dist[u]+w){
                Dist[v] = Dist[u]+w;
                Q.push({Dist[v],v});
            }
        }
    }
}
int main() {
    int N, M, u, v, w, source, goal;  
    cout<<"Enter number of nodes and edges respectively:";
    cin >> N >> M;
    G = new vec2[N+1]; 
    for(int i=0;i<M;++i)
    {
        cout<<"( , , ):"<<endl;
        cin >> u >> v >> w;
        G[u].push_back({v,w});
        G[v].push_back({u,w});
    }
    cin >> source;
    cin >> goal;
    dijkstraAlgo(source,N); 
    cout<<Dist[goal--];
    for(int i=0;i<N;i++)
    cout<<endl; 
    return 0;
}
