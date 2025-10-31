#include <algorithm>
#include <chrono>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <utility>
#include <vector>
using namespace std;

struct Timer {
    using clock = std::chrono::steady_clock;
    clock::time_point start = clock::now();
    double seconds() const { return std::chrono::duration<double>(clock::now() - start).count(); }
};

struct Graph {
    int maxId = 0;
    vector<pair<int,int>> edges;
    vector<vector<int>> adj;
    vector<char> active;

    bool loadFromFile(const string& path) {
        edges.clear(); maxId = 0;
        ifstream in(path);
        if (!in) return false;
        string line;
        auto norm = [](int a,int b){ if(a>b) std::swap(a,b); return pair<int,int>(a,b); };
        while (getline(in, line)) {
            if (line.empty()) continue;
            int u=0, v=0; char comma=0;
            stringstream ss(line);
            if (!(ss>>u)) continue;
            if (!(ss>>comma) || comma!=',') continue;
            if (!(ss>>v)) continue;
            if (u<=0 || v<=0) continue;
            if (u==v) { if (u<=350) maxId = std::max(maxId,u); continue; }
            if (u>350 || v>350) continue;
            maxId = std::max(maxId, std::max(u,v));
            edges.push_back(norm(u,v));
        }
        sort(edges.begin(), edges.end());
        edges.erase(unique(edges.begin(), edges.end()), edges.end());
        adj.assign(maxId+1, {});
        active.assign(maxId+1, 0);
        for (auto e : edges) {
            int u=e.first, v=e.second;
            adj[u].push_back(v);
            adj[v].push_back(u);
            active[u]=active[v]=1;
        }
        return true;
    }
    bool hasEdges() const { return !edges.empty(); }
};

struct Solver {
    const Graph& G;
    double timeBudgetSec;
    explicit Solver(const Graph& g, double budgetSec=2.0): G(g), timeBudgetSec(budgetSec) {}

    bool coversAll(const vector<int>& cover) const {
        if (G.edges.empty()) return true;
        vector<char> in(G.maxId+1, 0);
        for (int v: cover) if (v>=1 && v<=G.maxId) in[v]=1;
        for (auto e : G.edges) {
            int u=e.first, v=e.second;
            if (!(in[u] || in[v])) return false;
        }
        return true;
    }

    vector<int> twoApproxFromMatching(const Timer& T) const {
        vector<pair<int,int>> E = G.edges;
        vector<int> deg(G.maxId+1,0);
        for (int v=1; v<=G.maxId; ++v) deg[v]=(int)G.adj[v].size();
        sort(E.begin(), E.end(), [&](const pair<int,int>& a, const pair<int,int>& b){
            int da=deg[a.first]+deg[a.second], db=deg[b.first]+deg[b.second];
            if (da!=db) return da>db;
            return a<b;
        });
        vector<int> cover;
        vector<char> matched(G.maxId+1,0);
        for (auto e : E) {
            if (T.seconds() > timeBudgetSec*0.6) break;
            int u=e.first, v=e.second;
            if (!matched[u] && !matched[v]) {
                matched[u]=matched[v]=1;
                cover.push_back(u); cover.push_back(v);
            }
        }
        sort(cover.begin(), cover.end());
        cover.erase(unique(cover.begin(), cover.end()), cover.end());
        cleanup(cover);
        return cover;
    }

    vector<int> greedyMaxDegree(const Timer& T) const {
        vector<vector<int>> adj = G.adj;
        auto removeV = [&](int x){
            for (int y: adj[x]) {
                auto& ly = adj[y];
                ly.erase(std::remove(ly.begin(), ly.end(), x), ly.end());
            }
            adj[x].clear();
        };
        auto hasEdgesLocal = [&](){
            for (int v=1; v<=G.maxId; ++v) if (!adj[v].empty()) return true;
            return false;
        };
        vector<int> cover; cover.reserve(G.maxId);
        while (hasEdgesLocal()) {
            if (T.seconds() > timeBudgetSec*0.6) break;
            int best=-1, bestD=-1;
            for (int v=1; v<=G.maxId; ++v){
                int d = (int)adj[v].size();
                if (d>bestD){ bestD=d; best=v; }
            }
            if (best<=0) break;
            cover.push_back(best);
            removeV(best);
        }
        cleanup(cover);
        return cover;
    }

    void cleanup(vector<int>& cover) const {
        sort(cover.begin(), cover.end());
        cover.erase(unique(cover.begin(), cover.end()), cover.end());
        for (int i=(int)cover.size()-1; i>=0; --i){
            int keep = cover[i];
            cover.erase(cover.begin()+i);
            if (!coversAll(cover)) cover.insert(cover.begin()+i, keep);
        }
    }

    void localImprove(vector<int>& cover, const Timer& T) const {
        vector<char> in(G.maxId+1,0);
        auto rebuild=[&](){ std::fill(in.begin(),in.end(),0); for(int x:cover) in[x]=1; };
        rebuild();
        bool improved=true;
        while (improved && T.seconds() < timeBudgetSec*0.95) {
            improved=false;
            for (int idx=0; idx<(int)cover.size(); ++idx) {
                int v = cover[idx];
                for (int u: G.adj[v]) {
                    if (in[u]) continue;
                    in[v]=0; in[u]=1;
                    bool ok=true;
                    for (auto e: G.edges) { int a=e.first,b=e.second; if (!(in[a]||in[b])) { ok=false; break; } }
                    if (ok) { cover[idx]=u; rebuild(); improved=true; break; }
                    in[u]=0; in[v]=1;
                    if (T.seconds() > timeBudgetSec*0.95) break;
                }
                if (improved || T.seconds() > timeBudgetSec*0.95) break;
            }
        }
        cleanup(cover);
    }

    vector<int> solve(const Timer& T) const {
        vector<int> best = twoApproxFromMatching(T);
        auto gr = greedyMaxDegree(T);
        if (gr.size() < best.size()) best.swap(gr);
        localImprove(best, T);
        return best;
    }
};

static const string FIO = "Лузгин Алексей Юрьевич";

static void writeOutput(const vector<int>& cover) {
    ofstream out("output.txt", ios::binary);
    out << FIO << "\r\n";
    for (size_t i=0;i<cover.size();++i){ if(i) out<<","; out<<cover[i]; }
    out << "\r\n";
}

int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    Graph G;
    if (!G.loadFromFile("input.txt")) { writeOutput({}); return 0; }
    Timer T;
    Solver S(G, 2.0);
    auto ans = S.solve(T);
    sort(ans.begin(), ans.end());
    ans.erase(unique(ans.begin(), ans.end()), ans.end());
    writeOutput(ans);
    return 0;
}
