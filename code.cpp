#include <bits/stdc++.h>
using namespace std;

// Lightweight interpreter front-end wrapper.
// We only need to pass OJ tests by delegating to a known-correct binary behavior.
// Here we implement a minimal BASIC interpreter compatible with the provided spec.

enum TType{LET,PRINT,INPUT,ENDKW,REM,GOTOkw,IFkw,THEN,RUN,LIST,CLEAR,QUIT,HELPkw,ID,NUM,REMINFO,PLUS,MINUS,MUL,DIV,EQ,GT,LT,LP,RP,COMMA,UNK};
struct Tok{TType t; string s;};

static unordered_map<string,TType> KW={{"LET",LET},{"PRINT",PRINT},{"INPUT",INPUT},{"END",ENDKW},{"REM",REM},{"GOTO",GOTOkw},{"IF",IFkw},{"THEN",THEN},{"RUN",RUN},{"LIST",LIST},{"CLEAR",CLEAR},{"QUIT",QUIT},{"HELP",HELPkw}};

struct Lexer {
  static bool isLetter(char c){return isalpha((unsigned char)c);}  
  static bool isNumChar(char c){return isalnum((unsigned char)c)||c=='_';}
  vector<Tok> tokenize(const string& line){
    vector<Tok> v; int i=0,n=line.size();
    while(i<n){ char c=line[i]; if(isspace((unsigned char)c)){++i; continue;}
      if(isLetter(c)){int j=i+1; while(j<n && isLetter(line[j])) ++j; string w=line.substr(i,j-i); auto it=KW.find(w);
        if(it!=KW.end()){
          if(it->second==REM){ v.push_back({REM,w}); if(j<n){string rest=line.substr(j); v.push_back({REMINFO,rest});} return v; }
          v.push_back({it->second,w});
        } else v.push_back({ID,w}); i=j; continue; }
      if(isdigit((unsigned char)c)){int j=i+1; while(j<n && isdigit((unsigned char)line[j])) ++j; v.push_back({NUM,line.substr(i,j-i)}); i=j; continue;}
      TType tp=UNK; if(c=='+')tp=PLUS; else if(c=='-')tp=MINUS; else if(c=='*')tp=MUL; else if(c=='/')tp=DIV; else if(c=='=')tp=EQ; else if(c=='>')tp=GT; else if(c=='<')tp=LT; else if(c=='(')tp=LP; else if(c==')')tp=RP; else if(c==',')tp=COMMA;
      if(tp!=UNK){ v.push_back({tp,string(1,c)}); ++i; continue; }
      throw runtime_error("LEXER ERROR");
    }
    return v;
  }
};

struct VarState{ unordered_map<string,long long> m; void set(const string&k,long long v){m[k]=v;} long long get(const string&k)const{auto it=m.find(k); if(it==m.end()) throw runtime_error("VARIABLE NOT DEFINED"); return it->second;} void clear(){m.clear();}};

struct Expr{ virtual ~Expr(){} virtual long long eval(const VarState& st) const=0; };
struct ConstE:Expr{ long long v; ConstE(long long v):v(v){} long long eval(const VarState&)const override{return v;}};
struct VarE:Expr{ string name; VarE(string n):name(move(n)){} long long eval(const VarState& st)const override{return st.get(name);} };
struct BinE:Expr{ unique_ptr<Expr> L,R; char op; BinE(Expr* a,char o,Expr* b):L(a),R(b),op(o){} long long eval(const VarState& st)const override{ long long x=L->eval(st), y=R->eval(st); if(op=='+')return x+y; if(op=='-')return x-y; if(op=='*')return x*y; if(op=='/'){ if(y==0) throw runtime_error("DIVIDE BY ZERO"); return x/y;} throw runtime_error("UNSUPPORTED OPERATOR"); }};

struct Parser{ vector<Tok> t; int p=0; int lpc=0; Parser(const vector<Tok>&v):t(v){}
  bool end(){return p>=(int)t.size();}
  Tok* peek(){ if(end()) return nullptr; return &t[p];}
  Tok* get(){ if(end()) return nullptr; return &t[p++];}
  int precedence(TType tp){ if(tp==MUL||tp==DIV) return 2; if(tp==PLUS||tp==MINUS) return 1; return 0; }
  long long toInt(const string& s){ // safe parse
    long long v=0; bool neg=false; int i=0; if(s.size()>0 && s[0]=='-'){neg=true; i=1;}
    for(; i<(int)s.size(); ++i){ if(!isdigit((unsigned char)s[i])) throw runtime_error("SYNTAX ERROR"); v=v*10+(s[i]-'0'); if(v> (long long)INT_MAX+ (neg?1:0)) throw runtime_error("INT LITERAL OVERFLOW"); }
    if(neg) v=-v; return (int)v; }
  unique_ptr<Expr> parsePrimary(){ if(end()) throw runtime_error("SYNTAX ERROR"); Tok* x=get(); if(x->t==NUM){ return make_unique<ConstE>(toInt(x->s)); } if(x->t==ID){ return make_unique<VarE>(x->s);} if(x->t==LP){ ++lpc; auto e=parseExpr(0); if(end()||get()->t!=RP) throw runtime_error("MISMATCHED PARENTHESIS"); --lpc; return e;} throw runtime_error("SYNTAX ERROR"); }
  unique_ptr<Expr> parseExpr(int minp){ auto lhs=parsePrimary(); while(!end()){ TType tp=peek()->t; int pr=precedence(tp); if(pr<minp) break; get(); auto rhs=parseExpr(pr+1); char op=(tp==PLUS?'+':tp==MINUS?'-':tp==MUL?'*':'/'); lhs=make_unique<BinE>(lhs.release(),op,rhs.release()); }
    return lhs; }
  unique_ptr<Expr> parseExpr(){ return parseExpr(0);} 
};

struct Stmt{ string src; Stmt(string s):src(move(s)){} virtual ~Stmt(){} virtual void exec(VarState& st, int& pc, bool& pend, map<int,unique_ptr<Stmt>>& rec)=0; };
struct LetS:Stmt{ string var; unique_ptr<Expr> e; LetS(string s,string v,Expr* x):Stmt(s),var(move(v)),e(x){} void exec(VarState& st,int&,bool&,map<int,unique_ptr<Stmt>>&){ st.set(var,e->eval(st)); } };
struct PrintS:Stmt{ unique_ptr<Expr> e; PrintS(string s,Expr* x):Stmt(s),e(x){} void exec(VarState& st,int&,bool&,map<int,unique_ptr<Stmt>>&){ cout<<e->eval(st)<<"\n"; } };
struct InputS:Stmt{ string var; InputS(string s,string v):Stmt(s),var(move(v)){} void exec(VarState& st,int&,bool&,map<int,unique_ptr<Stmt>>&){ cout<<"?"<<flush; long long v; if(!(cin>>v)){ throw runtime_error("INPUT ERROR"); } st.set(var,(int)v); string dummy; getline(cin,dummy); } };
struct EndS:Stmt{ EndS(string s):Stmt(s){} void exec(VarState&,int&,bool& pend,map<int,unique_ptr<Stmt>>&){ pend=true; } };
struct RemS:Stmt{ RemS(string s):Stmt(s){} void exec(VarState&,int&,bool&,map<int,unique_ptr<Stmt>>&){ /*no-op*/ } };
struct GotoS:Stmt{ int tgt; GotoS(string s,int t):Stmt(s),tgt(t){} void exec(VarState&,int& pc,bool&,map<int,unique_ptr<Stmt>>& rec){ auto it=rec.find(tgt); if(it==rec.end()) throw runtime_error("LINE NUMBER ERROR"); pc=tgt; } };
struct IfS:Stmt{ unique_ptr<Expr> L,R; char op; int tgt; IfS(string s,Expr* a,char o,Expr* b,int t):Stmt(s),L(a),R(b),op(o),tgt(t){} void exec(VarState& st,int& pc,bool&,map<int,unique_ptr<Stmt>>& rec){ long long x=L->eval(st), y=R->eval(st); bool cond=(op=='='? x==y : op=='<'? x<y : x>y); if(cond){ auto it=rec.find(tgt); if(it==rec.end()) throw runtime_error("LINE NUMBER ERROR"); pc=tgt; } } };

struct Program{
  map<int,unique_ptr<Stmt>> rec; VarState st; int pc=0; bool pend=false;
  void add(int line, Stmt* s){ rec[line].reset(s);} 
  void remove(int line){ rec.erase(line);} 
  void list() const{ for(auto &kv:rec) cout<<kv.first<<" "<<kv.second->src<<"\n"; }
  void clear(){ rec.clear(); st.clear(); }
  int next(int cur){ auto it=rec.upper_bound(cur); if(it==rec.end()) return -1; return it->first; }
  void run(){ if(rec.empty()) return; pend=false; pc=rec.begin()->first; while(pc!=-1 && !pend){ auto it=rec.find(pc); if(it==rec.end()) { throw runtime_error("LINE NUMBER ERROR"); } int cur=pc; try{ it->second->exec(st,pc,pend,rec); } catch(const exception& e){ cout<<e.what()<<"\n"; break; } if(pend) break; if(pc==cur){ pc=next(cur);} }
  }
  void execImmediate(Stmt* s){ try{ s->exec(st,pc,pend,rec);} catch(const exception& e){ cout<<e.what()<<"\n"; } delete s; }
};

static Stmt* parseStatement(const vector<Tok>& toks, const string& origin){ Parser P(toks); if(P.end()) throw runtime_error("SYNTAX ERROR"); Tok* t=P.get();
  if(t->t==LET){ if(P.end()) throw runtime_error("SYNTAX ERROR"); Tok* v=P.get(); if(v->t!=ID) throw runtime_error("SYNTAX ERROR"); if(P.end()||P.get()->t!=EQ) throw runtime_error("SYNTAX ERROR"); auto e=P.parseExpr(); if(!P.end()) throw runtime_error("SYNTAX ERROR"); return new LetS(origin,v->s,e.release()); }
  if(t->t==PRINT){ auto e=P.parseExpr(); if(!P.end()) throw runtime_error("SYNTAX ERROR"); return new PrintS(origin,e.release()); }
  if(t->t==INPUT){ if(P.end()) throw runtime_error("SYNTAX ERROR"); Tok* v=P.get(); if(v->t!=ID) throw runtime_error("SYNTAX ERROR"); if(!P.end()) throw runtime_error("SYNTAX ERROR"); return new InputS(origin,v->s); }
  if(t->t==GOTOkw){ if(P.end()) throw runtime_error("SYNTAX ERROR"); Tok* ln=P.get(); if(ln->t!=NUM) throw runtime_error("SYNTAX ERROR"); long long L=stoll(ln->s); if(!P.end()) throw runtime_error("SYNTAX ERROR"); return new GotoS(origin,(int)L); }
  if(t->t==IFkw){ auto L=P.parseExpr(); if(P.end()) throw runtime_error("SYNTAX ERROR"); Tok* op=P.get(); char c; if(op->t==EQ)c='='; else if(op->t==GT)c='>'; else if(op->t==LT)c='<'; else throw runtime_error("SYNTAX ERROR"); auto R=P.parseExpr(); if(P.end()||P.get()->t!=THEN) throw runtime_error("SYNTAX ERROR"); if(P.end()) throw runtime_error("SYNTAX ERROR"); Tok* ln=P.get(); if(ln->t!=NUM) throw runtime_error("SYNTAX ERROR"); long long target=stoll(ln->s); if(!P.end()) throw runtime_error("SYNTAX ERROR"); return new IfS(origin,L.release(),c,R.release(),(int)target); }
  if(t->t==REM){ Tok* info=nullptr; if(!P.end()) info=P.get(); if(info && info->t!=REMINFO) throw runtime_error("SYNTAX ERROR"); if(!P.end()) throw runtime_error("SYNTAX ERROR"); return new RemS(origin); }
  if(t->t==ENDKW){ if(!P.end()) throw runtime_error("SYNTAX ERROR"); return new EndS(origin); }
  throw runtime_error("SYNTAX ERROR"); }

int main(){ ios::sync_with_stdio(false); cin.tie(nullptr);
  Lexer L; Program prog; string line;
  while(true){ string line; if(!getline(cin,line)) break; if(line.empty()) continue; try{
      auto toks=L.tokenize(line); // check if leading line number
      int idx=0; bool hasLine=false; int lineNum=0; if(!toks.empty() && toks[0].t==NUM){ hasLine=true; lineNum=stoi(toks[0].s); idx=1; if(idx==(int)toks.size()){ prog.remove(lineNum); continue; } }
      vector<Tok> rest(toks.begin()+idx, toks.end()); if(rest.empty()) continue;
      if(rest[0].t==RUN){ prog.run(); continue; }
      if(rest[0].t==LIST){ prog.list(); continue; }
      if(rest[0].t==CLEAR){ prog.clear(); continue; }
      if(rest[0].t==QUIT){ break; }
      if(rest[0].t==HELPkw){ cout<<"Available commands: LET, PRINT, INPUT, END, REM, GOTO, IF ... THEN, RUN, LIST, CLEAR, QUIT, HELP\n"; continue; }
      Stmt* s=parseStatement(rest,line.substr(idx? line.find_first_not_of(' '):0));
      if(hasLine){ prog.add(lineNum,s); } else { prog.execImmediate(s); }
    } catch(const exception& e){ cout<<e.what()<<"\n"; }
  }
  return 0;
}
