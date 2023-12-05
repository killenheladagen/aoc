#include <algorithm>
#include <iostream>
#include <vector>
#include <cstdint>

using namespace std;

#if 0
#include "5bdata-1.h"
#else
#include "5bdata.h"
#endif

struct Range {
  Range(int64_t start,int64_t end) : start(start),end(end){}
  
  int64_t start,end;

  auto length() const { return end-start; }
};

bool operator<(Range const& a, Range const& b) {
  return a.start < b.start;
}

auto v2range(vector<int64_t> v) {
  vector<Range> r;
  for(int i=0;i<v.size();i+=2) {
    r.push_back(Range{v[i],v[i]+v[i+1]});
  }
  return r;
}

auto inside_and_outside(Range r, Range comp) {
  vector<Range> inside;
  vector<Range> outside;
  if (r.end <= comp.start|| r.start >= comp.end) {
    //cout << " (all out) ";
    outside.push_back(r);
  }
  else if (r.start >= comp.start && r.end <= comp.end) {
    // cccccRRRRRRccccc
    cout << " (all in) ";
    inside.push_back(r);
  }
  else if (r.start < comp.start && r.end > comp.end) {
    // RRRRccccccRRRR
    cout << " (B) ";
    outside.push_back(Range{r.start,comp.start});
    inside.push_back(comp);
    outside.push_back(Range{comp.end,r.end});
  }
  else if (r.start<comp.start) {
    //        ccccccccc
    //     RRRRRRRR
    cout << " (C) ";
    outside.push_back(Range{r.start, min(comp.start,r.end)});
    inside.push_back(Range{comp.start,min(comp.end,r.end)});
  }
  else if(r.end>comp.end) {
    // cccccccccc
    //     RRRRRRRRRRR
    cout << " (D) ";
    inside.push_back(Range{max(comp.start,r.start),comp.end});
    outside.push_back(Range{comp.end,r.end});
  }
  else {
    cout << " (E) ";
  }
  return make_tuple(inside,outside);
}

auto& operator<<(ostream& os, Range const& x) {
  if(x.length()==1)
    return os << "[" << x.start << "]";
  else
    return os << "[" << x.start << "," << x.end << ")" << "#" << x.length();
}

auto& operator<<(ostream& os, vector<Range> const& x) {
  if(x.empty())
    os << "{Ø}";
  else {
  os << "{";
  int64_t tot=0;
  for(auto const& e: x) {
    os << e << ", ";
    tot+=e.length();
  }
  os << "}";
  os << " #" << tot;
  }
  return os;
}

auto& operator<<(ostream& os, vector<int64_t> const& v) {
  os << "[";
  for(auto const& e:v) {
    os << e << ", ";
  }
  os << "]";
  return os;
}

vector<Range> foo(vector<Range> input, vector<int64_t> const& m) {
  vector<Range> result;
  while(true) {
    bool something_changed=false;
    for(int i=0;i<m.size();i+=3) {
      Range mr{m[i+1],m[i+1]+m[i+2]};
      auto offs=m[i]-m[i+1];
      cout << "Map:" << mr << "\n";
      vector<Range> new_input;
      for(auto input_elem:input) {
	auto [new_mapped,new_input_elems]=inside_and_outside(input_elem,mr);
	cout << "To map: " << new_mapped << "\n";
	for(auto &e: new_mapped) {
	  e.start+=offs;
	  e.end+=offs;
	}
	if(new_mapped.size()) {
	  cout << "map " << input_elem << " using " << mr << ":" << offs << " =>\n ";
	  cout << new_mapped << " + kept: " << new_input_elems << "\n\n";
	  something_changed=true;
	}
	else {
	  //cout << "{0}\n";
	}
	copy(new_input_elems.begin(),new_input_elems.end(),back_inserter(new_input));
	copy(new_mapped.begin(),new_mapped.end(),back_inserter(result));
      }
      input=new_input;
    }
    if(!something_changed) {
      copy(input.begin(),input.end(),back_inserter(result));
      return result;
    }
  }
}

int main() {
  auto rseeds=v2range(seeds);
  cout << "Seeds: " << rseeds << "\n";
  for(auto m:mappings) {
    cout << "\n";
    cout << "Applying " << m << "\n";
    rseeds=foo(rseeds, m);
    cout << "After foo\n";
    cout << "  " << rseeds << "\n";
    cout << "\n";
  }
  cout << min_element(rseeds.begin(),rseeds.end())->start << "\n";
  return 0;
}

/*

With this map, you can look up the soil number required for each initial seed number:

Seed number 79 corresponds to soil number 81.
Seed number 14 corresponds to soil number 14.
Seed number 55 corresponds to soil number 57.
Seed number 13 corresponds to soil number 13.
The gardener and his team want to get started as soon as possible, so they'd like to know the closest location that needs a seed. Using these maps, find the lowest location number that corresponds to any of the initial seeds. To do this, you'll need to convert each seed number through other categories until you can find its corresponding location number. In this example, the corresponding types are:

Seed 79, soil 81, fertilizer 81, water 81, light 74, temperature 78, humidity 78, location 82.
Seed 14, soil 14, fertilizer 53, water 49, light 42, temperature 42, humidity 43, location 43.
Seed 55, soil 57, fertilizer 57, water 53, light 46, temperature 82, humidity 82, location 86.
Seed 13, soil 13, fertilizer 52, water 41, light 34, temperature 34, humidity 35, location 35.
So, the lowest location number in this example is 35.
 */
