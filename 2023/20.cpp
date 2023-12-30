#include "helpers.h"
using namespace std;

bool g_verbose = true;

int low_count;
int high_count;
bool rx_low_received = false;

struct pulse {
    string source;
    string dest;
    bool high;
};

struct module {
    module(string const &name, vector<string> const &dest) :name(name),
        destinations(dest) {}

    virtual ~module() = default;

    string name;
    vector<string> destinations;

    vector<pulse> process_pulse(bool high, string const &source) {
        if (high)
            high_count++;
        else
            low_count++;
        if (g_verbose)
            cerr << "\n  " << source << " -" << (high ? "high" : "low") << "-> "
                 << name;
        return do_process_pulse(high, source);
    }

    virtual void reg_input(string const &source) {}

  private:
    virtual vector<pulse> do_process_pulse(bool high, string const &source) = 0;
};

map<string, unique_ptr<module>> all_modules;

auto build_pulses(module const &source, vector<string> const &dest, bool high) {
    vector<pulse> res;
    for (auto &m : dest) {
        res.emplace_back(source.name, m, high);
    }
    return res;
}

struct dummy : module {
    dummy(string const &name) : module(name, {}) {
        cerr << "\nCreated dummy " << name;
    }

    vector<pulse> do_process_pulse(bool high, string const &source) override {
        if (!high) {
            cerr << "\n\n"
                 << name << " received " << (high ? "high" : "low") << " from "
                 << source << "\n";
            rx_low_received = true;
        }
        return vector<pulse>{};
    }
};

void play(vector<pulse> input) {
    vector<pulse> result;
    for (auto x : input) {
        auto &dest = all_modules[x.dest];
        if (!dest.get())
            dest = make_unique<dummy>(x.dest);
        for (auto p : dest->process_pulse(x.high, x.source))
            result.push_back(p);
    }
    if (!result.empty())
        play(result);
}

struct flip_flop : module {
    flip_flop(string const &name, vector<string> const &dest)
        : module(name, dest) {}

    vector<pulse> do_process_pulse(bool high, string const &source) override {
        if (!high) {
            state_on = !state_on;
            return build_pulses(*this, destinations, state_on);
        }
        return vector<pulse>{};
    }

    bool state_on = false;
};

struct conjmod : module {
    conjmod(string const &name, vector<string> const &dest)
        : module(name, dest) {}

    void reg_input(string const &source) { last_received[source] = false; }

    vector<pulse> do_process_pulse(bool high, string const &source) override {
        last_received[source] = high;
        return build_pulses(*this, destinations,
                            !all_of(last_received.begin(), last_received.end(),
                                    [](auto &e) { return e.second; }));
    }

    map<string, bool> last_received;
};

struct broadcaster : module {
    broadcaster(vector<string> const &dest) : module("broadcaster", dest) {}

    vector<pulse> do_process_pulse(bool high, string const &source) override {
        return build_pulses(*this, destinations, high);
    }
};

unique_ptr<module> make_module(string const &line) {
    auto v = split(line, ',');
    auto name = v[0].substr(1);
    auto dest = v;
    dest.erase(dest.begin());

    unique_ptr<module> m;
    if (v[0][0] == '%')
        m = make_unique<flip_flop>(name, dest);
    else if (v[0][0] == '&')
        m = make_unique<conjmod>(name, dest);
    else if (v[0] == "broadcaster")
        m = make_unique<broadcaster>(dest);
    else {
        cerr << "Illegal module name\n";
        abort();
    }
    return m;
}

void foo(bool verbose, bool wait_for_rx, vector<int> const &expected) {
    auto exp_it = expected.begin();

    for (int i = 0; i < 3; i++) {
        g_verbose = verbose;
        all_modules.clear();
        high_count = 0;
        low_count = 0;
        cerr << "\n";
        for (auto &line :
             read_file_as_string_vector("20-"s + to_string(i) + ".txt")) {
            cerr << "\n" << line << " => ";
            auto m = make_module(line);
            cerr << m->name << " dest=" << m->destinations;
            all_modules[m->name] = move(m);
        }
        cerr << "\n";
        for (auto &src : all_modules) {
            for (auto &dest : src.second->destinations) {
                cerr << "\n " << dest << ".reg_input(" << src.first << ")";
                auto it = all_modules.find(dest);
                if (it != all_modules.end())
                    it->second->reg_input(src.first);
            }
        }
        cerr << "\n";
        pulse start("button", "broadcaster", false);
        if (wait_for_rx && i == 2) {
            rx_low_received = false;
            g_verbose = false;
            int num_buttons = 1;
            while (!rx_low_received) {
                play({start});
                num_buttons++;
            }
            cerr << "\nrx_low_receved after " << num_buttons
                 << " button presses\n";
        } else {
            for (int j = 0; j < 1000; j++) {
                if (g_verbose)
                    cerr << "\n";
                play({start});
                g_verbose = j < i;
            }
        }
        auto res = high_count * low_count;
        cerr << "\n\nhigh * low = " << res;

        if (exp_it != expected.end()) {
            if (*exp_it != res) {
                cerr << "\n\nExpected " << *exp_it << " but counted " << res
                     << "\n";
                exit(1);
            }
            exp_it++;
        }
    }
}

int main() {
    foo(true, false, {32000000, 11687500, 856482136});
    foo(true, true, {});
    return 0;
}
