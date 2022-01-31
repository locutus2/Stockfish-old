#ifndef QLEARN_H
#define QLEARN_H

#include <map>
#include <vector>
#include <iostream>
#include "misc.h"

class Action
{
    int red;

    public:
    Action(int r = 0) : red(r) {}
    int getRed() const { return red; }
    bool operator<(const Action& a) const { return red < a.red; }
    bool operator>(const Action& a) const { return red > a.red; }
};

class State
{
    int state = 0;

    public:
    std::vector<Action> getActions() const
    {
        return { Action(-1), Action(0), Action(1) };
    }
    int getState() const { return state; }
    bool operator<(const State& s) const { return state < s.state; }
    bool operator>(const State& s) const { return state > s.state; }

    Action randomAction() const
    {
        std::vector<Action> a = getActions();
	return a[std::rand() % a.size()];
    }
};

template <typename S = State, typename A = Action>
class Environment
{
    public:
    double execute(const S& s0, const A& a, S& s1)
    {
        s1 = s0;
        return 0;
    }
};

template <typename S = State, typename A = Action>
class Policy
{
    public:
};

template <typename S = State, typename A = Action, typename P = Policy<S, A>, typename E = Environment<S, A>>
class QLearn
{
    static constexpr double DISCOUNT = 0.9;
    static constexpr double INIT_VALUE = 0.0;
    static constexpr int PRINT_ITERATION = 10000;

    struct Entry : private std::pair<int64_t, double>
    {
        Entry(double v = 0, int64_t n = 0) : std::pair<int64_t, double>(n, v) {}
        int count() const { return first; }
        double value() const { return second; }
        void inc_count(int i = 1) { first += i; }
        void set_value(double v) { second = v; }
        bool operator<(const Entry& e) const { return second < e.second; }
        bool operator>(const Entry& e) const { return second > e.second; }
    };

    int iteration;
    std::map<State, std::map<Action, Entry>> Q;

    public:
    QLearn()
    {
        init();
    }

    void init()
    {
        iteration = 0;
        Q.clear();
    }

    void update(const S& s0, const A&a, const S&s1, double reward)
    {
        Stockfish::dbg_mean_of(reward);

        ++iteration;
        Q[s0][a].inc_count();
        double alpha = 1.0 / Q[s0][a].count();
        double q = Q[s0][a].value();
	double maxQ = bestActionValue(s1);
        double d = alpha * (reward + DISCOUNT * maxQ - q);
	//std::cerr << "# S0=" << s0.getState() << " A=" << a.getRed() << " S1=" << s1.getState() << " Q=" << q << " r=" << reward << " maxQ=" << maxQ;
        q += d;
        Q[s0][a].set_value(q);
	//std::cerr << " => " << q << " " << Q[s0][a].value() << std::endl;

	print();
    }

    double bestActionValue(const S& s) 
    {
        auto best = std::max_element(Q[s].begin(), Q[s].end(), [](auto& a, auto& b) { return a.second.value() < b.second.value(); });
        return best != Q[s].end() ? best->second.value() : INIT_VALUE;
    }

    Action bestAction(const S& s) 
    {
        auto best = std::max_element(Q[s].begin(), Q[s].end(), [](auto& a, auto& b) { return a.second.value() < b.second.value(); });
        return best != Q[s].end() ? best->first : s.randomAction();
    }

    Action offPolicyAction(const S& s)
    {
        return s.randomAction();
    }

    void print(const S& s = S(), std::ostream& out = std::cerr)
    {
        if(iteration % PRINT_ITERATION == 0)
        {
            out << iteration << ";"
                << Q[s][Action(-1)].value() << ";" 
                << Q[s][Action(0)].value()  << ";" 
                << Q[s][Action(1)].value()  << std::endl;
        }
    }
};

using Learn = QLearn<>;

#endif
