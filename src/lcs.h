#ifndef LCS_H
#define LCS_H

#include <vector>
#include <string>
#include <set>
#include <iostream>

class LCS
{
    enum Condition { NONE, POSITIVE, NEGATIVE };

    const double MAX_FITNESS = 1.0;

    struct Rule
    {
        std::vector<Condition> condition;
        bool result;
        int numerosity;
        int64_t age;
        int64_t nPredictions;
        int64_t correctPredictions;
        double fitness;

        Rule();
    };

    int NC;
    int maxRules;
    int steps;
    std::string labelText;
    std::vector<std::string> paramsText;
    std::vector<Rule> rules;

    int wheelSelection() const;
    void updateRule(Rule& rule, bool label);
    void printExample(bool label, const std::vector<bool>& params, std::ostream& out = std::cerr) const;
    void printRule(const Rule& rule, std::ostream& out = std::cerr) const;
    void match(const std::vector<bool>& params, std::set<int>& matches) const;
    void learnStep(bool label, const std::vector<bool>& params, const std::set<int>& matches);
    bool isCovered(bool label, const std::set<int>& matches) const;
    void addCoveringRule(bool label, const std::vector<bool>& params);
    void subsumption();
    void geneticStep();
    void deletionStep();
    int rnd(int n) const;
    double rnd() const;

    public:
    bool DoLearning;

    LCS();
    void setParams(const std::string& label, const std::vector<std::string>& params);
    void resetStats();
    void init(int max_rules = 10);
    void learn(bool label, const std::vector<bool>& params);
    void print(std::ostream& out = std::cerr) const;
};

#endif
