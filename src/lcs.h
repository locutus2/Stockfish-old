#ifndef LCS_H
#define LCS_H

#include <vector>
#include <string>
#include <set>
#include <iostream>

class LCS
{
    enum Condition { NONE, POSITIVE, NEGATIVE };

    const int MAX_CONDITIONS = 5;
    const bool USE_GENERALIZATION = MAX_CONDITIONS > 0;
    const bool USE_SUBSUMPTION = false;
    const bool ONLY_CORRECT_MUTATIONS = true;
    const bool COVER_ONLY_ONE_CLAUSE = false;
    const double RANDOM_SKIPPING = 0.5;
    const double MAX_FITNESS = 1.0;
    const double MIN_FITNESS = 0.0;
    const double MIN_COVERAGE = 0.01;
    //const double MUTATION = 0.05;
    const double MUTATION = 0.01;

    const bool DEBUG = false;

    struct Rule
    {
        static int nextId;
        int id;
        std::vector<Condition> condition;
        bool result;
        int numerosity;
        int64_t age;
        int64_t nPredictions;
        int64_t correctPredictions;
        double accuracy;
        double fitness;
        double coverage;

        Rule();
        int countConditions() const;
        bool operator==(const Rule& rule) const; 
    };

    bool topToBottom;
    int NC;
    int maxRules;
    int steps;
    int nLearned;
    std::string labelText;
    std::vector<std::string> paramsText;
    std::vector<Rule> rules;
    std::vector<std::vector<Rule>> savedRules;

    template <typename Filter, typename Weight>
    int wheelSelection(const Filter& filter, const Weight& weight) const;

    bool hasDuplicate(const Rule& rule) const;
    void removeDuplicates();
    void generalizationStep(bool label, const std::set<int>& matches);
    void addGeneralizedRule(const Rule & rule);
    double calculateSubsumptionFitness(const Rule& rule) const;
    double calculateFitness(const Rule& rule) const;
    bool subsumpRule(const Rule& gRule, const Rule& sRule) const;
    void mutate(Rule& rule, const std::vector<bool>& params) const;
    void copyRule(const Rule& r1, Rule& r2) const;
    void crossover(Rule& r1, Rule& r2) const;
    int wheelSelectionBest(bool label, const std::set<int>& matches, int excludedRule = -1) const;
    int wheelSelectionWorst() const;
    void updateRule(Rule& rule, bool label);
    void printExample(bool label, const std::vector<bool>& params, std::ostream& out = std::cerr) const;
    void printRule(const Rule& rule, std::ostream& out = std::cerr) const;
    void match(const std::vector<bool>& params, std::set<int>& matches) const;
    bool matchRule(const std::vector<bool>& params, const Rule& rule) const;
    void learnStep(bool label, const std::vector<bool>& params, const std::set<int>& matches);
    bool isCovered(bool label, const std::set<int>& matches) const;
    void addCoveringRule(bool label, const std::vector<bool>& params);
    bool subsumption();
    void ruleDiscoveryStep(bool label, const std::vector<bool>& params, const std::set<int>& matches);
    int rnd(int n) const;
    double rnd() const;
    void simplifyRule(Rule& rule) const;

    public:
    bool DoLearning;
    int maxConditions;
    static std::string preconditionText;

    LCS();
    void deletionStep();
    void setParams(const std::string& label, const std::vector<std::string>& params);
    void resetStats();
    void learn();
    void storeRules();
    void restoreRules();
    void init(int max_rules = 10, bool top_to_bottom = false);
    void learn(bool label, const std::vector<bool>& params);
    void print(bool sort = true, bool pareto = false, std::ostream& out = std::cerr);
    void printAttrStats(std::ostream& out = std::cerr);
};

#define LCS_PRECONDITION(x) (LCS::preconditionText = #x, (x))

#endif
