#include <cassert>
#include <cstdlib>
#include <iomanip>
#include <algorithm>
#include <cmath>
#include "lcs.h"

// LCS learning (see https://en.wikipedia.org/wiki/Learning_classifier_system

LCS::Rule::Rule() : numerosity(1), age(0), nPredictions(0), correctPredictions(0), fitness(0)
{
}

LCS::LCS() : NC(0), DoLearning(true)
{
    std::srand(123); // e123.txt
    //std::srand(124); // e124.txt
}

int LCS::rnd(int n) const
{
    return std::rand() % n;
}

double LCS::rnd() const
{
    return std::rand() / (double)RAND_MAX;
}

void LCS::printRule(const Rule& rule, std::ostream& out) const
{
    assert(rule.condition.size() == paramsText.size());

    double accuracy = (rule.nPredictions ? 100.0 * rule.correctPredictions / rule.nPredictions : 0.0);
    out << "[accuracy: " << rule.correctPredictions << "/" << rule.nPredictions << "=" << std::fixed << std::setprecision(2) << accuracy << "%,"
        << "|fitness: " << rule.fitness
        << "|age: "     << rule.age
        << "|numerosity: " << rule.numerosity
        << "]";

    out << " IF";

    int count = 0;
    for(int i = 0; i < NC; ++i)
    {
        if(rule.condition[i] == POSITIVE)
        {
            if (count++ > 0)
                out << " AND";
            out << " " << paramsText[i];
        }
        else if(rule.condition[i] == NEGATIVE)
        {
            if (count++ > 0)
                out << " AND";
            out << " NOT(" << paramsText[i] << ")";
        }
    }

    if (count == 0)
        out << " true";

    out << " THEN";
    if(rule.result)
        out << " " << labelText;
    else
        out << " NOT(" << labelText << ")";

    out << std::endl;
}

void LCS::setParams(const std::string& label, const std::vector<std::string>& params)
{
    NC = params.size();
    labelText = label;
    paramsText = params;
}

void LCS::init(int max_rules)
{
    maxRules = max_rules;
    rules.clear();
    steps = 0;
}

void LCS::subsumption()
{
}

void LCS::copyRule(const Rule& r1, Rule& r2) const
{
    r2 = r1;
}

void LCS::crossover(Rule& r1, Rule& r2) const
{
    for (int i = 0; i < NC; ++i)
    {
        if(rnd(2) == 0)
        {
            std::iter_swap(r1.condition.begin()+i, r2.condition.begin()+i);
        }
    }
    r1.age = r2.age = 0;
    r1.nPredictions = r2.nPredictions = 0;
    r1.correctPredictions = r2.correctPredictions = 0;
    r1.fitness = r2.fitness = 0;
    r1.numerosity = r2.numerosity = 1;
}

void LCS::mutate(Rule& rule) const
{
    for (int i = 0; i < NC; ++i)
        if (rnd() < MUTATION)
            rule.condition[i] = (Condition)rnd(3);
}

void LCS::ruleDiscoveryStep(bool label, const std::vector<bool>& params, const std::set<int>& matches)
{
    int parent1 = wheelSelectionBest(label, matches);
    if(parent1 < 0) return;
    int parent2 = wheelSelectionBest(label, matches, parent1);
    if(parent2 < 0 || parent1 == parent2) return;

    Rule child1, child2;

    copyRule(rules[parent1], child1);
    copyRule(rules[parent2], child2);
    crossover(child1, child2);
    mutate(child1);
    mutate(child2);

    if (matchRule(params, child1))
    {
        updateRule(child1, label);
        rules.push_back(child1);
    }

    if (matchRule(params, child2))
    {
        updateRule(child2, label);
        rules.push_back(child2);
    }
}

int LCS::wheelSelectionBest(bool label, const std::set<int>& matches, int excludedRule) const
{
    std::vector<int> correct;

    for (int r : matches)
    {
        if(rules[r].result == label && r != excludedRule)
            correct.push_back(r);
    }

    if(correct.empty()) return -1;

    std::vector<double> prob(rules.size());

    double sum = 0;
    for (int i = 0; i < (int)correct.size(); ++i)
        //sum += prob[i] = std::max(0.0, std::exp(-rules[i].fitness) - std::exp(-MAX_FITNESS));
        sum += prob[i] = rules[correct[i]].fitness - MIN_FITNESS;

    double rand = rnd();
    for (int i = 0; i < (int)correct.size(); ++i)
    {
       rand -= prob[i] / sum;
       if (rand <= 0)
           return correct[i];
    }


    return correct[rnd(correct.size())];
}

int LCS::wheelSelectionWorst() const
{
    std::vector<double> prob(rules.size());

    double sum = 0;
    for (int i = 0; i < (int)rules.size(); ++i)
        //sum += prob[i] = std::max(0.0, std::exp(-rules[i].fitness) - std::exp(-MAX_FITNESS));
        sum += prob[i] = MAX_FITNESS - rules[i].fitness;

    double rand = rnd();
    for (int i = 0; i < (int)rules.size(); ++i)
    {
       rand -= prob[i] / sum;
       if (rand <= 0)
           return i;
    }


    return rnd(rules.size());
}

void LCS::deletionStep()
{
    while ((int)rules.size() > maxRules)
    {
        int r = wheelSelectionWorst();
        if(--rules[r].numerosity <= 0)
            rules.erase(rules.begin() + r);
    }
}

void LCS::resetStats()
{
    for (Rule& r : rules)
    {
        r.nPredictions = 0;
        r.correctPredictions = 0;
        r.fitness = 0;
    }
}

void LCS::learn(bool label, const std::vector<bool>& params)
{
    assert(params.size() == paramsText.size());

    std::set<int> matches;
    match(params, matches);

    for(int r : matches)
        updateRule(rules[r], label);

    if (DoLearning)
    {
        learnStep(label, params, matches);
        subsumption();
        ruleDiscoveryStep(label, params, matches);
        deletionStep();
    }

    ++steps;

    if (steps % 1000000 == 0)
    {

        //printExample(label, params);
        //print();
    }
}

void LCS::updateRule(Rule& rule, bool label)
{
    if (label == rule.result)
        ++rule.correctPredictions;

    ++rule.nPredictions;
    rule.fitness = 100.0 * rule.correctPredictions / rule.nPredictions;
}

void LCS::learnStep(bool label, const std::vector<bool>& params, const std::set<int>& matches)
{
    for (Rule& r : rules)
        ++r.age;

    if(!isCovered(label, matches))
    {
        addCoveringRule(label, params);
    }
}

bool LCS::matchRule(const std::vector<bool>& params, const Rule& rule) const
{
    assert((int)rule.condition.size() == NC);

    bool isMatch = true;
    for(int j = 0; j < NC; ++j)
    {
        if (params[j] && rule.condition[j] == NEGATIVE)
        {
            isMatch = false;
            break;
        }

        if (!params[j] && rule.condition[j] == POSITIVE)
        {
            isMatch = false;
            break;
        }
    }
    return isMatch;
}

void LCS::match(const std::vector<bool>& params, std::set<int>& matches) const
{
    assert((int)params.size() == NC);
    for (int i = 0; i < (int)rules.size(); ++i)
        if (matchRule(params, rules[i]))
            matches.insert(i);
}

bool LCS::isCovered(bool label, const std::set<int>& matches) const
{
    for(int r : matches)
        if(rules[r].result == label)
            return true;
    return false;
}

void LCS::addCoveringRule(bool label, const std::vector<bool>& params)
{
    Rule rule;

    rule.result = label;
    rule.condition.resize(NC);
    for (int i = 0; i < NC; ++i)
    {
        if (rnd(2) == 0)
        {
            if (params[i])
                rule.condition[i] = POSITIVE;
            else
                rule.condition[i] = NEGATIVE;
        }
        else
            rule.condition[i] = NONE;
    }
    updateRule(rule, label);

    rules.push_back(rule);
}

void LCS::printExample(bool label, const std::vector<bool>& params, std::ostream& out) const
{
    out << "-------------------" << std::endl;
    out << labelText << "=" << (int)label << " <= ";
    for(int i = 0; i < NC; ++i)
    {
        if(i) out << ",";
        out << " " << paramsText[i] << "=" << (int)params[i];
    }
    out << std::endl;
}

void LCS::print(bool sort, std::ostream& out)
{
    if(sort)
        std::stable_sort(rules.begin(), rules.end(), [](const Rule& a, const Rule& b) { return   a.fitness > b.fitness
                                                                                          || (a.fitness == b.fitness && a.nPredictions > b.nPredictions); } );
    out << "--------- step " << steps << " ----------" << std::endl;
    for(int i = 0; i < (int)rules.size(); ++i)
    {
        out << i+1 << ". ";
        printRule(rules[i], out);
    }
}
