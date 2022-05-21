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

void LCS::geneticStep()
{
}

int LCS::wheelSelection() const
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
        int r = wheelSelection();
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
        geneticStep();
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

void LCS::match(const std::vector<bool>& params, std::set<int>& matches) const
{
    assert((int)params.size() == NC);
    for (int i = 0; i < (int)rules.size(); ++i)
    {
        assert((int)rules[i].condition.size() == NC);
        bool isMatch = true;
        for(int j = 0; j < NC; ++j)
        {
            if (params[j] && rules[i].condition[j] == NEGATIVE)
            {
                isMatch = false;
                break;
            }

            if (!params[j] && rules[i].condition[j] == POSITIVE)
            {
                isMatch = false;
                break;
            }
        }

        if (isMatch)
            matches.insert(i);
    }
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
