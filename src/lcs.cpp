#include <cassert>
#include <cstdlib>
#include <iomanip>
#include <algorithm>
#include <cmath>
#include <ctime>
#include "lcs.h"
#include "misc.h"

// LCS learning (see https://en.wikipedia.org/wiki/Learning_classifier_system

LCS::Rule::Rule() : numerosity(1), age(0), nPredictions(0), correctPredictions(0), accuracy(0), fitness(0), coverage(0)
{
}

LCS::LCS() : NC(0), nLearned(0), DoLearning(true)
{
    //std::srand(123); // e123.txt
    //std::srand(124); // e124.txt
    std::srand(std::time(nullptr));
}

int LCS::rnd(int n) const
{
    return std::rand() % n;
}

double LCS::rnd() const
{
    return std::rand() / (double)RAND_MAX;
}

double LCS::calculateFitness(const Rule& rule) const
{
    double val = 0;
    val = 100.0 * rule.accuracy;
    //val = 100.0 * rule.accuracy * rule.coverage;
    return val;
}

double LCS::calculateSubsumptionFitness(const Rule& rule) const
{
    double val = 0;
    //val = 100.0 * rule.accuracy * (rule.coverage < MIN_COVERAGE ? rule.coverage / MIN_COVERAGE : 1.0);
    val = 100.0 * rule.accuracy;
    return val;
}

void LCS::storeRules()
{
    savedRules.push_back(rules);
}

void LCS::restoreRules()
{
    if (!savedRules.empty())
    {
        rules = savedRules.back();
        savedRules.pop_back();
    }
}

void LCS::printRule(const Rule& rule, std::ostream& out) const
{
    assert(rule.condition.size() == paramsText.size());

    out << "[accuracy: " << rule.correctPredictions << "/" << rule.nPredictions << "=" << std::fixed << std::setprecision(2) << 100 * rule.accuracy << "%"
        << "|fitness: " << rule.fitness
        << "|coverage: " << std::fixed << std::setprecision(2) << 100 * rule.coverage << "%"
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
    nLearned = 0;
}

bool LCS::subsumption()
{
    bool deleted = false;
    for (int i = 0; i < (int)rules.size(); ++i)
    {
        for (int j = (int)rules.size()-1; j > i; --j)
        {
            if (subsumpRule(rules[i], rules[j]))
            {
                rules[i].numerosity += rules[j].numerosity;
                //std::cerr << "subsum: general=";
                //printRule(rules[i]);
                //std::cerr << "subsum: special=";
                //printRule(rules[j]);
                rules.erase(rules.begin()+j);
                deleted = true;
            }
            else if (subsumpRule(rules[j], rules[i]))
            {
                rules[j].numerosity += rules[i].numerosity;
                //std::cerr << "subsum: general=";
                //printRule(rules[j]);
                //std::cerr << "subsum: special=";
                //printRule(rules[i]);
                rules.erase(rules.begin()+i);
                --i;
                deleted = true;
                break;
            }
        }
    }
    return deleted;
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
    r1.numerosity = r2.numerosity = 1;
    r1.fitness = r2.fitness = MIN_FITNESS;
}

void LCS::mutate(Rule& rule, const std::vector<bool>& params) const
{
    for (int i = 0; i < NC; ++i)
        if (rnd() < MUTATION)
        {
            if (ONLY_CORRECT_MUTATIONS)
                rule.condition[i] = rule.condition[i] == NONE ? (params[i] ? POSITIVE : NEGATIVE) : NONE;
            else
                rule.condition[i] = (Condition)((rule.condition[i] + 1 + rnd(2))%3);
                //rule.condition[i] = (Condition)rnd(3);
        }
}

void LCS::addGeneralizedRule(const Rule & rule)
{
    Rule r = rule;
    std::vector <int> cond;
    for(int i = 0; i < NC; ++i)
        if (rule.condition[i] != NONE)
            cond.push_back(i);

    if(!cond.empty())
    {
        r.condition[rnd(cond.size())] = NONE;
        r.numerosity++;
        rules.push_back(r);
    }
}

bool LCS::subsumpRule(const Rule& gRule, const Rule& sRule) const
{
    double gFitness = calculateSubsumptionFitness(gRule);
    double sFitness = calculateSubsumptionFitness(sRule);
    if (   gRule.result != sRule.result
        || gFitness < sFitness
        || (gFitness == sFitness && gRule.nPredictions < sRule.nPredictions))
        return false;

    bool theSame = true;
    for (int i = 0; i < NC; ++i)
    {
        if(gRule.condition[i] != NONE && gRule.condition[i] != sRule.condition[i])
            return false;
        else if(gRule.condition[i] != sRule.condition[i])
            theSame = false;
    }

    return theSame ? (gRule.nPredictions >= sRule.nPredictions) : true;
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
    mutate(child1, params);
    mutate(child2, params);


    if (matchRule(params, child1))
    {
        updateRule(child1, label);
        //std::cerr << "child1 => ";
        //printRule(child1);
        rules.push_back(child1);
    }

    if (matchRule(params, child2))
    {
        updateRule(child2, label);
        //std::cerr << "child2 => ";
        //printRule(child2);
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
        r.fitness = MIN_FITNESS;
    }
    nLearned = 0;
}

void LCS::generalizationStep(bool label, const std::set<int>& matches)
{
    int r = wheelSelectionBest(label, matches);
    if(r >= 0)
    {
        addGeneralizedRule(rules[r]);
    }
}

void LCS::learn(bool label, const std::vector<bool>& params)
{
    assert(params.size() == paramsText.size());

    if (rnd() < RANDOM_SKIPPING) return;

    std::set<int> matches;
    match(params, matches);

    ++nLearned;
    for(int r : matches)
        updateRule(rules[r], label);

    if (DoLearning)
    {
        Stockfish::dbg_hit_on(label);
        learnStep(label, params, matches);

        if(USE_SUBSUMPTION && subsumption())
        {
            matches.clear();
            match(params, matches);
        }

        ruleDiscoveryStep(label, params, matches);

        if (USE_GENERALIZATION)
        {
            matches.clear();
            match(params, matches);
            generalizationStep(label, matches);
        }

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
    rule.accuracy = (double)rule.correctPredictions / rule.nPredictions;
    rule.coverage = (double)rule.nPredictions / nLearned;
    rule.fitness = calculateFitness(rule);
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
        //if (rnd(2) == 0)
        if (rnd(NC) == 0)
        //if (true)
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

void LCS::printAttrStats(std::ostream& out)
{
    int n = 0;
    std::vector<std::pair<int,int>> count(NC);

    for(int i = 0; i < NC; ++i)
        count[i] = { i, 0 };

    for(const Rule&r : rules)
    {
        for(int i = 0; i < NC; ++i)
        {
            if(r.condition[i] != NONE)
            {
                count[i].second++;
                ++n;
            }
        }
    }

    out << "--------- attributes stats ----------" << std::endl;
    std::stable_sort(count.begin(), count.end(), [](const std::pair<int,int>& a, const std::pair<int,int>& b) { return   a.second > b.second; } );
    for(int i = 0; i < NC; ++i)
    {
        out << (i+1) << ". " << " count=" << count[i].second << " freq=" << 100.*count[i].second / n << "% => " << paramsText[count[i].first] << std::endl;
    }
}

void LCS::print(bool sort, bool pareto, std::ostream& out)
{
    if (pareto)
    {
        std::stable_sort(rules.begin(), rules.end(), [](const Rule& a, const Rule& b) { return   a.fitness > b.fitness
                                                                                              || (a.fitness == b.fitness && a.coverage > b.coverage); } );
        out << "--------- pareto step " << steps << " ----------" << std::endl;
        for(int label = 0; label < 2; ++label)
        {
            out << "=> Label: " << (label ? labelText : "NOT(" + labelText + ")") << std::endl;
            double lastFitness  =  MIN_FITNESS;
            double lastCoverage =  -1;

            for(int i = 0, j = 0; i < (int)rules.size(); ++i)
            {
                if (rules[i].result == bool(label) && (rules[i].coverage  > lastCoverage || (rules[i].fitness == lastFitness && rules[i].coverage >= lastCoverage)))
                {
                    out << j+1 << ". ";
                    printRule(rules[i], out);
                    ++j;
                    lastFitness = rules[i].fitness;
                    lastCoverage = rules[i].coverage;

                }
            }
        }
    }
    else
    {
        if(sort)
            std::stable_sort(rules.begin(), rules.end(), [](const Rule& a, const Rule& b) { return   a.fitness > b.fitness
                                                                                                  || (a.fitness == b.fitness && a.coverage > b.coverage); } );
        out << "--------- step " << steps << " ----------" << std::endl;
        for(int i = 0; i < (int)rules.size(); ++i)
        {
            out << i+1 << ". ";
            printRule(rules[i], out);
        }
    }
}
