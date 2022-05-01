#ifndef EVOLUTION_H
#define EVOLUTION_H

#include <algorithm>

#include "genetics.h"

class Evolution
{
    int N_POP;
    int N_DATA;
    int N_RECORDS;
    int iteration;

    std::vector<bool> labels;
    std::vector<std::vector<bool>> data;

    std::vector<BoolExpression*> terminals;
    std::vector<BoolExpression*> unaryFunctions;
    std::vector<BoolExpression*> binaryFunctions;

    struct Genom
    {
        double fitness;
        BoolExpression* genom;
    };

    std::vector<Genom> population;

    public:
    void setData(const std::vector<bool>& labels, const std::vector<std::vector<bool>>& data)
    {
         this->labels = labels;
         this->data = data;
	 N_RECORDS = data.size();
	 N_DATA = data[0].size();
    }

    void init(int n)
    {
        if (terminals.empty())
	{
            terminals = { new False(), new True() };
            for(int i = 0; i < N_DATA; ++i)
                terminals.push_back(new BoolVar(i));

            unaryFunctions = { new Not() };

            binaryFunctions = { new And(), new Or() };
	}

        N_POP = n;
	population.resize(N_POP);
        iteration = 0;

	for(int i = 0; i < N_POP; ++i)
	{
            population[i].genom = randomGenom();
            population[i].fitness = evaluateGenom(population[i].genom);
	}

        print();
    }

    BoolExpression* randomGenom() const
    {
        BoolExpression* genom = terminals[std::rand() % terminals.size()]->copy();
        return genom;
    }

    void print(std::ostream& out = std::cout)
    {
        out << "--------- iteration " << iteration << " ----------------" << std::endl;
        std::sort(population.begin(), population.end(), [](const Genom& a, const Genom& b) {
                                                            return a.fitness > b.fitness;
                                                        });

        double average = 0;
        for(int i = 0; i < (int)population.size(); ++i)
            average += population[i].fitness;

        out << "Maximum fitness: " << population[0].fitness << std::endl;
        out << "Average fitness: " << average / population.size() << std::endl;

	for(int i = 0; i < (int)population.size(); ++i)
	{
	    out << i << "\t" << population[i].fitness << "\t" << population[i].genom << std::endl;
	}
    }

    double evaluateGenom(BoolExpression* genom) const
    {
        double value = 0;
        int n = 0;

        for(int i = 0; i < N_RECORDS; ++i)
        {
            bool c = (*genom)(data[i]);
            if(c)
            {
                ++n;
                if(c == labels[i])
                    value += 1;
            }
	    /*
            ++n;
            if (c == labels[i])
                value += 1;
	    */
        }
        return n ? value / n : value;
    }

    double rnd() const
    {
        return std::rand() / (double)RAND_MAX;
    }

    int rnd(int n) const
    {
        return std::rand() % n;
    }

    BoolExpression* selectGenom() const
    {
        const Genom& g1 = population[rnd(population.size())];
        const Genom& g2 = population[rnd(population.size())];
        return g1.fitness > g2.fitness ? g1.genom : g2.genom;
    }

    BoolExpression* selectRandomNode(BoolExpression* g) const
    {
        std::vector<BoolExpression*> nodes = {g};
        return nodes[rnd(nodes.size())];
    }

    BoolExpression* mutateGenom(BoolExpression* g) const
    {
        int n = rnd(g->nodes);
        auto uf = dynamic_cast<UnaryBoolFunction*>(g);
        auto bf = dynamic_cast<BinaryBoolFunction*>(g);
	int r = rnd(n);

        if(r == 0)
        {
	    BoolExpression* ng = g;
            if (bf)
            {
                //ng = binaryFunctions[rnd(binaryFunctions.size())]->copy();
            }
	    else if (uf)
            {
                //ng = unaryFunctions[rnd(unaryFunctions.size())]->copy();
            }
            else
            {
                ng = terminals[rnd(terminals.size())]->copy();
            }

	    //delete g;
	    g = ng;
        }
        else
        {
            if (bf)
            {
                if (r < 1 + bf->arg1->nodes)
                {
                    //g = mutateGenom(bf->arg1);
                }
                else
                {
                    //g = mutateGenom(bf->arg2);
                }
            }
	    else if (uf)
            {
                //g = mutateGenom(uf->arg);
            }
        }
	return g;
    }
    
    void crossoverGenoms(BoolExpression* p1, BoolExpression* p2, BoolExpression* &c1, BoolExpression* &c2) const
    {
        c1 = p1->copy();
        c2 = p2->copy();
    }
    
    void run()
    {
        while(true)
        {
            ++iteration;
	    std::vector<Genom> new_population;

	    while((int)new_population.size() < N_POP)
	    {
                double r = rnd();

		if (r <= 0.8) // crossover
		{
                     Genom c1, c2;
		     crossoverGenoms(selectGenom(), selectGenom(), c1.genom, c2.genom);
                     c1.fitness = evaluateGenom(c1.genom);
                     c2.fitness = evaluateGenom(c2.genom);

                     new_population.push_back(c1);
                     if((int)new_population.size() < N_POP)
                         new_population.push_back(c2);
		}
		else // mutate
		{
                     Genom c;
                     c.genom = selectGenom()->copy();
                     c.genom = mutateGenom(c.genom);
                     c.fitness = evaluateGenom(c.genom);
                     new_population.push_back(c);
		}
	    }

	    population = new_population;
            print();
        }
    }
};

#endif
