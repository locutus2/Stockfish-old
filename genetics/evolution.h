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
        out << "--------- iteration " << iteration << std::endl;
        std::sort(population.begin(), population.end(), [](const Genom& a, const Genom& b) {
                                                            return a.fitness > b.fitness;
                                                        });
	for(int i = 0; i < N_POP; ++i)
	{
	    out << population[i].fitness << " " << population[i].genom << std::endl;
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
	    else
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

    void run()
    {
        while(true)
        {
            ++iteration;
            print();
        }
    }
};

#endif
