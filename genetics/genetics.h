#ifndef GENETICS_H
#define GENETICS_H

#include <iostream>
#include <vector>

class BoolExpression
{
    const bool SHOW_ID = false;

    static int next_id;

    protected:
    std::string name;
    int id;

    public:
    int nodes;

    BoolExpression(const std::string& n, int node_count = 1) : name(n), id(next_id++), nodes(node_count) {}
  
    const std::string prefix() const { return SHOW_ID ? std::to_string(id) + '#' : ""; };

    virtual bool operator()(const std::vector<bool>& x) const = 0;
    virtual std::string to_string() const = 0;
    virtual BoolExpression* copy() const = 0;
    virtual ~BoolExpression() {};
};

int BoolExpression::next_id = 0;

namespace std
{
    std::string to_string(const BoolExpression& e)
    {
        return e.to_string();
    }

    std::string to_string(const BoolExpression* e)
    {
        return e->to_string();
    }
}

class BoolTerminal : public BoolExpression
{
    protected:
    bool value;

    public:
    BoolTerminal(const std::string& n, bool v ) : BoolExpression(n), value(v) {}

    bool operator()(const std::vector<bool>& x) const
    {
        return value;
    }
   
    BoolExpression* copy() const
    {
        return new BoolTerminal(this->name, value);
    }
   
    std::string to_string() const
    {
        return prefix() + this->name;
    }
};

class UnaryBoolFunction : public BoolExpression
{
    public:
    BoolExpression* arg;

    UnaryBoolFunction(const std::string& n, BoolExpression* a) : BoolExpression(n, 1 + (a ? a->nodes: 0)), arg(a) {}
   
    std::string to_string() const
    {
        return prefix() + this->name + '(' + std::to_string(arg) + ')';
    }
};

class BinaryBoolFunction : public BoolExpression
{
    public:
    BoolExpression *arg1, *arg2;

    BinaryBoolFunction(const std::string& n, BoolExpression* a1, BoolExpression* a2) : BoolExpression(n, 1 + (a1 ? a1->nodes : 0) + (a2 ? a2->nodes : 0)), arg1(a1), arg2(a2) {}
   
    std::string to_string() const
    {
        return prefix() + this->name + '(' + std::to_string(arg1) + ", " + std::to_string(arg2) + ')';
    }
};

class BoolVar : public BoolExpression
{
    int n;

    public:
    BoolVar(int i) : BoolExpression("x"), n(i) {}
   
    bool operator()(const std::vector<bool>& x) const
    {
        return x[n];
    }

    BoolExpression* copy() const
    {
        return new BoolVar(n);
    }
   
    std::string to_string() const
    {
        return prefix() + this->name + std::to_string(n);
    }
};

class False : public BoolTerminal
{
    public:
    False() : BoolTerminal("FALSE", false) {}
};

class True : public BoolTerminal
{
    public:
    True() : BoolTerminal("TRUE", true) {}
};

class Not : public UnaryBoolFunction
{
    public:
    Not(BoolExpression* a = nullptr) : UnaryBoolFunction("NOT", a) {}
    Not(Not* a) : UnaryBoolFunction("NOT", a) {}
   
    BoolExpression* copy() const
    {
        return new Not(arg ? arg->copy() : nullptr);
    }
   
    bool operator()(const std::vector<bool>& x) const
    {
        return !(*this->arg)(x);
    }
};

class And : public BinaryBoolFunction
{
    public:
    And(BoolExpression* a1 = nullptr, BoolExpression* a2 = nullptr) : BinaryBoolFunction("AND", a1, a2) {}
   
    BoolExpression* copy() const
    {
        return new And(arg1 ? arg1->copy() : nullptr, arg2 ? arg2->copy() : nullptr);
    }
   
    bool operator()(const std::vector<bool>& x) const
    {
        return (*this->arg1)(x) && (*this->arg2)(x);
    }
};

class Or : public BinaryBoolFunction
{
    public:
    Or(BoolExpression* a1 = nullptr, BoolExpression* a2 = nullptr) : BinaryBoolFunction("OR", a1, a2) {}
   
    BoolExpression* copy() const
    {
        return new Or(arg1 ? arg1->copy() : nullptr, arg2 ? arg2->copy() : nullptr);
    }
   
    bool operator()(const std::vector<bool>& x) const
    {
        return (*this->arg1)(x) || (*this->arg2)(x);
    }
};

class Xor : public BinaryBoolFunction
{
    public:
    Xor(BoolExpression* a1 = nullptr, BoolExpression* a2 = nullptr) : BinaryBoolFunction("XOR", a1, a2) {}
   
    BoolExpression* copy() const
    {
        return new Xor(arg1 ? arg1->copy() : nullptr, arg2 ? arg2->copy() : nullptr);
    }
   
    bool operator()(const std::vector<bool>& x) const
    {
        return (*this->arg1)(x) ^ (*this->arg2)(x);
    }
};

class Equal : public BinaryBoolFunction
{
    public:
    Equal(BoolExpression* a1 = nullptr, BoolExpression* a2 = nullptr) : BinaryBoolFunction("EQ", a1, a2) {}
   
    BoolExpression* copy() const
    {
        return new Equal(arg1 ? arg1->copy() : nullptr, arg2 ? arg2->copy() : nullptr);
    }
   
    bool operator()(const std::vector<bool>& x) const
    {
        return (*this->arg1)(x) == (*this->arg2)(x);
    }
};

class NotEqual : public BinaryBoolFunction
{
    public:
    NotEqual(BoolExpression* a1 = nullptr, BoolExpression* a2 = nullptr) : BinaryBoolFunction("NEQ", a1, a2) {}
   
    BoolExpression* copy() const
    {
        return new NotEqual(arg1 ? arg1->copy() : nullptr, arg2 ? arg2->copy() : nullptr);
    }
   
    bool operator()(const std::vector<bool>& x) const
    {
        return (*this->arg1)(x) != (*this->arg2)(x);
    }
};

std::ostream& operator<<(std::ostream& out, const BoolExpression* e)
{
    return out << std::to_string(e);
}

#endif
