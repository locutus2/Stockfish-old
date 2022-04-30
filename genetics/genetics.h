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
    BoolExpression(const std::string& n) : name(n), id(next_id++) {}
  
    const std::string prefix() const { return SHOW_ID ? std::to_string(id) + '#' : ""; };

    virtual bool operator()(const std::vector<bool>& x) const = 0;
    virtual std::string to_string() const = 0;
};

int BoolExpression::next_id = 0;

namespace std
{
    std::string to_string(const BoolExpression& e)
    {
        return e.to_string();
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
   
    std::string to_string() const
    {
        return prefix() + this->name;
    }
};

class UnaryBoolFunction : public BoolExpression
{
    protected:
    BoolExpression* arg;

    public:
    UnaryBoolFunction(const std::string& n, BoolExpression& a) : BoolExpression(n), arg(&a) {}
   
    std::string to_string() const
    {
        return prefix() + this->name + '(' + std::to_string(*arg) + ')';
    }
};

class BinaryBoolFunction : public BoolExpression
{
    protected:
    BoolExpression *arg1, *arg2;

    public:
    BinaryBoolFunction(const std::string& n, BoolExpression& a1, BoolExpression& a2) : BoolExpression(n), arg1(&a1), arg2(&a2) {}
   
    std::string to_string() const
    {
        return prefix() + this->name + '(' + std::to_string(*arg1) + ", " + std::to_string(*arg2) + ')';
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
    Not(BoolExpression& a) : UnaryBoolFunction("NOT", a) {}
    Not(Not& a) : UnaryBoolFunction("NOT", a) {}
   
    bool operator()(const std::vector<bool>& x) const
    {
        return !(*this->arg)(x);
    }
};

class And : public BinaryBoolFunction
{
    public:
    And(BoolExpression& a1, BoolExpression& a2) : BinaryBoolFunction("AND", a1, a2) {}
   
    bool operator()(const std::vector<bool>& x) const
    {
        return (*this->arg1)(x) && (*this->arg2)(x);
    }
};

class Or : public BinaryBoolFunction
{
    public:
    Or(BoolExpression& a1, BoolExpression& a2) : BinaryBoolFunction("OR", a1, a2) {}
   
    bool operator()(const std::vector<bool>& x) const
    {
        return (*this->arg1)(x) || (*this->arg2)(x);
    }
};

class Xor : public BinaryBoolFunction
{
    public:
    Xor(BoolExpression& a1, BoolExpression& a2) : BinaryBoolFunction("XOR", a1, a2) {}
   
    bool operator()(const std::vector<bool>& x) const
    {
        return (*this->arg1)(x) ^ (*this->arg2)(x);
    }
};

class Equal : public BinaryBoolFunction
{
    public:
    Equal(BoolExpression& a1, BoolExpression& a2) : BinaryBoolFunction("EQ", a1, a2) {}
   
    bool operator()(const std::vector<bool>& x) const
    {
        return (*this->arg1)(x) == (*this->arg2)(x);
    }
};

class NotEqual : public BinaryBoolFunction
{
    public:
    NotEqual(BoolExpression& a1, BoolExpression& a2) : BinaryBoolFunction("NEQ", a1, a2) {}
   
    bool operator()(const std::vector<bool>& x) const
    {
        return (*this->arg1)(x) != (*this->arg2)(x);
    }
};

std::ostream& operator<<(std::ostream& out, const BoolExpression& e)
{
    return out << std::to_string(e);
}

#endif
