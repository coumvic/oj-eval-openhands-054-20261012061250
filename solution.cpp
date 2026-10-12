#include <bits/stdc++.h>
using namespace std;

class Term {
public:
    int a, b, c, d; // coefficient, x^b, sin^c(x), cos^d(x)
    
    Term(int a = 0, int b = 0, int c = 0, int d = 0) : a(a), b(b), c(c), d(d) {}
    
    bool operator==(const Term& other) const {
        return b == other.b && c == other.c && d == other.d;
    }
    
    bool operator!=(const Term& other) const {
        return !(*this == other);
    }
    
    bool operator<(const Term& other) const {
        if (b != other.b) return b > other.b;
        if (c != other.c) return c > other.c;
        return d > other.d;
    }
};

class Poly {
public:
    vector<Term> terms;
    
    Poly() {}
    Poly(const vector<Term>& t) : terms(t) {}
    
    void simplify() {
        // Sort terms
        sort(terms.begin(), terms.end());
        
        // Combine like terms
        vector<Term> result;
        for (const Term& t : terms) {
            if (t.a == 0) continue;
            
            if (!result.empty() && result.back() == t) {
                result.back().a += t.a;
            } else {
                result.push_back(t);
            }
        }
        
        // Remove zero coefficient terms
        terms.clear();
        for (const Term& t : result) {
            if (t.a != 0) {
                terms.push_back(t);
            }
        }
    }
    
    Poly operator+(const Poly& other) const {
        Poly result;
        result.terms = terms;
        result.terms.insert(result.terms.end(), other.terms.begin(), other.terms.end());
        result.simplify();
        return result;
    }
    
    Poly operator-(const Poly& other) const {
        Poly result;
        result.terms = terms;
        for (Term t : other.terms) {
            t.a = -t.a;
            result.terms.push_back(t);
        }
        result.simplify();
        return result;
    }
    
    Poly operator*(const Poly& other) const {
        Poly result;
        for (const Term& t1 : terms) {
            for (const Term& t2 : other.terms) {
                Term t(t1.a * t2.a, t1.b + t2.b, t1.c + t2.c, t1.d + t2.d);
                result.terms.push_back(t);
            }
        }
        result.simplify();
        return result;
    }
    
    Poly derivative() const {
        Poly result;
        for (const Term& t : terms) {
            // derivative of a * x^b * sin^c(x) * cos^d(x)
            // using product rule: sum of derivatives of each factor
            
            // derivative of x^b factor
            if (t.b > 0) {
                Term t1(t.a * t.b, t.b - 1, t.c, t.d);
                result.terms.push_back(t1);
            }
            
            // derivative of sin^c(x) factor
            if (t.c > 0) {
                // (sin^c(x))' = c * sin^(c-1)(x) * cos(x)
                Term t2(t.a * t.c, t.b, t.c - 1, t.d + 1);
                result.terms.push_back(t2);
            }
            
            // derivative of cos^d(x) factor
            if (t.d > 0) {
                // (cos^d(x))' = -d * sin(x) * cos^(d-1)(x)
                Term t3(-t.a * t.d, t.b, t.c + 1, t.d - 1);
                result.terms.push_back(t3);
            }
        }
        result.simplify();
        return result;
    }
};

class Frac {
public:
    Poly p, q;
    
    Frac() {}
    Frac(const Poly& p, const Poly& q) : p(p), q(q) {}
    Frac(int x) {
        p = Poly({Term(x, 0, 0, 0)});
        q = Poly({Term(1, 0, 0, 0)});
    }
    Frac(const Term& t) {
        p = Poly({t});
        q = Poly({Term(1, 0, 0, 0)});
    }
    
    Frac operator+(const Frac& other) const {
        // p1/q1 + p2/q2 = (p1*q2 + p2*q1) / (q1*q2)
        Frac result;
        result.p = p * other.q + other.p * q;
        result.q = q * other.q;
        return result;
    }
    
    Frac operator-(const Frac& other) const {
        // p1/q1 - p2/q2 = (p1*q2 - p2*q1) / (q1*q2)
        Frac result;
        result.p = p * other.q - other.p * q;
        result.q = q * other.q;
        return result;
    }
    
    Frac operator*(const Frac& other) const {
        // p1/q1 * p2/q2 = (p1*p2) / (q1*q2)
        Frac result;
        result.p = p * other.p;
        result.q = q * other.q;
        return result;
    }
    
    Frac operator/(const Frac& other) const {
        // p1/q1 / p2/q2 = (p1*q2) / (q1*p2)
        Frac result;
        result.p = p * other.q;
        result.q = q * other.p;
        return result;
    }
    
    Frac derivative() const {
        // (p/q)' = (p'*q - q'*p) / (q*q)
        Frac result;
        result.p = p.derivative() * q - q.derivative() * p;
        result.q = q * q;
        return result;
    }
};

string termToString(const Term& t) {
    string result;
    
    // Handle coefficient
    if (t.b == 0 && t.c == 0 && t.d == 0) {
        // Constant term
        return to_string(t.a);
    }
    
    // Handle sign and coefficient
    if (t.a == -1) {
        result += "-";
    } else if (t.a == 1) {
        // No coefficient for 1 (except for constant)
    } else {
        result += to_string(t.a);
    }
    
    // Handle x^b
    if (t.b > 0) {
        result += "x";
        if (t.b > 1) {
            result += "^" + to_string(t.b);
        }
    }
    
    // Handle sin^c(x)
    if (t.c > 0) {
        result += "sin";
        if (t.c > 1) {
            result += "^" + to_string(t.c);
        }
        result += "x";
    }
    
    // Handle cos^d(x)
    if (t.d > 0) {
        result += "cos";
        if (t.d > 1) {
            result += "^" + to_string(t.d);
        }
        result += "x";
    }
    
    return result;
}

string polyToString(const Poly& poly) {
    if (poly.terms.empty()) {
        return "0";
    }
    
    string result;
    for (size_t i = 0; i < poly.terms.size(); i++) {
        const Term& t = poly.terms[i];
        
        if (i == 0) {
            // First term - handle sign
            if (t.a < 0 && !(t.b == 0 && t.c == 0 && t.d == 0)) {
                result += "-";
            }
        } else {
            // Subsequent terms
            if (t.a < 0) {
                result += "-";
            } else {
                result += "+";
            }
        }
        
        // Add term (without sign for non-first terms)
        Term abs_t = t;
        if (i > 0) {
            abs_t.a = abs(t.a);
        } else if (t.a < 0 && !(t.b == 0 && t.c == 0 && t.d == 0)) {
            abs_t.a = -t.a;
        }
        
        result += termToString(abs_t);
    }
    
    return result;
}

string fracToString(const Frac& frac) {
    string numerator = polyToString(frac.p);
    string denominator = polyToString(frac.q);
    
    // Special cases
    if (numerator == "0") {
        return "0";
    }
    if (denominator == "1") {
        return numerator;
    }
    
    // Check if we need parentheses
    bool need_paren_num = (frac.p.terms.size() > 1);
    bool need_paren_den = (frac.q.terms.size() > 1);
    
    string result;
    if (need_paren_num) result += "(";
    result += numerator;
    if (need_paren_num) result += ")";
    
    result += "/";
    
    if (need_paren_den) result += "(";
    result += denominator;
    if (need_paren_den) result += ")";
    
    return result;
}

int getNumber(const string& s, int& pos) {
    int result = 0;
    while (pos < s.length() && isdigit(s[pos])) {
        result = result * 10 + (s[pos] - '0');
        pos++;
    }
    return result;
}

Term parseSingleTerm(const string& s, int& pos) {
    int a = 1, b = 0, c = 0, d = 0;
    
    // Parse coefficient
    if (pos < s.length() && s[pos] == '-') {
        a = -1;
        pos++;
    } else if (pos < s.length() && s[pos] == '+') {
        a = 1;
        pos++;
    }
    
    // Check if there's a number
    if (pos < s.length() && isdigit(s[pos])) {
        a *= getNumber(s, pos);
    }
    
    // Parse factors
    while (pos < s.length()) {
        if (s[pos] == 'x') {
            b = 1;
            pos++;
            if (pos < s.length() && s[pos] == '^') {
                pos++;
                b = getNumber(s, pos);
            }
        } else if (pos + 2 < s.length() && s.substr(pos, 3) == "sin") {
            c = 1;
            pos += 3;
            if (pos < s.length() && s[pos] == '^') {
                pos++;
                c = getNumber(s, pos);
            }
            if (pos < s.length() && s[pos] == 'x') pos++; // skip 'x' in "sinx" or "sin^nx"
        } else if (pos + 2 < s.length() && s.substr(pos, 3) == "cos") {
            d = 1;
            pos += 3;
            if (pos < s.length() && s[pos] == '^') {
                pos++;
                d = getNumber(s, pos);
            }
            if (pos < s.length() && s[pos] == 'x') pos++; // skip 'x' in "cosx" or "cos^nx"
        } else {
            break;
        }
    }
    
    return Term(a, b, c, d);
}

// Forward declarations
Frac parseExpression(const string& s, int& pos);
Frac parseTerm(const string& s, int& pos);
Frac parseFactor(const string& s, int& pos);

Frac parseFactor(const string& s, int& pos) {
    if (pos < s.length() && s[pos] == '(') {
        pos++; // skip '('
        Frac result = parseExpression(s, pos);
        pos++; // skip ')'
        return result;
    }
    
    // Parse a single term
    int old_pos = pos;
    Term t = parseSingleTerm(s, pos);
    return Frac(t);
}

Frac parseTerm(const string& s, int& pos) {
    Frac result = parseFactor(s, pos);
    
    while (pos < s.length() && (s[pos] == '*' || s[pos] == '/')) {
        char op = s[pos];
        pos++;
        Frac next_factor = parseFactor(s, pos);
        
        if (op == '*') {
            result = result * next_factor;
        } else {
            result = result / next_factor;
        }
    }
    
    return result;
}

Frac parseExpression(const string& s, int& pos) {
    Frac result = parseTerm(s, pos);
    
    while (pos < s.length() && (s[pos] == '+' || s[pos] == '-')) {
        char op = s[pos];
        pos++;
        Frac next_term = parseTerm(s, pos);
        
        if (op == '+') {
            result = result + next_term;
        } else {
            result = result - next_term;
        }
    }
    
    return result;
}

void solve(const string& s) {
    int pos = 0;
    Frac f = parseExpression(s, pos);
    Frac f_prime = f.derivative();
    
    cout << fracToString(f) << endl;
    cout << fracToString(f_prime) << endl;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    
    string expr;
    getline(cin, expr);
    
    solve(expr);
    
    return 0;
}