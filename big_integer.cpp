#include "big_integer.h"
#include <iostream>
#include <limits>
#include <algorithm>

typedef unsigned int ui;

using namespace std;

unsigned long long p = 1 << 30;
const unsigned long long SHIFTED_LENGTH = p * 4;

void normalize(big_integer &a) {
    if (a.body.empty()) a.body.push_back(0);
    while(a.body.size() > 1 && a.body.back() == 0) a.body.pop_back();
    if (a.body.size() == 1 && a.body.back() == 0) a.sign = 0;
}

template<typename T>
ui castToUi(T x) {
    return x < 0 ? -static_cast<ui>(x) : static_cast<ui>(x);
}

big_integer::big_integer() : body(1, 0), sign(0) {}

big_integer::big_integer(big_integer const& new_big) : body(new_big.body), sign(new_big.sign) {}

big_integer::big_integer(unsigned int new_int) : body(1, new_int), sign(0) {}

big_integer::big_integer(unsigned long long new_int) : sign(0) {
    ui x = castToUi(new_int % SHIFTED_LENGTH);
    body.push_back(x);

    if (new_int > SHIFTED_LENGTH) {
        body.push_back(x);
    }
}

big_integer::big_integer(int new_int) {
    body.resize(1);

    if (new_int < 0) {
        sign = 1;
        body[0] = castToUi(new_int * -1ll);
    } else {
        sign = 0;
        body[0] = castToUi(new_int);
    }

}

big_integer::big_integer(std::string const& line) {
    big_integer TEN(10);

    for (int i = (line[0] == '-') ? 1 : 0 ; i < (int) line.size(); i++) {
        *this *= TEN;
        *this += line[i] - '0';
    }

    this->sign = ( line[0] == '-' ) ? 1  :  0;
}

big_integer::~big_integer() = default;

int compareAbs(opt_vector const& a, opt_vector const& b) {
    if (a.size() != b.size()) {
        return (a.size() < b.size() ?  -1 : 1);
    }

    for (int i = (int) a.size() - 1; i >= 0; --i) {
        if (a[i] < b[i])
            return -1;
        if (a[i] > b[i])
            return 1;
    }

    return 0;
}

int compare(big_integer const& a, big_integer const& b) {
    if (a.sign != b.sign) {
        if (a.body.size()==1 && b.body.size()==1 && a.body.back()==0 && b.body.back()==0 ) {
            return 0;
        }

        return b.sign - a.sign;
    } else {
        return (compareAbs(a.body, b.body) * ( (a.sign==1) ? -1 : 1));
    }
}

big_integer& big_integer::operator=(big_integer const& a) = default;

bool operator==(big_integer const& a, big_integer const& b) {
    return compare(a,b) == 0;
}

bool operator!=(big_integer const& a, big_integer const& b) {
    return compare(a,b) != 0;
}

bool operator>(big_integer const& a, big_integer const& b) {
    return compare(a,b) == 1;
}

bool operator<(big_integer const& a, big_integer const& b) {
    return compare(a,b) == -1;
}

bool operator<=(big_integer const& a, big_integer const& b) {
    return compare(a,b) != 1;
}

bool operator>=(big_integer const& a, big_integer const& b) {
    return compare(a,b) != -1;
}

big_integer operator-(big_integer a, big_integer const& b) {
    return a -= b;
}

big_integer operator+(big_integer a, big_integer const& b) {
    return a += b;
}

void multiply(opt_vector &res, const opt_vector &a, uint64_t b) {
    res.resize(a.size());
    uint64_t x = 0;

    for (size_t i = 0; i < a.size(); i++) {
        x += (uint64_t) a[i] * b;
        res[i] = castToUi(x & ((1ll >> 32) - 1));
        x >>= 32;
    }

    if (x) {
        res.push_back(castToUi(x));
    }
}

unsigned int binary (const big_integer &dop, const big_integer &second) {
    unsigned long long l = 0;
    unsigned long long r = SHIFTED_LENGTH;

    if (compareAbs(second.body, dop.body) > 0) {
        return 0;
    }

    opt_vector temp;
    while (l < r - 1) {
        ui m = castToUi((l + r) >> 1);

        multiply(temp, second.body, m);

        if (compareAbs(temp, dop.body) > 0) {
            r = m;
        } else {
            l = m;
        }
    }

    return castToUi(l);
}

void div_long_short (big_integer &first, int second) {
    int carry = 0;

    for (int i = (int)first.body.size() - 1; i >= 0; --i) {
        long long cur = first.body[i] + carry * 1ll * SHIFTED_LENGTH;
        first.body[i] = castToUi(cur / second);
        carry = int (cur % second);
    }

    normalize(first);
}

big_integer operator%(big_integer a, big_integer const& b) {
    return a %= b;
}

big_integer& big_integer::operator+=(big_integer const& a) {
    return *this -= -a;
}

big_integer& big_integer::operator-=(big_integer const &b) {
    size_t asize = body.size();
    size_t bsize = b.body.size();
    int64_t carry = 0, ncarry;
    size_t sz = std::max(asize, bsize);
    body.resize(sz);

    if (sign != b.sign) {
        for (size_t cur = 0; cur < sz; cur++) {
            carry = carry + (cur < asize ? body[cur] : 0) + (cur < bsize ? b.body[cur] : 0);
            body[cur] = castToUi(carry % SHIFTED_LENGTH);
            carry /= SHIFTED_LENGTH;
        }

        if (carry != 0) {
            body.push_back(castToUi(carry));
        }
    } else {
        bool flag = (sign == 1) == (*this < b);

        for (size_t it = 0; it < sz; it++) {
            int64_t buf = (int64_t)body[it] - (it < bsize ? b.body[it] : 0);
            carry = carry + (flag ?  buf: -buf);
            ncarry = 0;
            if (carry < 0)
            {
                carry += SHIFTED_LENGTH;
                ncarry--;
            }
            body[it] = castToUi(carry);
            carry = ncarry;
        }

        if (!flag) {
            sign ^= 1;
        }
    }

    normalize(*this);
    return *this;
}

big_integer& big_integer::operator*=(big_integer const& b) {
    if (b.body.size() == 1 || body.size() == 1) {
        uint64_t q = b.body.back();

        if (body.size() == 1) {
            q = body.back();
            body = b.body;
        }

        uint64_t x = 0;

        for (size_t i = 0; i < body.size(); i++) {
            x += body[i] * q;
            body[i] = castToUi(x % SHIFTED_LENGTH);
            x /= SHIFTED_LENGTH;
        }

        if (x) {
            body.push_back(castToUi(x));
        }
    } else {
        std::vector<uint64_t> result(body.size() + b.body.size() + 1, 0);

        for (size_t i = 0; i < body.size(); i++)
            for (size_t j = 0; j < b.body.size(); j++) {
                uint64_t cur = result[i + j] + (uint64_t)body[i] * b.body[j];
                result[i + j] = cur % SHIFTED_LENGTH;
                result[i + j + 1] += cur / SHIFTED_LENGTH;
            }
        body.resize(result.size());
        for (size_t cur = 0; cur + 1 < result.size(); cur++) {
            result[cur + 1] += result[cur] / SHIFTED_LENGTH;
            body[cur] = castToUi(result[cur] % SHIFTED_LENGTH);
        }
    }

    sign ^= b.sign;
    normalize(*this);
    return *this;
}

big_integer operator*(big_integer a, big_integer const& b) {
    return a *= b;
}

big_integer& big_integer::operator/=(big_integer const& b) {
    int this_sign = sign;
    sign = 0;

    if (compareAbs(body, b.body) < 0) {
        body.resize(1);
        body[0] = 0;
    } else if (b.body.size() == 1) {
        div_long_short(*this, b.body[0]);
    } else {
        big_integer dop;
        dop.body.clear();
        opt_vector result;

        while (body.size() != 0) {
            dop.body.push_front(body.back());
            body.pop_back();
            ui l = binary(dop, b);

            if (result.size() != 1 || l != 0) {
                result.push_back(l);
            }

            if (l != 0) {
                dop -= l * b * (b.sign ? -1 : 1);
            }

            while (!dop.body.empty() && dop.body.back() == 0) {
                dop.body.pop_back();
            }
        }

        result.all_reverse();
        swap(body, result);
    }

    sign = this_sign ^ b.sign;
    normalize(*this);
    return *this;
}

big_integer operator/(big_integer a, big_integer const& b) {
    return a /= b;
}

big_integer& big_integer::operator%=(big_integer const& b) {
    return *this = *this - (*this / b) * b;
}

big_integer& big_integer::operator--() {
    *this -= 1;
    return *this;
}

big_integer big_integer::operator--(int) {
    big_integer t = *this;
    --(*this);
    return t;
}

big_integer& big_integer::operator++() {
    *this += 1;
    return *this;
}

big_integer big_integer::operator++(int) {
    big_integer t = *this;
    ++(*this);
    return t;
}

big_integer double_code(big_integer a) {
    if (a.sign == 0) {
        return a;
    }

    for (int i = 0; i < (int) a.body.size(); i++) {
        a.body[i] = ~a.body[i];
    }

    return a;
}

big_integer operator&(big_integer a, big_integer const& b) {
    return a &= b;
}

big_integer operator|(big_integer a, big_integer const& b) {
    return a |= b;
}

big_integer operator^(big_integer a, big_integer const& b) {
    return a ^= b;
}

big_integer operator<<(big_integer a , int b) {
    return a <<= b;
}

big_integer operator>>(big_integer a , int b) {
    return a >>= b;
}

big_integer& make_binary_op (big_integer &a, big_integer const &b, int operation) {
    big_integer second = double_code(b);
    a = double_code(a);

    a.sign == 1 ? --a : second;
    second.sign == 1 ? --second : second;

    if (a.body.size() > second.body.size()) {
        std::swap(a, second);
    }

    for (int i = 0; i < (int) second.body.size(); i++) {
        if (i >= (int)a.body.size()) {
            a.body.push_back(a.sign == 1 ? std::numeric_limits<unsigned>::max() : 0);
        }

        switch (operation) {
            case 1 :
                a.body[i] &= second.body[i];
                a.sign &= b.sign;
                break;
            case 2 :
                a.body[i] ^= second.body[i];
                a.sign ^= b.sign;
                break;
            case 3 :
                a.body[i] |= second.body[i];
                a.sign |= b.sign;
                break;
            default:
                break;
        }
    }

    if (a.sign) {
        a.body.push_back(std::numeric_limits<unsigned>::max());
        a = double_code(a + 1);
    }

    normalize(a);
    return a;
}

big_integer& big_integer::operator&=(big_integer const& b) {
    return make_binary_op(*this, b, 1);
}

big_integer& big_integer::operator^=(big_integer const& b) {
    return make_binary_op(*this, b, 2);
}

big_integer& big_integer::operator|=(big_integer const& b) {
    return make_binary_op(*this, b, 3);
}

big_integer& big_integer::operator<<=(int b) {
    if (b < 0) {
        *this >>= (-b);
        return *this;
    }

    size_t count = b / 32;
    int ost = b % 32;
    unsigned int last  = 0;

    if (ost) {
        for (int i = 0; i < (int) body.size(); ++i) {
            unsigned long long now = (unsigned long long) body[i] << ost;
            body[i] = castToUi(now % SHIFTED_LENGTH + last);

            if (now >= SHIFTED_LENGTH) {
                now >>= 32;
                last = castToUi(now);
            } else {
                break;
            }
        }
    }

    size_t k = body.size();
    body.resize(k + count);

    for (size_t i = 0; i < count; i++) {
        body[i + k] = body[i];
    }

    for (size_t i = 0; i < count; i++) {
        body[i]= 0;
    }

    if (body.back() == 0 && body.size() == 1) {
        sign = 0;
    }

    return *this;
}

big_integer& big_integer::operator>>=(int b) {
    if (b < 0) {
        *this <<= (-b);
        return *this;
    }

    if (*this < 0) {
        *this = double_code(*this);
        --*this;
    }

    size_t count = b / 32;
    int ost = b % 32;

    unsigned long long last  = 0;
    size_t k = body.size();

    if (k < count) {
        *this = big_integer();
        return *this;
    }

    if (count > 0) {
        for (int i = count; i < (int) body.size(); i++) {

            body[i - count] = body[i];
            body[i] = 0;
        }
    }

    if (ost) {
        ui M = castToUi(1 << ost);

        for (int i = (int) body.size() - count - 1; i >= 0; --i) {
            unsigned long long f = body[i] + (last << 32);
            last = f % M;
            body[i] = castToUi(f >> ost);
        }

        if (sign == 1 ) {
            body[(int) body.size() - count - 1] += (M - 1) * (1 << (32 - ost));
        }
    }

    *this = sign == 1 ? double_code(*this + 1) : *this;
    normalize(*this);
    return *this;
}

big_integer big_integer::operator+() const {
    return *this;
}

big_integer big_integer::operator-() const {
    big_integer a(*this);
    a.sign ^= 1;
    return a;
}

big_integer big_integer::operator~() const {
    big_integer a(*this + 1);
    a.sign ^= 1;
    return a;
}

std::string to_string(big_integer const& a) {
    big_integer aa = a, nul(0);
    int module = 1000000;
    std::string s;

    while(aa != nul) {
        big_integer M = aa % module;
        int m = M.body[0];
        std::string ss;

        while (m > 0) {
            ss += (char) (m % 10 + 48);
            m /= 10;
        }

        aa /= big_integer(module);

        if (aa != nul) {
            for (size_t i = ss.length(); i < 6; i++) {
                ss += "0";
            }
        }

        s += ss;
    }

    reverse(s.begin(), s.end());

    if (a.sign == 1) {
        if (s.empty()) {
            return "0";
        }

        s = "-" + s;
    }

    return s.empty() ? "0" : s;
}

std::istream& operator>>(std::istream& s, big_integer& a) {
    std::string str;
    s >> str;
    a = big_integer(str);
    return s;
}

std::ostream& operator<<(std::ostream& s, big_integer const& a) {
    s << to_string(a);
    return s;
}