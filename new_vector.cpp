//
// Created by Nikitos on 19.05.18.
//

#include "new_vector.h"
#include <iostream>
#include <cassert>

opt_vector::opt_vector() : small_number(0), opt_size(0), is_big(false) {}

opt_vector::opt_vector(const opt_vector& other) : opt_size(other.opt_size),
                                                  is_big(other.is_big)
{
    if (!other.is_big) {
        small_number = other.small_number;
    } else {
        other.big_number->link_counter++;
        big_number = other.big_number;
    }
}

opt_vector::opt_vector(size_t sz, unsigned val) : opt_size(sz) {
    if (sz > 1) {
        is_big = true;
        big_number = new my_vector();
        big_number->data.push_back(val);
        big_number->data.resize(sz);
        big_number->link_counter = 1;
    } else {
        is_big = false;
        small_number = val;
    }
}

void opt_vector::safe_delete() {
    if (big_number->link_counter > 1) {
        big_number->link_counter--;
    } else {
        delete big_number;
    }
}

void opt_vector::make_alone() {
    if (is_big && big_number -> link_counter > 1) {
        my_vector* new_vector = new my_vector();
        big_number->link_counter--;
        new_vector->data = big_number->data;
        new_vector->link_counter = 1;
        std::swap(new_vector, big_number);
    }
}

opt_vector::~opt_vector() {
    if (is_big && big_number -> link_counter > 0) {
        safe_delete();
    }
}

void swap(opt_vector& a, opt_vector& b) {
    swap(a.is_big, b.is_big);
    swap(a.opt_size, b.opt_size);
    swap(a.big_number, b.big_number);
}

opt_vector &opt_vector::operator=(opt_vector const &other) {
    opt_vector r(other);
    swap(*this, r);
    return *this;
}

unsigned& opt_vector::operator[](size_t index) {
    assert(index<opt_size);
    make_alone();

    if (is_big) {
        return big_number->data[index];
    } else {
        return small_number;
    }
}

unsigned const& opt_vector::operator[](size_t index) const {
    assert(index<opt_size);

    if (is_big) {
        return big_number->data[index];
    } else {
        return small_number;
    }
}

size_t opt_vector::size() const {
    return opt_size;
}

unsigned &opt_vector::back() {
    make_alone();

    if (is_big) {
        return big_number->data.back();
    } else {
        return small_number;
    }
}

unsigned const &opt_vector::back() const  {
    return (is_big) ? big_number->data.back() : small_number;
}

void opt_vector::resize(size_t new_size) {
    if (is_big) {
        make_alone();
        big_number -> data.resize(new_size);
    } else if (new_size > 1) {
        unsigned temp = small_number;
        big_number = new my_vector();
        big_number -> data.resize(new_size, 0);
        big_number -> link_counter = 1;
        if (opt_size != 0)
            big_number -> data[0] = temp;
        is_big = true;
    } else if (opt_size == 0) {
        small_number = 0;
    }

    opt_size = new_size;
}


void opt_vector::push_back(unsigned a) {
    if (is_big) {
        make_alone();
        big_number -> data.push_back(a);
    } else if (opt_size == 1) {
        unsigned temp = small_number;
        big_number = new my_vector();
        big_number -> data.push_back(temp);
        big_number -> data.push_back(a);
        big_number->link_counter = 1;
        is_big = true;
    } else {
        small_number = a;
    }

    opt_size++;
}

void opt_vector::pop_back() {
    assert(opt_size == 0);

    if (is_big) {
        make_alone();
        big_number -> data.pop_back();
    }

    opt_size--;
}


void opt_vector::clear() {
    if (is_big) {
        make_alone();
        big_number -> data.clear();
    }

    opt_size = 0;
}

void opt_vector::push_front(unsigned x) {
    if (!is_big) {
        make_alone();
        push_back(x);
        all_reverse();
    } else {
        big_number->data.insert(big_number->data.begin(), x);
        opt_size++;
    }
}

void opt_vector::all_reverse() {
    if (is_big) {
        make_alone();
        std::reverse(big_number->data.begin(), big_number->data.end());
    }
}

bool opt_vector::empty() {
    return opt_size == 0;
}